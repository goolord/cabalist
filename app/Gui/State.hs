{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

-- | Application state shared between the UI thread and worker threads.
--
-- Workers change the state with 'modifyState', which writes atomically and
-- then wakes the event loop so the next frame shows the change. The view only
-- reads the state and queues work; it never waits on git, cabal or Hackage.
--
-- Release steps run one at a time, in the order they were queued, on a single
-- worker: two cabal builds at once would fight over the store, and a batch
-- must upload a package's dependencies before the package itself.
module Gui.State
  ( Env (..)
  , AppState (..)
  , Repo (..)
  , Action (..)
  , actionLabel
  , Job (..)
  , JobStatus (..)
  , jobFinished
  , newEnv
  , readState
  , modifyState
  , openRepo
  , refreshRepo
  , refreshHackage
  , enqueue
  , cancelJob
  , cancelAll
  , clearFinished
  , saveSettings
  , UserSettings (..)
  , userSettingsFile
  , saveUserSettings
  , uiScales
  , setCredentials
  , repoStage
  , repoStatusOf
  , lastRepo
  , tshow
  )
where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Exception (SomeException, displayException, fromException, try)
import Control.Monad (forM, forM_, forever, join, unless, void, when)
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Cabalist.File (readUtf8, writeUtf8)
import GHC.Clock (getMonotonicTime)
import Cabalist.Config
import Cabalist.Git (currentBranch, gitTopLevel)
import Cabalist.Hackage
import Cabalist.Keyring (keyringName, loadLogin, saveLogin)
import Cabalist.Package
import Cabalist.Process
import Cabalist.Release
import Cabalist.Status
import Cabalist.Version (Bump, bumpLabel)
import System.Directory
import System.FilePath (takeDirectory, (</>))
import System.Process (ProcessHandle, terminateProcess)
import Text.Read (readMaybe)

-- | A release step, as the view offers it.
data Action
  = ActCheck
  | ActTagDist
  | ActBuild
  | ActUpload
  | ActUploadDocs
  | ActPublish
  | ActPublishDocs
  | ActBump !Bump !Bool
  -- ^ The component to bump, and whether to commit the bump.
  | ActCommitBump
  deriving (Eq, Show)

-- | What a step is called on its button and in the activity log alike.
actionLabel :: Action -> Text
actionLabel = \case
  ActCheck -> "Check package"
  ActTagDist -> "Tag and build"
  ActBuild -> "Rebuild from tarball"
  ActUpload -> "Upload candidate"
  ActUploadDocs -> "Upload candidate docs"
  ActPublish -> "Publish"
  ActPublishDocs -> "Publish docs"
  ActBump b _ -> "Bump " <> T.toLower (T.takeWhile (/= ' ') (bumpLabel b)) <> " version"
  ActCommitBump -> "Commit version bump"

data JobStatus
  = JobQueued
  | JobRunning
  | JobSucceeded
  | JobFailed !Text
  | JobCancelled !Text
  deriving (Eq, Show)

jobFinished :: JobStatus -> Bool
jobFinished = \case
  JobQueued -> False
  JobRunning -> False
  _ -> True

data Job = Job
  { jobId :: !Int
  , jobRoot :: !FilePath
  -- ^ The repository the job was queued in.
  , jobPackage :: !Package
  , jobAction :: !Action
  , jobBatch :: !Int
  -- ^ Jobs queued together. When one fails, the rest of its batch is
  -- skipped: there is no point uploading a package whose dependency failed.
  , jobOptions :: !Options
  , jobStatus :: !JobStatus
  , jobLog :: !(Seq Text)
  , jobStarted :: !(Maybe Double)
  , jobFinishedAt :: !(Maybe Double)
  }

-- | An open repository.
data Repo = Repo
  { repoRoot :: !FilePath
  , repoPackages :: ![Package]
  , repoBroken :: ![(FilePath, Text)]
  -- ^ .cabal files that could not be read.
  , repoConfig :: !Config
  , repoStatus :: !(Map Text PkgStatus)
  , repoHackage :: !(Map Text HackageInfo)
  , repoReleasedAt :: !(Map Text Text)
  -- ^ When Hackage got each released version, by package id.
  , repoBranch :: !(Maybe Text)
  }

data AppState = AppState
  { stRepo :: !(Maybe Repo)
  , stRevision :: !Int
  -- ^ Bumped whenever the repository's packages or statuses change.
  , stLoading :: !(Maybe Text)
  , stError :: !(Maybe Text)
  , stHackageLoading :: !Bool
  , stJobs :: !(Seq Job)
  , stNextJob :: !Int
  , stNextBatch :: !Int
  , stCabal :: !(Maybe (FilePath, Version))
  , stCabalLogin :: !(Maybe Text)
  -- ^ The kind of Hackage login cabal's config file holds, if any.
  , stCredentials :: !Credentials
  , stLoginSaved :: !Bool
  -- ^ Whether 'stCredentials' is the login saved in the keyring.
  , stLoginChosen :: !Bool
  -- ^ Whether a login was chosen in the dialog, which a saved login read
  -- afterwards must not replace.
  , stLoginError :: !(Maybe Text)
  -- ^ Why the keyring could not be read or written.
  , stDryRun :: !Bool
  , stOptions :: !Options
  -- ^ How steps build: hlint, the pristine build, sibling packages.
  , stUiScale :: !Float
  -- ^ The zoom of the whole window; zero follows the display's scaling.
  }

data Env = Env
  { envState :: !(IORef AppState)
  , envWake :: !(IORef (IO ()))
  -- ^ Wakes the event loop; installed by the view on its first frame.
  , envQueue :: !(Chan Int)
  , envCancelled :: !(IORef (Set Int))
  , envRunning :: !(IORef (Maybe (Int, ProcessHandle)))
  , envSettingsFile :: !(Maybe FilePath)
  -- ^ Where the build settings and dry run are kept; none for self-tests.
  , envDryRunFlag :: !Bool
  -- ^ Whether @--dry-run@ was given, which forces a dry run without saving it.
  , envKeyring :: !Bool
  -- ^ Whether logins can be saved in the keyring: not in self-tests, which
  -- have no settings file either.
  , envKeyringLock :: !(MVar ())
  -- ^ Held while the keyring is read or written, so changes apply in order.
  }

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | A new environment, with the user's settings read from a settings file if
-- one is given; without one the UI keeps a scale of 1. A dry run is forced on
-- by the flag, else as saved.
newEnv :: Maybe FilePath -> Bool -> IO Env
newEnv file dryRunFlag = do
  saved <- maybe (pure defaultUserSettings {usUiScale = 1}) loadUserSettings file
  let dryRun = dryRunFlag || usDryRun saved
  env <-
    Env
      <$> newIORef
        AppState
          { stRepo = Nothing
          , stRevision = 0
          , stLoading = Nothing
          , stError = Nothing
          , stHackageLoading = False
          , stJobs = Seq.empty
          , stNextJob = 1
          , stNextBatch = 1
          , stCabal = Nothing
          , stCabalLogin = Nothing
          , stCredentials = FromCabalConfig
          , stLoginSaved = False
          , stLoginChosen = False
          , stLoginError = Nothing
          , stDryRun = dryRun
          , stOptions = usOptions saved
          , stUiScale = usUiScale saved
          }
      <*> newIORef (pure ())
      <*> newChan
      <*> newIORef Set.empty
      <*> newIORef Nothing
      <*> pure file
      <*> pure dryRunFlag
      <*> pure (isJust file)
      <*> newMVar ()
  void . forkIO $ detectCabal env
  when (envKeyring env) . void . forkIO $ loadSavedLogin env
  void . forkIO . forever $ readChan (envQueue env) >>= runJob env
  pure env

readState :: Env -> IO AppState
readState = readIORef . envState

notify :: Env -> IO ()
notify env = join (readIORef (envWake env))

modifyState :: Env -> (AppState -> AppState) -> IO ()
modifyState env f = atomicModifyIORef' (envState env) (\s -> (f s, ())) >> notify env

--------------------------------------------------------------------------------
-- Tools

-- | Find the newest cabal, and whether its config holds a Hackage login.
detectCabal :: Env -> IO ()
detectCabal env = do
  found <- findCabal
  login <- maybe (pure Nothing) (cabalConfigLogin . fst) found
  modifyState env (\s -> s {stCabal = found, stCabalLogin = login})

-- | What kind of Hackage login cabal's config file holds: cabal fills in
-- whatever an upload needs from there without asking.
cabalConfigLogin :: FilePath -> IO (Maybe Text)
cabalConfigLogin cabal = do
  r <- try (cabalConfigFile cabal >>= readUtf8)
  pure $ case r of
    Left (_ :: SomeException) -> Nothing
    Right src ->
      let fields = [T.toLower (T.strip k) | l <- T.lines src, let (k, v) = T.breakOn ":" (T.stripStart l), not (T.null v), not ("--" `T.isPrefixOf` T.stripStart l)]
          has k = k `elem` fields
       in if has "token"
            then Just "an API token"
            else
              if has "password-command"
                then Just "a password command"
                else if has "username" && has "password" then Just "a username and password" else Nothing

--------------------------------------------------------------------------------
-- The repository

-- | The repository cabalist opened last, remembered across runs.
lastRepoFile :: IO FilePath
lastRepoFile = (</> "last-repository") <$> getXdgDirectory XdgState "cabalist"

lastRepo :: IO (Maybe FilePath)
lastRepo = do
  file <- lastRepoFile
  r <- try (readUtf8 file)
  pure $ case r of
    Left (_ :: SomeException) -> Nothing
    Right t | not (T.null (T.strip t)) -> Just (T.unpack (T.strip t))
    _ -> Nothing

rememberRepo :: FilePath -> IO ()
rememberRepo root = void (try (lastRepoFile >>= \f -> createDirectoryIfMissing True (takeDirectory f) >> writeUtf8 f (T.pack root)) :: IO (Either SomeException ()))

-- | Open the repository containing a directory: find its packages, read
-- their statuses, then ask Hackage about them.
openRepo :: Env -> FilePath -> IO ()
openRepo env dir = void . forkIO $ do
  modifyState env (\s -> s {stLoading = Just "Opening repository…", stError = Nothing})
  top <- gitTopLevel dir
  case top of
    Nothing ->
      modifyState env (\s -> s {stLoading = Nothing, stError = Just (T.pack dir <> " is not in a git repository")})
    Just root -> do
      rememberRepo root
      (pkgs, broken) <- discoverPackages root
      cfg <- loadConfig root pkgs
      branch <- currentBranch root
      statuses <- statusesFor root cfg Map.empty Map.empty pkgs
      modifyState env $ \s ->
        s
          { stRepo = Just (Repo root pkgs broken cfg statuses Map.empty Map.empty branch)
          , stRevision = stRevision s + 1
          , stLoading = Nothing
          , stError = if null pkgs then Just "No .cabal files found in this repository." else Nothing
          }
      refreshHackage env (map pkgName pkgs)

-- | Every package's status, read in parallel: each is a handful of git calls.
statusesFor :: FilePath -> Config -> Map Text HackageInfo -> Map Text Text -> [Package] -> IO (Map Text PkgStatus)
statusesFor root cfg hackage releasedAt pkgs = do
  vars <- forM pkgs $ \p -> do
    v <- newEmptyMVar
    _ <- forkIO $ do
      r <- try (packageStatus root cfg (Map.findWithDefault HackageUnknown (pkgName p) hackage) (Map.lookup (pkgId p) releasedAt) p)
      putMVar v (either (\(_ :: SomeException) -> Nothing) Just r)
    pure (pkgName p, v)
  results <- forM vars $ \(n, v) -> fmap (n,) <$> takeMVar v
  pure (Map.fromList [r | Just r <- results])

-- | Read the packages and their statuses again, keeping what Hackage said.
refreshRepo :: Env -> IO ()
refreshRepo env = do
  st <- readState env
  forM_ (stRepo st) $ \repo -> do
    let root = repoRoot repo
    (pkgs, broken) <- discoverPackages root
    branch <- currentBranch root
    statuses <- statusesFor root (repoConfig repo) (repoHackage repo) (repoReleasedAt repo) pkgs
    modifyState env $ \s ->
      s
        { stRepo = fmap (\r -> if repoRoot r == root then r {repoPackages = pkgs, repoBroken = broken, repoStatus = withHackage r statuses, repoBranch = branch} else r) (stRepo s)
        , stRevision = stRevision s + 1
        }
  where
    -- Hackage may have answered while the statuses were being read; keep
    -- its latest answer rather than the one they were read with.
    withHackage r = Map.mapWithKey (\n ps -> ps {psHackage = Map.findWithDefault (psHackage ps) n (repoHackage r)})

-- | Ask Hackage about some packages, all at once, and update their statuses
-- as the answers come in. For a version Hackage has, ask when it was
-- uploaded too: without a tag, that is what tells what changed since.
refreshHackage :: Env -> [Text] -> IO ()
refreshHackage env names = void . forkIO $ do
  modifyState env (\s -> s {stHackageLoading = True})
  done <- forM names $ \n -> do
    v <- newEmptyMVar
    _ <- forkIO $ do
      info <- fetchHackageInfo n
      modifyState env $ \s ->
        s
          { stRepo = fmap (\r -> r {repoHackage = Map.insert n info (repoHackage r), repoStatus = Map.adjust (\ps -> ps {psHackage = info}) n (repoStatus r)}) (stRepo s)
          , stRevision = stRevision s + 1
          }
      st <- readState env
      let released =
            [ pkgId p
            | Just r <- [stRepo st]
            , p <- repoPackages r
            , pkgName p == n
            , HackageVersions normal deprecated <- [info]
            , pkgVersion p `elem` normal <> deprecated
            ]
      forM_ released $ \pid ->
        fetchUploadTime pid >>= mapM_ (\t -> modifyState env (\s -> s {stRepo = fmap (\r -> r {repoReleasedAt = Map.insert pid t (repoReleasedAt r)}) (stRepo s)}))
      putMVar v ()
    pure v
  mapM_ takeMVar done
  -- Statuses count commits since each release, now that its time is known.
  refreshRepo env
  modifyState env (\s -> s {stHackageLoading = False})

repoStatusOf :: Repo -> Package -> Maybe PkgStatus
repoStatusOf repo p = Map.lookup (pkgName p) (repoStatus repo)

repoStage :: Repo -> Package -> Maybe Stage
repoStage repo p = stage p <$> repoStatusOf repo p

saveSettings :: Env -> Config -> IO ()
saveSettings env cfg = do
  st <- readState env
  forM_ (stRepo st) $ \repo -> do
    saveConfig (repoRoot repo) cfg
    modifyState env (\s -> s {stRepo = fmap (\r -> r {repoConfig = cfg}) (stRepo s)})
    void (forkIO (refreshRepo env))

--------------------------------------------------------------------------------
-- Build settings

-- | The settings kept for the user across repositories.
data UserSettings = UserSettings
  { usOptions :: !Options
  , usDryRun :: !Bool
  , usUiScale :: !Float
  -- ^ Zero follows the display's scaling.
  }

defaultUserSettings :: UserSettings
defaultUserSettings = UserSettings defaultOptions False 0

-- | The UI scales the settings offer, with their labels.
uiScales :: [(Float, Text)]
uiScales = (0, "Follow the display") : [(s, tshow (round (s * 100) :: Int) <> "%") | s <- [0.75, 1, 1.25, 1.5, 1.75, 2]]

userSettingsFile :: IO FilePath
userSettingsFile = (</> "settings") <$> getXdgDirectory XdgConfig "cabalist"

-- | The saved user settings, or the defaults.
loadUserSettings :: FilePath -> IO UserSettings
loadUserSettings file = do
  r <- try (readUtf8 file)
  pure $ case r of
    Left (_ :: SomeException) -> defaultUserSettings
    Right src -> foldl apply defaultUserSettings (T.lines src)
  where
    apply us l = case T.breakOn ":" l of
      (k, v) -> case (T.strip k, T.strip (T.drop 1 v)) of
        ("hlint", flag -> Just b) -> us {usOptions = (usOptions us) {optHlint = b}}
        ("build", flag -> Just b) -> us {usOptions = (usOptions us) {optBuild = b}}
        ("siblings", flag -> Just b) -> us {usOptions = (usOptions us) {optSiblings = b}}
        ("dry-run", flag -> Just b) -> us {usDryRun = b}
        ("ui-scale", readMaybe . T.unpack -> Just s) | s >= 0 && s <= 4 -> us {usUiScale = s}
        _ -> us
    flag = \case
      "true" -> Just True
      "false" -> Just False
      _ -> Nothing

-- | Use these user settings, and save them. A dry run forced by
-- @--dry-run@ is not saved: the file keeps what it said.
saveUserSettings :: Env -> UserSettings -> IO ()
saveUserSettings env us = do
  let opts = usOptions us
      dry = usDryRun us
  modifyState env $ \s ->
    s
      { stOptions = (stOptions s) {optHlint = optHlint opts, optBuild = optBuild opts, optSiblings = optSiblings opts}
      , stDryRun = dry
      , stUiScale = usUiScale us
      }
  forM_ (envSettingsFile env) $ \file -> do
    savedDry <-
      if envDryRunFlag env && dry
        then usDryRun <$> loadUserSettings file
        else pure dry
    void . (try :: IO () -> IO (Either SomeException ())) $ do
      createDirectoryIfMissing True (takeDirectory file)
      writeUtf8 file . T.unlines $
        [ "-- cabalist settings"
        , "hlint: " <> bool (optHlint opts)
        , "build: " <> bool (optBuild opts)
        , "siblings: " <> bool (optSiblings opts)
        , "dry-run: " <> bool savedDry
        , "ui-scale: " <> tshow (usUiScale us)
        ]
  where
    bool b = if b then "true" else "false"

-- | Use a login, and save it in the keyring or forget the saved one. The
-- keyring is asked in the background: it can show a dialog of its own.
setCredentials :: Env -> Credentials -> Bool -> IO ()
setCredentials env creds remember = do
  modifyState env (\s -> s {stCredentials = creds, stLoginChosen = True})
  when (envKeyring env) . void . forkIO . withMVar (envKeyringLock env) $ \() -> do
    saved <- stLoginSaved <$> readState env
    let keep = remember && creds /= FromCabalConfig
    unless (not keep && not saved) $ do
      r <- saveLogin (if keep then creds else FromCabalConfig)
      modifyState env $ \s -> case r of
        Right () -> s {stLoginSaved = keep, stLoginError = Nothing}
        Left e ->
          s
            { stLoginSaved = saved
            , stLoginError = Just ((if keep then "Could not save the login in " else "Could not remove the login from ") <> keyringName <> ": " <> e)
            }

-- | Use the login saved in the keyring, unless one was chosen meanwhile.
loadSavedLogin :: Env -> IO ()
loadSavedLogin env =
  withMVar (envKeyringLock env) $ \() ->
    loadLogin >>= \case
      Right Nothing -> pure ()
      Right (Just creds) -> modifyState env $ \s ->
        if stLoginChosen s then s else s {stCredentials = creds, stLoginSaved = True}
      Left e -> modifyState env (\s -> s {stLoginError = Just ("Could not read the saved login from " <> keyringName <> ": " <> e)})

--------------------------------------------------------------------------------
-- Jobs

-- | Queue steps to run one after another, as one batch.
enqueue :: Env -> Bool -> [(Package, Action)] -> IO ()
enqueue env force steps = unless (null steps) $ do
  ids <- atomicModifyIORef' (envState env) $ \s ->
    let first = stNextJob s
        batch = stNextBatch s
        opts' = (stOptions s) {optForce = force, optCredentials = stCredentials s, optDryRun = stDryRun s}
        jobs = [Job i root p a batch opts' JobQueued Seq.empty Nothing Nothing | (i, (p, a)) <- zip [first ..] steps]
        root = maybe "" repoRoot (stRepo s)
     in ( s {stJobs = stJobs s <> Seq.fromList jobs, stNextJob = first + length steps, stNextBatch = batch + 1}
        , map jobId jobs
        )
  notify env
  mapM_ (writeChan (envQueue env)) ids

updateJob :: Env -> Int -> (Job -> Job) -> IO ()
updateJob env jid f = modifyState env $ \s -> s {stJobs = fmap (\j -> if jobId j == jid then f j else j) (stJobs s)}

findJob :: AppState -> Int -> Maybe Job
findJob s jid = case Seq.findIndexL ((== jid) . jobId) (stJobs s) of
  Just i -> Seq.lookup i (stJobs s)
  Nothing -> Nothing

-- | Cancel a job: skip it if it is waiting, or stop its command if it runs.
cancelJob :: Env -> Int -> IO ()
cancelJob env jid = do
  atomicModifyIORef' (envCancelled env) (\c -> (Set.insert jid c, ()))
  running <- readIORef (envRunning env)
  case running of
    Just (rid, ph) | rid == jid -> void (try (terminateProcess ph) :: IO (Either SomeException ()))
    _ -> pure ()
  st <- readState env
  case findJob st jid of
    -- A skipped step skips the rest of its batch, as a failed one does: the
    -- steps after it may be releasing packages that depend on it.
    Just j | jobStatus j == JobQueued -> modifyState env $ \s ->
      s
        { stJobs =
            fmap
              ( \x ->
                  if
                    | jobId x == jid -> x {jobStatus = JobCancelled "Skipped"}
                    | jobBatch x == jobBatch j && jobStatus x == JobQueued && jobId x > jid ->
                        x {jobStatus = JobCancelled ("Skipped: " <> actionLabel (jobAction j) <> " of " <> pkgName (jobPackage j) <> " was skipped")}
                    | otherwise -> x
              )
              (stJobs s)
        }
    _ -> notify env

cancelAll :: Env -> IO ()
cancelAll env = do
  st <- readState env
  forM_ (toList (stJobs st)) $ \j -> unless (jobFinished (jobStatus j)) (cancelJob env (jobId j))

clearFinished :: Env -> IO ()
clearFinished env = modifyState env (\s -> s {stJobs = Seq.filter (not . jobFinished . jobStatus) (stJobs s)})

runJob :: Env -> Int -> IO ()
runJob env jid = do
  st <- readState env
  cancelled <- Set.member jid <$> readIORef (envCancelled env)
  case (findJob st jid, stRepo st) of
    (Just job, Just repo) | jobStatus job == JobQueued, not cancelled -> do
      t0 <- getMonotonicTime
      updateJob env jid (\j -> j {jobStatus = JobRunning, jobStarted = Just t0})
      result <- try (runAction env repo st job)
      t1 <- getMonotonicTime
      writeIORef (envRunning env) Nothing
      let status = case result of
            Right () -> JobSucceeded
            Left e -> case fromException e of
              Just (StepFailed msg) -> JobFailed msg
              Just StepCancelled -> JobCancelled "Cancelled"
              Nothing -> JobFailed (T.pack (displayException e))
      updateJob env jid $ \j ->
        j
          { jobStatus = status
          , jobFinishedAt = Just t1
          , jobLog = jobLog j |> summary status
          }
      -- A failed step skips the rest of its batch.
      unless (status == JobSucceeded) $
        modifyState env $ \s ->
          s
            { stJobs =
                fmap
                  ( \j ->
                      if jobBatch j == jobBatch job && jobStatus j == JobQueued
                        then j {jobStatus = JobCancelled ("Skipped: " <> actionLabel (jobAction job) <> " of " <> pkgName (jobPackage job) <> " did not finish")}
                        else j
                  )
                  (stJobs s)
            }
      refreshRepo env
      when (status == JobSucceeded && jobAction job `elem` [ActPublish, ActUpload]) $
        refreshHackage env [pkgName (jobPackage job)]
    _ -> pure ()
  where
    summary = \case
      JobSucceeded -> "✓ Done"
      JobFailed msg -> "✗ Failed: " <> msg
      JobCancelled msg -> "✗ " <> msg
      _ -> ""

-- | Run one step. The package is read again from disk first: an earlier step
-- of the batch may have bumped its version.
runAction :: Env -> Repo -> AppState -> Job -> IO ()
runAction env repo st job = do
  when (repoRoot repo /= jobRoot job) $
    failStep ("queued in " <> T.pack (jobRoot job) <> ", but " <> T.pack (repoRoot repo) <> " is open now; open it again to run this step")
  let root = repoRoot repo
      cfg = repoConfig repo
      logger =
        Logger
          { logLine = \l -> updateJob env (jobId job) (\j -> j {jobLog = jobLog j |> l})
          , logProcess = \mph -> writeIORef (envRunning env) (fmap (jobId job,) mph)
          , logCancelled = Set.member (jobId job) <$> readIORef (envCancelled env)
          }
  p <- either (\e -> failStep ("could not read " <> T.pack (pkgCabalFile (jobPackage job)) <> ": " <> e)) pure
    =<< readPackage root (pkgCabalFile (jobPackage job))
  (pkgs, _) <- discoverPackages root
  let hackage = Map.findWithDefault HackageUnknown (pkgName p) (repoHackage repo)
  status <- packageStatus root cfg hackage (Map.lookup (pkgId p) (repoReleasedAt repo)) p
  cabal <- case stCabal st of
    Just (exe, _) -> pure exe
    Nothing -> failStep "cabal was not found on the PATH"
  let unpublished = [pkgName q | q <- pkgs, Map.lookup (pkgName q) (repoHackage repo) == Just HackageAbsent]
      ctx = Ctx root cfg pkgs (jobOptions job) logger status unpublished cabal
      say = logLine logger
  say ("# " <> actionLabel (jobAction job) <> ": " <> pkgId p <> (if optDryRun (jobOptions job) then " (dry run: nothing is uploaded or pushed)" else ""))
  case jobAction job of
    ActCheck -> cabalCheck ctx p
    ActTagDist -> tagDist ctx p
    ActBuild -> pristineBuild ctx p
    ActUpload -> upload ctx p
    ActUploadDocs -> uploadDocs ctx p False
    ActPublish -> publish ctx p
    ActPublishDocs -> uploadDocs ctx p True
    ActBump b commit -> do
      new <- bumpPackage ctx p b
      when commit $ commitBump ctx p new
    ActCommitBump -> commitBump ctx p (pkgVersion p)
