{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

-- | The main window.
--
-- A release is shown as a track of four stops (version, tag, candidate,
-- published) with one button for the next step, and beside it buttons to
-- redo the tag or update the candidate once there is a tag. Everything else
-- a release can need (checks, rebuilds, documentation) waits in a menu, and
-- build settings live in the settings dialog. Work runs in the background; a
-- one-line activity bar reports it, and opens onto the log.
module Gui.View
  ( ViewCache
  , newViewCache
  , appView
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, join, unless, void, when)
import Data.Char (chr)
import Data.Foldable (toList)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find, findIndex)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import Cabalist.Config
import Cabalist.Hackage
import Cabalist.Keyring (keyringName)
import Cabalist.Package
import Cabalist.Release (Credentials (..), Options (..))
import Cabalist.Status
import Cabalist.Version (Bump (..), bumpLabel, bumpVersion)
import GHC.Clock (getMonotonicTime)
import Gui.State
import Gui.Style
import NanoUI
import NanoUI.Backend.Sdl (FileDialogId, FileDialogResult (..), askOpenFolderDialog, defaultFileDialogOptions, pollFileDialogUi, setSdlUiScale)
import NanoUI.Context (Context (..))
import NanoUI.Monad (askContext, askInput)
import NanoUI.Testing (UiCursorKind (..), textFieldActive)
import System.FilePath (takeFileName, (</>))
import System.Info (os)
import System.Process (spawnProcess)

--------------------------------------------------------------------------------
-- Caches kept between frames

data ViewCache = ViewCache
  { vcFolderDialog :: !(IORef (Maybe FileDialogId))
  , vcInitialPath :: !FilePath
  , vcOpened :: !(IORef Bool)
  -- ^ Whether the initial repository has been opened yet.
  , vcFailSeen :: !(IORef Int)
  -- ^ The newest failed job the log has been opened for.
  , vcAutoJob :: !(IORef (Maybe Int))
  -- ^ A failed job the log was turned to without being asked, which it
  -- leaves once newer work starts.
  }

newViewCache :: FilePath -> IO ViewCache
newViewCache initial = ViewCache <$> newIORef Nothing <*> pure initial <*> newIORef False <*> newIORef 0 <*> newIORef Nothing

--------------------------------------------------------------------------------
-- One frame's inputs

-- | A piece of the view's own state: its value this frame, and its setter.
data Var a = Var
  { val :: !a
  , put :: a -> NanoUI ()
  }

var :: NanoUI (a, a -> NanoUI ()) -> NanoUI (Var a)
var hook = uncurry Var <$> hook

-- | Show a control for a variable and keep what it returns.
bind :: Eq a => Var a -> (a -> NanoUI a) -> NanoUI ()
bind v control = do
  new <- control (val v)
  when (new /= val v) (put v new)

-- | A release the user must confirm.
data Pending = Pending
  { pendingTitle :: !Text
  , pendingSteps :: ![(Package, Action)]
  }
  deriving (Eq)

data Vars = Vars
  { selected :: !(Var (Maybe Text))
  -- ^ The package whose release shows.
  , selecting :: !(Var Bool)
  -- ^ Whether the list shows checkboxes, for releasing several at once.
  , checked :: !(Var (Set Text))
  , selectedJob :: !(Var (Maybe Int))
  , logOpen :: !(Var Bool)
  , moreOpen :: !(Var Bool)
  , detailsOpen :: !(Var Bool)
  , bumpFor :: !(Var (Maybe Text))
  , bumpChoice :: !(Var Int)
  , bumpCommit :: !(Var Bool)
  , pending :: !(Var (Maybe Pending))
  , loginOpen :: !(Var Bool)
  , loginMode :: !(Var Int)
  , loginUser :: !(Var Text)
  , loginPass :: !(Var Text)
  , loginToken :: !(Var Text)
  , loginRemember :: !(Var Bool)
  , settingsOpen :: !(Var Bool)
  , formatDraft :: !(Var Text)
  , remoteDraft :: !(Var Text)
  , hlintDraft :: !(Var Bool)
  , buildDraft :: !(Var Bool)
  , siblingsDraft :: !(Var Bool)
  , dryRunDraft :: !(Var Bool)
  , scaleDraft :: !(Var Int)
  -- ^ An index into 'uiScales'.
  , logSticky :: !(Var Bool)
  , logPrevY :: !(Var Float)
  , logWidest :: !(Var (Int, Int, Text))
  -- ^ The job, how many of its lines have been looked at, and the longest of
  -- them, tabs expanded. Logs only grow, so each line is looked at once.
  }

useVars :: NanoUI Vars
useVars = do
  selected <- var (useState Nothing)
  selecting <- var (useFlag False)
  checked <- var (useState Set.empty)
  selectedJob <- var (useState Nothing)
  logOpen <- var (useFlag False)
  moreOpen <- var (useFlag False)
  detailsOpen <- var (useFlag False)
  bumpFor <- var (useState Nothing)
  bumpChoice <- var (useInt (fromEnum BumpD))
  bumpCommit <- var (useFlag True)
  pending <- var (useState Nothing)
  loginOpen <- var (useFlag False)
  loginMode <- var (useInt 0)
  loginUser <- var (useText "")
  loginPass <- var (useText "")
  loginToken <- var (useText "")
  loginRemember <- var (useFlag False)
  settingsOpen <- var (useFlag False)
  formatDraft <- var (useText "")
  remoteDraft <- var (useText "")
  hlintDraft <- var (useFlag False)
  buildDraft <- var (useFlag False)
  siblingsDraft <- var (useFlag False)
  dryRunDraft <- var (useFlag False)
  scaleDraft <- var (useInt 0)
  logSticky <- var (useFlag True)
  logPrevY <- var (useFloat 0)
  logWidest <- var (useState (-1, 0, ""))
  pure Vars {..}

data Frame = Frame
  { env :: Env
  , cache :: ViewCache
  , ctx :: Context
  , inp :: Input
  , st :: AppState
  , vars :: Vars
  , repo :: Maybe Repo
  , packages :: [Package]
  , current :: Maybe Package
  -- ^ The selected package, or the first.
  , jobs :: [Job]
  , job :: Maybe Job
  -- ^ The job the log shows: the one picked, or the one running, or the newest.
  , anyModal :: Bool
  , now :: Double
  }

prepareFrame :: Env -> ViewCache -> Context -> Input -> AppState -> Vars -> IO Frame
prepareFrame env cache ctx inp st vars@Vars {..} = do
  t <- getMonotonicTime
  let repo = stRepo st
      packages = maybe [] repoPackages repo
      jobs = toList (stJobs st)
  pure
    Frame
      { current = case val selected >>= \n -> find ((== n) . pkgName) packages of
          Just p -> Just p
          Nothing -> listToMaybe packages
      , job = case val selectedJob >>= \i -> find ((== i) . jobId) jobs of
          Just j -> Just j
          Nothing -> case find ((== JobRunning) . jobStatus) jobs of
            Just j -> Just j
            Nothing -> listToMaybe (reverse jobs)
      , anyModal = isJust (val bumpFor) || isJust (val pending) || val loginOpen || val settingsOpen
      , now = t
      , ..
      }

--------------------------------------------------------------------------------
-- The window

appView :: Env -> ViewCache -> NanoUI ()
appView env cache = do
  ctx <- askContext
  inp <- askInput
  uiIO $ readIORef (ctxWakeLoop ctx) >>= mapM_ (writeIORef (envWake env))
  opened <- uiIO (readIORef (vcOpened cache))
  unless opened $ uiIO $ do
    writeIORef (vcOpened cache) True
    unless (null (vcInitialPath cache)) $ openRepo env (vcInitialPath cache)
  st <- uiIO (readState env)
  vars <- useVars
  frame <- uiIO (prepareFrame env cache ctx inp st vars)
  openLogOnFailure frame
  setSdlUiScale (stUiScale st)

  columnWith (fillW . fillH . gap 0 . tight) $ do
    header frame
    separator
    rowWith (fillW . fillH . gap 0 . tight) $ do
      surface (palWell palette) (palWell palette) 0 (fixedW listW . fillH . tight) $
        columnWith (fillW . fillH . gap 0 . tight) $ do
          packageList frame
          batchBar frame
      separator
      columnWith (fillW . fillH . gap 0 . tight) (releasePane frame)
    activity frame

  bumpDialog frame
  publishDialog frame
  loginDialog frame
  settingsDialog frame
  pollFolder frame
  keyboardShortcuts frame

listW :: Float
listW = 340

-- | Open the log on a job that failed, once: the failure is what to read.
openLogOnFailure :: Frame -> NanoUI ()
openLogOnFailure Frame {vars = Vars {..}, ..} = do
  seen <- uiIO (readIORef (vcFailSeen cache))
  let failed = [jobId j | j <- jobs, isFailure (jobStatus j)]
  case reverse failed of
    newest : _ | newest > seen -> do
      uiIO (writeIORef (vcFailSeen cache) newest >> writeIORef (vcAutoJob cache) (Just newest))
      put logOpen True
      put selectedJob (Just newest)
    _ -> do
      -- Once a newer step is under way, the log follows it again, unless the
      -- failed job was picked by hand.
      auto <- uiIO (readIORef (vcAutoJob cache))
      forM_ auto $ \a ->
        when (any (\j -> jobId j > a && not (jobFinished (jobStatus j))) jobs) $ do
          uiIO (writeIORef (vcAutoJob cache) Nothing)
          when (val selectedJob == Just a) (put selectedJob Nothing)
  where
    isFailure = \case
      JobFailed _ -> True
      _ -> False

--------------------------------------------------------------------------------
-- Header

-- | The repository's name and path, and what applies to all of it.
header :: Frame -> NanoUI ()
header frame@Frame {vars = Vars {..}, ..} =
  rowWith (fillW . fixedH 60 . padXY 20 0 . gap 12 . alignMid . tight) $ do
    case repo of
      Just r -> do
        labelWith (tight . alignMid . fontSize sizeTitle . fontSemiBold . ink palText) (T.pack (takeFileName (repoRoot r)))
        labelWith (tight . alignMid . fontSize sizeSmall . ink palQuiet) (T.pack (repoRoot r))
      Nothing -> labelWith (tight . alignMid . fontSize sizeTitle . fontSemiBold . ink palText) "cabalist"
    case (stLoading st, stError st) of
      (Just msg, _) -> spinnerWith alignMid 14 >> small palQuiet msg
      (_, Just err) -> small palCoral (ellipsize 90 err)
      _ -> when (stHackageLoading st) $ spinnerWith alignMid 14 >> small palQuiet "Asking Hackage"
    flex
    forM_ (stLoginError st) $ \e -> small palCoral (ellipsize 60 e)
    when (stDryRun st) $ small palAmber "Dry run"
    styled quiet $ do
      whenM (compactButton "Open…") (askFolder frame)
      disabledWhen (isNothing repo || isJust (stLoading st)) $
        whenM (compactButton "Refresh") (uiIO (refresh frame))
      whenM (compactButton (loginLabel st)) $ do
        -- Show the login in use, as saved or as typed earlier.
        case stCredentials st of
          UserPassword u pw -> put loginMode 1 >> put loginUser u >> put loginPass pw
          ApiToken tok -> put loginMode 2 >> put loginToken tok
          FromCabalConfig -> put loginMode 0
        put loginRemember (stLoginSaved st)
        put loginOpen True
      whenM (compactButton "Settings") $ do
        forM_ repo $ \r -> do
          put formatDraft (cfgTagFormat (repoConfig r))
          put remoteDraft (cfgRemote (repoConfig r))
        put hlintDraft (optHlint (stOptions st))
        put buildDraft (optBuild (stOptions st))
        put siblingsDraft (optSiblings (stOptions st))
        put dryRunDraft (stDryRun st)
        put scaleDraft (fromMaybe 0 (findIndex ((== stUiScale st) . fst) uiScales))
        put settingsOpen True

loginLabel :: AppState -> Text
loginLabel st = case (stCredentials st, stCabalLogin st) of
  (UserPassword u _, _) -> "Hackage: " <> u
  (ApiToken _, _) -> "Hackage: token"
  (FromCabalConfig, Just _) -> "Hackage: cabal config"
  (FromCabalConfig, Nothing) -> "Log in to Hackage"

askFolder :: Frame -> NanoUI ()
askFolder Frame {..} = do
  asked <- askOpenFolderDialog defaultFileDialogOptions
  forM_ asked $ \did -> uiIO (writeIORef (vcFolderDialog cache) (Just did))

-- | Read the repository and Hackage again.
refresh :: Frame -> IO ()
refresh Frame {..} = forM_ repo $ \r -> openRepo env (repoRoot r)

pollFolder :: Frame -> NanoUI ()
pollFolder Frame {..} = do
  asked <- uiIO (readIORef (vcFolderDialog cache))
  forM_ asked $ \did ->
    pollFileDialogUi did >>= \case
      FileDialogPending -> pure ()
      FileDialogSelected (path : _) -> uiIO (writeIORef (vcFolderDialog cache) Nothing >> openRepo env path)
      _ -> uiIO (writeIORef (vcFolderDialog cache) Nothing)

--------------------------------------------------------------------------------
-- Package list

packageList :: Frame -> NanoUI ()
packageList frame@Frame {vars = Vars {..}, ..} =
  columnWith (fillW . fillH . gap 0 . tight) $ do
    rowWith (fillW . fixedH 44 . padXY 16 0 . gap 4 . alignMid . tight) $
      if val selecting
        then styled quiet $ do
          whenM (compactButton "Select unreleased") (put checked (Set.fromList (map pkgName unreleased)))
          flex
          whenM (compactButton "Done") (put selecting False >> put checked Set.empty)
        else do
          labelWith (tight . alignMid . ink palMuted) (countOf (length packages) "package")
          flex
          styled quiet $ unless (null packages) $ whenM (compactButton "Select") (put selecting True)
    scrollWith (fillW . fillH . tight) $
      columnWith (fillW . gap 0 . tight) $
        forM_ packages $ \p -> withKey (pkgName p) (packageRow frame p)
  where
    -- Packages with a version to release, not waiting on a bump.
    unreleased = [p | p <- packages, Just s <- [repo >>= \r -> repoStage r p], s `elem` [StageReadyToTag, StageTagged, StageCandidate, StageUploaded]]

packageRow :: Frame -> Package -> NanoUI ()
packageRow Frame {vars = Vars {..}, ..} p =
  rowWith (fillW . fixedH 52 . gap 0 . alignMid . tight) $ do
    when (val selecting) $ do
      spacer (Fixed 12) Fit
      let isChecked = pkgName p `Set.member` val checked
      ticked <- rowWith (alignMid . tight) (checkbox "" isChecked)
      when (ticked /= isChecked) $
        put checked (if ticked then Set.insert (pkgName p) (val checked) else Set.delete (pkgName p) (val checked))
    clicked <- listRow isSelected (pkgName p) (showVersion (pkgVersion p)) note'
    when clicked $ put selected (Just (pkgName p)) >> put moreOpen False
  where
    isSelected = fmap pkgName current == Just (pkgName p)
    note' = case repo >>= \r -> repoStage r p of
      Nothing -> Nothing
      Just s -> Just (stageLabel s, stageColour s)

-- | The colour a stage is reported in: quiet once released, amber while the
-- release waits on the version, lavender while it is under way.
stageColour :: Stage -> Color
stageColour = \case
  StageReleased -> palQuiet palette
  StageNeedsBump -> palAmber palette
  StageCommitBump -> palAmber palette
  _ -> palPurpleInk palette

-- | A clickable row: the name and version, and under them the state in a
-- few words after a dot of its colour. Drawn as one widget, so a click
-- anywhere on it selects it.
listRow :: Bool -> Text -> Text -> Maybe (Text, Color) -> NanoUI Bool
listRow isSelected title version note' = do
  let noteText = maybe "" fst note'
      noteColour = maybe (palQuiet palette) snd note'
  (resp, ()) <-
    customWidget
      defaultCustomWidgetSpec
        { widgetLayout = (fillW . fixedH 52 . alignMid) defaultLayout
        , widgetContent = drawKey (T.intercalate "\0" [title, version, noteText]) [noteColour] [if isSelected then 1 else 0]
        , widgetDraw = \dc r -> runCanvas $ do
            let fm = cdcFont dc
                bg
                  | isSelected = palSelect palette
                  | cdcPressed dc = palPressed palette
                  | cdcHovered dc = palRaised palette
                  | otherwise = palWell palette
                x0 = rectX r + 16
                y1 = rectY r + 17
                y2 = rectY r + 36
            drawRect r bg
            when isSelected $ drawRect (Rect (rectX r) (rectY r) 3 (rectH r)) (palPurpleInk palette)
            drawText (V2 x0 y1) AlignStart AlignMiddle title (palText palette)
            unless (T.null version) $
              drawText (V2 (x0 + lineWidth fm title + 8) y1) AlignStart AlignMiddle version (palQuiet palette)
            unless (T.null noteText) $ do
              drawCircle (V2 (x0 + 3) y2) 3 noteColour
              drawText (V2 (x0 + 12) y2) AlignStart AlignMiddle noteText (palQuiet palette)
        , widgetCursor = Just (const UiCursorPointer)
        }
  pure (respClicked resp)

-- | Steps for every ticked package at once, in dependency order.
batchBar :: Frame -> NanoUI ()
batchBar Frame {vars = Vars {..}, ..} = do
  let ticked = [p | p <- packages, pkgName p `Set.member` val checked]
      ordered = releaseOrder packages ticked
      queue action = uiIO (enqueue env False [(p, action) | p <- ordered])
  when (val selecting && not (null ticked)) $ do
    separator
    columnWith (fillW . padXY 16 14 . gap 10 . tight) $ do
      labelWith (tight . fillW . ink palMuted) (countOf (length ticked) "package" <> ", dependencies first:")
      labelWith (tight . fillW . fontSize sizeSmall . ink palQuiet) (T.intercalate ", " (map pkgName ordered))
      rowWith (fillW . gap 8 . alignMid . tight) $ do
        whenM (compactButton "Tag and build") (queue ActTagDist)
        whenM (compactButton "Upload candidates") (queue ActUpload)
      styled primary $
        whenM (compactButton ("Publish " <> countOf (length ordered) "package" <> "…")) $
          put pending (Just (Pending ("Publish " <> countOf (length ordered) "package") [(p, ActPublish) | p <- ordered]))

--------------------------------------------------------------------------------
-- Release pane

releasePane :: Frame -> NanoUI ()
releasePane frame@Frame {..} =
  case (repo, current) of
    (Just r, Just p) ->
      scrollWith (fillW . fillH . tight) $
        columnWith (fillW . padXY 40 32 . gap 0 . tight) $
          packageRelease frame r p (repoStatusOf r p)
    (Nothing, _) -> emptyState frame
    (Just _, Nothing) ->
      columnWith (fillW . padXY 40 32 . gap 8 . tight) $ do
        labelWith (tight . fontSize sizeTitle . fontSemiBold . ink palText) "No packages here"
        labelWith (tight . ink palMuted) "cabalist lists every .cabal file git knows about. This repository has none."

emptyState :: Frame -> NanoUI ()
emptyState frame =
  columnWith (fillW . padXY 40 32 . gap 12 . tight) $ do
    labelWith (tight . fontSize sizeTitle . fontSemiBold . ink palText) "Open a repository"
    labelWith (tight . ink palMuted) "Every Cabal package in it is listed, including those in subdirectories."
    spacer Fit (Fixed 4)
    whenM (styled primary (actionButton "Open…")) (askFolder frame)

packageRelease :: Frame -> Repo -> Package -> Maybe PkgStatus -> NanoUI ()
packageRelease frame@Frame {vars = Vars {..}} r p mStatus = do
  rowWith (fillW . gap 12 . tight) $ do
    labelWith (tight . alignBaseline . fontSize sizeDisplay . fontSemiBold . ink palText) (pkgName p)
    labelWith (tight . alignBaseline . fontSize sizeTitle . ink palQuiet) (showVersion (pkgVersion p))
  unless (T.null (pkgSynopsis p)) $ do
    spacer Fit (Fixed 4)
    labelWith (tight . ink palMuted) (pkgSynopsis p)
  forM_ mStatus (hackageLinks p)
  spacer Fit (Fixed 28)
  case mStatus of
    Nothing -> rowWith (tight . gap 8 . alignMid) $ spinnerWith alignMid 16 >> small palQuiet "Reading the repository"
    Just s -> do
      let next = stage p s
          (at, colour) = trackPosition next
      releaseTrack ["Version", "Tag", "Candidate", "Published"] at colour
      spacer Fit (Fixed 16)
      labelWith (tight . fillW . ink palText) (hint next s)
      spacer Fit (Fixed 18)
      actions frame p s next
      let notes = warnings p s <> unreleasedDependencies r p
      unless (null notes) $ do
        spacer Fit (Fixed 20)
        columnWith (fillW . gap 6 . tight) $
          forM_ (zip [0 :: Int ..] notes) $ \(i, w) -> withKey i $
            labelWith (tight . fillW . ink palAmber) w
      spacer Fit (Fixed 32)
      toggled <- disclosure (val detailsOpen) "Details"
      when toggled (put detailsOpen (not (val detailsOpen)))
      when (val detailsOpen) $ do
        spacer Fit (Fixed 8)
        facts r p s

-- | Where a stage sits on the track, and the colour of that stop. Past the
-- last stop, the whole track is done.
trackPosition :: Stage -> (Int, Color)
trackPosition = \case
  StageNeedsBump -> (0, palAmber palette)
  StageCommitBump -> (0, palAmber palette)
  StageReadyToTag -> (1, palPurpleInk palette)
  StageTagged -> (1, palPurpleInk palette)
  StageCandidate -> (2, palPurpleInk palette)
  StageUploaded -> (3, palPurpleInk palette)
  StageReleased -> (4, palSage palette)

-- | What the release is waiting for, with what is known about it.
hint :: Stage -> PkgStatus -> Text
hint next s = case next of
  StageReleased
    | Just t <- psReleasedAt s -> "Released " <> T.takeWhile (/= 'T') t <> ". Nothing in the package has changed since."
  StageNeedsBump
    | newer -> countOf (length (psUntagged s)) "commit" <> " changed the package since this version was released. Bump the version to release again."
  StageCandidate
    | newer -> "The tarball was built before the latest commits. Update the candidate to tag them, rebuild, and upload, or upload it as it is."
  StageUploaded
    | newer -> "The candidate on Hackage predates the latest commits. Update it to include them, or publish it as it is."
  _ -> stageHint next
  where
    newer = not (null (psUntagged s))

-- | Links to what Hackage has of the package: the candidate while one is up
-- and unpublished, and this version once it is released, or else the
-- package's page if Hackage has any version of it.
hackageLinks :: Package -> PkgStatus -> NanoUI ()
hackageLinks p s =
  unless (null links) $ do
    spacer Fit (Fixed 8)
    clicked <-
      richTextWith (tight . fillW . fontSize sizeSmall) $
        concat
          [ [inlineWith (ink palQuiet) (name <> "  "), hyperlink url (T.drop (T.length "https://") url), "      "]
          | (name, url) <- links
          ]
    forM_ clicked (uiIO . openUrl)
  where
    published = isPublished p s
    links =
      [("Candidate", candidateUrl (pkgId p)) | psCandidateUploaded s, not published]
        <> [("Released", packageUrl (pkgId p)) | published]
        <> [("On Hackage", packageUrl (pkgName p)) | not published, onHackage]
    onHackage = case psHackage s of
      HackageVersions normal deprecated -> not (null (normal <> deprecated))
      _ -> False

unreleasedDependencies :: Repo -> Package -> [Text]
unreleasedDependencies r p =
  [ "Depends on " <> T.intercalate " and " deps <> ", which Hackage doesn't have yet. Release " <> (if length deps == 1 then "it" else "them") <> " first, or select them all and release them together."
  | not (null deps)
  ]
  where
    deps =
      [ pkgName q
      | q <- internalDeps (repoPackages r) p
      , Map.lookup (pkgName q) (repoHackage r) == Just HackageAbsent
      ]

-- | The next step as the one filled button, what else fits this moment
-- beside it, and everything else in a menu.
actions :: Frame -> Package -> PkgStatus -> Stage -> NanoUI ()
actions frame@Frame {vars = Vars {..}, ..} p s next =
  rowWith (fillW . gap 10 . alignMid . tight) $ do
    case busy of
      Just j -> do
        spinnerWith alignMid 18
        labelWith (tight . alignMid . ink palMuted) (actionLabel (jobAction j) <> (if jobStatus j == JobQueued then ", waiting" else ""))
      Nothing -> do
        forM_ (primaryStep next) $ \(txt, go) -> whenM (styled primary (actionButton txt)) go
        styled quiet $ forM_ (secondarySteps next <> redoSteps) $ \(txt, go) -> whenM (actionButton txt) go
    moreMenu frame p s next
  where
    busy = find (\j -> pkgName (jobPackage j) == pkgName p && not (jobFinished (jobStatus j))) jobs
    queue action = uiIO (enqueue env False [(p, action)])
    -- Once there is a tag, it can be moved to HEAD and everything after it
    -- made again, until the version is published.
    redoSteps =
      [ step
      | not (isPublished p s)
      , not (psVersionUncommitted s)
      , isJust (psTagCommit s)
      , step <- [("Redo tag and build", redoTag), ("Update candidate", updateCandidate)]
      ]
    redoTag = uiIO (enqueue env True [(p, ActTagDist)])
    askBump = put bumpFor (Just (pkgName p))
    askPublish = put pending (Just (Pending ("Publish " <> pkgId p) [(p, ActPublish)]))
    primaryStep = \case
      StageNeedsBump -> Just ("Bump version…", askBump)
      StageCommitBump -> Just ("Commit version bump", queue ActCommitBump)
      StageReadyToTag -> Just ("Tag and build", queue ActTagDist)
      StageTagged -> Just ("Build tarball", queue ActTagDist)
      StageCandidate -> Just ("Upload candidate", queue ActUpload)
      StageUploaded -> Just ("Publish…", askPublish)
      StageReleased -> Nothing
    secondarySteps = \case
      StageCandidate -> [("Publish now…", askPublish)]
      _ -> []
    -- Move the tag to HEAD, make the tarball again over the old one, and
    -- upload it as the candidate, as hkgr's upload --force does.
    updateCandidate = uiIO (enqueue env True [(p, ActUpload)])

-- | Everything a release can need that is not its next step.
moreMenu :: Frame -> Package -> PkgStatus -> Stage -> NanoUI ()
moreMenu Frame {vars = Vars {..}, ..} p s next = do
  btn <- styled quiet (actionButton' "More")
  when (respClicked btn) (put moreOpen (not (val moreOpen)))
  let cfg = (defaultPopupConfig (AnchorRect (respRect btn))) {cfgPlacement = PlacementBelow, cfgOffset = 4}
  (popupResp, chosen) <- popup (val moreOpen) cfg $
    columnWith (tight . gap 0 . minW 240) $ do
      picks <-
        sequence
          [ item True "Check package" (queue ActCheck)
          , item (next /= StageNeedsBump) "Bump version…" (put bumpFor (Just (pkgName p)))
          , item (psTarball s) "Rebuild from tarball" (queue ActBuild)
          , item (psTarball s && pkgHasLibrary p) "Upload candidate docs" (queue ActUploadDocs)
          , item (psTarball s && pkgHasLibrary p) "Publish docs…" (put pending (Just (Pending ("Publish documentation for " <> pkgId p) [(p, ActPublishDocs)])))
          ]
      menuSeparator
      web <-
        sequence
          [ item True "View candidate on Hackage" (uiIO (openUrl (candidateUrl (pkgId p))))
          , item True "View package on Hackage" (uiIO (openUrl (packageUrl (pkgName p))))
          ]
      pure (listToMaybe [go | Just go <- picks <> web])
  forM_ (join chosen) $ \go -> put moreOpen False >> go
  when (respClicked popupResp) (put moreOpen False)
  where
    queue action = uiIO (enqueue env False [(p, action)])
    item enabled txt go
      | enabled = (\c -> if c then Just go else Nothing) <$> menuItem txt
      | otherwise = Nothing <$ menuItemDisabled txt

-- | The particulars, for when the summary is not enough.
facts :: Repo -> Package -> PkgStatus -> NanoUI ()
facts r p s =
  columnWith (fillW . gap 0 . tight) $ do
    field "Directory" (displayDir (pkgDir p))
    field "Tag" $ case psTagCommit s of
      Nothing -> psTag s <> ", not made yet"
      Just c
        | psTagOnHead s -> psTag s <> " at HEAD (" <> T.take 8 c <> ")"
        | otherwise -> psTag s <> " at " <> T.take 8 c
    field "Previous release" $ case psPrevRelease s of
      Nothing -> "None tagged"
      Just (v, t) -> showVersion v <> ", tagged " <> t <> ", " <> countOf (psChangesSincePrev s) "commit" <> " ago"
    field "Hackage" $ case psHackage s of
      HackageUnknown -> "Asking"
      HackageUnreachable e -> "Couldn't reach it: " <> e
      HackageAbsent -> "Doesn't have this package"
      info@(HackageVersions normal _) ->
        maybe "No versions" (("Latest is " <>) . showVersion) (latestVersion info) <> ", of " <> countOf (length normal) "release"
    field "Tarball" $
      if psTarball s
        then T.replace "\\" "/" (T.pack (".cabalist" </> T.unpack (pkgId p) <> ".tar.gz"))
        else "Not built"
    field "Changelog" $ case psChangelog s of
      Nothing -> "None"
      Just f -> T.pack f <> (if psChangelogHasEntry s then "" else ", no entry for " <> showVersion (pkgVersion p))
    let deps = map pkgName (internalDeps (repoPackages r) p)
    unless (null deps) $ field "Depends on" (T.intercalate ", " deps)
  where
    field :: Text -> Text -> NanoUI ()
    field title value = rowWith (fillW . fixedH 28 . gap 0 . alignMid . tight) $ do
      labelWith (tight . fixedW 160 . alignMid . ink palQuiet) title
      labelWith (tight . alignMid . ink palMuted) (ellipsize 100 value)

displayDir :: FilePath -> Text
displayDir d = if d == "." then "The repository root" else T.replace "\\" "/" (T.pack d)

--------------------------------------------------------------------------------
-- Activity and the log

-- | One line on the work in hand, or on the last piece of work, and the log
-- under it when open. Nothing shows until something has run.
activity :: Frame -> NanoUI ()
activity frame@Frame {vars = Vars {..}, ..} =
  unless (null jobs) $ do
    separator
    surface (palRaised palette) (palRaised palette) 0 (fillW . tight) $
      rowWith (fillW . fixedH 44 . padXY 20 0 . gap 10 . alignMid . tight) $ do
        case running of
          Just j -> do
            spinnerWith alignMid 16
            labelWith (tight . alignMid . ink palText) (describe j)
            small palQuiet (elapsed (maybe 0 (now -) (jobStarted j)))
          Nothing -> forM_ (listToMaybe (reverse jobs)) $ \j -> do
            jobMark (jobStatus j)
            labelWith (tight . alignMid . ink palText) (describe j)
            small palQuiet (outcome now j)
        unless (waiting == 0) $ small palQuiet (tshow waiting <> " more waiting")
        flex
        styled quiet $ do
          when (isJust running || waiting > 0) $
            whenM (compactButton "Stop all") (uiIO (cancelAll env))
          whenM (compactButton (if val logOpen then "Hide log" else "Show log")) (put logOpen (not (val logOpen)))
    when (val logOpen) $ do
      separator
      rowWith (fillW . fixedH 300 . gap 0 . tight) $ do
        jobList frame
        separator
        logView frame
  where
    running = find ((== JobRunning) . jobStatus) jobs
    waiting = length (filter ((== JobQueued) . jobStatus) jobs)

describe :: Job -> Text
describe j = jobName j <> " " <> pkgId (jobPackage j)

-- | A step's name, as its button said: a forced tag or upload redoes the tag.
jobName :: Job -> Text
jobName j
  | jobAction j == ActTagDist && optForce (jobOptions j) = "Redo tag and build"
  | jobAction j == ActUpload && optForce (jobOptions j) = "Update candidate"
  | otherwise = actionLabel (jobAction j)

outcome :: Double -> Job -> Text
outcome t j = case jobStatus j of
  JobQueued -> "waiting"
  JobRunning -> elapsed (maybe 0 (t -) (jobStarted j))
  JobSucceeded -> "done in " <> elapsed (fromMaybe 0 ((-) <$> jobFinishedAt j <*> jobStarted j))
  JobFailed msg -> "failed: " <> ellipsize 60 msg
  JobCancelled msg -> ellipsize 60 (T.toLower msg)

elapsed :: Double -> Text
elapsed secs =
  let s = round secs :: Int
   in if s < 60 then tshow s <> "s" else tshow (s `div` 60) <> "m " <> tshow (s `mod` 60) <> "s"

jobMark :: JobStatus -> NanoUI ()
jobMark = \case
  JobQueued -> icon 10 (palQuiet palette) iconDot
  JobRunning -> spinnerWith alignMid 14
  JobSucceeded -> icon 16 (palSage palette) iconCheck
  JobFailed _ -> icon 16 (palCoral palette) iconCross
  JobCancelled _ -> icon 16 (palQuiet palette) iconCross

jobList :: Frame -> NanoUI ()
jobList Frame {vars = Vars {..}, ..} =
  surface (palWell palette) (palWell palette) 0 (fixedW listW . fillH . tight) $
    columnWith (fillW . fillH . gap 0 . tight) $ do
      rowWith (fillW . fixedH 40 . padXY 16 0 . alignMid . tight) $ do
        labelWith (tight . alignMid . ink palMuted) (countOf (length jobs) "step")
        flex
        styled quiet $ disabledWhen (not (any (jobFinished . jobStatus) jobs)) $
          whenM (compactButton "Clear finished") (uiIO (clearFinished env))
      scrollWith (fillW . fillH . tight) $
        columnWith (fillW . gap 0 . tight) $
          forM_ (reverse jobs) $ \j -> withKey (jobId j) $
            rowWith (fillW . fixedH 52 . gap 0 . alignMid . tight) $ do
              clicked <- listRow (Just (jobId j) == fmap jobId job) (describeShort j) "" (Just (outcome now j, markColour (jobStatus j)))
              when clicked $ uiIO (writeIORef (vcAutoJob cache) Nothing) >> put selectedJob (Just (jobId j))
              unless (jobFinished (jobStatus j)) $
                styled quiet $ whenM (compactButton (if jobStatus j == JobRunning then "Stop" else "Skip")) (uiIO (cancelJob env (jobId j)))
              spacer (Fixed 8) Fit
  where
    describeShort j = jobName j <> ", " <> pkgName (jobPackage j)
    markColour = \case
      JobSucceeded -> palSage palette
      JobFailed _ -> palCoral palette
      JobRunning -> palPurpleInk palette
      _ -> palQuiet palette

-- | The chosen job's output. It follows new lines until scrolled up, and
-- only the lines in view are laid out, so a long cabal build stays cheap.
logView :: Frame -> NanoUI ()
logView Frame {vars = Vars {..}, ..} =
  columnWith (fillW . fillH . gap 0 . tight) $ do
    rowWith (fillW . fixedH 40 . padXY 16 0 . gap 8 . alignMid . tight) $ do
      labelWith (tight . alignMid . ink palMuted) (maybe "" describe job)
      flex
      forM_ job $ \j -> styled quiet $ do
        unless (val logSticky) $ whenM (compactButton "Follow") (put logSticky True)
        whenM (compactButton "Copy") $ uiIO (void (ctxClipboardSet ctx (T.unlines (toList (jobLog j)))))
    separator
    let logLines = maybe Seq.empty jobLog job
        n = Seq.length logLines
        jid = maybe (-1) jobId job
        (seenJob, seen, widest0) = val logWidest
        widest
          | seenJob == jid && seen <= n = foldl' longer widest0 (Seq.drop seen logLines)
          | otherwise = foldl' longer "" logLines
    when ((seenJob, seen, widest0) /= (jid, n, widest)) (put logWidest (jid, n, widest))
    -- Only the lines in view are laid out, so the content takes its width
    -- from the longest line of the whole log: otherwise the horizontal
    -- scrollbar would come and go with the lines on screen.
    widestW <- uiIO (fst <$> ctxResolveMeasure ctx sizeSmall WeightNormal FontStyleNormal FontMono widest)
    scrollWid <- withKey ("log" :: Text) nextId
    metrics <- uiIO (getScrollMetrics ctx scrollWid)
    offset <- uiIO (getScrollOffset2D ctx scrollWid)
    let -- The viewport is clear of the horizontal scrollbar when there is one,
        -- so only the column's own padding goes under the last line.
        viewH = maybe 200 (rectH . scrollViewport) metrics
        totalH = fromIntegral n * logRowH + 12
        maxOff = max 0 (totalH - viewH)
        curY = v2Y offset
        userScrolled = abs (curY - val logPrevY) > 0.5
        atBottom = maxOff <= 0 || curY >= maxOff - 8
        sticky = if userScrolled then atBottom else val logSticky
        targetY = if sticky then maxOff else min maxOff curY
    when (abs (targetY - curY) > 0.5) $ uiIO (setScrollOffset2D ctx scrollWid (V2 (v2X offset) targetY))
    when (sticky /= val logSticky) (put logSticky sticky)
    when (abs (targetY - val logPrevY) > 0.5) (put logPrevY targetY)
    let firstVis = max 0 (floor (targetY / logRowH) - 2)
        lastVis = min (n - 1) (ceiling ((targetY + viewH) / logRowH) + 2)
    void $ withKey ("log" :: Text) $ scrollArea2D (fillW . fillH) $
      columnWith (padXY 16 6 . gap 0 . tight . minW (max 900 (widestW + 32))) $ do
        when (firstVis > 0) $ spacer Fit (Fixed (fromIntegral firstVis * logRowH))
        forM_ [firstVis .. lastVis] $ \i -> forM_ (Seq.lookup i logLines) $ \l ->
          withKey i $ rowWith (tight . fixedH logRowH . alignMid) $
            labelWith (tight . alignMid . fontMono . fontSize sizeSmall . fontColor (lineColour l)) (if T.null l then " " else expandTabs l)
        when (lastVis < n - 1) $ spacer Fit (Fixed (fromIntegral (n - 1 - lastVis) * logRowH))
  where
    logRowH = 20
    expandTabs = T.replace "\t" "    "
    longer w l = let l' = expandTabs l in if T.length l' > T.length w then l' else w

-- | Commands stand out from their output; failures and warnings are coloured.
lineColour :: Text -> Color
lineColour l
  | "$ " `T.isPrefixOf` l = palText palette
  | "✓" `T.isPrefixOf` l = palSage palette
  | "✗" `T.isPrefixOf` l || "cabalist:" `T.isPrefixOf` l = palCoral palette
  | "error" `T.isInfixOf` low = palCoral palette
  | "warning" `T.isInfixOf` low = palAmber palette
  | otherwise = palMuted palette
  where
    low = T.toLower l

--------------------------------------------------------------------------------
-- Dialogs

-- | A modal dialog, open while it has a subject. Returns the subject and what
-- the body chose, if it chose anything this frame.
dialogFor :: Maybe s -> (s -> Text) -> NanoUI () -> (s -> NanoUI (Maybe c)) -> NanoUI (Maybe (s, c))
dialogFor subject title close body = do
  (resp, chosen) <- modal (isJust subject) (maybe "" title subject) $
    for subject $ \s -> fmap (s,) <$> body s
  when (respClicked resp) close
  pure (join (join chosen))

dialogBody :: Layout -> Layout
dialogBody l = l {layoutPadding = Padding 0 0 0 0}

data Choice = ChoiceCancel | ChoiceOk

buttonsRow :: Text -> (Theme -> Theme) -> Bool -> NanoUI (Maybe Choice)
buttonsRow okText okStyle okDisabled =
  rowWith (fillW . gap 8 . alignMid . tight) $ do
    flex
    cancel <- styled quiet (actionButton "Cancel")
    ok <- disabledWhen okDisabled (styled okStyle (actionButton okText))
    pure (listToMaybe [c | (True, c) <- [(cancel, ChoiceCancel), (ok, ChoiceOk)]])

note :: Text -> NanoUI ()
note = labelWith (tight . fontSize sizeSmall . ink palQuiet)

bumpDialog :: Frame -> NanoUI ()
bumpDialog Frame {vars = Vars {..}, ..} = do
  let subject = val bumpFor >>= \n -> find ((== n) . pkgName) packages
  chosen <- dialogFor subject (\p -> "Bump " <> pkgName p) (put bumpFor Nothing) $ \p ->
    columnWith (gap 14 . minW 480 . dialogBody) $ do
      labelWith (tight . ink palMuted) ("The version is " <> showVersion (pkgVersion p) <> ". Bump it to")
      let bumps = [minBound .. maxBound] :: [Bump]
          optionText b = showVersion (bumpVersion b (pkgVersion p)) <> "   " <> T.toLower (T.takeWhile (/= ' ') (bumpLabel b)) <> " change"
      bind bumpChoice (radio (map optionText bumps))
      bind bumpCommit (checkbox "Commit the .cabal file and changelog")
      note "The changelog gets an entry for the new version."
      separator
      buttonsRow "Bump version" primary False
  forM_ chosen $ \(p, c) -> do
    case c of
      ChoiceOk -> uiIO (enqueue env False [(p, ActBump (toEnum (max 0 (min 3 (val bumpChoice)))) (val bumpCommit))])
      ChoiceCancel -> pure ()
    put bumpFor Nothing

publishDialog :: Frame -> NanoUI ()
publishDialog Frame {vars = Vars {..}, ..} = do
  chosen <- dialogFor (val pending) pendingTitle (put pending Nothing) $ \pd ->
    columnWith (gap 14 . minW 520 . dialogBody) $ do
      labelWith (tight . ink palMuted) $
        if stDryRun st
          then "Dry run: the steps run, but nothing is pushed or uploaded."
          else "Hackage keeps every release, so this can't be undone."
      columnWith (gap 6 . tight) $
        forM_ (zip [0 :: Int ..] (pendingSteps pd)) $ \(i, (p, a)) -> withKey i $
          labelWith (tight . ink palText) (actionLabel a <> " " <> pkgId p <> tagOf p)
      unless (stDryRun st) $
        note ("Each tag is pushed to " <> remote <> " first.")
      when (isNothing (stCabalLogin st) && stCredentials st == FromCabalConfig && not (stDryRun st)) $
        labelWith (tight . ink palAmber) "cabal has no Hackage login. Log in first."
      separator
      buttonsRow (pendingTitle pd) primary False
  forM_ chosen $ \(pd, c) -> do
    case c of
      ChoiceOk -> uiIO (enqueue env False (pendingSteps pd))
      ChoiceCancel -> pure ()
    put pending Nothing
  where
    remote = maybe "origin" (cfgRemote . repoConfig) repo
    tagOf p = maybe "" (\r -> ", tagged " <> renderTag (cfgTagFormat (repoConfig r)) p) repo

loginDialog :: Frame -> NanoUI ()
loginDialog Frame {vars = Vars {..}, ..} = do
  chosen <- dialogFor (if val loginOpen then Just () else Nothing) (const "Log in to Hackage") (put loginOpen False) $ \() ->
    columnWith (gap 14 . minW 480 . dialogBody) $ do
      let fromConfig = case stCabalLogin st of
            Just kind -> "Use cabal's config (it has " <> kind <> ")"
            Nothing -> "Use cabal's config (it has no login)"
      bind loginMode (radio [fromConfig, "Username and password", "API token"])
      scope $ case val loginMode of
        1 -> do
          bind loginUser (textInputConfigured defaultTextInputConfig {ticPlaceholder = "Username", ticLayout = fillW (ticLayout defaultTextInputConfig)})
          bind loginPass (textInputConfigured defaultTextInputConfig {ticPlaceholder = "Password", ticPassword = True, ticLayout = fillW (ticLayout defaultTextInputConfig)})
        2 ->
          bind loginToken (textInputConfigured defaultTextInputConfig {ticPlaceholder = "API token", ticPassword = True, ticLayout = fillW (ticLayout defaultTextInputConfig)})
        _ -> pure ()
      let keyring = envKeyring env
      when (keyring && val loginMode /= 0) $
        bind loginRemember (checkbox ("Remember it in " <> keyringName))
      note $
        if
          | keyring && val loginMode /= 0 && val loginRemember -> "Saved in " <> keyringName <> ", for the next time cabalist starts."
          | keyring && stLoginSaved st -> "The login saved in " <> keyringName <> " will be removed."
          | val loginMode == 0 -> "cabal reads its config for each upload."
          | otherwise -> "Kept in memory until cabalist closes."
      separator
      buttonsRow "Use this login" primary (not complete)
  forM_ chosen $ \((), c) -> do
    case c of
      ChoiceOk -> uiIO (setCredentials env creds (val loginRemember))
      ChoiceCancel -> pure ()
    put loginOpen False
  where
    creds = case val loginMode of
      1 -> UserPassword (T.strip (val loginUser)) (val loginPass)
      2 -> ApiToken (T.strip (val loginToken))
      _ -> FromCabalConfig
    complete = case creds of
      UserPassword u pw -> not (T.null u) && not (T.null pw)
      ApiToken t -> not (T.null t)
      FromCabalConfig -> True

settingsDialog :: Frame -> NanoUI ()
settingsDialog Frame {vars = Vars {..}, ..} = do
  chosen <- dialogFor (if val settingsOpen then Just () else Nothing) (const "Settings") (put settingsOpen False) $ \() ->
    columnWith (gap 18 . minW 540 . dialogBody) $ do
      forM_ repo $ \r -> section "Tags" $ do
        rowWith (fillW . gap 8 . alignMid . tight) $ do
          bind formatDraft (textInputConfigured defaultTextInputConfig {ticLayout = (fixedH 34 . fillW) (ticLayout defaultTextInputConfig)})
          let presetIx = fromMaybe 0 (findIndex (== val formatDraft) tagFormats)
          picked <- selectWith (fixedW 200) tagFormats presetIx
          when (picked /= presetIx) $ put formatDraft (tagFormats !! picked)
        note $
          if valid
            then
              maybe "" (\p -> "Tags look like " <> renderTag (val formatDraft) p <> ".") current
                <> (if length (repoPackages r) > 1 && not ("{name}" `T.isInfixOf` val formatDraft) then " Include {name}: the packages would share tags." else "")
            else "Tags need {version}, and can't hold spaces or ~^:?*[\\"
        rowWith (fillW . gap 8 . alignMid . tight) $ do
          labelWith (tight . alignMid . ink palMuted) "Push them to"
          bind remoteDraft (textInputConfigured defaultTextInputConfig {ticLayout = (fixedW 200 . fixedH 34) (ticLayout defaultTextInputConfig)})
      section "Building" $ do
        bind hlintDraft (checkbox "Run hlint on the release")
        bind buildDraft (checkbox "Build the tarball after making it")
        bind siblingsDraft (checkbox "Build against this repository's own dependencies")
        bind dryRunDraft (checkbox "Dry run: log uploads and pushes instead of doing them")
      section "Display" $
        rowWith (fillW . gap 8 . alignMid . tight) $ do
          labelWith (tight . alignMid . ink palMuted) "Scale the window's contents"
          bind scaleDraft (selectWith (fixedW 200) (map snd uiScales))
      columnWith (fillW . gap 12 . tight) $ do
        note ("cabal " <> maybe "not found" (showVersion . snd) (stCabal st) <> ". Tag settings are saved in .cabalist/config; the rest apply to every repository.")
        separator
        buttonsRow "Save" primary (isJust repo && (not valid || T.null (T.strip (val remoteDraft))))
  forM_ chosen $ \((), c) -> do
    case c of
      ChoiceOk -> uiIO $ do
        forM_ repo $ \r -> saveSettings env (repoConfig r) {cfgTagFormat = T.strip (val formatDraft), cfgRemote = T.strip (val remoteDraft)}
        saveUserSettings
          env
          UserSettings
            { usOptions = (stOptions st) {optHlint = val hlintDraft, optBuild = val buildDraft, optSiblings = val siblingsDraft}
            , usDryRun = val dryRunDraft
            , usUiScale = maybe 1 fst (listToMaybe (drop (val scaleDraft) uiScales))
            }
      _ -> pure ()
    put settingsOpen False
  where
    valid = validTagFormat (T.strip (val formatDraft))
    section title body = columnWith (fillW . gap 10 . tight) $ do
      labelWith (tight . fontSemiBold . ink palText) title
      body

--------------------------------------------------------------------------------
-- Keyboard

-- | Escape closes a dialog or the menu; Ctrl+R refreshes; up and down move
-- through the packages while no text field has the keys.
keyboardShortcuts :: Frame -> NanoUI ()
keyboardShortcuts frame@Frame {vars = Vars {..}, ..} = do
  editing <- uiIO (textFieldActive ctx)
  when (pressed KeyEscape) $ do
    put bumpFor Nothing
    put pending Nothing
    put loginOpen False
    put settingsOpen False
    put moreOpen False
  when (ctrlR && not anyModal) $ uiIO (refresh frame)
  unless (editing || anyModal) $ do
    let names = map pkgName packages
        ix = maybe 0 (\p -> fromMaybe 0 (findIndex (== pkgName p) names)) current
        move d = forM_ (listToMaybe (drop (ix + d) names)) (put selected . Just)
    when (pressed KeyDown) (move 1)
    when (pressed KeyUp && ix > 0) (move (-1))
  where
    pressed key = inputKeysElem key (inputKeys inp)
    -- Ctrl+R arrives as the letter or as its control character, 18.
    ctrlR = modCtrl (inputModifiers inp) && T.any (`elem` ['r', 'R', chr 18]) (inputChars inp)

--------------------------------------------------------------------------------
-- Small helpers

small :: (Palette -> Color) -> Text -> NanoUI ()
small colour = labelWith (alignMid . tight . fontSize sizeSmall . ink colour)

countOf :: Int -> Text -> Text
countOf k noun = tshow k <> " " <> noun <> (if k == 1 then "" else "s")

ellipsize :: Int -> Text -> Text
ellipsize n t
  | T.length t > n = T.stripEnd (T.take (n - 1) t) <> "…"
  | otherwise = t

-- | Open a web page in the default browser.
--
-- On Windows, process quotes every argument, and rundll32 cannot read a
-- quoted @url.dll,FileProtocolHandler@, so the page is handed to Explorer.
openUrl :: Text -> IO ()
openUrl url = void (try (void (spawnProcess opener (args <> [T.unpack url]))) :: IO (Either SomeException ()))
  where
    (opener, args) = case os of
      "mingw32" -> ("explorer", [])
      "darwin" -> ("open", [])
      _ -> ("xdg-open", [])
