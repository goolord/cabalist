-- | The release steps, after hkgr's cautious, repeatable workflow:
--
-- 1. 'tagDist' tags the version and makes the tarball with @cabal sdist@ from
--    a pristine clone of the tag, then builds the tarball in a clean
--    directory to catch files missing from it.
-- 2. 'upload' sends the tarball to Hackage as a candidate. It can be repeated
--    (with 'optForce' moving the tag) until the candidate looks right.
-- 3. 'publish' pushes the tag and publishes the release, then marks the
--    version published so cabalist will not release it again.
--
-- A package in a subdirectory of a monorepo is handled the same way: the
-- whole repository is cloned, and cabal runs in the package's directory with
-- a @cabal.project@ of its own, so a project file at the repository root does
-- not pull the other packages in.
module Cabalist.Release
  ( Credentials (..)
  , Options (..)
  , defaultOptions
  , Ctx (..)
  , checkPackage
  , cabalCheck
  , tagDist
  , untag
  , pristineBuild
  , upload
  , publish
  , uploadDocs
  , bumpPackage
  , commitBump
  , uploadArgs
  , tokenConfig
  , cabalConfigFile
  , findCabal
  )
where

import Control.Exception (SomeException, finally, onException, try)
import Control.Monad (filterM, forM_, unless, void, when)
import Data.ByteString qualified as B
import Data.Char (isSpace)
import Data.Containers.ListUtils (nubOrd)
import Data.List (find, maximumBy)
import Data.Maybe (catMaybes, isJust)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Cabalist.File (editUtf8, readUtf8, writeUtf8)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import GHC.Clock (getMonotonicTimeNSec)
import Cabalist.Config
import Cabalist.Git
import Cabalist.Hackage (candidateUrl, packageUrl)
import Cabalist.Package
import Cabalist.Process
import Cabalist.Status (PkgStatus (..), isPublished)
import Cabalist.Version (Bump, findChangelog, setVersionField, addChangelogEntry)
import Cabalist.Version qualified as V
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)
import System.FilePath (getSearchPath, isAbsolute, (<.>), (</>))

-- | How to log in to Hackage for an upload.
data Credentials
  = FromCabalConfig
  -- ^ Let cabal use the username, password, or token in its config file.
  | UserPassword !Text !Text
  | ApiToken !Text
  deriving (Eq)

data Options = Options
  { optForce :: !Bool
  -- ^ Move an existing tag to HEAD and replace the tarball.
  , optExistingTag :: !Bool
  -- ^ Make the tarball from a tag made by hand, without tagging.
  , optHlint :: !Bool
  , optBuild :: !Bool
  -- ^ Build the tarball in a clean directory after making it.
  , optSiblings :: !Bool
  -- ^ In that build, use the repository's own copies of the packages it
  -- depends on rather than Hackage's: for releasing several packages of a
  -- monorepo together, before their dependencies are published.
  , optDryRun :: !Bool
  -- ^ Log uploads and pushes instead of running them.
  , optCredentials :: !Credentials
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optForce = False
    , optExistingTag = False
    , optHlint = True
    , optBuild = True
    , optSiblings = False
    , optDryRun = False
    , optCredentials = FromCabalConfig
    }

-- | Everything a step needs.
data Ctx = Ctx
  { ctxRoot :: !FilePath
  , ctxConfig :: !Config
  , ctxPackages :: ![Package]
  -- ^ Every package in the repository, for 'optSiblings'.
  , ctxOptions :: !Options
  , ctxLog :: !Logger
  , ctxStatus :: !PkgStatus
  -- ^ The package's status when the step was queued.
  , ctxUnpublished :: ![Text]
  -- ^ Packages of the repository that Hackage has no release of.
  , ctxCabal :: !FilePath
  -- ^ The cabal executable to run (see 'findCabal').
  }

say :: Ctx -> Text -> IO ()
say ctx = logLine (ctxLog ctx)

run :: Ctx -> FilePath -> FilePath -> [String] -> IO [Text]
run ctx = runLogged_ (ctxLog ctx)

git_ :: Ctx -> [String] -> IO ()
git_ ctx args = () <$ run ctx (ctxRoot ctx) "git" args

tagOf :: Ctx -> Package -> Text
tagOf ctx = renderTag (cfgTagFormat (ctxConfig ctx))

-- | Refuse to release a version whose version field is uncommitted, or that
-- is already published. Uncommitted changes elsewhere in the package are
-- shown but allowed, as hkgr allows them: the release is built from the tag.
checkPackage :: Ctx -> Package -> Bool -> IO ()
checkPackage ctx p showDiff = do
  let root = ctxRoot ctx
      st = ctxStatus ctx
  when (pkgNameMismatch p) $
    failStep ("the .cabal file must be named " <> pkgName p <> ".cabal")
  when showDiff $ do
    dirty <- dirtyFiles root (pkgDir p)
    unless (null dirty) $ do
      say ctx "=== uncommitted changes (not part of the release) ==="
      mapM_ (say ctx) dirty
      say ctx "=== end of uncommitted changes ==="
  changed <- versionFieldChanged root (pkgCabalFile p)
  when changed $ failStep "commit or revert the changed version field first"
  published <- doesFileExist (publishedMarker root p)
  when (published || isPublished p st) $
    failStep (pkgId p <> " was already published")

-- | @cabal check@ in the package's directory of the working tree.
cabalCheck :: Ctx -> Package -> IO ()
cabalCheck ctx p = () <$ run ctx (ctxRoot ctx </> pkgDir p) (ctxCabal ctx) ["check"]

-- | Tag the version (unless it is tagged and not forced) and make the tarball
-- from the tag. If making the tarball fails, the tag goes back to where it
-- was.
tagDist :: Ctx -> Package -> IO ()
tagDist ctx p = do
  let root = ctxRoot ctx
      opts = ctxOptions ctx
      tag = tagOf ctx p
  checkPackage ctx p True
  mTagHash <- tagCommit root tag
  let tagExists = isJust mTagHash
  when (tagExists && not (optForce opts)) $ assertTagOnBranch ctx tag
  if optExistingTag opts
    then do
      unless tagExists $ failStep ("tag " <> tag <> " does not exist")
      sdist ctx p (optForce opts)
    else
      if tagExists && not (optForce opts)
        then do
          headHash <- revParse root "HEAD"
          say ctx ("tag " <> tag <> " is " <> (if headHash == mTagHash then "on HEAD" else "not on HEAD"))
          haveTarball <- doesFileExist (tarballPath root p)
          when haveTarball $
            failStep ("tag " <> tag <> " and its tarball exist: use Force to move the tag and replace the tarball")
          sdist ctx p False
        else do
          git_ ctx (["tag"] <> ["--force" | optForce opts] <> [T.unpack tag])
          say ctx ((if tagExists then "moved tag " else "tagged ") <> tag)
          -- The tag is new (or forced), so any tarball left from an earlier
          -- attempt is stale.
          sdist ctx p True `onException` do
            -- Undo even when the job was cancelled: the cleanup must not
            -- itself be stopped by the cancellation.
            let undo = ctx {ctxLog = (ctxLog ctx) {logCancelled = pure False}}
            say undo "resetting the tag"
            case mTagHash of
              Just old -> git_ undo ["tag", "--force", T.unpack tag, T.unpack old]
              Nothing -> git_ undo ["tag", "--delete", T.unpack tag]
            -- A tarball without its tag would read as ready to upload.
            void (try (removeFile (tarballPath root p)) :: IO (Either SomeException ()))

assertTagOnBranch :: Ctx -> Text -> IO ()
assertTagOnBranch ctx tag = do
  onBranch <- tagOnBranch (ctxRoot ctx) tag
  unless onBranch $ failStep (tag <> " is no longer on a branch: use Force to move it")

-- | Step back to before the tag: delete the tarball and the tag, so the
-- version can be tagged again. Only the local tag goes, as tags are pushed
-- when a version is published, and a published version cannot step back.
untag :: Ctx -> Package -> IO ()
untag ctx p = do
  let root = ctxRoot ctx
      tag = tagOf ctx p
  when (isPublished p (ctxStatus ctx)) $ failStep (pkgId p <> " is published: its tag stays")
  -- The window may still have shown Back as the upload finished.
  when (psCandidateUploaded (ctxStatus ctx)) $
    failStep ("the candidate for " <> pkgId p <> " is on Hackage: republish it instead")
  forM_ [candidateMarker root p, tarballPath root p] $ \f -> do
    exists <- doesFileExist f
    when exists $ removeFile f >> say ctx ("removed " <> T.pack f)
  tagExists <- isJust <$> tagCommit root tag
  when tagExists $ do
    git_ ctx ["tag", "--delete", T.unpack tag]
    say ctx ("deleted tag " <> tag)

-- | A fresh directory under the system temporary directory, removed after.
withTempDir :: Text -> (FilePath -> IO a) -> IO a
withTempDir label k = do
  tmp <- getTemporaryDirectory
  stamp <- getMonotonicTimeNSec
  let dir = tmp </> ("cabalist-" <> T.unpack label <> "-" <> show stamp)
  createDirectoryIfMissing True dir
  -- git marks its object files read-only; removePathForcibly copes with that.
  k dir `finally` (try (removePathForcibly dir) :: IO (Either SomeException ()))

-- | Stop cabal from finding a project file above a package's directory.
ensureProject :: Ctx -> FilePath -> [FilePath] -> IO ()
ensureProject ctx dir extra = do
  let file = dir </> "cabal.project"
  exists <- doesFileExist file
  unless exists $ do
    let lines' = ("packages: ." : ["          " <> forwardSlashes d | d <- extra])
    writeUtf8 file (T.unlines (map T.pack lines'))
    say ctx ("wrote " <> T.pack file <> (if null extra then "" else " (with the repository's copies of its dependencies)"))
  where
    forwardSlashes = map (\c -> if c == '\\' then '/' else c)

-- | Clone the repository at the package's tag and run @cabal sdist@ in the
-- package's directory, writing the tarball to the work directory. Then build
-- it, unless asked not to. An existing tarball is replaced only when
-- @replace@ says so.
sdist :: Ctx -> Package -> Bool -> IO ()
sdist ctx p replace = do
  let root = ctxRoot ctx
      opts = ctxOptions ctx
      target = tarballPath root p
      tag = tagOf ctx p
  ensureWorkDir root
  haveTarget <- doesFileExist target
  when haveTarget $
    if replace
      then removeFile target
      else failStep (T.pack target <> " exists already")
  -- A new tarball has not been uploaded, whatever the old one was.
  void (try (removeFile (candidateMarker root p)) :: IO (Either SomeException ()))
  withTempDir "sdist" $ \tmp -> do
    let clone = tmp </> "repo"
    git_ ctx ["clone", "-q", "--no-checkout", root, clone]
    () <$ run ctx clone "git" ["-c", "advice.detachedHead=false", "checkout", "-q", "refs/tags/" <> T.unpack tag]
    hasModules <- doesFileExist (clone </> ".gitmodules")
    when hasModules $ () <$ run ctx clone "git" ["submodule", "update", "--init", "--recursive"]
    let pkgTmp = clone </> pkgDir p
    () <$ run ctx pkgTmp (ctxCabal ctx) ["check"]
    when (optHlint opts) $ do
      hasHlint <- haveProgram "hlint"
      if hasHlint
        then do
          say ctx "# hlint (advice only)"
          -- hlint exits non-zero when it has suggestions; they are advice.
          (code, _) <- runLogged (ctxLog ctx) {logLine = \l -> unless ("cabalist: hlint exited" `T.isPrefixOf` l) (say ctx l)} pkgTmp "hlint" ["--no-summary", "."] Nothing
          when (code /= ExitSuccess) $ say ctx "# hlint has suggestions (advice only; the release continues)"
        else say ctx "# hlint is not installed; skipping it"
    ensureProject ctx pkgTmp []
    () <$ run ctx pkgTmp (ctxCabal ctx) ["sdist", "--output-directory=" <> workDir root, "."]
  made <- doesFileExist target
  unless made $ failStep ("cabal sdist did not write " <> T.pack target)
  say ctx ("wrote " <> T.pack target)
  when (optBuild opts) $ pristineBuild ctx p

-- | Unpack the tarball into a clean directory and build it there. This is
-- what catches files missing from @extra-source-files@ and friends.
pristineBuild :: Ctx -> Package -> IO ()
pristineBuild ctx p = do
  let root = ctxRoot ctx
      tarball = tarballPath root p
  exists <- doesFileExist tarball
  unless exists $ failStep ("no tarball yet: tag and sdist first (" <> T.pack tarball <> ")")
  withUnpacked ctx p $ \dir -> do
    say ctx ("# building " <> pkgId p <> " from its tarball")
    () <$ cabalUnpacked ctx dir ["build"]

-- | Run cabal in an unpacked tarball. There a dependency comes from Hackage
-- rather than the repository, so a package published since the last
-- @cabal update@ is unknown to cabal; update the index and try once more.
cabalUnpacked :: Ctx -> FilePath -> [String] -> IO [Text]
cabalUnpacked ctx dir args = do
  (code, out) <- runLogged (ctxLog ctx) dir (ctxCabal ctx) args Nothing
  case code of
    ExitSuccess -> pure out
    ExitFailure _
      | any ("unknown package: " `T.isInfixOf`) out -> do
          say ctx "# cabal's package index is missing a package; updating it and trying again"
          () <$ run ctx dir (ctxCabal ctx) ["update"]
          run ctx dir (ctxCabal ctx) args
      | otherwise -> failStep ("cabal " <> T.pack (unwords (take 1 args)) <> " failed")

-- | Unpack the tarball in a temporary directory and run an action in the
-- package directory inside it, which has a project file of its own.
withUnpacked :: Ctx -> Package -> (FilePath -> IO a) -> IO a
withUnpacked ctx p k = do
  let root = ctxRoot ctx
  withTempDir "build" $ \tmp -> do
    -- tar is given a relative name, so neither bsdtar (Windows) nor GNU tar
    -- (which reads "C:" as a remote host) trips over a drive letter.
    copyFile (tarballPath root p) (tmp </> "package.tar.gz")
    () <$ run ctx tmp "tar" ["-xzf", "package.tar.gz"]
    let dir = tmp </> T.unpack (pkgId p)
    -- The repository's own copies of dependencies: all of them when asked,
    -- and otherwise those Hackage does not have yet, which could not be
    -- built at all. Those still have to be released first.
    let deps = transitiveDeps (ctxPackages ctx) p
        unpublished = [q | q <- deps, pkgName q `elem` ctxUnpublished ctx]
        local = if optSiblings (ctxOptions ctx) then deps else unpublished
    unless (optSiblings (ctxOptions ctx) || null unpublished) $
      say ctx ("note: building against the repository's " <> T.intercalate ", " (map pkgName unpublished) <> ", not on Hackage yet; release " <> (if length unpublished == 1 then "it" else "them") <> " first")
    let siblings = [absolute root (pkgDir q) | q <- local]
    ensureProject ctx dir siblings
    k dir
  where
    absolute root d = if isAbsolute d then d else root </> d

-- | Packages of the repository a package depends on, directly or not.
transitiveDeps :: [Package] -> Package -> [Package]
transitiveDeps pkgs p0 = go [] (internalDeps pkgs p0)
  where
    go seen [] = reverse seen
    go seen (q : qs)
      | pkgName q `elem` map pkgName seen || pkgName q == pkgName p0 = go seen qs
      | otherwise = go (q : seen) (qs <> internalDeps pkgs q)

-- | The arguments for @cabal upload@, and what to feed its prompts. A token
-- is not among them: 'cabalUpload' hands it to cabal in a config file
-- ('tokenConfig'), since other programs can read a command line.
uploadArgs :: Credentials -> Bool -> Bool -> FilePath -> ([String], Maybe Text)
uploadArgs creds isPublish docs file =
  let flags = ["--publish" | isPublish] <> ["--documentation" | docs]
   in case creds of
        FromCabalConfig -> (["upload"] <> flags <> [file], Nothing)
        -- cabal prompts for whatever its config lacks: the username, then
        -- the password.
        UserPassword user pass -> (["upload", "--username=" <> T.unpack user] <> flags <> [file], Just (pass <> "\n"))
        ApiToken _ -> (["upload"] <> flags <> [file], Nothing)

-- | The user's cabal config with a token as its Hackage login, in place of
-- any login it had. Without a config to start from, it names Hackage as the
-- repository, as cabal's default config does.
tokenConfig :: Maybe Text -> Text -> Text
tokenConfig userConfig tok =
  T.unlines (("token: " <> tok) : maybe defaultRepo (filter (not . loginField) . T.lines) userConfig)
  where
    -- Only top-level fields: sections indent theirs.
    loginField l =
      let (k, v) = T.breakOn ":" l
       in not (T.null v) && not (T.null k) && not (isSpace (T.head k)) && T.toLower (T.strip k) `elem` ["token", "username", "password", "password-command"]
    defaultRepo = ["repository hackage.haskell.org", "  url: http://hackage.haskell.org/"]

-- | Where cabal reads its config: @cabal path --config-file@ asks cabal,
-- which knows every place it looks. Older cabals lack the command, so for
-- them follow @CABAL_CONFIG@ and @CABAL_DIR@ as they would.
cabalConfigFile :: FilePath -> IO FilePath
cabalConfigFile cabal =
  readCmdOk "." cabal ["path", "--config-file"] >>= \case
    Just out | not (T.null (T.strip out)) -> pure (T.unpack (T.strip (T.takeWhileEnd (/= '\n') (T.stripEnd out))))
    _ -> do
      configEnv <- lookupEnv "CABAL_CONFIG"
      dirEnv <- lookupEnv "CABAL_DIR"
      case (configEnv, dirEnv) of
        (Just f, _) | not (null f) -> pure f
        (_, Just d) | not (null d) -> pure (d </> "config")
        _ -> (</> "config") <$> getAppUserDataDirectory "cabal"

-- | Run an action with a temporary cabal config holding the token, readable
-- only by the user, removed afterwards.
withTokenConfig :: Ctx -> Text -> (FilePath -> IO a) -> IO a
withTokenConfig ctx tok k = do
  userFile <- cabalConfigFile (ctxCabal ctx)
  userConfig <- either (\(_ :: SomeException) -> Nothing) Just <$> try (readUtf8 userFile)
  tmp <- getTemporaryDirectory
  -- openTempFile creates the file with mode 0600 on Unix; on Windows the
  -- temporary directory is the user's own.
  (path, h) <- openTempFile tmp "cabalist-upload.config"
  (B.hPut h (encodeUtf8 (tokenConfig userConfig tok)) `finally` hClose h >> k path)
    `finally` void (try (removeFile path) :: IO (Either SomeException ()))

-- | Run @cabal upload@, treating an error in its output as failure too:
-- older cabals exit successfully when Hackage rejects an upload.
cabalUpload :: Ctx -> Bool -> Bool -> FilePath -> IO ()
cabalUpload ctx isPublish docs file = do
  let creds = optCredentials (ctxOptions ctx)
      (args, input) = uploadArgs creds isPublish docs file
  if optDryRun (ctxOptions ctx)
    then say ctx ("dry run: would run " <> renderCommand "cabal" args <> case creds of ApiToken _ -> ", with the token in a temporary config file"; _ -> "")
    else do
      (code, out) <- case creds of
        ApiToken tok -> withTokenConfig ctx tok $ \cfg ->
          runLogged (ctxLog ctx) (ctxRoot ctx) (ctxCabal ctx) (("--config-file=" <> cfg) : args) input
        _ -> runLogged (ctxLog ctx) (ctxRoot ctx) (ctxCabal ctx) args input
      let rejected = find (\l -> any (`T.isInfixOf` T.toLower l) ["error uploading", "error:", "401 unauthorized", "403 forbidden"]) out
      case (code, rejected) of
        (ExitSuccess, Nothing) -> pure ()
        (_, Just l) -> failStep ("upload failed: " <> T.strip l)
        (ExitFailure _, Nothing) -> failStep "cabal upload failed"

-- | Upload the tarball as a candidate, tagging and making it first if needed.
upload :: Ctx -> Package -> IO ()
upload ctx p = do
  prepareUpload ctx p
  cabalUpload ctx False False (tarballPath (ctxRoot ctx) p)
  say ctx ("candidate: " <> candidateUrl (pkgId p))
  unless (optDryRun (ctxOptions ctx)) $
    writeUtf8 (candidateMarker (ctxRoot ctx) p) (pkgId p <> "\n")

-- | Everything an upload needs: checks, and the tag and tarball.
prepareUpload :: Ctx -> Package -> IO ()
prepareUpload ctx p = do
  let root = ctxRoot ctx
      opts = ctxOptions ctx
      tag = tagOf ctx p
  checkPackage ctx p False
  tagExists <- isJust <$> tagCommit root tag
  when (not tagExists && optExistingTag opts) $ failStep ("tag " <> tag <> " does not exist")
  haveTarball <- doesFileExist (tarballPath root p)
  when (optForce opts || not tagExists || not haveTarball) $ tagDist ctx p
  assertTagOnBranch ctx tag
  untagged <- untaggedCommits root tag (pkgDir p)
  unless (null untagged) $ do
    say ctx ("commits touching the package after " <> tag <> " (not in the release):")
    mapM_ (say ctx . ("  " <>)) untagged

-- | Push the tag (and the branch, when the tag is ahead of it), publish the
-- release on Hackage, and mark the version published.
publish :: Ctx -> Package -> IO ()
publish ctx p = do
  let root = ctxRoot ctx
      opts = ctxOptions ctx
      tag = tagOf ctx p
      remote = T.unpack (cfgRemote (ctxConfig ctx))
      push args =
        if optDryRun opts
          then say ctx ("dry run: would run " <> renderCommand "git" ("push" : args))
          else git_ ctx ("push" : args)
  prepareUpload ctx p
  tagHash <- maybe (failStep ("tag " <> tag <> " disappeared")) (pure . T.unpack) =<< tagCommit root tag
  mBranch <- currentBranch root
  headIsAncestor <- isAncestor root "HEAD" tagHash
  case mBranch of
    Just branch | headIsAncestor, not (T.null branch) -> do
      say ctx ("pushing " <> branch <> " up to the tag")
      push ["--quiet", remote, tagHash <> ":refs/heads/" <> T.unpack branch]
    _ -> pure ()
  push [remote, "refs/tags/" <> T.unpack tag]
  cabalUpload ctx True False (tarballPath root p)
  unless (optDryRun opts) $ do
    stamp <- formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" <$> getCurrentTime
    writeUtf8 (publishedMarker root p) (T.pack ("published " <> stamp <> "\n"))
  say ctx ((if optDryRun opts then "dry run finished for " else "published: ") <> packageUrl (pkgId p))

-- | Build documentation for Hackage from the tarball and upload it, as a
-- candidate's documentation or the release's.
uploadDocs :: Ctx -> Package -> Bool -> IO ()
uploadDocs ctx p isPublish = do
  unless (pkgHasLibrary p) $ failStep (pkgName p <> " has no library to document")
  withUnpacked ctx p $ \dir -> do
    -- Only the main library: Hackage hosts its docs alone, and haddocking
    -- sublibraries for Hackage fails outright (their docs directory is
    -- never created).
    out <- cabalUnpacked ctx dir ["haddock", "--haddock-for-hackage", "--enable-documentation", T.unpack ("lib:" <> pkgName p)]
    docs <- findDocsTarball dir out
    case docs of
      Nothing -> failStep "cabal haddock did not report a documentation tarball"
      Just file -> do
        say ctx ("documentation: " <> T.pack file)
        cabalUpload ctx isPublish True file
  say ctx ((if isPublish then "documentation published: " else "candidate documentation: ") <> (if isPublish then packageUrl (pkgId p) else candidateUrl (pkgId p)))
  where
    findDocsTarball dir out = do
      let named = [T.unpack (T.strip w) | l <- out, w <- T.words l, "-docs.tar.gz" `T.isSuffixOf` w]
          fallback = dir </> "dist-newstyle" </> T.unpack (pkgId p <> "-docs.tar.gz")
      existing <- filterExisting (map (absolute dir) named <> [fallback])
      pure (case existing of (f : _) -> Just f; [] -> Nothing)
    absolute dir f = if isAbsolute f then f else dir </> f
    filterExisting fs = concat <$> mapM (\f -> (\e -> [f | e]) <$> doesFileExist f) fs

-- | Set a new version in the .cabal file and add a changelog entry for it.
bumpPackage :: Ctx -> Package -> Bump -> IO Version
bumpPackage ctx p b = do
  let root = ctxRoot ctx
      new = V.bumpVersion b (pkgVersion p)
      cabalFile = root </> pkgCabalFile p
  either failStep (\_ -> pure ()) =<< editUtf8 cabalFile (setVersionField new)
  say ctx ("version " <> showVersion (pkgVersion p) <> " -> " <> showVersion new <> " in " <> T.pack (pkgCabalFile p))
  mChangelog <- findChangelog (root </> pkgDir p)
  forM_ mChangelog $ \f -> do
    today <- formatTime defaultTimeLocale "%Y-%m-%d" <$> getCurrentTime
    edited <- editUtf8 (root </> pkgDir p </> f) (Right . addChangelogEntry new (T.pack today))
    when (edited == Right True) $
      say ctx ("added a " <> showVersion new <> " entry to " <> T.pack f)
  pure new

-- | Commit the .cabal file and changelog after a bump.
commitBump :: Ctx -> Package -> Version -> IO ()
commitBump ctx p new = do
  let root = ctxRoot ctx
  mChangelog <- findChangelog (root </> pkgDir p)
  let files = pkgCabalFile p : [pkgDir p </> f | Just f <- [mChangelog]]
  git_ ctx (["add", "--"] <> map gitPathspec files)
  git_ ctx (["commit", "-m", T.unpack ("Bump " <> pkgName p <> " to " <> showVersion new), "--"] <> map gitPathspec files)


-- | The newest cabal on the PATH, and its version. A machine can have several
-- (an old one left in cabal's own bin directory, say, ahead of ghcup's), and
-- an old cabal cannot read newer .cabal files or upload with a token.
findCabal :: IO (Maybe (FilePath, Version))
findCabal = do
  -- findExecutables stops at the first match on Windows, so walk the PATH.
  dirs <- getSearchPath
  exes <- filterM doesFileExist (nubOrd [d </> "cabal" <.> exeExtension | d <- dirs])
  found <- mapM (\exe -> fmap (exe,) . (>>= parseVersion) <$> readCmdOk "." exe ["--numeric-version"]) exes
  pure $ case catMaybes found of
    [] -> Nothing
    fs -> Just (maximumBy (comparing snd) fs)
