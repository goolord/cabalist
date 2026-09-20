-- | A headless run of the real window against a throwaway monorepo, in dry-run
-- mode: it opens the repository, runs Tag and build on a package in a
-- subdirectory through the job queue, opens each dialog, and saves a
-- screenshot of each state. Nothing is pushed or uploaded.
--
-- 'screenshot' renders a repository of your own the same way, for a look at
-- the window without running anything.
module Gui.SelfTest
  ( selfTest
  , screenshot
  , makeDemoRepo
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch, displayException, fromException)
import Control.Monad (replicateM_, unless, void, when)
import Data.Foldable (toList)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Cabalist.Process (nullLogger, runLogged_)
import GHC.Clock (getMonotonicTime)
import Cabalist.Package (Package (..))
import Cabalist.Release (Options (..))
import Gui.State
import Gui.View (ViewCache, appView, newViewCache)
import NanoUI (Color, Input (..), Key (..), Rect (..), V2 (..), emptyInput, inputKeysFromList)
import NanoUI.Backend.Sdl (SdlOptions (..), saveScreenshot, sdlDrawFrame, withSdl)
import NanoUI.Testing (collectOverlayTextSpans, collectTextSpans, newPixelContext, withTheme)
import NanoUI.Testing.Harness (clickPos, findExact, hasText, requireSpan)
import System.Directory
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)

-- | A two-package monorepo: @cabalist-demo-core@, and @cabalist-demo-app@
-- which depends on it, under @packages/@.
makeDemoRepo :: FilePath -> IO ()
makeDemoRepo root = do
  removePathForcibly root
  let file rel body = do
        createDirectoryIfMissing True (takeDirectory (root </> rel))
        T.writeFile (root </> rel) body
      pkg :: Text -> Text -> [Text] -> IO ()
      pkg name modName deps = do
        let dir = "packages/" <> T.unpack name
        file (dir <> "/" <> T.unpack name <> ".cabal") $
          T.unlines
            [ "cabal-version: 3.0"
            , "name:          " <> name
            , "version:       0.1.0.0"
            , "synopsis:      A package for trying cabalist"
            , "description:   Part of the monorepo cabalist's self-test makes."
            , "license:       MIT"
            , "license-file:  LICENSE"
            , "maintainer:    selftest@example.com"
            , "category:      Testing"
            , "build-type:    Simple"
            , "extra-doc-files: CHANGELOG.md"
            , ""
            , "library"
            , "    exposed-modules:  " <> modName
            , "    build-depends:    " <> T.intercalate ", " ("base <5" : deps)
            , "    hs-source-dirs:   src"
            , "    default-language: Haskell2010"
            ]
        file (dir <> "/LICENSE") "MIT\n"
        file (dir <> "/CHANGELOG.md") "# Revision history\n\n## 0.1.0.0 -- 2026-09-18\n\n* First version.\n"
        file (dir <> "/src/" <> T.unpack modName <> ".hs") ("module " <> modName <> " where\n\nanswer :: Int\nanswer = 42\n")
  pkg "cabalist-demo-core" "Core" []
  pkg "cabalist-demo-app" "App" ["cabalist-demo-core"]
  file "cabal.project" "packages: packages/*\n"
  let git args = void (runLogged_ nullLogger root "git" args)
  git ["init", "-q", "-b", "main"]
  git ["config", "user.email", "selftest@example.com"]
  git ["config", "user.name", "cabalist self-test"]
  git ["add", "-A"]
  git ["commit", "-q", "-m", "Initial"]

-- | Wait (up to two minutes) until the repository is open and Hackage has
-- answered, or opening failed.
waitForRepo :: Env -> IO AppState
waitForRepo env = getMonotonicTime >>= go
  where
    go start = do
      st <- readState env
      now <- getMonotonicTime
      let ready = isJust (stRepo st) && not (stHackageLoading st) && isJust (stCabal st)
          failed = isJust (stError st) && not (isJust (stLoading st))
      if ready || failed || now - start > 120
        then pure st
        else threadDelay 50000 >> go start

data Headless = Headless
  { hFrame :: Input -> IO ()
  , hBase :: Input
  , hSave :: FilePath -> IO ()
  , hSaveWith :: Input -> FilePath -> IO ()
  -- ^ 'hSave' for a state the pointer is part of, such as a hovered stop.
  , hSpans :: IO [(Rect, Text, Color, Color, Rect)]
  }

headless :: SdlOptions -> Env -> ViewCache -> (Headless -> IO a) -> IO a
headless opts env cache k = do
  ctx0 <- newPixelContext >>= \c -> maybe (pure c) (withTheme c) (sdlAppTheme opts)
  withSdl opts {sdlWindowHidden = True, sdlWindowResizable = False} ctx0 $ \ctx sdlEnv -> do
    let base = emptyInput {inputWindowSize = sdlWindowSize opts, inputMousePos = V2 (-1) (-1)}
        frame forceFull i = void (sdlDrawFrame ctx (appView env cache) sdlEnv i forceFull)
        save inp path = do
          replicateM_ 2 (frame False inp)
          frame True inp
          saved <- saveScreenshot sdlEnv path
          unless saved $ fail ("could not save " <> path)
        spans = do
          a <- collectTextSpans ctx
          b <- collectOverlayTextSpans ctx base
          pure (a <> b)
    k Headless {hFrame = frame False, hBase = base, hSave = save base, hSaveWith = save, hSpans = spans}

-- | Render the window for a repository and save it.
screenshot :: SdlOptions -> FilePath -> FilePath -> IO ()
screenshot opts repoDir out = do
  env <- newEnv Nothing True
  cache <- newViewCache repoDir
  headless opts env cache $ \h -> do
    replicateM_ 3 (hFrame h (hBase h))
    _ <- waitForRepo env
    replicateM_ 3 (hFrame h (hBase h))
    hSave h out
  exitSuccess

selfTest :: SdlOptions -> FilePath -> IO ()
selfTest opts dir =
  selfTestSteps opts dir `catch` \(e :: SomeException) ->
    case fromException e of
      Just (code :: ExitCode) -> exitWith code
      Nothing -> do
        hPutStrLn stderr ("selftest: FAILED: " <> displayException e)
        hFlush stderr
        exitWith (ExitFailure 2)

selfTestSteps :: SdlOptions -> FilePath -> IO ()
selfTestSteps opts dir = do
  createDirectoryIfMissing True dir
  tmp <- getTemporaryDirectory
  let root = tmp </> "cabalist-selftest-repo"
  makeDemoRepo root
  env <- newEnv Nothing True
  cache <- newViewCache root
  headless opts env cache $ \h -> do
    let base = hBase h
        frame = hFrame h
        settle = replicateM_ 3 (frame base)
        step name = putStrLn ("selftest: " <> name) >> hFlush stdout
        shot name = hSave h (dir </> name <> ".bmp")
        dumpVisible = do
          visible <- hSpans h
          hPutStrLn stderr ("selftest: visible text: " <> show [t | (_, t, _, _, _) <- take 120 visible])
          hSave h (dir </> "failure.bmp")
        -- Waits a little: statuses are read again in the background after
        -- each job, a moment after the job itself reports finishing.
        expect needle = getMonotonicTime >>= \start ->
          let go = do
                present <- hasText needle <$> hSpans h
                t <- getMonotonicTime
                if present
                  then pure ()
                  else
                    if t - start > 20
                      then dumpVisible >> fail ("expected to see " <> show needle)
                      else threadDelay 50000 >> settle >> go
           in go
        expectGone needle = do
          present <- hasText needle <$> hSpans h
          when present $ dumpVisible >> fail ("expected " <> show needle <> " to be gone")
        click label = do
          found <- findExact label <$> hSpans h
          when (found == Nothing) dumpVisible
          target <- requireSpan ("no " <> show label <> " on screen") found
          clickPos frame base target
          settle
        -- The first span whose text passes a test, for labels that depend
        -- on the machine (the login button names cabal's config).
        clickWhere what ok = do
          found <- hSpans h
          let hit = listToMaybe [V2 (x + w / 2) (y + hh / 2) | (Rect x y w hh, t, _, _, _) <- found, ok t]
          when (hit == Nothing) dumpVisible
          target <- requireSpan ("no " <> what <> " on screen") hit
          clickPos frame base target
          settle
        press key = frame base {inputKeys = inputKeysFromList [key]} >> settle
        -- Frames keep running while jobs do, as the real event loop would.
        waitForJobs = getMonotonicTime >>= \start ->
          let go = do
                settle
                st <- readState env
                t <- getMonotonicTime
                let busy = any (not . jobFinished . jobStatus) (toList (stJobs st))
                if busy && t - start < 600 then threadDelay 100000 >> go else pure st
           in go

    settle
    st0 <- waitForRepo env
    settle
    case stRepo st0 of
      Nothing -> fail ("the demo repository did not open: " <> maybe "timed out" T.unpack (stError st0))
      Just _ -> pure ()
    step "repository open"
    expect "2 packages"
    expect "cabalist-demo-app"
    expect "This version has not been released"
    expect "Tag and build"
    shot "01-open"

    step "tag and build from a clean clone"
    click "Tag and build"
    st1 <- waitForJobs
    case [j | j <- toList (stJobs st1)] of
      (j : _) | jobStatus j == JobSucceeded -> pure ()
      (j : _) -> do
        mapM_ (T.hPutStrLn stderr) (toList (jobLog j))
        dumpVisible
        fail ("the job did not succeed: " <> show (jobStatus j))
      [] -> dumpVisible >> fail "no job was queued"
    settle
    expect "Upload candidate"
    expect "Publish now…"
    expect "done in"
    shot "02-tagged"

    step "the log"
    click "Show log"
    expect "✓ Done"
    shot "03-log"
    doneAt <- requireSpan "no ✓ Done" . findExact "✓ Done" =<< hSpans h
    replicateM_ 5 (frame base {inputMousePos = doneAt, inputScroll = V2 0 (-3)} >> settle)
    shot "03b-log-scrolled"
    click "Hide log"

    step "the track's stops, then the bump dialog"
    mapM_ expect ["Version", "Tag", "Candidate", "Published"]
    click "More"
    shot "04-more"
    click "Bump version…"
    expect "Bump cabalist-demo-app"
    expect "0.1.0.1"
    shot "05-bump"
    click "Cancel"
    expectGone "Bump cabalist-demo-app"

    step "publish confirmation (dry run)"
    click "Publish now…"
    expect "Publish cabalist-demo-app-0.1.0.0"
    expect "nothing is pushed or uploaded"
    shot "06-publish"
    press KeyEscape
    expectGone "nothing is pushed or uploaded"

    step "login dialog"
    clickWhere "the login button" (\t -> "Log in to Hackage" == t || "Hackage: " `T.isPrefixOf` t)
    expect "API token"
    click "API token"
    shot "07-login"
    press KeyEscape

    step "settings dialog"
    click "Settings"
    expect "Building"
    shot "08-settings"
    click "Cancel"
    expectGone "Building"

    step "a candidate is up: its link, and republishing it"
    -- A dry run uploads nothing, so mark the candidate uploaded as a real
    -- upload would, then read the repository again.
    T.writeFile (root </> ".cabalist" </> "cabalist-demo-app-0.1.0.0.tar.gz.candidate") "cabalist-demo-app-0.1.0.0\n"
    refreshRepo env
    expect "Republish candidate"
    expect "The candidate is on Hackage"
    shot "09-candidate-up"
    click "Republish candidate"
    st3 <- waitForJobs
    case reverse (toList (stJobs st3)) of
      (j : _) | jobStatus j == JobSucceeded, optForce (jobOptions j) -> pure ()
      js -> dumpVisible >> fail ("republishing the candidate did not succeed: " <> show [(jobAction j, jobStatus j) | j <- js])
    expect "Republish candidate cabalist-demo-app-0.1.0.0"
    -- The new tarball replaced the uploaded one, so it waits to be uploaded.
    -- (The candidate link is rich text, which the span collector does not
    -- report; 09-candidate-up.bmp shows it.)
    expect "The tarball is built"

    step "a stop back on the track says what it does, and does it"
    tagStop <- requireSpan "no Tag stop" . findExact "Tag" =<< hSpans h
    -- The tooltip only shows while the pointer is on the stop, and settling
    -- puts it back outside the window.
    replicateM_ 3 (frame base {inputMousePos = tagStop})
    tip <- hSpans h
    unless (hasText "Replace the tag and tarball" tip) $
      dumpVisible >> fail "the Tag stop did not say what clicking it does"
    hSaveWith h base {inputMousePos = tagStop} (dir </> "09b-stop-hovered.bmp")
    click "Tag"
    st4 <- waitForJobs
    case reverse (toList (stJobs st4)) of
      (j : _) | jobStatus j == JobSucceeded, jobAction j == ActTagDist, optForce (jobOptions j) -> pure ()
      js -> dumpVisible >> fail ("replacing the tag did not succeed: " <> show [(jobAction j, jobStatus j) | j <- js])
    expect "Replace the tag and tarball cabalist-demo-app-0.1.0.0"
    expect "The tarball is built"

    step "several packages: dependencies first, candidates uploaded (dry run)"
    click "Select"
    click "Select unreleased"
    expect "2 packages, dependencies first:"
    expect "cabalist-demo-core, cabalist-demo-app"
    shot "10-select"
    click "Upload candidates"
    st2 <- waitForJobs
    let batch = drop 3 (toList (stJobs st2))
    case batch of
      [core, app]
        | pkgName (jobPackage core) == "cabalist-demo-core"
        , pkgName (jobPackage app) == "cabalist-demo-app"
        , all ((== JobSucceeded) . jobStatus) batch
        , any ("dry run: would run cabal upload" `T.isInfixOf`) (toList (jobLog app)) ->
            pure ()
      _ -> do
        mapM_ (\j -> mapM_ (T.hPutStrLn stderr) (toList (jobLog j))) batch
        dumpVisible
        fail ("the batch did not run in dependency order: " <> show [(pkgName (jobPackage j), jobStatus j) | j <- batch])
    click "Show log"
    -- The log was scrolled up earlier, so it no longer follows new lines.
    click "Follow"
    expect "candidate: https://hackage.haskell.org/package/cabalist-demo-app-0.1.0.0/candidate"
    shot "11-batch-done"

    step "passed"
  exitSuccess
