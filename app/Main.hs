-- | cabalist: a GUI for making Hackage releases of the Cabal packages in a
-- git repository, monorepos included.
--
-- @cabalist [--dry-run] [DIRECTORY]@ opens the repository containing
-- DIRECTORY, or else the one opened last, or else the current directory's.
-- @--selftest DIR@ drives the window headlessly against a throwaway
-- repository and saves screenshots to DIR; @--screenshot OUT.bmp DIRECTORY@
-- saves one of a repository of your own.
module Main (main) where

import Cabalist.Git (gitTopLevel)
import Data.Maybe (fromMaybe)
import Gui.SelfTest (screenshot, selfTest)
import Gui.State (lastRepo, newEnv)
import Gui.Style (appTheme)
import Gui.View (appView, newViewCache)
import NanoUI (Size (..))
import NanoUI.Backend.Sdl (NanoUIFont (..), SdlOptions (..), defaultSdlOptions, runSdlApp)
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, hSetEncoding, stderr, stdout, utf8)

windowOptions :: SdlOptions
windowOptions =
  defaultSdlOptions
    { sdlWindowTitle = "cabalist"
    , sdlWindowSize = Size 1500 1000
    , sdlAppTheme = Just appTheme
    , sdlAppFont = FontSearch ["SegUIVar", "Segoe UI", "Inter", "Cantarell", "Helvetica Neue"]
    , sdlAppMonoFont = FontSearch ["Cascadia Mono", "Consolas", "JetBrains Mono", "DejaVu Sans Mono", "Menlo"]
    , sdlAppFontSize = 17
    }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  -- Logs carry ✓ and ✗, which a legacy console code page cannot encode.
  mapM_ (`hSetEncoding` utf8) [stdout, stderr]
  args <- getArgs
  case args of
    ["--selftest", dir] -> selfTest windowOptions dir
    ["--screenshot", out, dir] -> screenshot windowOptions dir out
    _ -> do
      let dryRun = "--dry-run" `elem` args
          dirs = filter (/= "--dry-run") args
      initial <- case dirs of
        [d] -> do
          ok <- doesDirectoryExist d
          if ok then pure d else hPutStrLn stderr ("cabalist: no such directory: " <> d) >> exitFailure
        [] -> startingRepo
        _ -> hPutStrLn stderr "usage: cabalist [--dry-run] [DIRECTORY]" >> exitFailure
      env <- newEnv dryRun
      cache <- newViewCache initial
      runSdlApp windowOptions (appView env cache)

-- | The current directory's repository if there is one, else the repository
-- opened last, else nothing.
startingRepo :: IO FilePath
startingRepo = do
  cwd <- getCurrentDirectory
  here <- gitTopLevel cwd
  case here of
    Just root -> pure root
    Nothing -> fromMaybe "" <$> lastRepo
