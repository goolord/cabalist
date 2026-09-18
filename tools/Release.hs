{-# LANGUAGE LambdaCase #-}

-- | Builds cabalist for release:
--
-- > runghc tools/Release.hs              -- dist/cabalist-VERSION-OS-ARCH/
-- > runghc tools/Release.hs --archive    -- the same, zipped (Windows) or tarred
-- > runghc tools/Release.hs --no-upx     -- skip compressing the executable
-- > runghc tools/Release.hs --clean      -- remove dist/ and the release build
--
-- The build uses cabal.project.release, which links every Haskell library in.
-- On Windows SDL3, SDL3_ttf and everything they need are linked in too, from
-- MSYS2's static libraries, so the release is a single executable. The
-- executable is stripped, then packed with @upx --best@ (except on macOS, where
-- UPX does not work).
module Main (main) where

import Control.Monad (filterM, unless, when)
import Data.Char (isSpace)
import Data.List (find, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import System.FilePath
import System.Info (arch, os)
import System.Process (callProcess, readProcess)

data Options = Options {archive, upx, clean :: Bool}

main :: IO ()
main = do
  opts <- parseArgs =<< getArgs
  if clean opts
    then mapM_ removePathForcibly ["dist", buildDir]
    else release opts

parseArgs :: [String] -> IO Options
parseArgs = go (Options False True False)
  where
    go o = \case
      [] -> pure o
      "--archive" : rest -> go o {archive = True} rest
      "--no-upx" : rest -> go o {upx = False} rest
      "--clean" : rest -> go o {clean = True} rest
      arg : _ -> die ("unknown argument " ++ arg ++ "; expected --archive, --no-upx or --clean")

buildDir :: FilePath
buildDir = "dist-newstyle-release"

data Platform = Windows | MacOS | Linux
  deriving (Eq)

platform :: Platform
platform = case os of
  "mingw32" -> Windows
  "darwin" -> MacOS
  _ -> Linux

platformName :: String
platformName = case platform of
  Windows -> "windows"
  MacOS -> "macos"
  Linux -> "linux"

release :: Options -> IO ()
release opts = do
  version <- packageVersion
  linkOptions <- if platform == Windows then staticSdl else pure []
  let cabalArgs = ["--project-file=cabal.project.release", "--builddir=" ++ buildDir] ++ linkOptions
      name = "cabalist-" ++ version ++ "-" ++ platformName ++ "-" ++ arch
      stage = "dist" </> name
      exe = stage </> "cabalist" <.> exeExtension

  built <- trim <$> readProcess "cabal" (["list-bin"] ++ cabalArgs ++ ["exe:cabalist"]) ""
  -- Neither cabal nor GHC relinks when only what is linked in changes (a newer
  -- SDL, say), and cabal keeps its own record of the executable, so drop the
  -- component's whole build directory, .../x/cabalist, to relink every time.
  case find ((== "x") . takeFileName . takeDirectory) (takeWhile hasParent (iterate takeDirectory built)) of
    Just component -> removePathForcibly component
    Nothing -> die ("no x/cabalist directory above " ++ built)
  callProcess "cabal" (["build"] ++ cabalArgs ++ ["exe:cabalist"])

  removePathForcibly stage
  createDirectoryIfMissing True stage
  copyFile built exe
  when (platform == Windows) (checkImports exe)
  strip <- stripProgram
  callProcess strip [exe]
  when (platform == MacOS) $
    callProcess "codesign" ["--force", "--sign", "-", exe]
  when (upx opts && platform /= MacOS) $ do
    callProcess "upx" ["--best", exe]
    callProcess "upx" ["-t", exe]
  mapM_ (\f -> copyFile f (stage </> f)) ["LICENSE", "README.md", "CHANGELOG.md"]
  putStrLn ("release: " ++ stage)

  when (archive opts) $ do
    file <- case platform of
      Windows -> do
        -- Windows' own tar writes zip files.
        systemRoot <- fromMaybe "C:\\Windows" <$> lookupEnv "SystemRoot"
        let zip' = name <.> "zip"
        removePathForcibly ("dist" </> zip')
        callProcess (systemRoot </> "System32" </> "tar.exe") ["-C", "dist", "-a", "-cf", "dist" </> zip', name]
        pure zip'
      _ -> do
        let tarball = name <.> "tar.gz"
        callProcess "tar" ["-C", "dist", "-czf", "dist" </> tarball, name]
        pure tarball
    putStrLn ("archive: " ++ "dist" </> file)

packageVersion :: IO String
packageVersion = do
  cabalFile <- lines <$> readFile "cabalist.cabal"
  case mapMaybe (stripPrefix "version:") cabalFile of
    v : _ -> pure (trim v)
    [] -> die "no version in cabalist.cabal"

-- | GHC options that link SDL3, SDL3_ttf and their dependencies in statically
-- from MSYS2's archives.
--
-- nano-ui-sdl asks for -lSDL3 and -lSDL3_ttf, which find the DLL import
-- libraries, and ahead of anything added here; naming the static archives as
-- inputs puts them first. -Bstatic makes the rest of the list static. libatomic
-- is GCC's, and clang has what it holds built in. MSYS2's libpng calls ucrt's
-- setjmp by the name GCC's import library gives it, which GHC's lacks.
staticSdl :: IO [String]
staticSdl = do
  libdir <- sdlLibDir
  libs <- words <$> readProcess "pkg-config" ["--static", "--libs-only-l", "sdl3-ttf", "sdl3"] ""
  let links = filter (/= "-latomic") libs ++ ["-lstdc++", "-lwinpthread"]
  -- All in one --ghc-options: given one option per --ghc-options, the link came
  -- out with freetype, harfbuzz and iconv still as DLLs.
  pure . pure . ("--ghc-options=" ++) . unwords . map quote $
    [libdir </> "libSDL3_ttf.a", libdir </> "libSDL3.a", "-optl-L" ++ libdir, "-optl-Wl,-Bstatic"]
      ++ map ("-optl" ++) links
      ++ ["-optl-Wl,-Xlink=-alternatename:__imp__setjmp=__imp_setjmp"]
  where
    quote s = if any isSpace s then "\"" ++ s ++ "\"" else s

sdlLibDir :: IO FilePath
sdlLibDir = trim <$> readProcess "pkg-config" ["--variable=libdir", "sdl3"] ""

-- | Fail when the executable still loads a DLL from MSYS2, which would mean
-- the static link above let a library through, and the release would not run
-- on its own.
checkImports :: FilePath -> IO ()
checkImports exe = do
  objdump <- ghcTool "objdump.exe"
  dlls <- mapMaybe dllName . lines <$> readProcess objdump ["-p", exe] ""
  msysBin <- (</> ".." </> "bin") <$> sdlLibDir
  leaked <- filterM (doesFileExist . (msysBin </>)) dlls
  unless (null leaked) $
    die ("the release would need these DLLs from " ++ msysBin ++ ": " ++ unwords leaked)
  where
    dllName l = trim <$> stripPrefix "DLL Name:" (dropWhile isSpace l)

-- | GHC on Windows brings its own binutils, which are not usually on the PATH.
stripProgram :: IO FilePath
stripProgram
  | platform == Windows = ghcTool "strip.exe"
  | otherwise = pure "strip"

ghcTool :: FilePath -> IO FilePath
ghcTool tool = do
  libdir <- trim <$> readProcess "ghc" ["--print-libdir"] ""
  pure (takeDirectory libdir </> "mingw" </> "bin" </> tool)

hasParent :: FilePath -> Bool
hasParent p = takeDirectory p /= p

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
