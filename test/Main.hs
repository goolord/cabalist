-- | Tests of the release logic against a throwaway monorepo: two packages in
-- subdirectories, one depending on the other. The release steps run for real
-- (git tags, cabal sdist, pristine builds), except that uploads and pushes are
-- dry runs.
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Cabalist.Config
import Cabalist.File (editUtf8, readUtf8, writeUtf8)
import Cabalist.Hackage
import Cabalist.Package
import Cabalist.Process
import Cabalist.Release
import Cabalist.Status
import Cabalist.Version
import System.Directory
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  unitTests failures
  repoTests failures
  n <- readIORef failures
  if n == 0 then putStrLn "all tests passed" else putStrLn (show n <> " test(s) failed") >> exitFailure

check :: IORef Int -> String -> Bool -> IO ()
check failures name ok = do
  putStrLn ((if ok then "ok   " else "FAIL ") <> name)
  unless ok $ modifyIORef' failures (+ 1)

ver :: Text -> Version
ver t = maybe (error ("bad version " <> T.unpack t)) id (parseVersion t)

unitTests :: IORef Int -> IO ()
unitTests failures = do
  let t = check failures
  t "bump patch pads" (bumpVersion BumpD (ver "0.4") == ver "0.4.0.1")
  t "bump minor drops the rest" (bumpVersion BumpC (ver "1.2.3.4") == ver "1.2.4")
  t "bump major" (bumpVersion BumpB (ver "1.2.3") == ver "1.3")
  t "set version keeps spacing" $
    setVersionField (ver "0.2.0.0") "cabal-version: 3.0\nname:    x\nversion:            0.1.0.0\n"
      == Right "cabal-version: 3.0\nname:    x\nversion:            0.2.0.0\n"
  t "set version ignores indented fields" $
    setVersionField (ver "2") "name: x\n  version: 9\nVersion: 1\n" == Right "name: x\n  version: 9\nVersion: 2\n"
  let changelog = "# Revision history for x\n\n## 0.1.0.0 -- 2026-01-01\n\n* First version.\n"
      added = addChangelogEntry (ver "0.1.1") "2026-09-18" changelog
  t "changelog entry above the newest" $
    added == "# Revision history for x\n\n## 0.1.1 -- 2026-09-18\n\n* \n\n## 0.1.0.0 -- 2026-01-01\n\n* First version.\n"
  t "changelog entry is found" (changelogMentions (ver "0.1.1") added)
  t "changelog entry not duplicated" (addChangelogEntry (ver "0.1.1") "2027-01-01" added == added)
  t "changelog with no entries" $
    addChangelogEntry (ver "1.0") "2026-09-18" "# Changes\n" == "# Changes\n\n## 1.0 -- 2026-09-18\n\n* \n"
  t "hackage preferred json" $
    parsePreferred "{\"normal-version\":[\"0.2\",\"0.1.1\"],\"deprecated-version\":[\"0.1\"]}"
      == HackageVersions [ver "0.2", ver "0.1.1"] [ver "0.1"]
  t "latest includes deprecated" (latestVersion (HackageVersions [ver "0.2"] [ver "0.3"]) == Just (ver "0.3"))
  let p = Package "foo-bar" (ver "1.2") "pkgs/foo-bar" "pkgs/foo-bar/foo-bar.cabal" "" [] True False
  t "render tag" (renderTag "{name}-v{version}" p == "foo-bar-v1.2")
  t "parse tag" (parseTagVersion "{name}-v{version}" p "foo-bar-v0.9.1" == Just (ver "0.9.1"))
  t "parse tag of another package" (parseTagVersion "{name}-v{version}" p "foo-v0.9.1" == Nothing)
  t "parse root tag" (parseTagVersion "v{version}" p "v3" == Just (ver "3"))
  t "upload args, token" (fst (uploadArgs (ApiToken "abc") True False "x.tar.gz") == ["upload", "--token=abc", "--publish", "x.tar.gz"])
  t "upload args, password on stdin" (uploadArgs (UserPassword "me" "pw") False True "d.tar.gz" == (["upload", "--username=me", "--documentation", "d.tar.gz"], Just "pw\n"))
  t "logged commands hide secrets" (renderCommand "cabal" ["upload", "--token=abc", "--password=pw", "x.tar.gz"] == "cabal upload --token=… --password=… x.tar.gz")

writeRepoFile :: FilePath -> FilePath -> Text -> IO ()
writeRepoFile root rel body = do
  let path = root </> rel
  createDirectoryIfMissing True (takeDirectory path)
  T.writeFile path body

cabalFile :: Text -> Text -> [Text] -> Text
cabalFile name version deps =
  T.unlines
    [ "cabal-version: 3.0"
    , "name:          " <> name
    , "version:       " <> version
    , "synopsis:      The " <> name <> " test package"
    , "description:   A package for testing cabalist."
    , "license:       MIT"
    , "license-file:  LICENSE"
    , "maintainer:    test@example.com"
    , "category:      Testing"
    , "build-type:    Simple"
    , "extra-doc-files: CHANGELOG.md"
    , ""
    , "library"
    , "    exposed-modules:  " <> T.toTitle name
    , "    build-depends:    " <> T.intercalate ", " ("base <5" : deps)
    , "    hs-source-dirs:   src"
    , "    default-language: Haskell2010"
    ]

git :: FilePath -> [String] -> IO ()
git root args = () <$ runLogged_ nullLogger root "git" args

repoTests :: IORef Int -> IO ()
repoTests failures = do
  let t = check failures
  tmp <- getTemporaryDirectory
  let root = tmp </> "cabalist-test-repo"
  removePathForcibly root
  createDirectoryIfMissing True root
  let pkg name deps = do
        let dir = "packages" </> T.unpack name
        writeRepoFile root (dir </> T.unpack name <> ".cabal") (cabalFile name "0.1.0.0" deps)
        writeRepoFile root (dir </> "LICENSE") "MIT\n"
        writeRepoFile root (dir </> "CHANGELOG.md") "# Revision history\n\n## 0.1.0.0 -- 2026-01-01\n\n* First version.\n"
        writeRepoFile root (dir </> "src" </> T.unpack (T.toTitle name) <> ".hs") ("module " <> T.toTitle name <> " where\n\nanswer :: Int\nanswer = 42\n")
  pkg "alpha" []
  pkg "beta" ["alpha"]
  writeRepoFile root "cabal.project" "packages: packages/*\n"
  git root ["init", "-q", "-b", "main"]
  git root ["config", "user.email", "test@example.com"]
  git root ["config", "user.name", "Test"]
  git root ["add", "-A"]
  git root ["commit", "-q", "-m", "Initial"]

  (pkgs, bad) <- discoverPackages root
  t "discovers both packages" (map pkgName pkgs == ["alpha", "beta"] && null bad)
  t "package dirs are relative" (map pkgDir pkgs == ["packages" </> "alpha", "packages" </> "beta"])
  (alpha, beta) <- case pkgs of
    [a, b] -> pure (a, b)
    _ -> fail "expected two packages"
  t "internal dependency" (map pkgName (internalDeps pkgs beta) == ["alpha"])
  t "dependencies first" (map pkgName (releaseOrder pkgs [beta, alpha]) == ["alpha", "beta"])
  fmt <- guessTagFormat root pkgs
  t "monorepo tag format" (fmt == "{name}-v{version}")
  git root ["tag", "alpha-0.0.9"]
  fmt' <- guessTagFormat root pkgs
  t "tag format follows existing tags" (fmt' == "{name}-{version}")
  git root ["tag", "--delete", "alpha-0.0.9"]

  cfg <- loadConfig root pkgs
  cabalExe <- maybe "cabal" fst <$> findCabal
  st0 <- packageStatus root cfg HackageAbsent Nothing alpha
  t "fresh package is ready to tag" (stage alpha st0 == StageReadyToTag)
  t "changelog entry seen" (psChangelogHasEntry st0)

  logRef <- newIORef []
  let lg = nullLogger {logLine = \l -> modifyIORef' logRef (l :)}
      ctxFor _ st opts = Ctx root cfg pkgs opts lg st [] cabalExe
      opts0 = defaultOptions {optHlint = False, optDryRun = True}
      attempt name act = do
        r <- try act
        case r of
          Left (e :: SomeException) -> do
            out <- readIORef logRef
            mapM_ T.putStrLn (reverse (take 40 out))
            putStrLn ("  " <> name <> ": " <> show e)
            pure False
          Right () -> pure True

  okAlpha <- attempt "tagdist alpha" (tagDist (ctxFor alpha st0 opts0) alpha)
  t "tag and sdist alpha (subdirectory package, pristine build)" okAlpha
  tarball <- doesFileExist (tarballPath root alpha)
  t "tarball written" tarball
  st1 <- packageStatus root cfg HackageAbsent Nothing alpha
  t "alpha tagged with tarball" (psTagOnHead st1 && stage alpha st1 == StageCandidate)
  again <- try (tagDist (ctxFor alpha st1 opts0) alpha)
  t "tagdist refuses to repeat without force" (either (\(_ :: StepError) -> True) (const False) again)

  -- beta depends on alpha, which is not on Hackage: its pristine build only
  -- works against the repository's copy.
  stB <- packageStatus root cfg HackageAbsent Nothing beta
  okBeta <- attempt "tagdist beta" (tagDist (ctxFor beta stB opts0 {optSiblings = True}) beta)
  t "tag and sdist beta against its sibling" okBeta

  okUpload <- attempt "upload alpha (dry run)" (upload (ctxFor alpha st1 opts0) alpha)
  t "candidate upload dry run" okUpload
  okPublish <- attempt "publish alpha (dry run)" (publish (ctxFor alpha st1 opts0) alpha)
  t "publish dry run" okPublish
  out <- readIORef logRef
  t "dry run names the upload" (any ("dry run: would run cabal upload --publish" `T.isInfixOf`) out)

  -- A candidate that is up waits to be published, until a new tarball
  -- replaces the one uploaded.
  writeRepoFile root (".cabalist" </> "alpha-0.1.0.0.tar.gz.candidate") "alpha-0.1.0.0\n"
  stUp <- packageStatus root cfg HackageAbsent Nothing alpha
  t "an uploaded candidate waits to be published" (stage alpha stUp == StageUploaded)

  -- More commits after the tag: updating the candidate moves the tag to them
  -- and makes the tarball again over the old one.
  writeRepoFile root ("packages" </> "alpha" </> "src" </> "Alpha.hs") "module Alpha where\n\nanswer :: Int\nanswer = 41\n"
  git root ["commit", "-q", "-am", "Fix alpha before release"]
  stNewer <- packageStatus root cfg HackageAbsent Nothing alpha
  t "commits after the tag are noticed" (length (psUntagged stNewer) == 1 && stage alpha stNewer == StageUploaded)
  okUpdate <- attempt "update candidate" (upload (ctxFor alpha stNewer opts0 {optForce = True}) alpha)
  t "update candidate (forced upload)" okUpdate
  stUpdated <- packageStatus root cfg HackageAbsent Nothing alpha
  t "the tag moved to the new commit" (psTagOnHead stUpdated && null (psUntagged stUpdated))
  t "the new tarball is not uploaded yet" (psTarball stUpdated && not (psCandidateUploaded stUpdated) && stage alpha stUpdated == StageCandidate)

  -- Released on Hackage, then changed: the version needs bumping.
  writeRepoFile root ("packages" </> "alpha" </> "src" </> "Alpha.hs") "module Alpha where\n\nanswer :: Int\nanswer = 43\n"
  git root ["commit", "-q", "-am", "Change alpha"]
  st2 <- packageStatus root cfg (HackageVersions [ver "0.1.0.0"] []) Nothing alpha
  t "changed after release needs a bump" (stage alpha st2 == StageNeedsBump)
  new <- bumpVersionStep (ctxFor alpha st2 opts0) alpha
  t "bumped to 0.1.0.1" (new == ver "0.1.0.1")
  st3 <- packageStatus root cfg (HackageVersions [ver "0.1.0.0"] []) Nothing alpha
  t "uncommitted bump" (stage alpha st3 == StageCommitBump && psChangelogHasEntry st3)
  okCommit <- attempt "commit bump" (commitBump (ctxFor alpha st3 opts0) alpha new)
  t "commit the bump" okCommit
  alpha' <- discoverPackages root >>= \case
    (a : _, _) -> pure a
    _ -> fail "alpha vanished"
  st4 <- packageStatus root cfg (HackageVersions [ver "0.1.0.0"] []) Nothing alpha'
  t "bumped version is ready to tag" (stage alpha' st4 == StageReadyToTag && psPrevRelease st4 /= Nothing)

  -- Edits keep a file's CRLF line endings and its UTF-8 text.
  let crlfFile = root </> "crlf.cabal"
  writeUtf8 crlfFile "name: x\r\nauthor: Zo\235\r\nversion: 1\r\n"
  edited <- editUtf8 crlfFile (setVersionField (ver "2"))
  after <- readUtf8 crlfFile
  t "edit keeps CRLF and UTF-8" (edited == Right True && after == "name: x\r\nauthor: Zo\235\r\nversion: 2\r\n")

  removed <- try (removePathForcibly root)
  when (either (\(_ :: SomeException) -> True) (const False) removed) $ putStrLn "note: could not remove the test repository"
  where
    bumpVersionStep ctx p = bumpPackage ctx p BumpD
