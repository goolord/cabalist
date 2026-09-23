-- | Finding the Cabal packages in a repository and reading what a release
-- needs from their .cabal files.
--
-- A monorepo keeps packages in subdirectories, so every .cabal file git knows
-- about (tracked, or untracked but not ignored) is a candidate, wherever it
-- sits. Files git ignores, such as those under dist-newstyle, are skipped.
module Cabalist.Package
  ( Package (..)
  , pkgId
  , pkgIsRoot
  , pkgHasDocs
  , discoverPackages
  , readPackage
  , releaseOrder
  , internalDeps
  , Version
  , showVersion
  , parseVersion
  )
where

import Control.Exception (IOException, try)
import Data.ByteString qualified as B
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Package (depPkgName, unPackageName)
import Distribution.Package qualified as C
import Distribution.PackageDescription
  ( GenericPackageDescription (..)
  , PackageDescription (..)
  , LibraryVisibility (..)
  , buildInfo
  , libBuildInfo
  , libVisibility
  , unUnqualComponentName
  , targetBuildDepends
  )
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.CondTree (condTreeData)
import Distribution.Types.Version (Version)
import Distribution.Utils.ShortText (fromShortText)
import Cabalist.Git (trackedFiles)
import System.FilePath (normalise, takeBaseName, takeDirectory, takeExtension, (</>))

data Package = Package
  { pkgName :: !Text
  , pkgVersion :: !Version
  , pkgDir :: !FilePath
  -- ^ Relative to the repository root; @.@ for a package at the root.
  , pkgCabalFile :: !FilePath
  -- ^ Relative to the repository root.
  , pkgSynopsis :: !Text
  , pkgDeps :: ![Text]
  -- ^ Every package the library, sublibraries and executables depend on.
  , pkgHasLibrary :: !Bool
  -- ^ A main library.
  , pkgPublicSubLibs :: ![Text]
  -- ^ The public sublibraries, whose documentation Hackage shows alongside
  -- the main library's.
  , pkgNameMismatch :: !Bool
  -- ^ The .cabal file is not named after the package, which cabal sdist
  -- and Hackage reject.
  }
  deriving (Eq, Show)

-- | @name-version@, as tarballs and Hackage URLs spell it.
pkgId :: Package -> Text
pkgId p = pkgName p <> "-" <> showVersion (pkgVersion p)

pkgIsRoot :: Package -> Bool
pkgIsRoot p = normalise (pkgDir p) == "."

-- | Whether the package has a library Hackage can document.
pkgHasDocs :: Package -> Bool
pkgHasDocs p = pkgHasLibrary p || not (null (pkgPublicSubLibs p))

showVersion :: Version -> Text
showVersion = T.pack . prettyShow

parseVersion :: Text -> Maybe Version
parseVersion = simpleParsec . T.unpack . T.strip

-- | Every package in the repository, sorted by directory, and the .cabal
-- files that could not be read.
discoverPackages :: FilePath -> IO ([Package], [(FilePath, Text)])
discoverPackages root = do
  files <- filter ((== ".cabal") . takeExtension) <$> trackedFiles root "*.cabal"
  results <- mapM (\f -> (f,) <$> readPackage root (normalise f)) files
  let ok = [p | (_, Right p) <- results]
      bad = [(f, e) | (f, Left e) <- results]
  pure (sortOn (\p -> (not (pkgIsRoot p), pkgDir p, pkgName p)) ok, bad)

-- | Read one .cabal file, given relative to the root.
readPackage :: FilePath -> FilePath -> IO (Either Text Package)
readPackage root rel = do
  r <- try (B.readFile (root </> rel))
  pure $ case r of
    Left (e :: IOException) -> Left (T.pack (show e))
    Right bytes -> case parseGenericPackageDescriptionMaybe bytes of
      Nothing -> Left "could not parse the .cabal file"
      Just gpd -> Right (fromDescription gpd)
  where
    fromDescription gpd =
      let pd = packageDescription gpd
          ident = package pd
          name = T.pack (unPackageName (C.pkgName ident))
          libDeps = concatMap (targetBuildDepends . libBuildInfo) (concatMap toList (condLibrary gpd))
          subDeps = concatMap (targetBuildDepends . libBuildInfo) (concatMap (toList . snd) (condSubLibraries gpd))
          exeDeps = concatMap (targetBuildDepends . buildInfo) (concatMap (toList . snd) (condExecutables gpd))
          deps = Set.toList (Set.fromList [T.pack (unPackageName (depPkgName d)) | d <- libDeps <> subDeps <> exeDeps])
       in Package
            { pkgName = name
            , pkgVersion = C.pkgVersion ident
            , pkgDir = normalise (takeDirectory rel)
            , pkgCabalFile = rel
            , pkgSynopsis = T.strip (T.pack (fromShortText (synopsis pd)))
            , pkgDeps = filter (/= name) deps
            , pkgHasLibrary = isJust (condLibrary gpd)
            , pkgPublicSubLibs =
                [ T.pack (unUnqualComponentName n)
                | (n, t) <- condSubLibraries gpd
                , libVisibility (condTreeData t) == LibraryVisibilityPublic
                ]
            , pkgNameMismatch = T.pack (takeBaseName rel) /= name
            }

-- | The packages of this repository that a package depends on. Applied to
-- the packages alone, it indexes them once for every package asked about.
internalDeps :: [Package] -> Package -> [Package]
internalDeps pkgs = \p -> mapMaybe (`Map.lookup` byName) (pkgDeps p)
  where
    byName = Map.fromList [(pkgName q, q) | q <- pkgs]

-- | The packages in an order where each comes after the packages of the
-- repository it depends on, so a batch release uploads dependencies first.
-- Ties keep the given order. Dependency cycles, which Cabal would reject
-- anyway, are broken arbitrarily rather than looping.
releaseOrder :: [Package] -> [Package] -> [Package]
releaseOrder allPkgs chosen = go Set.empty chosen []
  where
    chosenNames = Set.fromList (map pkgName chosen)
    internal = internalDeps allPkgs
    depsOf p = [d | d <- map pkgName (internal p), d `Set.member` chosenNames]
    go _ [] acc = reverse acc
    go placed pending acc =
      case break (all (`Set.member` placed) . depsOf) pending of
        (before, p : after) -> go (Set.insert (pkgName p) placed) (before <> after) (p : acc)
        -- Only a cycle is left: take the first package anyway.
        (p : rest, []) -> go (Set.insert (pkgName p) placed) rest (p : acc)
        ([], []) -> reverse acc
