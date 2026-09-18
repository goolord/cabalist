-- | Per-repository settings, kept in @.cabalist/config@ at the repository
-- root next to the tarballs cabalist makes. The directory ignores itself (it
-- holds a @.gitignore@ of @*@), so nothing needs adding to the repository.
--
-- The one setting that matters is the tag format. A repository with one
-- package at its root usually tags @v1.2@, as hkgr does; a monorepo needs the
-- package name in the tag, as in @nano-ui-v1.2@. The default follows whatever
-- the repository's existing tags already do.
module Cabalist.Config
  ( Config (..)
  , defaultConfig
  , workDir
  , tarballPath
  , publishedMarker
  , candidateMarker
  , loadConfig
  , saveConfig
  , ensureWorkDir
  , renderTag
  , tagPattern
  , parseTagVersion
  , tagFormats
  , guessTagFormat
  , validTagFormat
  )
where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Cabalist.File (readUtf8, writeUtf8)
import Cabalist.Git (listTags)
import Cabalist.Package (Package (..), Version, parseVersion, pkgId, pkgIsRoot, showVersion)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((<.>), (</>))

data Config = Config
  { cfgTagFormat :: !Text
  -- ^ With @{name}@ and @{version}@ placeholders.
  , cfgRemote :: !Text
  -- ^ The remote a release pushes its tag to.
  }
  deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config {cfgTagFormat = "{name}-v{version}", cfgRemote = "origin"}

-- | Where cabalist keeps its tarballs, published markers, and settings.
workDir :: FilePath -> FilePath
workDir root = root </> ".cabalist"

tarballPath :: FilePath -> Package -> FilePath
tarballPath root p = workDir root </> T.unpack (pkgId p) <.> "tar.gz"

-- | Written next to the tarball once a release is published, so cabalist
-- refuses to tag or upload that version again (as hkgr does).
publishedMarker :: FilePath -> Package -> FilePath
publishedMarker root p = tarballPath root p <.> "published"

configFile :: FilePath -> FilePath
configFile root = workDir root </> "config"

ensureWorkDir :: FilePath -> IO ()
ensureWorkDir root = do
  createDirectoryIfMissing True (workDir root)
  let ignore = workDir root </> ".gitignore"
  exists <- doesFileExist ignore
  unless exists $ writeUtf8 ignore "*\n"

-- | The saved settings, or a default guessed from the repository's tags.
loadConfig :: FilePath -> [Package] -> IO Config
loadConfig root pkgs = do
  guessed <- guessTagFormat root pkgs
  let base = defaultConfig {cfgTagFormat = guessed}
  r <- try (readUtf8 (configFile root))
  pure $ case r of
    Left (_ :: IOException) -> base
    Right src -> foldl apply base (T.lines src)
  where
    apply cfg l = case T.breakOn ":" l of
      (k, v) | not (T.null v) -> case T.strip k of
        "tag-format" | validTagFormat (T.strip (T.drop 1 v)) -> cfg {cfgTagFormat = T.strip (T.drop 1 v)}
        "remote" | not (T.null (T.strip (T.drop 1 v))) -> cfg {cfgRemote = T.strip (T.drop 1 v)}
        _ -> cfg
      _ -> cfg

saveConfig :: FilePath -> Config -> IO ()
saveConfig root cfg = do
  ensureWorkDir root
  writeUtf8 (configFile root) $
    T.unlines
      [ "-- cabalist settings for this repository"
      , "tag-format: " <> cfgTagFormat cfg
      , "remote: " <> cfgRemote cfg
      ]

-- | A format must name the version, and the package too unless it is the
-- only package, which 'guessTagFormat' and the settings dialog see to.
validTagFormat :: Text -> Bool
validTagFormat f = "{version}" `T.isInfixOf` f && not (T.any (`elem` (" ~^:?*[\\" :: String)) (T.replace "{name}" "" (T.replace "{version}" "" f)))

renderTag :: Text -> Package -> Text
renderTag fmt p = T.replace "{name}" (pkgName p) (T.replace "{version}" (showVersion (pkgVersion p)) fmt)

-- | A glob matching the package's tags for any version.
tagPattern :: Text -> Package -> String
tagPattern fmt p = T.unpack (T.replace "{name}" (pkgName p) (T.replace "{version}" "*" fmt))

-- | The version a tag names, if it is one of the package's tags.
parseTagVersion :: Text -> Package -> Text -> Maybe Version
parseTagVersion fmt p tag =
  let withName = T.replace "{name}" (pkgName p) fmt
      (pre, post0) = T.breakOn "{version}" withName
      post = T.drop (T.length "{version}") post0
   in if T.isPrefixOf pre tag && T.isSuffixOf post tag && T.length tag > T.length pre + T.length post
        then parseVersion (T.drop (T.length pre) (T.dropEnd (T.length post) tag))
        else Nothing

-- | Formats in common use.
tagFormats :: [Text]
tagFormats = ["{name}-v{version}", "{name}-{version}", "v{version}", "{version}", "{name}/v{version}", "{name}/{version}"]

-- | The format most of the repository's existing tags follow. With no such
-- tags: @v{version}@ for a single package at the root, as hkgr tags, and
-- @{name}-v{version}@ for a monorepo.
guessTagFormat :: FilePath -> [Package] -> IO Text
guessTagFormat root pkgs = do
  tags <- listTags root "*"
  let single = case pkgs of
        [p] -> pkgIsRoot p
        _ -> False
      candidates = if single then tagFormats else filter ("{name}" `T.isInfixOf`) tagFormats
      score fmt = length [() | t <- tags, any (\p -> parseTagVersion fmt p t /= Nothing) pkgs]
      fallback = if single then "v{version}" else "{name}-v{version}"
      ranked = sortOn (Down . snd) [(f, s) | f <- candidates, let s = score f, s > 0]
  pure $ case ranked of
    ((f, _) : _) -> f
    [] -> fallback


-- | Written next to the tarball once it is uploaded as a candidate. Making a
-- new tarball removes it.
candidateMarker :: FilePath -> Package -> FilePath
candidateMarker root p = tarballPath root p <.> "candidate"
