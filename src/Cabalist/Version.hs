-- | Bumping a package's version: the new version itself, the edit to the
-- .cabal file's version field, and a changelog entry for it.
module Cabalist.Version
  ( Bump (..)
  , bumpLabel
  , bumpVersion
  , setVersionField
  , findChangelog
  , changelogMentions
  , addChangelogEntry
  )
where

import Data.Char (isDigit, isSpace, toLower)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Types.Version (mkVersion, versionNumbers)
import Cabalist.Package (Version, showVersion)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))

-- | Which component of an @A.B.C.D@ version to increment. Under the PVP,
-- @A.B@ is the major version: bump it for breaking changes, @C@ for
-- additions, and @D@ for everything else.
data Bump = BumpA | BumpB | BumpC | BumpD
  deriving (Eq, Show, Enum, Bounded)

bumpLabel :: Bump -> Text
bumpLabel = \case
  BumpA -> "Epoch (A.x.x.x)"
  BumpB -> "Major (x.B.x.x)"
  BumpC -> "Minor (x.x.C.x)"
  BumpD -> "Patch (x.x.x.D)"

-- | Increment one component, padding with zeros up to it and dropping the
-- components after it: bumping @C@ of @0.4@ gives @0.4.1@.
bumpVersion :: Bump -> Version -> Version
bumpVersion b v =
  let i = fromEnum b
      ns = versionNumbers v <> repeat 0
   in mkVersion (take i ns <> [ns !! i + 1])

-- | Replace the value of the top-level @version:@ field, keeping the field's
-- own spacing and every other line as it was.
setVersionField :: Version -> Text -> Either Text Text
setVersionField v src =
  case break isVersionLine ls of
    (_, []) -> Left "no top-level version field found"
    (before, l : after) ->
      let (key, rest) = T.breakOn ":" l
          spacing = T.takeWhile isSpace (T.drop 1 rest)
          new = key <> ":" <> spacing <> showVersion v
       in Right (T.intercalate "\n" (before <> [new] <> after))
  where
    ls = T.splitOn "\n" src
    isVersionLine l =
      let (key, rest) = T.breakOn ":" l
       in not (T.null rest)
            && not (T.null key)
            && not (maybe False (isSpace . fst) (T.uncons key))
            && T.toLower (T.stripEnd key) == "version"

-- | The package's changelog, if it has one.
findChangelog :: FilePath -> IO (Maybe FilePath)
findChangelog dir = do
  names <- listDirectory dir
  let candidates = sortOn (map toLower) [n | n <- names, isChangelog (map toLower n)]
  case listToMaybe candidates of
    Nothing -> pure Nothing
    Just n -> do
      isFile <- doesFileExist (dir </> n)
      pure (if isFile then Just n else Nothing)
  where
    isChangelog n = any (`startsWith` n) ["changelog", "changes", "history"]
    startsWith p n = take (length p) n == p

-- | Whether a changelog has an entry heading for a version.
changelogMentions :: Version -> Text -> Bool
changelogMentions v = any mentions . T.lines
  where
    ver = showVersion v
    mentions l =
      let l' = T.strip l
       in isHeading l' && any (== ver) (versionTokens l')
    isHeading l = "#" `T.isPrefixOf` l || "=" `T.isPrefixOf` l || startsWithVersion l || "[" `T.isPrefixOf` l || "v" `T.isPrefixOf` l
    startsWithVersion l = maybe False (isDigit . fst) (T.uncons l)

-- | The version-like words of a line, with brackets and a leading v removed.
versionTokens :: Text -> [Text]
versionTokens l =
  [ t'
  | t <- T.split (\c -> isSpace c || c `elem` ("[]()," :: String)) l
  , let t' = if "v" `T.isPrefixOf` t then T.drop 1 t else t
  , not (T.null t')
  , T.all (\c -> isDigit c || c == '.') t'
  ]

-- | Add an entry for a new version above the newest existing one, in the
-- style of the entries already there (@## 0.1.0.0 -- 2026-01-02@ as cabal
-- init writes them, by default). A changelog that already has an entry for
-- the version is returned unchanged.
addChangelogEntry :: Version -> Text -> Text -> Text
addChangelogEntry v date src
  | changelogMentions v src = src
  | otherwise =
      case break isEntryHeading ls of
        (before, heading : after) ->
          T.intercalate "\n" (before <> [entryLike heading, "", "* ", "", heading] <> after)
        (_, []) ->
          -- No entries yet: after the title if there is one, else at the top.
          case ls of
            (title : rest) | "#" `T.isPrefixOf` title ->
              T.intercalate "\n" ([title, "", "## " <> ver <> " -- " <> date, "", "* ", ""] <> dropWhile T.null rest)
            _ -> T.intercalate "\n" (["## " <> ver <> " -- " <> date, "", "* ", ""] <> ls)
  where
    ls = T.splitOn "\n" src
    ver = showVersion v
    isEntryHeading l =
      let s = T.strip l
       in ("#" `T.isPrefixOf` s || "[" `T.isPrefixOf` s || maybe False (isDigit . fst) (T.uncons s))
            && not (null (versionTokens s))
            && T.count "#" (T.takeWhile (== '#') s) /= 1
    -- The heading of the newest entry, with its version and any date swapped
    -- for the new ones.
    entryLike heading =
      let hashes = T.takeWhile (== '#') (T.strip heading)
          prefix = if T.null hashes then "" else hashes <> " "
       in prefix <> ver <> " -- " <> date
