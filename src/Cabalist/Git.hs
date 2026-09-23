-- | The git queries cabalist makes. Each runs quietly in the repository root
-- and answers from git's output; the release steps run their git commands
-- through the logger instead.
module Cabalist.Git
  ( gitTopLevel
  , revParse
  , tagCommit
  , listTags
  , commitsTouching
  , untaggedCommits
  , commitsSince
  , dirtyFiles
  , versionFieldChanged
  , tagOnBranch
  , currentBranch
  , isAncestor
  , trackedFiles
  , gitPathspec
  , forwardSlashes
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Cabalist.File (topLevelKey)
import Cabalist.Process (readCmd, readCmdOk)
import System.Exit (ExitCode (..))
import System.FilePath (normalise)
import Text.Read (readMaybe)

git :: FilePath -> [String] -> IO (Maybe Text)
git root = readCmdOk root "git"

-- | The root of the work tree containing a directory.
gitTopLevel :: FilePath -> IO (Maybe FilePath)
gitTopLevel dir = fmap (normalise . T.unpack) <$> git dir ["rev-parse", "--show-toplevel"]

-- | The commit a revision names.
revParse :: FilePath -> String -> IO (Maybe Text)
revParse root rev = git root ["rev-parse", "-q", "--verify", rev <> "^{commit}"]

-- | The commit a tag points at, if the tag exists.
tagCommit :: FilePath -> Text -> IO (Maybe Text)
tagCommit root tag = revParse root ("refs/tags/" <> T.unpack tag)

-- | Tags matching a glob.
listTags :: FilePath -> String -> IO [Text]
listTags root pattern = maybe [] T.lines <$> git root ["tag", "--list", pattern]

-- | A path for git's pathspec, relative to the root and with forward slashes.
-- The root itself is @.@.
gitPathspec :: FilePath -> String
gitPathspec dir = case forwardSlashes (normalise dir) of
  "" -> "."
  p -> p

forwardSlashes :: FilePath -> String
forwardSlashes = map (\c -> if c == '\\' then '/' else c)

-- | How many commits in @range@ (@A..B@) change anything under @dir@.
commitsTouching :: FilePath -> String -> FilePath -> IO Int
commitsTouching root range dir =
  fromMaybe 0 . (>>= readMaybe . T.unpack)
    <$> git root ["rev-list", "--count", range, "--", gitPathspec dir]

-- | One line per commit after the tag that touches @dir@, newest first.
untaggedCommits :: FilePath -> Text -> FilePath -> IO [Text]
untaggedCommits root tag dir =
  maybe [] T.lines
    <$> git root ["log", "--pretty=format:%h %s", T.unpack tag <> "..HEAD", "--", gitPathspec dir]

-- | Uncommitted changes under a directory, as @git status --porcelain@ lines.
dirtyFiles :: FilePath -> FilePath -> IO [Text]
dirtyFiles root dir =
  maybe [] (filter (not . T.null) . T.lines)
    <$> git root ["status", "--porcelain", "--untracked-files=no", "--", gitPathspec dir]

-- | Whether the version field of a .cabal file differs from HEAD.
versionFieldChanged :: FilePath -> FilePath -> IO Bool
versionFieldChanged root cabalFile = do
  diff <- fromMaybe "" <$> git root ["diff", "-U0", "HEAD", "--", gitPathspec cabalFile]
  pure $ any isVersionChange (T.lines diff)
  where
    isVersionChange l = case T.uncons l of
      Just ('+', rest) -> topLevelKey rest == Just "version"
      _ -> False

-- | Whether some branch contains the tag's commit. A tag that was moved off
-- every branch (after a rebase, say) needs moving again before a release.
tagOnBranch :: FilePath -> Text -> IO Bool
tagOnBranch root tag =
  maybe False (not . T.null) <$> git root ["branch", "--contains", "refs/tags/" <> T.unpack tag]

currentBranch :: FilePath -> IO (Maybe Text)
currentBranch root = fmap T.strip <$> git root ["branch", "--show-current"]

-- | Whether @a@ is an ancestor of (or the same commit as) @b@.
isAncestor :: FilePath -> String -> String -> IO Bool
isAncestor root a b = do
  (code, _) <- readCmd root "git" ["merge-base", "--is-ancestor", a, b]
  pure (code == ExitSuccess)

-- | Tracked and untracked (but not ignored) files matching a pathspec, in
-- which @*@ matches slashes too, relative to the root, with forward slashes.
trackedFiles :: FilePath -> String -> IO [FilePath]
trackedFiles root pathspec =
  maybe [] (map T.unpack . T.lines)
    <$> git root ["ls-files", "--cached", "--others", "--exclude-standard", "--", pathspec]

-- | One line per commit since a time (ISO 8601) that touches @dir@, newest
-- first: what changed since a release that was never tagged.
commitsSince :: FilePath -> Text -> FilePath -> IO [Text]
commitsSince root time dir =
  maybe [] (filter (not . T.null) . T.lines)
    <$> git root ["log", "--pretty=format:%h %s", "--since=" <> T.unpack time, "HEAD", "--", gitPathspec dir]
