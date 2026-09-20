-- | Where each package stands in its release: its tag, what changed since
-- its last release, what cabalist has built, and what Hackage has. 'stage'
-- turns that into the next thing to do.
module Cabalist.Status
  ( PkgStatus (..)
  , Stage (..)
  , stageLabel
  , stageHint
  , packageStatus
  , stage
  , isPublished
  , warnings
  )
where

import Data.List (sortOn)
import Data.Maybe (catMaybes, isJust)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Cabalist.File (readUtf8)
import Cabalist.Config
import Cabalist.Git
import Cabalist.Hackage
import Cabalist.Package
import Cabalist.Version (changelogMentions, findChangelog)
import Control.Exception (IOException, try)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

data PkgStatus = PkgStatus
  { psTag :: !Text
  -- ^ The tag this version would be released under.
  , psTagCommit :: !(Maybe Text)
  , psTagOnHead :: !Bool
  , psTagOnBranch :: !Bool
  , psUntagged :: ![Text]
  -- ^ Commits after the tag that touch the package, newest first.
  , psPrevRelease :: !(Maybe (Version, Text))
  -- ^ The newest tag of another version, and the tag itself.
  , psChangesSincePrev :: !Int
  -- ^ Commits touching the package since that tag (all of its history if
  -- there is none).
  , psDirty :: ![Text]
  , psVersionUncommitted :: !Bool
  , psTarball :: !Bool
  , psCandidateUploaded :: !Bool
  -- ^ This tarball was uploaded as a candidate.
  , psPublishedHere :: !Bool
  -- ^ cabalist's published marker exists for this version.
  , psChangelog :: !(Maybe FilePath)
  , psChangelogHasEntry :: !Bool
  , psHackage :: !HackageInfo
  , psReleasedAt :: !(Maybe Text)
  -- ^ When Hackage got this version, if it has it.
  }
  deriving (Eq, Show)

-- | Read a package's status from git and the work directory. Hackage is
-- asked separately ('fetchHackageInfo'), as it needs the network.
packageStatus :: FilePath -> Config -> HackageInfo -> Maybe Text -> Package -> IO PkgStatus
packageStatus root cfg hackage releasedAt p = do
  let fmt = cfgTagFormat cfg
      tag = renderTag fmt p
      dir = pkgDir p
  mTag <- tagCommit root tag
  headC <- revParse root "HEAD"
  onBranch <- maybe (pure False) (const (tagOnBranch root tag)) mTag
  -- Changes since this version: after its tag, or else after Hackage got it.
  untagged <- case (mTag, releasedAt) of
    (Just _, _) -> untaggedCommits root tag dir
    (Nothing, Just t) -> commitsSince root t dir
    (Nothing, Nothing) -> pure []
  tags <- listTags root (tagPattern fmt p)
  let released =
        sortOn
          (Down . fst)
          [ (v, t)
          | t <- tags
          , Just v <- [parseTagVersion fmt p t]
          , v /= pkgVersion p
          ]
      prev = case released of
        (r : _) -> Just r
        [] -> Nothing
  changes <- case prev of
    Just (_, t) -> commitsTouching root (T.unpack t <> "..HEAD") dir
    Nothing -> commitsTouching root "HEAD" dir
  dirty <- dirtyFiles root dir
  versionChanged <- versionFieldChanged root (pkgCabalFile p)
  tarball <- doesFileExist (tarballPath root p)
  candidate <- doesFileExist (candidateMarker root p)
  published <- doesFileExist (publishedMarker root p)
  changelog <- findChangelog (root </> dir)
  hasEntry <- case changelog of
    Nothing -> pure False
    Just f ->
      try (readUtf8 (root </> dir </> f)) >>= \case
        Left (_ :: IOException) -> pure False
        Right src -> pure (changelogMentions (pkgVersion p) src)
  pure
    PkgStatus
      { psTag = tag
      , psTagCommit = mTag
      , psTagOnHead = isJust mTag && mTag == headC
      , psTagOnBranch = onBranch
      , psUntagged = untagged
      , psPrevRelease = prev
      , psChangesSincePrev = changes
      , psDirty = dirty
      , psVersionUncommitted = versionChanged
      , psTarball = tarball
      , psCandidateUploaded = tarball && candidate
      , psPublishedHere = published
      , psChangelog = changelog
      , psChangelogHasEntry = hasEntry
      , psHackage = hackage
      , psReleasedAt = releasedAt
      }

isPublished :: Package -> PkgStatus -> Bool
isPublished p s = psPublishedHere s || case psHackage s of
  HackageVersions normal deprecated -> pkgVersion p `elem` normal <> deprecated
  _ -> False

-- | The next step of a package's release.
data Stage
  = StageReleased
  -- ^ This version is on Hackage and nothing has changed since.
  | StageNeedsBump
  -- ^ This version is on Hackage (or older than what is), and the package
  -- changed since: the version needs bumping.
  | StageCommitBump
  -- ^ The version was bumped but not committed.
  | StageReadyToTag
  | StageTagged
  -- ^ Tagged, with no tarball yet.
  | StageCandidate
  -- ^ A tarball is ready to upload as a candidate or publish.
  | StageUploaded
  -- ^ The tarball is up as a candidate, waiting to be published.
  deriving (Eq, Ord, Show, Enum, Bounded)

stage :: Package -> PkgStatus -> Stage
stage p s
  | psVersionUncommitted s = StageCommitBump
  | isPublished p s = if psChangesSincePrevRelease > 0 then StageNeedsBump else StageReleased
  | olderThanHackage = StageNeedsBump
  | psTarball s = if psCandidateUploaded s then StageUploaded else StageCandidate
  | isJust (psTagCommit s) = StageTagged
  | otherwise = StageReadyToTag
  where
    -- Once this version is out, "since the previous release" means since
    -- this version's own tag.
    psChangesSincePrevRelease = length (psUntagged s)
    olderThanHackage = maybe False (> pkgVersion p) (latestVersion (psHackage s))

-- | A few words for a package list.
stageLabel :: Stage -> Text
stageLabel = \case
  StageReleased -> "released"
  StageNeedsBump -> "changed since release"
  StageCommitBump -> "bump not committed"
  StageReadyToTag -> "unreleased"
  StageTagged -> "tagged"
  StageCandidate -> "tarball ready"
  StageUploaded -> "candidate up"

-- | What the package's release is waiting for, in a sentence.
stageHint :: Stage -> Text
stageHint = \case
  StageReleased -> "This version is on Hackage and nothing has changed since."
  StageNeedsBump -> "The package changed since this version was released. Bump its version to release again."
  StageCommitBump -> "The new version is not committed yet. Commit it before tagging."
  StageReadyToTag -> "This version has not been released. Tag it and build its tarball from a clean checkout."
  StageTagged -> "The tag exists. Build the tarball from it."
  StageCandidate -> "The tarball is built. Upload it as a candidate to check it on Hackage, or publish it now."
  StageUploaded -> "The candidate is on Hackage. Check it, then publish."

-- | Things worth knowing before releasing, in the order they matter.
warnings :: Package -> PkgStatus -> [Text]
warnings p s =
  catMaybes
    [ whenTrue (pkgNameMismatch p) "The .cabal file isn't named after the package. Hackage rejects that."
    , whenTrue (not (null (psDirty s))) (count (length (psDirty s)) "file has" "files have" <> " uncommitted changes. They won't be in the release.")
    , whenTrue (isJust (psTagCommit s) && not (psTagOnBranch s)) "The tag isn't on any branch any more. Replace the tag and tarball to release."
    , whenTrue (not released && not (null (psUntagged s))) (count (length (psUntagged s)) "commit after the tag touches" "commits after the tag touch" <> " this package. The tarball doesn't have them until the tag and tarball are replaced.")
    , whenTrue (not released && psChangelog s == Nothing) "The package has no changelog."
    , whenTrue (not released && isJust (psChangelog s) && not (psChangelogHasEntry s)) ("The changelog has no entry for " <> showVersion (pkgVersion p) <> ".")
    , case psHackage s of
        HackageUnreachable e -> Just ("Couldn't reach Hackage: " <> e)
        _ -> Nothing
    ]
  where
    released = isPublished p s
    whenTrue c msg = if c then Just msg else Nothing
    count :: Int -> Text -> Text -> Text
    count 1 one _ = "1 " <> one
    count n _ many = T.pack (show n) <> " " <> many
