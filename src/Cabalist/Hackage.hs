-- | What Hackage knows about a package: its released versions, and whether a
-- candidate of a version is waiting. Queries go through @curl@, which ships
-- with Windows 10 and later as well as every Unix, so cabalist carries no TLS
-- stack of its own.
module Cabalist.Hackage
  ( HackageInfo (..)
  , packageUrl
  , candidateUrl
  , fetchHackageInfo
  , fetchUploadTime
  , latestVersion
  , parsePreferred
  )
where

import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Cabalist.Package (Version, parseVersion)
import Cabalist.Process (readCmd)
import System.Exit (ExitCode (..))
import Text.Read (readMaybe)

data HackageInfo
  = HackageUnknown
  -- ^ Not asked yet.
  | HackageUnreachable Text
  | HackageAbsent
  -- ^ Hackage has no package of that name.
  | HackageVersions [Version] [Version]
  -- ^ Normal and deprecated versions.
  deriving (Eq, Show)

hackageUrl :: Text
hackageUrl = "https://hackage.haskell.org"

packageUrl :: Text -> Text
packageUrl nameOrId = hackageUrl <> "/package/" <> nameOrId

candidateUrl :: Text -> Text
candidateUrl pid = packageUrl pid <> "/candidate"

latestVersion :: HackageInfo -> Maybe Version
latestVersion = \case
  HackageVersions normal deprecated -> case normal <> deprecated of
    [] -> Nothing
    vs -> Just (maximum vs)
  _ -> Nothing

-- | GET a URL, returning the HTTP status and body.
curl :: [String] -> String -> IO (Either Text (Int, Text))
curl extra url = do
  (code, out) <- readCmd "." "curl" (["-sS", "-L", "--max-time", "20", "-w", "\n%{http_code}"] <> extra <> [url])
  case code of
    ExitFailure 127 -> pure (Left "curl is not installed")
    ExitFailure n -> pure (Left ("curl failed (exit " <> T.pack (show n) <> ")"))
    ExitSuccess ->
      let (body, status) = T.breakOnEnd "\n" out
       in case readMaybe (T.unpack status) of
            Just s -> pure (Right (s, T.dropEnd 1 body))
            Nothing -> pure (Left "unexpected response from Hackage")

fetchHackageInfo :: Text -> IO HackageInfo
fetchHackageInfo name =
  curl ["-H", "Accept: application/json"] (T.unpack (packageUrl name <> "/preferred")) >>= \case
    Left err -> pure (HackageUnreachable err)
    Right (404, _) -> pure HackageAbsent
    Right (200, body) -> pure (parsePreferred body)
    Right (s, _) -> pure (HackageUnreachable ("Hackage answered " <> T.pack (show s)))

-- | Read @/package/NAME/preferred@ as JSON:
-- @{"normal-version":["0.2","0.1"],"deprecated-version":[]}@. Only these
-- two string arrays are needed, so this reads just them.
parsePreferred :: Text -> HackageInfo
parsePreferred body = HackageVersions (versionsOf "normal-version") (versionsOf "deprecated-version")
  where
    versionsOf key =
      case T.breakOn ("\"" <> key <> "\"") body of
        (_, rest) | T.null rest -> []
        (_, rest) ->
          let afterKey = T.dropWhile (/= '[') rest
              list = T.takeWhile (/= ']') (T.drop 1 afterKey)
           in mapMaybe (parseVersion . T.filter (\c -> c /= '"' && not (isSpace c))) (T.splitOn "," list)

-- | When a release was uploaded, as Hackage reports it (ISO 8601, UTC).
fetchUploadTime :: Text -> IO (Maybe Text)
fetchUploadTime pid =
  curl [] (T.unpack (packageUrl pid <> "/upload-time")) >>= \case
    Right (200, body) | not (T.null (T.strip body)) -> pure (Just (T.strip body))
    _ -> pure Nothing
