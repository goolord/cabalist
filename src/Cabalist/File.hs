-- | Reading and writing text files as UTF-8, whatever the locale, and without
-- newline translation: "Data.Text.IO" would read .cabal files in the ANSI code
-- page on Windows and write every line back with CRLF. Also the one bit of
-- parsing the line-based files cabalist edits share.
module Cabalist.File
  ( readUtf8
  , writeUtf8
  , editUtf8
  , topLevelKey
  )
where

import Data.ByteString qualified as B
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)

readUtf8 :: FilePath -> IO Text
readUtf8 path = decodeUtf8Lenient <$> B.readFile path

writeUtf8 :: FilePath -> Text -> IO ()
writeUtf8 path = B.writeFile path . encodeUtf8

-- | Rewrite a file through a function of its text, which sees @\\n@ line
-- endings. A file that used CRLF keeps it. Returns whether the file changed.
editUtf8 :: FilePath -> (Text -> Either Text Text) -> IO (Either Text Bool)
editUtf8 path f = do
  src <- readUtf8 path
  let crlf = "\r\n" `T.isInfixOf` src
      plain = if crlf then T.replace "\r\n" "\n" src else src
  case f plain of
    Left e -> pure (Left e)
    Right new
      | new == plain -> pure (Right False)
      | otherwise -> do
          writeUtf8 path (if crlf then T.replace "\n" "\r\n" new else new)
          pure (Right True)

-- | The name of a top-level @name: value@ field, lower-cased: in a .cabal
-- file or cabal's config, a field indented under a section is not one.
topLevelKey :: Text -> Maybe Text
topLevelKey l = case T.breakOn ":" l of
  (k, v) | not (T.null v), Just (c, _) <- T.uncons k, not (isSpace c) -> Just (T.toLower (T.stripEnd k))
  _ -> Nothing
