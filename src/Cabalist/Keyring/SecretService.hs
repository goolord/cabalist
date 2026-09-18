-- | The keyring on Linux and the BSDs: the freedesktop Secret Service (GNOME
-- Keyring, KWallet, KeePassXC), through libsecret's @secret-tool@. The
-- secret goes to it on standard input, never on its command line.
module Cabalist.Keyring.SecretService
  ( backendName
  , readItem
  , writeItem
  , deleteItem
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.Process

backendName :: Text
backendName = "the system keyring"

attributes :: [String]
attributes = ["service", "cabalist", "account", "hackage.haskell.org"]

readItem :: IO (Either Text (Maybe ByteString))
readItem =
  findExecutable "secret-tool" >>= \case
    -- Nothing can have been saved without it.
    Nothing -> pure (Right Nothing)
    Just tool ->
      secretTool tool ("lookup" : attributes) B.empty >>= \case
        Left e -> pure (Left e)
        Right (ExitSuccess, out, _) -> pure (Right (Just out))
        -- Exit 1 and no message: there is no such secret.
        Right (_, _, err) | B.null err -> pure (Right Nothing)
        Right (_, _, err) -> pure (Left (message err))

writeItem :: ByteString -> IO (Either Text ())
writeItem secret =
  findExecutable "secret-tool" >>= \case
    Nothing -> pure (Left "secret-tool is not installed (it comes with libsecret: the libsecret-tools package on Debian and Ubuntu)")
    Just tool -> ok <$> secretTool tool (["store", "--label=cabalist: Hackage login"] <> attributes) secret

deleteItem :: IO (Either Text ())
deleteItem =
  findExecutable "secret-tool" >>= \case
    Nothing -> pure (Right ())
    Just tool -> ok <$> secretTool tool ("clear" : attributes) B.empty

ok :: Either Text (ExitCode, ByteString, ByteString) -> Either Text ()
ok = \case
  Left e -> Left e
  Right (ExitSuccess, _, _) -> Right ()
  Right (_, _, err) -> Left (if B.null err then "secret-tool failed" else message err)

message :: ByteString -> Text
message = T.strip . decodeUtf8Lenient

-- | Run secret-tool with the given bytes on its standard input.
secretTool :: FilePath -> [String] -> ByteString -> IO (Either Text (ExitCode, ByteString, ByteString))
secretTool tool args input = do
  r <- try $ do
    (Just hIn, Just hOut, Just hErr, ph) <-
      createProcess (proc tool args) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
    errVar <- newEmptyMVar
    _ <- forkIO (B.hGetContents hErr >>= putMVar errVar)
    B.hPut hIn input >> hClose hIn
    out <- B.hGetContents hOut
    err <- takeMVar errVar
    code <- waitForProcess ph
    pure (code, out, err)
  pure $ case r of
    Left (e :: IOException) -> Left ("could not run secret-tool: " <> T.pack (show e))
    Right x -> Right x
