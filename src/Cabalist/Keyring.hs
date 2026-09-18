{-# LANGUAGE CPP #-}

-- | A Hackage login saved in the operating system's keyring: Credential
-- Manager on Windows, the keychain on macOS, and the Secret Service
-- elsewhere. There is one saved login, for hackage.haskell.org.
module Cabalist.Keyring
  ( keyringName
  , loadLogin
  , saveLogin
  , forgetLogin
  , encodeLogin
  , decodeLogin
  )
where

import Control.Exception (SomeException, displayException, try)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Cabalist.Release (Credentials (..))
#if defined(mingw32_HOST_OS)
import Cabalist.Keyring.Windows
#elif defined(darwin_HOST_OS)
import Cabalist.Keyring.MacOS
#else
import Cabalist.Keyring.SecretService
#endif

-- | Where the login is kept, for the UI: "Remember it in …".
keyringName :: Text
keyringName = backendName

-- | The saved login, if there is one.
loadLogin :: IO (Either Text (Maybe Credentials))
loadLogin =
  guarded readItem >>= \case
    Left e -> pure (Left e)
    Right Nothing -> pure (Right Nothing)
    Right (Just bytes) -> pure (maybe (Left "the saved login is not one cabalist can read") (Right . Just) (decodeLogin bytes))

-- | Save a login, replacing any saved before. 'FromCabalConfig' is no login
-- of cabalist's own: it forgets the saved one.
saveLogin :: Credentials -> IO (Either Text ())
saveLogin FromCabalConfig = forgetLogin
saveLogin creds = guarded (writeItem (encodeLogin creds))

forgetLogin :: IO (Either Text ())
forgetLogin = guarded deleteItem

guarded :: IO (Either Text a) -> IO (Either Text a)
guarded act = either (\(e :: SomeException) -> Left (T.pack (displayException e))) id <$> try act

-- | A login as the bytes kept in the keyring: its kind on the first line,
-- then its fields.
encodeLogin :: Credentials -> ByteString
encodeLogin = encodeUtf8 . \case
  ApiToken tok -> "token\n" <> tok
  UserPassword user pass -> "password\n" <> user <> "\n" <> pass
  FromCabalConfig -> ""

decodeLogin :: ByteString -> Maybe Credentials
decodeLogin bytes = case decodeUtf8' bytes of
  Left _ -> Nothing
  Right t -> case T.breakOn "\n" t of
    ("token", rest) | Just tok <- T.stripPrefix "\n" rest, not (T.null tok) -> Just (ApiToken tok)
    ("password", rest)
      | Just fields <- T.stripPrefix "\n" rest
      , (user, pass) <- T.breakOn "\n" fields
      , Just pass' <- T.stripPrefix "\n" pass
      , not (T.null user) ->
          Just (UserPassword user pass')
    _ -> Nothing
