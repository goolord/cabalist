{-# LANGUAGE MultiWayIf #-}

-- | The keyring on Windows: a generic credential in Credential Manager,
-- listed under Windows Credentials.
module Cabalist.Keyring.Windows
  ( backendName
  , readItem
  , writeItem
  , deleteItem
  )
where

import Control.Exception (finally)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Unsafe qualified as BU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word8)
import Foreign (Ptr, alloca, castPtr, fillBytes, free, peek)
import Foreign.C.String (CWString, withCWString)

foreign import ccall safe "cabalist_cred_write"
  c_write :: CWString -> CWString -> CWString -> Ptr Word8 -> Word32 -> IO Word32

foreign import ccall safe "cabalist_cred_read"
  c_read :: CWString -> Ptr (Ptr Word8) -> Ptr Word32 -> IO Word32

foreign import ccall safe "cabalist_cred_delete"
  c_delete :: CWString -> IO Word32

backendName :: Text
backendName = "Windows Credential Manager"

target :: String
target = "cabalist/hackage.haskell.org"

errorNotFound :: Word32
errorNotFound = 1168

readItem :: IO (Either Text (Maybe ByteString))
readItem =
  withCWString target $ \t -> alloca $ \pp -> alloca $ \pn -> do
    err <- c_read t pp pn
    if
      | err == 0 -> do
          p <- peek pp
          n <- fromIntegral <$> peek pn
          Right . Just <$> B.packCStringLen (castPtr p, n) `finally` (fillBytes p 0 n >> free p)
      | err == errorNotFound -> pure (Right Nothing)
      | otherwise -> pure (Left (winError err))

writeItem :: ByteString -> IO (Either Text ())
writeItem secret =
  withCWString target $ \t ->
    withCWString "cabalist" $ \user ->
      withCWString "The Hackage login cabalist uploads with" $ \comment ->
        BU.unsafeUseAsCStringLen secret $ \(p, n) ->
          check <$> c_write t user comment (castPtr p) (fromIntegral n)

deleteItem :: IO (Either Text ())
deleteItem = withCWString target $ \t -> do
  err <- c_delete t
  pure (if err == errorNotFound then Right () else check err)

check :: Word32 -> Either Text ()
check 0 = Right ()
check err = Left (winError err)

winError :: Word32 -> Text
winError err = "Windows error " <> T.pack (show err)
