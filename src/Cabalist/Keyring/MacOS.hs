{-# LANGUAGE MultiWayIf #-}

-- | The keyring on macOS: a generic password in the default keychain, which
-- Keychain Access lists under the name "cabalist".
module Cabalist.Keyring.MacOS
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
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word8)
import Foreign (Ptr, alloca, castPtr, fillBytes, free, peek)
import Foreign.C.String (CString, withCString)

foreign import ccall safe "cabalist_keychain_write"
  c_write :: CString -> CString -> Ptr Word8 -> Word32 -> IO Int32

foreign import ccall safe "cabalist_keychain_read"
  c_read :: CString -> CString -> Ptr (Ptr Word8) -> Ptr Word32 -> IO Int32

foreign import ccall safe "cabalist_keychain_delete"
  c_delete :: CString -> CString -> IO Int32

backendName :: Text
backendName = "the macOS keychain"

withItem :: (CString -> CString -> IO a) -> IO a
withItem k = withCString "cabalist" $ \service -> withCString "hackage.haskell.org" (k service)

errSecItemNotFound :: Int32
errSecItemNotFound = -25300

readItem :: IO (Either Text (Maybe ByteString))
readItem =
  withItem $ \service account -> alloca $ \pp -> alloca $ \pn -> do
    err <- c_read service account pp pn
    if
      | err == 0 -> do
          p <- peek pp
          n <- fromIntegral <$> peek pn
          Right . Just <$> B.packCStringLen (castPtr p, n) `finally` (fillBytes p 0 n >> free p)
      | err == errSecItemNotFound -> pure (Right Nothing)
      | otherwise -> pure (Left (osError err))

writeItem :: ByteString -> IO (Either Text ())
writeItem secret =
  withItem $ \service account ->
    BU.unsafeUseAsCStringLen secret $ \(p, n) ->
      check <$> c_write service account (castPtr p) (fromIntegral n)

deleteItem :: IO (Either Text ())
deleteItem = withItem $ \service account -> do
  err <- c_delete service account
  pure (if err == errSecItemNotFound then Right () else check err)

check :: Int32 -> Either Text ()
check 0 = Right ()
check err = Left (osError err)

osError :: Int32 -> Text
osError err = "keychain error " <> T.pack (show err)
