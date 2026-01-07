-- | Utilities for working with base64
--
-- Intended for qualified import.
--
-- > import Util.Base64 qualified as Base64
module Util.Base64 (
    ByteString(..)
  , AsStorable(..)
  ) where

import Control.Monad
import Data.Aeson
import Data.Base64.Types
import Data.ByteString qualified as BS
import Data.ByteString.Base64
import Data.ByteString.Char8 qualified as BS.Char8
import Data.ByteString.Internal qualified as BS.Internal
import Data.String
import Data.Text qualified as Text
import Foreign
import System.IO.Unsafe

{-------------------------------------------------------------------------------
  Bytestrings that use base64 for serialization
-------------------------------------------------------------------------------}

newtype ByteString = WrapByteString {
      unwrapByteString :: BS.ByteString
    }

instance Show ByteString where
  show = toBase64 . unwrapByteString

instance IsString ByteString where
  fromString = either error WrapByteString . fromBase64

instance ToJSON ByteString where
  toJSON = toJSON . toBase64 . unwrapByteString

instance FromJSON ByteString where
  parseJSON = parseJSON >=> either fail (return . WrapByteString) . fromBase64

{-------------------------------------------------------------------------------
  Deriving via support
-------------------------------------------------------------------------------}

newtype AsStorable a = WrapAsStorable { unwrapAsStorable :: a }

fromStorable :: Storable a => AsStorable a -> ByteString
fromStorable = WrapByteString . pokeToByteString . unwrapAsStorable

toStorable :: Storable a => ByteString -> Either String (AsStorable a)
toStorable = fmap WrapAsStorable . peekFromByteString . unwrapByteString

instance Storable a => Show (AsStorable a) where
  show = show . fromStorable

instance Storable a => IsString (AsStorable a) where
  fromString = either error id . toStorable . fromString

instance Storable a => ToJSON (AsStorable a) where
  toJSON = toJSON . fromStorable

instance Storable a => FromJSON (AsStorable a) where
  parseJSON = parseJSON >=> either fail return . toStorable

{-------------------------------------------------------------------------------
  Internal auxiliary: translating to and from base64
-------------------------------------------------------------------------------}

toBase64 :: BS.ByteString -> String
toBase64 = BS.Char8.unpack . extractBase64 . encodeBase64'

fromBase64 :: String -> Either String BS.ByteString
fromBase64 =
      either (Left . Text.unpack) Right
    . decodeBase64Untyped
    . BS.Char8.pack

{-------------------------------------------------------------------------------
  Internal auxiliary: using 'Storable' for serialization

  This is a bit awkward, but the only serialization provided in GHC for
  'Fingerprint' is 'Storable', so we roll with it.
-------------------------------------------------------------------------------}

pokeToByteString :: forall a. Storable a => a -> BS.ByteString
pokeToByteString key = unsafePerformIO $
      BS.Internal.create (sizeOf (undefined :: a)) $ \ptr -> do
        poke (castPtr ptr) key

peekFromByteString :: forall a. Storable a => BS.ByteString -> Either String a
peekFromByteString bs =
    if len == sizeOf (undefined :: a) then
      unsafePerformIO $
        withForeignPtr fptr $ \ptr ->
          Right <$> peek (castPtr ptr)
    else
      Left "invalid length"
  where
    fptr :: ForeignPtr Word8
    len  :: Int
    (fptr, len) = BS.Internal.toForeignPtr0 bs

