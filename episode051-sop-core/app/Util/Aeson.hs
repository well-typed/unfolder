module Util.Aeson (
    -- * Decode values
    decodeEitherWith
  , decodeFailWith
  , decodeFail
  , decodeObjectWith
    -- * Construct values
  , valueList
  , key
  ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types
import Data.ByteString.Lazy (LazyByteString)
import Data.Vector qualified as Vector

{-------------------------------------------------------------------------------
  Decode values
-------------------------------------------------------------------------------}

decodeEitherWith ::
     FromJSON a
  => (a -> Parser b)
  -> LazyByteString -> Either String b
decodeEitherWith parser bs = eitherDecode bs >>= parseEither parser

decodeFailWith ::
     (MonadFail m, FromJSON a)
  => (a -> Parser b) -> LazyByteString -> m b
decodeFailWith f = either fail return . decodeEitherWith f

decodeFail :: (MonadFail m, FromJSON b) => LazyByteString -> m b
decodeFail = decodeFailWith parseJSON

decodeObjectWith :: String -> Value -> (Object -> Parser a) -> Parser a
decodeObjectWith label = flip (withObject label)

{-------------------------------------------------------------------------------
  Construct values
-------------------------------------------------------------------------------}

valueList :: [Value] -> Value
valueList = Array . Vector.fromList

key :: String -> Key
key = Key.fromString