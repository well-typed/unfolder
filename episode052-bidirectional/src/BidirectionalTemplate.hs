{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module BidirectionalTemplate where

-- import Autodocodec
-- import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson
import Data.Text
import Data.Aeson.Types
import GHC.Generics

data MyRecord =
  MkMyRecord
    { x :: Int
    , y :: Maybe Text
    }
  deriving stock (Eq, Show)
  deriving stock Generic

{-
instance FromJSON MyRecord where
  parseJSON =
    withObject "MyRecord" $ \ val ->
      MkMyRecord
        <$> val .: "field_x"
        <*> val .:? "field_y"
-}

instance FromJSON MyRecord where
  parseJSON =
    withObject "MyRecord" $ \ val ->
      MkMyRecord
        <$> explicitParseField parseJSON val "field_x" 
        <*> explicitParseFieldMaybe parseJSON val "field_y"

{-
instance ToJSON MyRecord where
  toJSON r =
    Object $ mconcat
      [ "field_x" .= r.x
      , "field_y" .?= r.y
      ]
-}

instance ToJSON MyRecord where
  toJSON r =
    Object $
      (explicitToField toJSON "field_x" r.x)
      <> maybe mempty (explicitToField toJSON "field_y") r.y -- explicitToFieldOmit isNothing toJSON "field_y" r.y

{-
instance HasCodec MyRecord where
  codec =
    object "MyRecord" $
      MkMyRecord
        <$> requiredField "field_x" "x coordinate" .= (.x)
        <*> optionalField "field_y" "y coordinate" .= (.y)
-}

-- >>> Aeson.encode (MkMyRecord { x = 4, y = Just "foo" })
-- "{\"field_x\":4,\"field_y\":\"foo\"}"
--
-- >>> Aeson.encode (MkMyRecord { x = 4, y = Nothing })
-- "{\"field_x\":4}"
--
-- >>> Aeson.decode "{\"field_x\": 44}" :: Maybe MyRecord
-- Just (MkMyRecord {x = 44, y = Nothing})
--
-- >>> :i ValueCodec
-- type role Codec nominal representational representational
-- type Codec :: * -> * -> * -> *
-- data Codec context input output where
--   ...
--   ValueCodec :: (Coercible Value input, Coercible Value output) =>
--                 Codec Value input output
--   ...
--   	-- Defined in ‘Autodocodec.Codec’
-- type ValueCodec :: * -> * -> *
-- type ValueCodec = Codec Value :: * -> * -> *
--   	-- Defined in ‘Autodocodec.Codec’

