{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
module BidirectionalAutoTemplate where

import Data.Aeson (Value, Object, FromJSON(..), ToJSON(..), withObject, Value(..), explicitToField)
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Parser, explicitParseField, explicitParseFieldMaybe)
import Data.Kind
import Data.Text (Text)
import qualified Data.Text as Text

data MyRecord =
  MkMyRecord
    { x :: Int
    , y :: Maybe Text
    }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via Autodocodec MyRecord

class HasCodec a where
  codec :: Codec Value a a

type Codec :: Type -> Type -> Type -> Type
data Codec ctx print parse where
  IntCodec ::
       Codec Value Int Int
  TextCodec ::
       Codec Value Text Text
  ObjCodec ::
       Text
    -> Codec Object print parse
    -> Codec Value print parse
  RequiredKeyCodec ::
       Text
    -> Codec Value print parse
    -> Maybe Text
    -> Codec Object print parse
  OptionalKeyCodec ::
       Text
    -> Codec Value print parse
    -> Maybe Text
    -> Codec Object (Maybe print) (Maybe parse)
  PrintMapCodec ::
       (newPrint -> oldPrint)
    -> Codec Object oldPrint parse
    -> Codec Object newPrint parse
  Pure ::
       parse
    -> Codec Object print parse
  Ap ::
       Codec Object print (parse1 -> parse2)
    -> Codec Object print parse1
    -> Codec Object print parse2

parseJSONViaCodec :: Codec ctx print parse -> ctx -> Parser parse
parseJSONViaCodec IntCodec val = parseJSON val
parseJSONViaCodec TextCodec val = parseJSON val
parseJSONViaCodec (ObjCodec t objc) val =
  withObject (Text.unpack t) (parseJSONViaCodec objc) val
parseJSONViaCodec (RequiredKeyCodec t valc _md) obj =
  explicitParseField (parseJSONViaCodec valc) obj (fromText t)
parseJSONViaCodec (OptionalKeyCodec t valc _md) obj =
  explicitParseFieldMaybe (parseJSONViaCodec valc) obj (fromText t)
parseJSONViaCodec (PrintMapCodec _inf c) x =
  parseJSONViaCodec c x
parseJSONViaCodec (Ap cf cx) x =
  parseJSONViaCodec cf x <*> parseJSONViaCodec cx x
parseJSONViaCodec (Pure x) _ =
  pure x

toJSONViaCodec :: Codec ctx print parse -> print -> ctx
toJSONViaCodec IntCodec x = toJSON x
toJSONViaCodec TextCodec x = toJSON x
toJSONViaCodec (ObjCodec _ objc) x =
  Object (toJSONViaCodec objc x)
toJSONViaCodec (RequiredKeyCodec t valc _md) x =
  explicitToField (toJSONViaCodec valc) (fromText t) x
toJSONViaCodec (OptionalKeyCodec _ _valc _md) Nothing =
  mempty
toJSONViaCodec (OptionalKeyCodec t valc _md) (Just x) =
  explicitToField (toJSONViaCodec valc) (fromText t) x
toJSONViaCodec (PrintMapCodec inf c) x =
  toJSONViaCodec c (inf x)
toJSONViaCodec (Ap cf cx) x =
  toJSONViaCodec cf x <> toJSONViaCodec cx x
toJSONViaCodec (Pure _) _ =
  mempty

instance Applicative (Codec Object print) where
  pure = Pure
  (<*>) = Ap

instance Functor (Codec Object print) where
  fmap f x = pure f <*> x

requiredField :: HasCodec a => Text -> Maybe Text -> Codec Object a a
requiredField txt mdoc =
  RequiredKeyCodec txt codec mdoc

optionalField :: HasCodec a => Text -> Maybe Text -> Codec Object (Maybe a) (Maybe a)
optionalField txt mdoc =
  OptionalKeyCodec txt codec mdoc

(.=) :: Codec Object oldPrint parse -> (newPrint -> oldPrint) -> Codec Object newPrint parse
c .= fi = PrintMapCodec fi c

object :: Text -> Codec Object print parse -> Codec Value print parse
object objName objCodec = ObjCodec objName objCodec

instance HasCodec Int where
  codec = IntCodec

instance HasCodec Text where
  codec = TextCodec

instance HasCodec MyRecord where
  codec =
    object "Record" $
      MkMyRecord
        <$> (requiredField "field_x" (Just "x field") .= (.x))
        <*> (optionalField "field_y" (Just "y field") .= (.y))

-- for DerivingVia
--
newtype Autodocodec a = MkAutodocodec a

instance HasCodec a => FromJSON (Autodocodec a) where
  parseJSON val = MkAutodocodec <$> parseJSONViaCodec codec val

instance HasCodec a => ToJSON (Autodocodec a) where
  toJSON (MkAutodocodec x) = toJSONViaCodec codec x

