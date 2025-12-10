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
{-# OPTIONS_GHC -Wno-unused-imports #-}
module BidirectionalAuto where

import Data.Aeson (Value, Object, FromJSON(..), ToJSON(..), withObject, Value(..), explicitToField, encode, decode)
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

