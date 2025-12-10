{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Bidirectional where

-- import Autodocodec
-- import Data.Aeson (FromJSON(..), ToJSON(..))
-- import Data.Aeson
import Data.Text (Text)
-- import GHC.Generics

data MyRecord =
  MkMyRecord
    { x :: Int
    , y :: Maybe Text
    }
  deriving stock (Eq, Show)

