{-# OPTIONS_GHC -Wno-orphans #-}

module Kanren.Instances () where

import Data.Text (Text)

import Kanren.Core
import Kanren.LogicalBase ()

-- | 'Logical' instance for 'Text'
--
-- This instance is trivial: we treat 'Text' as indivisible/structure-free (we
-- cannot have partially known 'Text' values).
instance Logical Text
