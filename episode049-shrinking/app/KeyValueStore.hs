module KeyValueStore (
    KeyValueStore -- opaque
    -- * Construction
  , empty
  , insert
    -- * Query
  , lookup
  ) where

import Prelude hiding (lookup)

import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.IntMap qualified as IntMap
import Data.Map qualified as Map

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data KeyValueStore a b = KVS{
      ids  :: Map a Int
    , vals :: IntMap b
    }

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: KeyValueStore a b
empty = KVS{
      ids  = Map.empty
    , vals = IntMap.empty
    }

insert :: Ord a => a -> b -> KeyValueStore a b -> KeyValueStore a b
insert key value KVS{ids, vals} = KVS{
      ids  =    Map.insert key   newId ids
    , vals = IntMap.insert newId value vals
    }
  where
    newId = Map.size ids

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

lookup :: Ord a => a -> KeyValueStore a b -> Maybe b
lookup key KVS{ids, vals} = Map.lookup key ids >>= flip IntMap.lookup vals