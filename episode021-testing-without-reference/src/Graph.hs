-- | Concrete graph representation
--
-- This is not intended as a demonstration of how to implement graphs /well/,
-- merely as /convenient/ vehicle for the properties we want to test.
module Graph (
    -- * Definition
    Graph(..)
    -- * Construction
  , fromEdges
  , addEdge
  , union
  , mapVertices
    -- * Queries
  , toEdges
  , hasVertices
  , haveVertices
    -- Translation
  , vertices
  , neighbours
  , cost
  , toDijkstra
    -- * Paths
  , Path
  , pathSource
  , pathDest
  , pathCostIfValid
  , pathCost
  , pathValid
  ) where

import Control.Monad
import Data.Bifunctor
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import GHC.Stack

import Dijkstra qualified
import GHC.Show (appPrec)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Simple representation of graphs
--
-- This represents directed graphs where edge vertices must have at least one
-- connnected edge (either incoming or outgoing).
--
-- All functions on graphs that combine edges will take the minimum cost.
newtype Graph v c = MkGraph {
      edges :: Map (v, v) c
    }

instance (Show v, Show c) => Show (Graph v c) where
  showsPrec d gr = showParen (d > appPrec) $
      showString "fromEdges " . shows (toEdges gr)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromEdges :: (Ord v, Ord c) => [((v, v), c)] -> Graph v c
fromEdges = MkGraph . Map.fromListWith min

addEdge :: (Ord v, Ord c) => (v, v) -> c -> Graph v c -> Graph v c
addEdge (a, b) c = union $ fromEdges [((a, b), c)]

union :: (Ord v, Ord c) => Graph v c -> Graph v c -> Graph v c
union (MkGraph es) (MkGraph es') = MkGraph $ Map.unionWith min es es'

mapVertices :: (Ord v', Ord c) => (v -> v') -> Graph v c -> Graph v' c
mapVertices f (MkGraph es) = MkGraph $ Map.mapKeysWith min (bimap f f) es

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

toEdges :: Graph v c -> [((v, v), c)]
toEdges = Map.toList . edges

hasVertices :: Eq v => Graph v c -> [v] -> Bool
hasVertices gr = all (`elem` vertices gr)

-- | Generalization of 'hasVertices' to multiple graphs
haveVertices :: Eq v => [Graph v c] -> [v] -> Bool
haveVertices grs vs = all (`hasVertices` vs) grs

{-------------------------------------------------------------------------------
  Translation to the representation in module "Dijkstra"
-------------------------------------------------------------------------------}

vertices :: Eq v => Graph v c -> [v]
vertices = nub . concatMap (\((a, b), _c) -> [a, b]) . toEdges

neighbours :: forall v c. Eq v => Graph v c -> v -> [v]
neighbours gr v = mapMaybe aux (toEdges gr)
  where
    aux :: ((v, v), c) -> Maybe v
    aux ((a, b), _c) = if a == v then Just b else Nothing

cost :: (Ord v, Show v, HasCallStack) => Graph v c -> v -> v -> c
cost gr a b =
    case Map.lookup (a, b) (edges gr) of
      Nothing -> error $ "no path between " ++ show (a, b)
      Just c  -> c

toDijkstra :: (Ord v, Show v) => Graph v c -> Dijkstra.Graph v c
toDijkstra gr = Dijkstra.MkGraph{
      vertices   = vertices   gr
    , neighbours = neighbours gr
    , cost       = cost       gr
    }

{-------------------------------------------------------------------------------
  Paths
-------------------------------------------------------------------------------}

type Path v = NonEmpty v

pathSource :: Path v -> v
pathSource (v :| _) = v

pathDest :: Path v -> v
pathDest (v :| []) = v
pathDest (_ :| vs) = last vs

-- | Check if the path is valid, and if so, return its cost
pathCostIfValid :: forall v c. (Ord v, Num c) => Graph v c -> Path v -> Maybe c
pathCostIfValid gr@(MkGraph es) = go 0
  where
    go :: c -> Path v -> Maybe c
    go !acc (v :| []) = do
        guard $ v `elem` vertices gr
        Just acc
    go !acc (v :| (v' : vs)) = do
        c <- Map.lookup (v, v') es
        go (acc + c) (v' :| vs)

-- | Cost of a path, /assuming/ it is valid
pathCost :: (Ord v, Num c) => Graph v c -> Path v -> c
pathCost gr path =
    case pathCostIfValid gr path of
      Just c  -> c
      Nothing -> error "invalid path"

pathValid :: forall v c. (Ord v, Num c) => Graph v c -> Path v -> Bool
pathValid gr = isJust . pathCostIfValid gr