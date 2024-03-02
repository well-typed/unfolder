-- | Dijkstra's shortest path algorithm
--
-- This was discussed in Unfolder Episode 20: Dijkstra's shortest paths
module Dijkstra (
    Graph(..)
  , Info(..)
  , dijkstra
  , example
  , pathBetween
  ) where

import Data.Foldable
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord

-- | Graphs are parameterised over:
--
-- * the type of vertices @v@
-- * the type of costs @c@ (assumed to be non-negative!)
--
data Graph v c =
  MkGraph
    { vertices   :: [v]
    , neighbours :: v -> [v]
    , cost       :: v -> v -> c
    }

data Info v c =
  MkInfo
    { dist :: !(Map v c) -- not in map: INFINITY
    , prev :: !(Map v v) -- not in map: UNDEFINED
    }
  deriving Show

minViewOn :: (Ord c, Eq v) => (v -> c) -> [v] -> Maybe (v, [v])
minViewOn _ [] = Nothing
minViewOn f xs =
  let
    m = minimumBy (comparing f) xs
  in
    Just (m, delete m xs)

data Distance c =
    Finite c
  | Infinity
  deriving (Eq, Ord, Show) -- order of the constructors matters!

-- >>> compare Nothing (Just 3)
-- LT
-- >>> compare Infinity (Finite 3)
-- GT

distanceOf :: Ord v => Info v c -> v -> Distance c
distanceOf info v =
  case Map.lookup v (dist info) of
    Nothing -> Infinity
    Just x  -> Finite x

dijkstra ::
  forall v c.
  (Num c, Ord c, Ord v) =>
  Graph v c -> v -> Info v c
dijkstra graph source =
  let
    loop :: [v] -> Info v c -> Info v c
    loop q info =
      case minViewOn (distanceOf info) q of
        Nothing ->
          -- we're done!
          info
        Just (u, q') ->
          case distanceOf info u of
            Infinity -> -- nothing can happen anymore
              info
            Finite du ->
              let
                update :: Info v c -> v -> Info v c
                update i v =
                  let
                    alt = du + (cost graph) u v
                  in
                    if Finite alt < distanceOf i v
                      then
                        MkInfo
                          (Map.insert v alt (dist i))
                          (Map.insert v u   (prev i))
                      else
                        i

                info' =
                  foldl'
                    update
                    info
                    (filter (`elem` q') (neighbours graph u))
              in
                loop q' info'
  in
    loop (vertices graph)
      (MkInfo
        { dist = Map.singleton source 0
        , prev = Map.empty
        }
      )


-- >>> dijkstra example A
-- MkInfo {dist = fromList [(A,0),(B,4),(C,5),(D,10),(E,9),(F,11)],
--         prev = fromList [(B,A),(C,A),(D,E),(E,C),(F,D)]}


data Vertex = A | B | C | D | E | F | G | H
  deriving (Eq, Ord, Show, Enum)

example :: Graph Vertex Int
example =
  MkGraph
    { vertices = [A .. H]
    , neighbours = \ v ->
        case v of
          A -> [B, C]
          B -> [D]
          C -> [B, E]
          D -> [C, F]
          E -> [D, F]
          F -> []
          G -> [H]
          H -> [G]
    , cost = \ v w ->
        case (v, w) of
          (A, B) -> 4
          (A, C) -> 5
          (B, D) -> 7
          (C, B) -> 3
          (C, E) -> 4
          (D, C) -> 2
          (D, F) -> 1
          (E, D) -> 1
          (E, F) -> 3
          (G, H) -> 5
          (H, G) -> 5
          _ -> error "unconnected vertices"
    }

pathBetween :: forall v c. Ord v => Info v c -> (v, v) -> Maybe (NonEmpty v)
pathBetween info (src, dest) =
    go [] dest
  where
    -- Work backwards from the destination towards the source
    go :: [v] -> v -> Maybe (NonEmpty v)
    go acc v =
        if v == src then
          Just (v :| acc)
        else do
          v' <- Map.lookup v (prev info)
          go (v:acc) v'

