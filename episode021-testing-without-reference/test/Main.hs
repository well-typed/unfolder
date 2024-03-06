{-
  The Haskell Unfolder episode 21: Testing without a reference

  The best case scenario when testing a piece of sofware is when we have a
  reference implementation to compare against. Often however such a reference is
  not available, begging the question how to test a function if we cannot verify
  what that function computes exactly. In this episode we will consider how to
  define properties to verify the implementation of Dijkstra's shortest path
  algorithm we discussed in Episode 20; you may wish to watch that episode
  first, but it's not required: we will mostly treat the algorithm as a black
  box for the sake of testing it.

  We can only scratch the surface here; for an in-depth discussion of this
  topic, we highly recommend "How to Specify It!: A Guide to Writing Properties
  of Pure Functions" by John Hughes.
  <https://research.chalmers.se/publication/517894/file/517894_Fulltext.pdf>
-}
module Main (main) where

import Control.Monad
import Data.Function (on)
import Data.List (nubBy)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Test.Tasty
import Test.Tasty.QuickCheck

import Dijkstra (Info, dijkstra)
import Dijkstra qualified
import Graph (Graph, Path, pathSource, pathDest)
import Graph qualified

import Util.QuickCheck

{-------------------------------------------------------------------------------
  Costs
-------------------------------------------------------------------------------}

-- | Edge cost ("length")
--
-- Note: the choice of 'Int' has consequences; for example, if the algorithm
-- we're testing would suffer from rounding errors when using floating point,
-- we would not find it.
newtype Cost = Cost Int
  deriving newtype (Show, Eq, Ord, Num)

instance Arbitrary Cost where
  arbitrary       = Cost <$> choose (0, 100)
  shrink (Cost c) = Cost <$> shrink c

-- | Adding additional edges to a path cannot /reduce/ its cost
prop_costMonotone :: Cost -> Cost -> Property
prop_costMonotone c1 c2 =
    counterexample (show (c1 + c2)) $
      c1 <= c1 + c2

{-------------------------------------------------------------------------------
  Graphs
-------------------------------------------------------------------------------}

data Vertex = A | B | C | D | E | Vertex Int
  deriving stock (Show, Eq, Ord)

allVertices :: [Vertex]
allVertices = [A, B, C, D, E] ++ map Vertex [6 ..]

instance Arbitrary (Graph Vertex Cost) where
  arbitrary = sized $ \sz -> do
      numVertices <- choose (0, sz)
      -- When we choose from a small set of vertices, we are more likely to
      -- generate graphs with larger connected components.
      let vertices = take numVertices allVertices
      if null vertices then
        return $ Graph.fromEdges []
      else do
        -- We typically want more edges than vertices
        numEdges <- choose (0, 10 * sz)
        fmap (Graph.fromEdges . nubBy ((==) `on` fst)) $
          replicateM numEdges $ do
            a :: Vertex <- elements vertices
            b :: Vertex <- elements vertices
            c :: Cost   <- arbitrary
            return ((a, b), c)

  shrink = map Graph.fromEdges . shrinkList shrinkCost . Graph.toEdges
    where
      shrinkCost :: ((Vertex, Vertex), Cost) -> [((Vertex, Vertex), Cost)]
      shrinkCost ((a, b), c) = map ((a, b), ) (shrink c)

{-------------------------------------------------------------------------------
  Random path
-------------------------------------------------------------------------------}

data PathInGraph = PathInGraph {
      pathIn :: Graph Vertex Cost
    , path   :: Path Vertex
    }
  deriving Show

randomPathFrom :: forall v c. Eq v => Graph v c -> v -> Int -> Gen (Path v)
randomPathFrom gr = go
  where
    go :: v -> Int -> Gen (Path v)
    go v 0 = return (v :| [])
    go v n = do
        let nv = Graph.neighbours gr v
        if null nv then
          return (v :| [])
        else do
          v' <- elements nv
          NE.cons v <$> go v' (n - 1)

instance Arbitrary PathInGraph where
  arbitrary = sized $ \sz -> do
      gr   <- arbitrary `suchThat` (not . null . Graph.edges)
      src  <- elements (Graph.vertices gr)
      n    <- choose (0, sz)
      path <- randomPathFrom gr src n
      return $ PathInGraph {
          pathIn = gr
        , path   = path
        }

  shrink PathInGraph{pathIn, path} = concat [
       -- Shrink the path (it will remain valid)
       [ PathInGraph {
             pathIn = pathIn
           , path   = path'
           }
       | path' <- dropEitherEnd path
       ]

       -- Shrink the graph (/provided/ the path is still valid)
     , [ PathInGraph {
             pathIn = pathIn'
           , path   = path
           }
       | pathIn' <- shrink pathIn
       , Graph.pathValid pathIn' path
       ]
     ]

{-------------------------------------------------------------------------------
  Properties

  == 'prop_valid'

  Before we can verify anything about the path being the /shortest/, we can
  verify that it is a path /at all/.

  == 'prop_shortest'

  This is very direct translation of the definition of \"shortest\", but it's
  not a terribly powerful test. Nonetheless, it helps find obvious bugs.
  For example,

  * try making the loop in 'dijkstra' terminate immediately
    (change the 'Nothing' guard to an underscore)
  * try changing the "nothing can happen anymore" case to 'undefined'

  Also try to see what happens if you have a bug in the test instead; e.g.,
  try @(<)@ and @(==)@ instead of @(<=)@.

  == 'prop_union'

  A useful technique for testing without a reference is to think about how you
  expect the output of a function to change as you change the input to that
  function.

  == 'prop_disjoint'

  Second example, where we can be more precise: we know the /exact/ cost
  we expect here.

  To get a counter example, you could try changing the update to the @dist@
  map in the algorithm to @(Map.insert v (alt + 1) (dist i))@.
-------------------------------------------------------------------------------}

prop_valid :: Graph Vertex Cost -> Property
prop_valid gr = gr `Graph.hasVertices` [A, B] ==>
    case Map.lookup B (Dijkstra.dist info) of
      Nothing   -> property Discard -- we did not find any paths
      Just cost -> counterexample ("dijkstra reported cost " ++ show cost) $
        case Dijkstra.pathBetween info (A, B) of
          Nothing   -> counterexample "cost but no path" False
          Just path -> counterexample ("dijkstra found path " ++ show path) $
            case Graph.pathCostIfValid gr path of
              Nothing         -> counterexample "path is invalid" False
              Just actualCost -> cost === actualCost
  where
    info :: Info Vertex Cost
    info = dijkstra (Graph.toDijkstra gr) A

prop_shortest :: PathInGraph -> Property
prop_shortest PathInGraph{pathIn, path} =
    -- Unlike in 'prop_valid', we /know/ the Nothing case should not happen
    case Map.lookup (pathDest path) (Dijkstra.dist info) of
      Nothing   -> counterexample "No path found" False
      Just cost -> counterexample (show (cost, actualCost)) $
        cost <= actualCost
  where
    info :: Info Vertex Cost
    info = dijkstra (Graph.toDijkstra pathIn) (pathSource path)

    actualCost :: Cost
    actualCost = Graph.pathCost pathIn path

prop_union :: Graph Vertex Cost -> Graph Vertex Cost -> Property
prop_union a b = a `Graph.hasVertices` [A, B] ==>
    case Map.lookup B (Dijkstra.dist inA) of
      Nothing      -> property Discard
      Just costInA -> counterexample ("cost in A: " ++ show costInA) $
        case Map.lookup B (Dijkstra.dist inAB) of
          Nothing       -> counterexample "No path found" $ False
          Just costInAB -> counterexample ("cost in AB: " ++ show costInAB) $
            costInA >= costInAB
  where
    ab :: Graph Vertex Cost
    ab = Graph.union a b

    inA, inAB :: Info Vertex Cost
    inA  = dijkstra (Graph.toDijkstra a)  A
    inAB = dijkstra (Graph.toDijkstra ab) A

prop_disjoint :: Graph Vertex Cost -> Graph Vertex Cost -> Cost -> Property
prop_disjoint a b n = [a, b] `Graph.haveVertices` [A, B] ==>
    case (Map.lookup B (Dijkstra.dist inA), Map.lookup B (Dijkstra.dist inB)) of
      (Just costInA, Just costInB) ->
        counterexample ("combined graph: " ++ show ab) $
          case Map.lookup (Right B) (Dijkstra.dist inAB) of
            Nothing -> counterexample "No path found" False
            Just costInAB ->
              counterexample (show (costInA, costInB, costInAB)) $
                costInAB === costInA + costInB + n
      _otherwise ->
        property Discard
  where
    ab :: Graph (Either Vertex Vertex) Cost
    ab = Graph.addEdge (Left B, Right A) n $
           Graph.union (Graph.mapVertices Left  a)
                       (Graph.mapVertices Right b)

    inA, inB :: Info Vertex Cost
    inA  = dijkstra (Graph.toDijkstra a)  A
    inB  = dijkstra (Graph.toDijkstra b)  A

    inAB :: Info (Either Vertex Vertex) Cost
    inAB = dijkstra (Graph.toDijkstra ab) (Left A)

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain $ testGroup "Dijkstra" [
      testProperty "costMonotone" prop_costMonotone
    , testProperty "valid"        prop_valid
    , testProperty "shortest"     prop_shortest
    , testProperty "union"        prop_union
    , testProperty "disjoint"     prop_disjoint
    ]
