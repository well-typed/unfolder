{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O2 #-}

module Main where

import Prelude hiding (Monoid(..), Semigroup(..), Monad(..))

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word
import System.Random.SplitMix qualified as SplitMix
import Test.Inspection qualified as Inspection
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

{-------------------------------------------------------------------------------
  Episode: laws

  What is the point of laws? When /can/ we benefit from them, and when /should/
  we think about them? How do we go about proving laws, and what do we do when
  laws don't /quite/ hold?

  Side note on equality: We use @≡@ to talk about equality in laws. Without
  getting to precise about this, what we mean is that when @x ≡ y@, then we
  should feel free to substitute @x@ for @y@ anywhere in our code.
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Lenses (setters)
-------------------------------------------------------------------------------}

-- | Setter
--
-- Law:
--
-- > set x (set x' y) ≡ set x y
type Setter a b = b -> a -> a

-- | Example setter: set the first component of a pair
first :: Setter (a, b) a
first x (_, b) = (x, b)

-- | Example of using 'first'
--
-- We'd expect these two definitions to behave the same.
useFirst, useFirst' :: (Int, Bool) -> (Int, Bool)
useFirst  (a, b) = first 1 (first 2 (a, b))
useFirst' (a, b) = first 1 (a, b)

-- | If we abstract over the setter, are these still the same?
useSetter, useSetter' :: Setter (Int, Int) Int -> (Int, Int) -> (Int, Int)
useSetter  set (a, b) = set 1 (set 2 (a, b))
useSetter' set (a, b) = set 1 (a, b)

prop_setter_first :: Int -> Int -> (Int, Int) -> Property
prop_setter_first x x' (a, b) =
        first x (first x' (a, b))
    === first x (a, b)

{-
  LHS:

  >   first x (first x' (a, b))
  > ≡ first x (x', b)
  > ≡ (x, b)

  RHS:

  >   first x (a, b)
  > ≡ (x, b)

  QED.
-}

{-------------------------------------------------------------------------------
  Monoids

  Why do the monoid laws matter? Suppose you have somewhere in your code

  > foo = complicated1 <> complicated2 <> complicated3

  and you realize that actually @complicated1 <> complicated2@ is a useful
  concept of its own, so you replace this code with

  > newAbstraction = complicated1 <> complicated2
  > foo = newAbstraction <> complicated3

  Is this okay to do?

  Note: in recent @base@ 'Monoid' is split into 'Semigroup' and 'Monoid'; for
  simplicity we don't make that distinction here.
-------------------------------------------------------------------------------}

-- | Monoid
--
-- First law: identity
--
-- > mempty <> x ≡ x
-- > x <> mempty ≡ x
--
-- Second law: associativity
--
-- > x <> (y <> z) ≡ x <> (y <> z)
class Monoid m where
  mempty :: m
  (<>)   :: m -> m -> m  -- also known as mappend

{-------------------------------------------------------------------------------
  Monoid example: lists
-------------------------------------------------------------------------------}

instance Monoid [a] where
  mempty = []
  (<>)   = (++)

prop_mempty_right_list :: [Int] -> Property
prop_mempty_right_list xs = xs <> [] === xs

{-
  Recall the definition of (++):

  > []     ++ ys = ys
  > (x:xs) ++ ys = x : (xs ++ ys)

  The left identity law therefore follows immediately. The right identity law
  is harder:

  > xs ++ [] ≡ xs

  Proof by induction. Base case:

  >   [] ++ []
  > ≡ []

  Induction case:

  >   (x:xs) ++ []
  >      { apply definition }
  > ≡ x : (xs ++ [])
  >      { induction hypothesis }
  > ≡ x : xs
-}

{-------------------------------------------------------------------------------
  Monoid example: Int
-------------------------------------------------------------------------------}

instance Monoid Int where
  mempty = 0
  (<>)   = (+)

prop_mappend_Int :: Int -> Int -> Int -> Property
prop_mappend_Int x y z = x <> (y <> z) === (x <> y) <> z

{-------------------------------------------------------------------------------
  Example of a law that doesn't /quite/ hold
-------------------------------------------------------------------------------}

data Prog =
    Done
  | Print Int
  | Seq Prog Prog
  deriving (Show)

run :: Prog -> [Int]
run Done      = []
run (Print i) = [i]
run (Seq p q) = run p <> run q

instance Monoid Prog where
  mempty = Done
  (<>)   = Seq

{-
  Laws immediately fail:

  >   p <> mempty
  > ≡ Seq p Done

  But we can say they are true "up to interpretation".
 -}

prop_mempty_right_prog :: Prog -> Property
prop_mempty_right_prog p = run (p <> mempty) === run p

{-
  Proof for right identity:

  >   run (p <> mempty)
  >     { definition of (<>) and mempty }
  > ≡ run (Seq p Done)
  >     { definition of run for Seq }
  > ≡ run p ++ run Done
  >     { definition of run for Done }
  > ≡ run p ++ []
  >     { now we can rely on the fact that lists are a monoid }
  > ≡ run p

  The other laws proceed similarly.
-}

{-------------------------------------------------------------------------------
  Second example of a law that doesn't /quite/ hold

  Note: in base this has superclasses (Functor, Applicative), but we simplify.
-------------------------------------------------------------------------------}

-- | Monad
--
-- We will only consider one of the three monad laws here:
--
-- > return x >>= f ≡ f x
class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

type SimpleGen a = PRNG -> a

genBelow :: Int -> SimpleGen Int
genBelow n prng = genInt prng `mod` n

instance Monad ((->) PRNG) where
  return x = \_ -> x
  x >>=  f = \prng -> let (prng1, prng2) = splitPRNG prng
                      in f (x prng1) prng2

{-
  Attempt to prove the law:

  >   return x >>= f
  >     { unfold definition of return }
  > ≡ (\_ -> x) >>= f
  >     { unfold definition of (>>=) }
  > ≡ \prng -> let (prng1, prng2) = splitPRNG prng
  >            in f ((\_ -> x) prng1) prng2
  >    { simplify }
  > ≡ \prng -> let (_, prng2) = splitPRNG prng
  >            in f x prng2

  So the proof fails.

  This is another example of "laws up to interpretaiton", where here the usual
  interpetation is the resulting probability distribution. In the rest of
  this section we sketch how we might test this.
-}

newtype ProbDistr a = ProbDistr (Map a Double)
  deriving (Show)

instance Eq a => Eq (ProbDistr a) where
  ProbDistr p == ProbDistr q = and [
        Map.keys p == Map.keys q
      , all (uncurry approxEq) $ zip (Map.elems p) (Map.elems q)
      ]
    where
      approxEq :: Double -> Double -> Bool
      approxEq x y = abs (x - y) <= 0.01

computeProbDistr :: forall a. Ord a => SimpleGen a -> ProbDistr a
computeProbDistr g = ProbDistr $
    fmap (\n -> fromIntegral n / fromIntegral numSamples) counts
  where
    numSamples :: Word64
    numSamples = 1_000_000

    counts :: Map a Integer
    counts = Map.unionsWith (+) $
               map (\seed -> Map.singleton (g $ mkPRNG seed) 1)
               [1 .. numSamples]

-- | Test the monad law for one very specific generator (genBelow 10)
--
-- Ideally of course we'd test the law for /arbitrary/ generators.
test_return_left_genBelow :: Assertion
test_return_left_genBelow =
   assertEqual ""
        (computeProbDistr (genBelow 10))
        (computeProbDistr (return 10 >>= genBelow))

{-------------------------------------------------------------------------------
  Bonus material: using inspection testing (not covered in the episode)

  For some laws we can use inspection testing to proof that two equations are
  the same. This works only if the GHC core generated from both expressions is
  identical (which in turn depends on ghc version, optimization settings, etc.).
-------------------------------------------------------------------------------}

setter_first_lhs, setter_first_rhs :: a -> a -> (a, b) -> (a, b)
setter_first_lhs x x' (a, b) = first x (first x' (a, b))
setter_first_rhs x _  (a, b) = first x (a, b)

proof_setter_first :: Inspection.Result
proof_setter_first = $(Inspection.inspectTest $
                     'setter_first_lhs
      Inspection.=== 'setter_first_rhs
    )

mempty_left_Int_lhs, mempty_left_Int_rhs :: Int -> Int
mempty_left_Int_lhs x = mempty <> x
mempty_left_Int_rhs x = x

proof_mempty_left_Int :: Inspection.Result
proof_mempty_left_Int = $(Inspection.inspectTest $
                     'mempty_left_Int_lhs
      Inspection.=== 'mempty_left_Int_rhs
    )

mappend_Int_lhs, mappend_Int_rhs :: Int -> Int -> Int -> Int
mappend_Int_lhs x y z = x <> (y <> z)
mappend_Int_rhs x y z = (x <> y) <> z

proof_mappend_Int :: Inspection.Result
proof_mappend_Int = $(Inspection.inspectTest $
                     'mappend_Int_lhs
      Inspection.=== 'mappend_Int_rhs
    )

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain $ testGroup "laws" [
      testGroup "Properties" [
          testProperty "prop_setter_first"         prop_setter_first
        , testProperty "prop_mappend_Int"          prop_mappend_Int
        , testProperty "prop_mempty_right_list"    prop_mempty_right_list
        , testProperty "prop_mempty_right_prog"    prop_mempty_right_prog
        , testCase     "test_return_left_genBelow" test_return_left_genBelow
        ]
    , testGroup "Bonus: inspection testing" [
                       testProof  "proof_setter_first"    proof_setter_first
        ,              testProof  "proof_mempty_left_Int" proof_mempty_left_Int
        , expectFail $ testProof  "proof_mappend_Int"     proof_mappend_Int
        ]
    ]

{-------------------------------------------------------------------------------
  Auxiliary infrastructure required to run the tests
-------------------------------------------------------------------------------}

testProof :: String -> Inspection.Result -> TestTree
testProof name result = testCaseInfo name $ do
    case result of
      Inspection.Failure err -> assertFailure err
      Inspection.Success msg -> pure msg

instance Arbitrary Prog where
  arbitrary = sized $ \sz -> do
      n <- choose (0, sz)
      go n
    where
      go :: Int -> Gen Prog
      go 0 = pure Done
      go 1 = oneof [Print <$> arbitrary, pure Done]
      go n = do
          onLeft <- choose (0, n)
          let onRight = n - onLeft
          Seq <$> go onLeft <*> go onRight

  shrink Done      = []
  shrink (Print n) = Print <$> shrink n
  shrink (Seq p q) = concat [
        [ Seq p' q  | p' <- shrink p ]
      , [ Seq p  q' | q' <- shrink q ]
      , [ p ]
      , [ q ]
      ]

{-------------------------------------------------------------------------------
  Simple wrapper around splitmix

  The only point of this wrapper is to make the code above more readable.
-------------------------------------------------------------------------------}

type PRNG = SplitMix.SMGen

mkPRNG :: Word64 -> SplitMix.SMGen
mkPRNG = SplitMix.mkSMGen

genInt :: PRNG -> Int
genInt = fst . SplitMix.nextInt

splitPRNG :: PRNG -> (PRNG, PRNG)
splitPRNG = SplitMix.splitSMGen