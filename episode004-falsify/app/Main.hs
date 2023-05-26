module Main (main) where

import Control.Monad
import Control.Selective
import Data.Bits
import Data.Char
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Word

import Test.Tasty
import Test.Tasty.Falsify

import Test.Falsify.Predicate ((.$))

import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Range     qualified as Range

{-------------------------------------------------------------------------------
  1: Introduction

  Note that the counter-example we get to 'prop_incr1' is always the minimum
  one, even though we did not write a shrinker: shrinking is integrated.
-------------------------------------------------------------------------------}

-- | Function we want to test
incr :: Word -> Word
incr x = x .|. 1

-- | Property that the function should have
--
-- When you run this test, make sure to also look at the information in the log.
prop_incr1 :: Property ()
prop_incr1 = do
    x <- gen $ Gen.integral $ Range.between (0, 100)
    assert $ P.eq .$ ("expected", x + 1)
                  .$ ("actual", incr x)

{-------------------------------------------------------------------------------
  2: Writing your own generators
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  2a: below

  We do not have to write a shrinker (unlike in QuickCheck), but we still need
  to think about shrinking. In this section, 'below' is an example of something
  that shrinks poorly.

  In order to test shrinking, we need to write a property that fails (because
  shrinking only happens when properties fail), but fails with a specific value:
  the value that we want to compare before and after shrinking.
-------------------------------------------------------------------------------}

below :: Word -> Gen Word
below n = (\p -> fromIntegral p `mod` n) <$> Gen.prim

prop_incr2 :: Property ()
prop_incr2 = do
    x <- gen $ below 100
    assert $ P.eq .$ ("expected", x + 1)
                  .$ ("actual", incr x)

prop_below_shrinking1 :: Property ()
prop_below_shrinking1 = testShrinking P.ge $ do
    x <- gen $ below 100
    testFailed x

prop_below_shrinking2 :: Property ()
prop_below_shrinking2 = testShrinking P.ge $ do
    p <- gen $ Gen.prim
    let x :: Word = fromIntegral p `mod` 100
    testFailed x

{-------------------------------------------------------------------------------
  2b: fraction and atMost

  Function 'atMost' in this section is similar to 'below', but has much better
  shrinking behaviour.
-------------------------------------------------------------------------------}

fraction :: Gen Double
fraction = aux <$> Gen.prim
  where
    aux :: Word64 -> Double
    aux x = fromIntegral x / fromIntegral (maxBound :: Word64)

atMost :: Word -> Gen Word
atMost x = aux <$> fraction
  where
    aux :: Double -> Word
    aux f = round $ f * fromIntegral x

prop_atMost_shrinking :: Property ()
prop_atMost_shrinking = testShrinking P.ge $ do
    x <- gen $ atMost 100
    testFailed x

{-------------------------------------------------------------------------------
  2c: list1

  There are two things to note about 'list1':

  1. It does not shrink very well: it can only shrink to a prefix of the list.

  2. It /can/ however can go back and forth between shrinking the list length
     and shrinking elements of the list. To see this, run with

     > cabal run episode4 -- -p allEqual1 --falsify-verbose --falsify-replay=015e26adf11a950ba46f0199f382963769

     This is what sets falsify apart from Hedgehog.
-------------------------------------------------------------------------------}

list1 :: Word -> Gen a -> Gen [a]
list1 maxLen g = do
    n <- Gen.integral $ Range.between (0, maxLen)
    replicateM (fromIntegral n) g

allEqual :: Eq a => [a] -> Bool
allEqual xs = length (nub xs) <= 1

prop_allEqual1 :: Property ()
prop_allEqual1 = do
    xs <- gen $ list1 10 $ Gen.bool False
    assert $ P.satisfies ("allEqual", allEqual) .$ ("xs", xs)

{-------------------------------------------------------------------------------
  2d: list2

  Here we write a generator which can drop arbitrary elements from the list.
-------------------------------------------------------------------------------}

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = do
    x <- Gen.bool False
    if x then Just <$> g else pure Nothing

list2 :: Word -> Gen a -> Gen [a]
list2 n g = catMaybes <$> replicateM (fromIntegral n) (genMaybe g)

prop_allEqual2 :: Property ()
prop_allEqual2 = do
    xs <- gen $ list2 10 $ Gen.bool False
    assert $ P.satisfies ("allEqual", allEqual) .$ ("xs", xs)

{-------------------------------------------------------------------------------
  3: Functions

  Suppose we wanted to test the property that

  > filter p (map f xs) == map f (filter p xs)

  We can use the 'Gen.fun' combinator to generate arbitrary functions. It will
  create a (usually) infinite object describing the function, which maps any
  input of the function to a random output, produced by the generator that you
  provide as argument to 'Gen.fun'.
-------------------------------------------------------------------------------}

prop_map_filter :: Property ()
prop_map_filter = do
    Fn (f :: Word64 -> Word64) <- gen $ Gen.fun $ Gen.prim
    Fn (p :: Word64 -> Bool)   <- gen $ Gen.fun $ Gen.bool False
    xs <- gen $ list1 10 $ Gen.integral $ Range.between (0, 10)
    assert $ P.eq .$ ("lhs", filter p (map f xs))
                  .$ ("rhs", map f (filter p xs))

{-------------------------------------------------------------------------------
  4: Selective functors

  We expect that 'genLetterOrNumber1' should shrink towards a 'Right' value, but
  it doesn't always do that: if we are currently generating a 'Left' value, then
  the 'Right' value (generated by 'genNumber') will trivially shrink towards its
  minimum value (because it's not used); and that minimum value might not be a
  counter-example (in 'prop_letterOrNumber1', @0@ is not a counter-example to
  'prop').

  Moral of the story: do not generate elements that are not used. Instead make
  use of the selective functor interface:

  > ifS :: Selective f => f Bool -> f a -> f a -> f a

  When we do this in 'genLetterOrNumber2', we get the shrinking we want. This
  is something that is not available in Hypothesis.

  Note: if you conditionally call one generator or another, something like

  > do x <- Gen.bool False
  >     if x then Left <$> genLetter else Right <$> genNumber

  then these two generators cannot shrink independently from each other; see
  the paper for details (this is the behaviour you'd get in Hypothesis).
-------------------------------------------------------------------------------}

genLetterOrNumber1 :: Gen (Either Char Int)
genLetterOrNumber1 = do
    a <- genLetter
    b <- genNumber
    x <- Gen.bool False
    return $ if x then Left a else Right b

prop_letterOrNumber1 :: Property ()
prop_letterOrNumber1 = do
    x <- gen $ genLetterOrNumber1
    assert $ P.satisfies ("prop", prop) .$ ("x", x)
  where
    prop :: Either Char Int -> Bool
    prop (Left  c) = c == 'a'
    prop (Right i) = i == 0

genLetterOrNumber2 :: Gen (Either Char Int)
genLetterOrNumber2 =
    ifS (Gen.bool False)
        (Left  <$> genLetter)
        (Right <$> genNumber)

prop_letterOrNumber2 :: Property ()
prop_letterOrNumber2 = do
    x <- gen $ genLetterOrNumber2
    assert $ P.satisfies ("prop", prop) .$ ("x", x)
  where
    prop :: Either Char Int -> Bool
    prop (Left  c) = c == 'a'
    prop (Right i) = i == 0

{-------------------------------------------------------------------------------
  Tasty driver
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain $ testGroup "Unfolder Episode 4" [
      testProperty "incr1"            prop_incr1
    , testProperty "incr2"            prop_incr2
    , testProperty "below_shrinking1" prop_below_shrinking1
    , testProperty "below_shrinking2" prop_below_shrinking2
    , testProperty "atMost_shrinking" prop_atMost_shrinking
    , testProperty "allEqual1"        prop_allEqual1
    , testProperty "allEqual2"        prop_allEqual2
    , testProperty "map_filter"       prop_map_filter
    , testProperty "letterOrNumber1"  prop_letterOrNumber1
    , testProperty "letterOrNumber1"  prop_letterOrNumber2
    ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Generate character between @a@ and @z@
--
-- This function isn't quite as pretty as it should be; see
-- <https://github.com/well-typed/falsify/issues/51>.
genLetter :: Gen Char
genLetter = fmap chr $ Gen.int $ Range.between (ord 'a', ord 'z')

genNumber :: Gen Int
genNumber = Gen.integral $ Range.between (0, 100)
