module Main where

import Control.Monad
import Data.Vector.Storable qualified as Storable (Vector)
import Data.Vector.Storable qualified as Vector
import Foreign
import Test.Tasty
import Test.Tasty.QuickCheck

import Ep38.CUDA qualified

main :: IO ()
main = defaultMain $ testGroup "Episode 38" [
      testProperty "map" $ \x (SmallArray inp) ->
        testRoughlyEqual
          (Vector.map (+ x)   inp)
          (Ep38.CUDA.mapAdd x inp)
    , testProperty "fold" $ \(SmallArray inp) ->
        testRoughlyEqual
          (Vector.foldl' (+) 0 inp)
          (Ep38.CUDA.foldAdd   inp)
    , testProperty "scan" $ \(SmallArray inp) ->
        testRoughlyEqual
          (Vector.postscanl (+) 0 inp)
          (Ep38.CUDA.scanAdd      inp)
    ]

{-------------------------------------------------------------------------------
  QuickCheck auxiliary
-------------------------------------------------------------------------------}

-- | Array with between 1 and 1024 elements
newtype SmallArray = SmallArray {
      unwrapSmallArray :: Storable.Vector Float
    }
  deriving stock (Show)

smallArrayFromList :: [Float] -> SmallArray
smallArrayFromList = SmallArray . Vector.fromList

smallArrayToList :: SmallArray -> [Float]
smallArrayToList = Vector.toList . unwrapSmallArray

instance Arbitrary SmallArray where
  arbitrary = do
      n <- choose (1, 1024)
      smallArrayFromList <$> replicateM n arbitrary

  shrink =
        map smallArrayFromList
      . filter (not . null)
      . shrink
      . smallArrayToList

{-------------------------------------------------------------------------------
  Compare test results in a way that allows for floating rounding errors
-------------------------------------------------------------------------------}

class RoughlyEqual a where
  roughlyEqual :: a -> a -> Bool

instance RoughlyEqual Float where
  roughlyEqual x y = abs (x - y) < 0.01

instance (Storable a, RoughlyEqual a) => RoughlyEqual (Storable.Vector a) where
  roughlyEqual xs ys = and [
        Vector.length xs == Vector.length ys
      , and $ zipWith roughlyEqual (Vector.toList xs) (Vector.toList ys)
      ]

testRoughlyEqual :: (RoughlyEqual a, Show a) => a -> a -> Property
testRoughlyEqual expected actual =
    counterexample (show expected ++ " /= " ++ show actual) $
      roughlyEqual expected actual
