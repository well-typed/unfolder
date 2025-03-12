-- | The Haskell Unfolder episode 40: understanding through a model
--
-- QuickCheck is useful for more than just testing. Comparing the behaviour of a
-- system to a model can be used to check if a system under construction is
-- working correctly, but it can also be used to better understand an already
-- existing system. In this episode we show that this does not need to be very
-- difficult, by designing a model that we can use to understand tensor
-- convolutions in an existing large library.
module Ep40.Tensor where

import Prelude hiding (zipWith, replicate, map)

import Data.List qualified as List
import Data.Type.Nat
import Data.Vec.Lazy (Vec(..))
import Test.Tensor.TestValue

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Tensor (sometimes known as multi-dimensional arrays)
--
-- A 1D tensor is essentially a list, a 2D tensor a list of lists, etc.

-- Invariants:
--
-- * The tensor must be rectangular
-- * The tensor cannot have any empty dimensions
data Tensor n a where
  Scalar :: { getScalar :: a } -> Tensor Z a
  Tensor :: { getTensor :: [Tensor n a] } -> Tensor (S n) a

deriving stock instance Show a => Show (Tensor n a)
deriving stock instance Eq   a => Eq   (Tensor n a)

deriving stock instance Functor     (Tensor n)
deriving stock instance Traversable (Tensor n)
deriving stock instance Foldable    (Tensor n)

{-------------------------------------------------------------------------------
  Example
-------------------------------------------------------------------------------}

blur :: Tensor Nat2 TestValue
blur = Tensor [
      Tensor [Scalar 0.1, Scalar 0.1, Scalar 0.1]
    , Tensor [Scalar 0.1, Scalar 0.2, Scalar 0.1]
    , Tensor [Scalar 0.1, Scalar 0.1, Scalar 0.1]
    ]

{-------------------------------------------------------------------------------
  Simple functions
-------------------------------------------------------------------------------}

inject :: Tensor n a -> Tensor (S n) a
inject x = Tensor [x]

project :: Tensor (S n) a -> Tensor n a
project (Tensor [x]) = x
project _ = error "not a singleton"

type Size n = Vec n Int

size :: Tensor n a -> Size n
size (Scalar _)  = VNil
size (Tensor xs) = List.length xs ::: size (List.head xs)

replicate :: Size n -> a -> Tensor n a
replicate VNil       x = Scalar x
replicate (n ::: ns) x = Tensor $ List.replicate n (replicate ns x)

zipWith :: (a -> b -> c) -> Tensor n a -> Tensor n b -> Tensor n c
zipWith f (Scalar a)  (Scalar b)  = Scalar (f a b)
zipWith f (Tensor as) (Tensor bs) = Tensor $ List.zipWith (zipWith f) as bs

map :: (Tensor n a -> Tensor m b) -> Tensor (S n) a -> Tensor (S m) b
map f (Tensor xs) = Tensor $ List.map f xs

{-------------------------------------------------------------------------------
  Example
-------------------------------------------------------------------------------}

mapExample :: Tensor Nat1 TestValue
mapExample = map (Scalar . sum) blur

{-------------------------------------------------------------------------------
  Auxiliary: lists
-------------------------------------------------------------------------------}

-- | The first @r@ sublists of length @n@
--
-- >    consecutive 4 3 [1..6]
-- > == [[1,2,3],[2,3,4],[3,4,5],[4,5,6]]
consecutive :: Int -> Int -> [a] -> [[a]]
consecutive r n = List.take r . List.map (List.take n) . List.tails

-- | Distribute @f@ over @[]@
distribList :: Functor f => Int -> f [a] -> [f a]
distribList 0 _   = []
distribList n fxs = (head <$> fxs) : distribList (n - 1) (tail <$> fxs)

{-------------------------------------------------------------------------------
  Convolutions
-------------------------------------------------------------------------------}

-- | Distribute @f@ over 'Tensor'
distrib :: Functor f => Size n -> f (Tensor n a) -> Tensor n (f a)
distrib VNil       = Scalar . fmap getScalar
distrib (n ::: ns) = Tensor . fmap (distrib ns) . distribList n . fmap getTensor

-- | Compute number of subtensors
numSubs :: Size n -> Size n -> Size n
numSubs VNil       VNil       = VNil
numSubs (k ::: ks) (i ::: is) = (i - k + 1)  ::: numSubs ks is

-- | Subtensors of the specified size
--
-- You can think of this as a generalization of 'consecutive'
subs :: Size n -> Tensor n a -> Tensor n (Tensor n a)
subs = \ks xs ->
    go (numSubs ks (size xs)) ks xs
  where
    go :: Size n -> Size n -> Tensor n a -> Tensor n (Tensor n a)
    go VNil       VNil       (Scalar x)  = Scalar (Scalar x)
    go (r ::: rs) (n ::: ns) (Tensor xs) = Tensor [
          Tensor <$> distrib rs selected
        | selected <- consecutive r n (List.map (go rs ns) xs)
        ]

-- | Convolution
convolve :: forall n a. Num a => Tensor n a -> Tensor n a -> Tensor n a
convolve kernel input =
    aux <$> subs (size kernel) input
  where
    aux :: Tensor n a -> a
    aux = sum . zipWith (*) kernel
