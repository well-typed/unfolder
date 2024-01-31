{-# OPTIONS_GHC -O0 #-}

{-
    Repeated application: a new perspective on foldl' (unfolder Episode 19)

    In this beginner-oriented episode we introduce a useful combinator called
    `repeatedly`, which captures the concept "repeatedly execute an action to a
    bunch of arguments". We will discuss both how to implement this combinator
    as well as some use cases.
-}

module Main where

import Control.Monad
import Data.Coerce
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Test.QuickCheck

{-------------------------------------------------------------------------------
  Example 1: Repeatedly updating state
-------------------------------------------------------------------------------}

data Pos = Pos { x :: !Int, y :: !Int }
  deriving (Show)

origin :: Pos
origin = Pos{x = 0, y = 0}

data Step = StepLeft | StepRight | StepUp | StepDown | StepAway
  deriving (Show)

step :: Step -> Pos -> Pos
step StepLeft  Pos{x, y} = Pos{x = x - 1, y}
step StepRight Pos{x, y} = Pos{x = x + 1, y}
step StepUp    Pos{x, y} = Pos{x, y = y + 1}
step StepDown  Pos{x, y} = Pos{x, y = y - 1}
step StepAway  Pos{x, y} = Pos{x = x * 2, y = y * 2}

exampleSteps :: [Step]
exampleSteps = [StepRight, StepRight, StepUp, StepAway]

stepAll :: [Step] -> Pos -> Pos
stepAll = repeatedly step

{-------------------------------------------------------------------------------
  Definition of repeatedly
-------------------------------------------------------------------------------}

-- | Definition with explicit recursion
repeatedly1 :: (a -> b -> b) -> ([a] -> b -> b)
repeatedly1 _ []     !b = b
repeatedly1 f (a:as) !b = repeatedly1 f as (f a b)

-- | Definition in terms of foldl'
--
-- Compare the type of @foldl'@:
--
-- foldl' :: (b -> a -> b) -> b -> [a] -> b
--
-- We can define
--
-- >    \f as b -> foldl' (\a b -> f b a) b as
-- > == \f as b -> foldl' (flip f) b as
-- > == \f as b -> flip (foldl' (flip f)) as b
-- > == \f      -> flip (foldl' (flip f))
-- > == \f      -> flip . foldl' . flip $ f
-- > ==            flip . foldl' . flip
repeatedly2 :: forall a b. (a -> (b -> b)) -> [a] -> b -> b
repeatedly2 = flip . foldl' . flip

-- | Generalization to 'Foldable'
repeatedly :: Foldable t => (a -> b -> b) -> (t a -> b -> b)
repeatedly = flip . foldl' . flip

{-------------------------------------------------------------------------------
  Example 2: annotating QuickCheck properties
-------------------------------------------------------------------------------}

prop_addZero :: Int -> Property
prop_addZero n =
    collects  [
        if n == 0 then "zero"     else "not zero"
      , if even n then "even"     else "odd"
      , if n < 0  then "negative" else "positive"
      ]
    $ n + 0 === n

collects :: Show a => [a] -> Property -> Property
collects = repeatedly collect

{-------------------------------------------------------------------------------
  Example 3: map inversion

  When reproducing this example, it may be useful to take advantage of HLS's
  "define" feature, which creates new findings of undefined functions, using
  the type information of the context.
-------------------------------------------------------------------------------}

invert :: forall a b. (Ord a, Ord b) => Map a (Set b) -> Map b (Set a)
invert xs =
    repeatedly (uncurry aux) (Map.toList xs) Map.empty
  where
    aux :: a -> Set b -> Map b (Set a) -> Map b (Set a)
    aux a = repeatedly (ins a)

    ins :: a -> b -> Map b (Set a) -> Map b (Set a)
    ins a b = Map.alter (updateSet a) b

    updateSet :: a -> Maybe (Set a) -> Maybe (Set a)
    updateSet a Nothing   = Just $ Set.singleton a
    updateSet a (Just as) = Just $ Set.insert a as

{-------------------------------------------------------------------------------
  Monadic version

  We could easily generalize 'repeatedlyM' to 'Foldable' also; not doing so just
  makes the signature slightly simpler for the sake of presentation in the
  epsiode.
-------------------------------------------------------------------------------}

repeatedlyM1 :: Monad m => (a -> b -> m b) -> ([a] -> b -> m b)
repeatedlyM1 _ []     !b = return b
repeatedlyM1 f (a:as) !b = do b' <- f a b
                              repeatedlyM1 f as b'

repeatedlyM :: Monad m => (a -> b -> m b) -> ([a] -> b -> m b)
repeatedlyM = flip . foldM . flip

{-------------------------------------------------------------------------------
  Example use
-------------------------------------------------------------------------------}

stepM :: Step -> Pos -> Either String Pos
stepM StepLeft  Pos{x, y} = Right Pos{x = x - 1, y}
stepM StepRight Pos{x, y} = Right Pos{x = x + 1, y}
stepM StepUp    Pos{x, y} = Right Pos{x, y = y + 1}
stepM StepDown  Pos{x, y} = Right Pos{x, y = y - 1}
stepM StepAway  (Pos 0 0) = Left "Cannot step away"
stepM StepAway  Pos{x, y} = Right Pos{x = x * 2, y = y * 2}

exampleSteps2 :: [Step]
exampleSteps2 = [StepRight, StepLeft, StepAway, StepUp]

{-------------------------------------------------------------------------------
  We now have one more way to write 'repeatedly'

  The 'main' function can be used to verify that this definition is sufficiently
  strict (so that that the computation runs in constant space).
-------------------------------------------------------------------------------}

newtype StrictIdentity a = StrictIdentity a

instance Functor StrictIdentity where
  fmap = coerce

instance Applicative StrictIdentity where
  pure  = coerce
  (<*>) = ap

instance Monad StrictIdentity where
  StrictIdentity x >>= f = f $! x

repeatedly3 :: forall a b. (a -> b -> b) -> ([a] -> b -> b)
repeatedly3 = coerce (repeatedlyM @StrictIdentity @a @b)

main :: IO ()
main = print $ repeatedly3 (+) [1 .. 1_000_000] (0 :: Int)
