{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE QuantifiedConstraints   #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Data.Functor.Identity
import Data.Kind
import Data.Proxy

{-------------------------------------------------------------------------------
  Example 1: GADTs
-------------------------------------------------------------------------------}

data Value (a :: Type) where
  VInt  :: Int  -> Value Int
  VBool :: Bool -> Value Bool

value :: Value a -> a
value (VInt  x) = x
value (VBool x) = x

data Dict (c :: k -> Constraint) (a :: k) where
  Dict :: c a => Dict c a

dictShowInt :: Dict Show Int
dictShowInt = Dict

-- Type error: "No instance for Show (IO Int)"
-- dictShowIO :: Dict Show (IO Int)
-- dictShowIO = Dict

canShowValue :: Value a -> Dict Show a
canShowValue (VInt  _) = Dict
canShowValue (VBool _) = Dict

-- We could add a Show a => constraint here, but it should not be necessary!
showValue :: Value a -> String
showValue v =
    case canShowValue v of
      Dict -> show (value v)

{-------------------------------------------------------------------------------
  Not covered in the episode: using CPS instead of Dict

  This is an alternative approach, but (arguably) leads to code that is less
  nice and less composable.
-------------------------------------------------------------------------------}

canShowValueCPS :: Value a -> (Show a => r) -> r
canShowValueCPS (VInt  _) k = k
canShowValueCPS (VBool _) k = k

{-------------------------------------------------------------------------------
  Interlude: Heretogenous products, type families
-------------------------------------------------------------------------------}

-- | Heterogenous product
--
-- Example:
--
-- > NP Maybe '[Int, Bool, Char] ~= (Maybe Int, Maybe Bool, Maybe Char)
data NP (f :: k -> Type) (as :: [k]) where
  Nil  :: NP f '[]
  Cons :: f a -> NP f as -> NP f (a ': as)

type family AllF (c :: k -> Constraint) (as :: [k]) :: Constraint where
  AllF c '[]       = ()
  AllF c (a ': as) = (c a, AllF c as)

-- To understand why this type checks, consider what happens when we pattern
-- match on the @Cons@ constructor:
--
-- 1. We learn that @as ~ a ': as'@
-- 2. Therefore @AllF Eq as@ reduces to @(Eq a, AllF Eq as')@
-- 3. From @Eq a@ ghc can reduce @Eq (Identity a)@, therefore we can do @x == y@
-- 4. The @AllF Eq as'@ enables the recursive call.
equalNP :: AllF Eq as => NP Identity as -> NP Identity as -> Bool
equalNP Nil         Nil         = True
equalNP (Cons x xs) (Cons y ys) = x == y && equalNP xs ys

{-------------------------------------------------------------------------------
  Example 2: Proving allNP
-------------------------------------------------------------------------------}

class    AllF c as => All c as
instance AllF c as => All c as

-- | Prove an @All c@ constraint
--
-- If you want to understand this function, try to write it again by pattern
-- matching on the @NP@ but not on the @Dict@s, and look at the error messages
-- you get from ghc.
allNP :: NP (Dict c) as -> Dict (All c) as
allNP Nil            = Dict
allNP (Cons Dict ds) = case allNP ds of
                         Dict -> Dict

mapNP :: (forall a. f a -> g a) -> NP f as -> NP g as
mapNP _ Nil         = Nil
mapNP f (Cons x xs) = Cons (f x) (mapNP f xs)

exampleAll :: NP Value as -> Dict (All Show) as
exampleAll = allNP . mapNP canShowValue

{-------------------------------------------------------------------------------
  Example 3: Implication
-------------------------------------------------------------------------------}

implies ::
      ( All c as
      , forall a. c a => d a
      )
   => Proxy c -> Proxy d -> NP f as -> Dict (All d) as
implies _  _  Nil         = Dict
implies pc pd (Cons _ xs) = case implies pc pd xs of
                              Dict -> Dict

foo :: All Ord as => NP Identity as -> NP Identity as -> Bool
foo xs ys =
    case implies (Proxy @Ord) (Proxy @Eq) xs of
      Dict -> equalNP xs ys

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = putStrLn "Hello, Haskell!"
