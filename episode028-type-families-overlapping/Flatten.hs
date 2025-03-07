{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Flatten where

import Data.Kind
import Data.Proxy
import Data.Foldable

class Flatten a where
  type Flattened a :: Type
  flatten :: a -> Flattened a

instance Flatten Bool where
  type Flattened Bool = Bool
  flatten x = x

type family IsList (a :: Type) :: Bool where
  IsList [a] = 'True
  IsList _ = 'False

instance Flatten' (IsList a) [a] => Flatten [a] where
  type Flattened [a] = Flattened' (IsList a) [a]
  flatten xs = flatten' (Proxy @(IsList a)) xs

class Flatten' (b :: Bool) a where
  type Flattened' b a :: Type
  flatten' :: Proxy b -> a -> Flattened' b a

instance Flatten a => Flatten' False [a] where
  type Flattened' False [a] = [Flattened a]
  flatten' _ xs = map flatten xs

instance (Flattened [a] ~ [b], Flatten [a]) => Flatten' True [[a]] where
  type Flattened' True [[a]] = Flattened [a]
  flatten' _ xss = concatMap flatten xss

{-
instance Flatten a => Flatten [a] where
  type Flattened [a] = [Flattened a]
  flatten xs = map flatten xs

instance Flatten a => Flatten [[a]] where
  type Flattened [[a]] = [Flattened a]
  flatten xss = concatMap flatten xss
-}

-- >>> flatten [[False, True],[True], [False, False]]
-- [False,True,True,False,False]
