{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Flatten' where

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

instance Flatten' (IsList (Flattened a)) [a] => Flatten [a] where
  type Flattened [a] = Flattened' (IsList (Flattened a)) [a]
  flatten xs = flatten' (Proxy @(IsList (Flattened a))) xs

class Flatten' (b :: Bool) a where
  type Flattened' b a :: Type
  flatten' :: Proxy b -> a -> Flattened' b a

instance Flatten a => Flatten' False [a] where
  type Flattened' False [a] = [Flattened a]
  flatten' _ xs = map flatten xs

instance (Flattened a ~ [b], Flatten a) => Flatten' True [a] where
  type Flattened' True [a] = Flattened a
  flatten' _ xs = concatMap flatten xs

instance Flatten (Maybe a) where
  type Flattened (Maybe a) = [a]
  flatten Nothing = []
  flatten (Just x) = [x]

-- >>> flatten [[False, True],[True], [False, False]]
-- [False,True,True,False,False]

-- >>> flatten [Just False, Just True]
-- [False,True]
