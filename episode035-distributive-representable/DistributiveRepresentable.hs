{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module DistributiveRepresentable where

import Control.Applicative
import Data.Coerce
import Data.Kind
import Rendering


import Data.Distributive
import Data.Functor.Compose
import Data.Functor.Rep
import GHC.Generics (Generic1(..))


data Three a = MkThree { p0 :: a, p1 :: a, p2 :: a }
  deriving stock (Foldable, Functor, Generic1)

newtype Grid a = MkGrid { unGrid :: Three (Three a) }
  deriving stock (Foldable, Functor)
  deriving (Applicative, Representable) via Compose Three Three

-- >>> grid1
-- 0 1 2 
-- 3 4 5 
-- 6 7 8 
-- >>> (,) <$> grid1 <*> grid1
-- (0,0) (1,1) (2,2) 
-- (3,3) (4,4) (5,5) 
-- (6,6) (7,7) (8,8) 

instance Applicative Three where
  pure :: a -> Three a
  pure x = MkThree x x x

  (<*>) :: Three (a -> b) -> Three a -> Three b
  MkThree f1 f2 f3 <*> MkThree x1 x2 x3 = MkThree (f1 x1) (f2 x2) (f3 x3)

-- newtype Compose f g x = Compose { getCompose :: f (g x) }
--
-- instance (Applicative f, Applicative g) => Applicative (Compose f g)

-- class Functor d => Distributive d where
--   distribute :: Functor f => f (d a) -> d (f a)

instance Distributive Three where
  distribute :: Functor f => f (Three a) -> Three (f a)
  distribute ft = MkThree (p0 <$> ft) (p1 <$> ft) (p2 <$> ft)

transposeGrid :: Grid a -> Grid a
transposeGrid (MkGrid tta) = MkGrid (distribute tta)

instance Distributive Grid where
  distribute fg = MkGrid (getCompose (distribute (Compose . unGrid <$> fg)))

-- class Distributive r => Representable r where
--   type Rep r
--
--   index :: r a -> (Rep r -> a)
--   tabulate :: (Rep r -> a) -> r a

data IxThree = Zero | One | Two
  deriving stock (Eq, Show)

instance Representable Three

-- >>> index (MkThree 4 7 9) One
-- 7
-- >>> index grid1 (Two, Zero)
-- 6

-- >>> transposeGrid grid1
-- 0 3 6 
-- 1 4 7 
-- 2 5 8 

indices :: Representable r => r (Rep r)
indices = tabulate id

-- >>> indices @Grid
-- (Zero,Zero) (Zero,One) (Zero,Two) 
-- (One,Zero)  (One,One)  (One,Two)  
-- (Two,Zero)  (Two,One)  (Two,Two)  

setter :: (Eq (Rep r), Representable r) => Rep r -> r a -> a -> r a
setter i ra a = tabulate (\ j -> if j == i then a else index ra j)

positions' :: (Applicative r, Representable r, Eq (Rep r)) => r a -> r (a, a -> r a)
positions' grid =
  (\ a i -> (a, setter i grid)) <$> grid <*> indices

positions :: Grid a -> Grid (a, a -> Grid a)
positions (MkGrid (MkThree (MkThree a11 a12 a13) (MkThree a21 a22 a23) (MkThree a31 a32 a33))) =
  MkGrid (MkThree (MkThree p11 p12 p13) (MkThree p21 p22 p23) (MkThree p31 p32 p33))
  where
    p11 = (a11, \ b11 -> MkGrid (MkThree (MkThree b11 a12 a13) (MkThree a21 a22 a23) (MkThree a31 a32 a33)))
    p12 = (a12, \ b12 -> MkGrid (MkThree (MkThree a11 b12 a13) (MkThree a21 a22 a23) (MkThree a31 a32 a33)))
    p13 = (a13, \ b13 -> MkGrid (MkThree (MkThree a11 a12 b13) (MkThree a21 a22 a23) (MkThree a31 a32 a33)))
    p21 = (a21, \ b21 -> MkGrid (MkThree (MkThree a11 a12 a13) (MkThree b21 a22 a23) (MkThree a31 a32 a33)))
    p22 = (a22, \ b22 -> MkGrid (MkThree (MkThree a11 a12 a13) (MkThree a21 b22 a23) (MkThree a31 a32 a33)))
    p23 = (a23, \ b23 -> MkGrid (MkThree (MkThree a11 a12 a13) (MkThree a21 a22 b23) (MkThree a31 a32 a33)))
    p31 = (a31, \ b31 -> MkGrid (MkThree (MkThree a11 a12 a13) (MkThree a21 a22 a23) (MkThree b31 a32 a33)))
    p32 = (a32, \ b32 -> MkGrid (MkThree (MkThree a11 a12 a13) (MkThree a21 a22 a23) (MkThree a31 b32 a33)))
    p33 = (a33, \ b33 -> MkGrid (MkThree (MkThree a11 a12 a13) (MkThree a21 a22 a23) (MkThree a31 a32 b33)))














-- Example grids

grid1 :: Grid Int
grid1 =
  MkGrid
    (MkThree
      (MkThree 0 1 2)
      (MkThree 3 4 5)
      (MkThree 6 7 8)
    )

grid2 :: Grid Int
grid2 =
  MkGrid
    (MkThree
      (MkThree 0 1 0)
      (MkThree 1 0 1)
      (MkThree 0 1 0)
    )

-- Helper code for showing grids

instance Show a => Show (Three a) where
  show = renderThree (plain . show)

instance Show a => Show (Grid a) where
  show = renderGrid (plain . show)

renderThree :: (a -> Block) -> Three a -> Block
renderThree r (MkThree a1 a2 a3) =
  r a1 `sNextTo` r a2 `sNextTo` r a3

renderGrid :: (a -> Block) -> Grid a -> Block
renderGrid r (MkGrid (MkThree (MkThree a11 a12 a13) (MkThree a21 a22 a23) (MkThree a31 a32 a33))) =
  foldr sNextTo ""
    [ r a11 `above` r a21 `above` r a31
    , r a12 `above` r a22 `above` r a32
    , r a13 `above` r a23 `above` r a33
    ]
