{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module VarArgs where

import Data.Kind
import Prelude hiding (sum)
import Data.List (intercalate)

--
-- Example 1. Summing up integers.
--

class Sum a where
  sum' :: Int -> a

instance Sum Int where
  sum' acc = acc

instance (b ~ Int, Sum a) => Sum (b -> a) where
  sum' !acc i = sum' (acc + i)

sum :: Sum a => a
sum = sum' 0

-- >>> sum 1 2 3 4 5 :: Int
-- 15

--
-- Example 2a. Collecting integers in a list.
--

class Collect a where
  collect' :: [Int] -> a

instance b ~ Int => Collect [b] where
  collect' xs = reverse xs

instance (b ~ Int, Collect a) => Collect (b -> a) where
  collect' xs x = collect' (x : xs)

collect :: Collect a => a
collect = collect' []

-- >>> collect 1 2 3 :: [Int]
-- [1,2,3]
-- >>> length (collect 1 2 3)
-- 3
-- >>> map (+1) (collect 1 2 3 4 5)
-- [2,3,4,5,6]

--
-- Example 2b. Collecting other elements in a list.
-- (Not shown in episode.)
--

class GCollect b a | a -> b where
  gcollect' :: [b] -> a

instance GCollect b [b] where
  gcollect' xs = reverse xs

instance GCollect b a => GCollect b (b -> a) where
  gcollect' xs x = gcollect' (x : xs)

gcollect :: GCollect b a => a
gcollect = gcollect' []

-- >>> gcollect 'a' 'b' 'c' :: String
-- "abc"
-- >>> gcollect 1 2 3 :: [Int]
-- [1,2,3]
-- >>> gcollect 1 2 3 :: [Double]
-- [1.0,2.0,3.0]
-- >>> length (gcollect False True)
-- 2

--
-- Example 2c. Instead of reversing, use a continuation /
-- difference list. (Not shown in episode.)
--

class DCollect b a | a -> b where
  dcollect' :: ([b] -> [b]) -> a

instance DCollect b [b] where
  dcollect' f = f []

instance DCollect b a => DCollect b (b -> a) where
  dcollect' f x = dcollect' (f . (x :))

dcollect :: DCollect b a => a
dcollect = dcollect' id

-- >>> dcollect 'a' 'b' 'c' :: String
-- "abc"
-- >>> dcollect 1 2 3 :: [Int]
-- [1,2,3]
-- >>> dcollect 1 2 3 :: [Double]
-- [1.0,2.0,3.0]

--
-- Example 2d. Instead of a functional dependency, use
-- a type family / associated type. (Not shown in episode.)
--

class TCollect a where
  type TElem a :: Type
  tcollect' :: ([TElem a] -> [TElem a]) -> a

instance TCollect [b] where
  type TElem [b] = b
  tcollect' f = f []

instance (b ~ TElem a, TCollect a) => TCollect (b -> a) where
  type TElem (b -> a) = TElem a
  tcollect' f x = tcollect' (f . (x :))

tcollect :: TCollect a => a
tcollect = tcollect' id

-- >>> tcollect 'a' 'b' 'c' :: String
-- "abc"
-- >>> tcollect 1 2 3 :: [Int]
-- [1,2,3]
-- >>> tcollect 1 2 3 :: [Double]
-- [1.0,2.0,3.0]

--
-- Example 3. Variadic fold.
--

class Fold b r a where
  fold :: (r -> b -> r) -> r -> a

instance Fold b r r where
  fold _upd acc = acc

instance (b ~ c, Fold c r a) => Fold c r (b -> a) where
  fold upd !acc x = fold upd (upd acc x)

sumAsFold :: Fold Int Int a => a
sumAsFold = fold @Int @Int (+) 0

sumAsFold' :: Fold Double Double a => a
sumAsFold' = fold @Double @Double (+) 0

-- The following require ambiguous types and potentially
-- additional type arguments on use:
--
sumAsFold'' :: forall b a. Num b => Fold b b a => a
sumAsFold'' = fold @b @b (+) 0

reverseAsFold :: forall b a. Fold b [b] a => a
reverseAsFold = fold @b @[b] (flip (:)) []

-- >>> sumAsFold 1 2 3 :: Int
-- 6
-- >>> take (sumAsFold 1 2 3) [1..100]
-- [1,2,3,4,5,6]
-- >>> sumAsFold' 1 2.5 3.7 :: Double
-- 7.2
-- >>> sumAsFold'' @Int 1 2 3 :: Int
-- 6
-- >>> reverseAsFold "foo" "bar" "baz" :: [String]
-- ["baz","bar","foo"]
-- >>> reverseAsFold @Int 1 2 3 4 :: [Int]
-- [4,3,2,1]

--
-- Example 4. Showing arguments of different types.
-- (This is similar to Text.Printf ...)
--

class VarShow a where
  varShow' :: [String] -> a

instance VarShow String where
  varShow' xs = intercalate "," (reverse xs)

instance (Show b, VarShow a) => VarShow (b -> a) where
  varShow' xs x = varShow' (show x : xs)

varShow :: VarShow a => a
varShow = varShow' []

-- >>> varShow False 'x' 2 :: String
-- "False,'x',2"

--
-- Example 5a. Collecting arguments of different types
-- in a heterogeneous list (reversed).
--

data HList xs where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x : xs)

deriving instance Show (HList '[])
deriving instance (Show x, Show (HList xs)) => Show (HList (x : xs))

class HCollect xs a | a -> xs where
  hcollect' :: HList xs -> a

instance HCollect xs (HList xs) where
  hcollect' xs = xs

instance HCollect (x : xs) a => HCollect xs (x -> a) where
  hcollect' xs x = hcollect' (HCons x xs)

hcollect :: HCollect '[] a => a
hcollect = hcollect' HNil

isHList :: HList xs -> HList xs
isHList = id

-- >>> isHList (hcollect False 'x' 2)
-- HCons 2 (HCons 'x' (HCons False HNil))

--
-- Example 5b. Collecting elements in the correct
-- order. (Another option is to define reverse on HList
-- and use the same approach as in Example 2a.)
-- (Not shown in episode.)
--

class HDCollect xs ys a | a -> xs ys where
  hdcollect' :: (HList xs -> HList ys) -> a

instance HDCollect '[] ys (HList ys) where
  hdcollect' f = f HNil

instance HDCollect xs ys a => HDCollect (x : xs) ys (x -> a) where
  hdcollect' f x = hdcollect' (f . HCons x)

hdcollect :: HDCollect xs xs a => a
hdcollect = hdcollect' id

-- >>> isHList (hdcollect False 'x' 2)
-- HCons False (HCons 'x' (HCons 2 HNil))

--
-- Example 5c. Using type families instead.
-- (Not shown in episode.)
--

class HTCollect a where
  type HTSrc a :: [Type]
  type HTTgt a :: [Type]
  htcollect' :: (HList (HTSrc a) -> HList (HTTgt a)) -> a

instance HTCollect (HList ys) where
  type HTSrc (HList ys) = '[]
  type HTTgt (HList ys) = ys
  htcollect' f = f HNil

instance HTCollect a => HTCollect (x -> a) where
  type HTSrc (x -> a) = x : HTSrc a
  type HTTgt (x -> a) = HTTgt a
  htcollect' f x = htcollect' (f . HCons x)

htcollect :: (HTSrc a ~ HTTgt a, HTCollect a) => a
htcollect = htcollect' id

-- >>> isHList (htcollect False 'x' 2)
-- HCons False (HCons 'x' (HCons 2 HNil))

