{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
module ListFusion where

import Prelude hiding (enumFromTo, foldr, map, sum)
import GHC.Exts hiding (build)

-- INLINE   [k]    don't inline before phase k, try to inline from phase k on
-- NOINLINE [k]    don't inline before phase k, same as no pragma from phase k on

{-# INLINE [1] build #-}
build :: (forall r. (a -> r -> r) -> r -> r) -> [a]
build builder = builder (:) []

{-# NOINLINE [1] foldr #-}
foldr :: (a -> r -> r) -> r -> [a] -> r
foldr cons nil = go
  where
    go []       = nil
    go (x : xs) = x `cons` go xs

{-# NOINLINE [1] sum #-}
sum :: [Int] -> Int
sum = go 0
  where
    go !acc []       = acc
    go !acc (x : xs) = go (acc + x) xs

{-# INLINE sumFB #-}
sumFB :: [Int] -> Int
sumFB list =
  foldr (\ x r !acc -> r (acc + x)) (\ !acc -> acc) list 0

{-# NOINLINE [1] map #-}
map :: (a -> b) -> [a] -> [b]
map _ []       = []
map f (x : xs) = f x : map f xs

{-# INLINE mapFB #-}
mapFB :: (a -> b) -> [a] -> [b]
mapFB f list =
  build $ \ cons nil ->
  foldr (\ x r -> f x `cons` r) nil list

{-# NOINLINE [1] enumFromTo #-}
enumFromTo :: Int -> Int -> [Int]
enumFromTo lo hi =
  let
    go i =
      i :
      if i == hi
        then []
        else go (i + 1)
  in
    if lo > hi then [] else go lo

{-# INLINE enumFromToFB #-}
enumFromToFB :: Int -> Int -> [Int]
enumFromToFB lo hi =
  build $ \ cons nil ->
  let
    go i =
      i `cons`
      if i == hi
        then nil
        else go (i + 1)
  in
    if lo > hi then nil else go lo

-- In order to get *exactly* the same Core code GHC produces for our
-- example pipeline, use the following function instead of
-- enumFromTo:

{-# INLINE enumFromTo' #-}
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' (I# lo) (I# hi) =
  eftInt lo hi

{-# NOINLINE [1] eftInt #-}
eftInt :: Int# -> Int# -> [Int]
eftInt lo hi =
  let
    go x =
      I# x :
      if isTrue# (x ==# hi)
        then []
        else go (x +# 1#)
  in
    if isTrue# (lo ># hi) then [] else go lo

{-# INLINE eftIntFB #-}
eftIntFB :: Int# -> Int# -> [Int]
eftIntFB lo hi =
  build $ \ cons nil ->
    let
      go x =
        I# x `cons`
        if isTrue# (x ==# hi)
          then nil
          else go (x +# 1#)
    in
      if isTrue# (lo ># hi) then nil else go lo

{-# RULES

"sum-sumFB"               forall xs. sum xs = sumFB xs
"map-mapFB"               forall f xs. map f xs = mapFB f xs
"enumFromTo-enumFromToFB" forall f xs. enumFromTo f xs = enumFromToFB f xs
"eftInt-eftIntFB"         forall lo hi. eftInt lo hi = eftIntFB lo hi
"foldr-build-fusion"      forall cons nil (builder :: forall r. (a -> r -> r) -> r -> r). foldr cons nil (build builder) = builder cons nil

#-}
