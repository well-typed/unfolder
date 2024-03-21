module List where

import Prelude hiding (enumFromTo, map, sum)

sum :: [Int] -> Int
sum = go 0
  where
    go !acc []       = acc
    go !acc (x : xs) = go (acc + x) xs

map :: (a -> b) -> [a] -> [b]
map _ []       = []
map f (x : xs) = f x : map f xs

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
