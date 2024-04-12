module Multiply where

import Add

multiply :: (Ord a, Num a) => a -> a -> a
multiply n m =
    if m <= 0 then
      0
    else
      add n (multiply n (m - 1))

multiplyInt :: Int -> Int -> Int
multiplyInt n m =
    if m <= 0 then
      0
    else
      add n (multiplyInt n (m - 1))
