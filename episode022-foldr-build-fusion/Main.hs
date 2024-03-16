{-# OPTIONS_GHC -ddump-rule-firings #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
module Main where

import Criterion.Main
import qualified List as L
import qualified ListFusion as F
import Prelude as P

prelude :: Int -> Int
prelude n = P.sum (P.map (\ x -> x * x) [1 .. n])

own :: Int -> Int
own n     = L.sum (L.map (\ x -> x * x) (L.enumFromTo 1 n))

fusion :: Int -> Int
fusion n  = F.sum (F.map (\ x -> x * x) (F.enumFromTo 1 n))

-- This version should produce identical to prelude
fusion' :: Int -> Int
fusion' n  = F.sum (F.map (\ x -> x * x) (F.enumFromTo' 1 n))

-- This version should produce code identical to fusion
-- (and should correspond to monolithic4 from module Monolithic)
monolithic :: Int -> Int
monolithic n =
  let
    go i !acc =
      if i == n
       then acc + i * i
       else go (i + 1) (acc + i * i)
  in
    if 1 > n then 0 else go 1 0

-- Look for -ddump-simpl output such as:
--
-- prelude = fusion'
-- monolithic = fusion
--
-- to witness the equivalent code generation.

main :: IO ()
main =
  defaultMain
    [ bench "prelude"    (nf prelude    10000)
    , bench "own"        (nf own        10000)
    , bench "fusion"     (nf fusion     10000)
    , bench "fusion'"    (nf fusion'    10000)
    , bench "monolithic" (nf monolithic 10000)
    ]
