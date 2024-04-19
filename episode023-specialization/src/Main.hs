module Main where

import Multiply

import Data.List
import GHC.Word
import Numeric.Natural

main :: IO ()
main = do
    let
      xsInt     = [1 .. 9]         :: [Int]
      xsDouble  = reverse [1 .. 9] :: [Double]
      xsInteger = [1, 3 .. 15]     :: [Integer]
      xsNatural = [1]              :: [Natural]
      xsWord64  = [8, 7, 6]        :: [Word64]

      !resultInt     = myProduct xsInt
      !resultDouble  = myProduct xsDouble
      !resultInteger = myProduct xsInteger
      !resultNatural = myProduct xsNatural
      !resultWord64  = myProduct xsWord64

    putStrLn "Done!"

myProduct :: (Ord a, Num a) => [a] -> a
myProduct = foldl' multiply 1
