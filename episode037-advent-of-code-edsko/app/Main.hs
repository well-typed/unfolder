{-# LANGUAGE PartialTypeSignatures #-}
module Main (main) where

import Data.List
import System.Environment
import Control.Monad
import Data.Maybe (mapMaybe)

{-------------------------------------------------------------------------------
  Part 1
-------------------------------------------------------------------------------}

type Matrix a = [[a]]
type Line   a = [a]

diagonals :: Matrix a -> [Line a]
diagonals orig = concat [
      transpose $ zipWith drop [0 ..] orig
    , transpose $ map reverse $ zipWith take [0 ..] orig
    ]

allDirections :: Matrix a-> [Line a]
allDirections orig = concat [
      orig                           -- horizontals
    , transpose orig                 -- verticals
    , diagonals orig                 -- diagonals (sloping down)
    , diagonals (map reverse orig)   -- diagonals (sloping up)
    ]

count :: Eq a => [a] -> [a] -> Int
count needle haystack = length $ filter (needle `isPrefixOf`) (tails haystack)

part1 :: Matrix Char -> Int
part1 input =
    sum $ map (count "XMAS") allDirs
  where
    forward  = allDirections input
    backward = map reverse forward
    allDirs  = forward ++ backward

{-------------------------------------------------------------------------------
  Part 2
-------------------------------------------------------------------------------}

takeExactly :: Int -> [a] -> Maybe [a]
takeExactly n xs = let taken = take n xs in taken <$ guard (length taken == n)

-- | Compute all (square) submatrices of the specified dimension
--
-- Technically speaking this returns the /transpose/ of all submatrices, but
-- for our purposes here this doesn't matter.
submatrices :: forall a. Int -> Matrix a -> [Matrix a]
submatrices n = \xs ->
    concatMap forStartingLine $ mapMaybe (takeExactly n) (tails xs)
  where
    forStartingLine :: Matrix a -> [Matrix a]
    forStartingLine xs = mapMaybe (takeExactly n) (tails (transpose xs))

-- | Match a matrix against a pattern, intended to be of the same size
match :: Matrix (a -> Bool) -> Matrix a -> Bool
match pattern xs = and $ zipWith ($) (concat pattern) (concat xs)

xMasPatterns :: [Matrix (Char -> Bool)]
xMasPatterns = [
      -- Down, down
      [ [is 'M', ignore, is 'M']
      , [ignore, is 'A', ignore]
      , [is 'S', ignore, is 'S'] ]

      -- Down, up
    , [ [is 'M', ignore, is 'S']
      , [ignore, is 'A', ignore]
      , [is 'M', ignore, is 'S'] ]

      -- Up, down
    , [ [is 'S', ignore, is 'M']
      , [ignore, is 'A', ignore]
      , [is 'S', ignore, is 'M'] ]

      -- Up, up
    , [ [is 'S', ignore, is 'S']
      , [ignore, is 'A', ignore]
      , [is 'M', ignore, is 'M'] ]
    ]
  where
    is :: Char -> Char -> Bool
    is = (==)

    ignore :: Char -> Bool
    ignore = const True

part2 :: Matrix Char -> Int
part2 input = length $ filter subIsMatch (submatrices 3 input)
  where
    subIsMatch :: Matrix Char -> Bool
    subIsMatch sub = any (\pattern -> match pattern sub) xMasPatterns

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- lines <$> readFile inputFile

    putStrLn $ "#XMAS:  " ++ show (part1 input)
    putStrLn $ "#X-MAS: " ++ show (part2 input)
