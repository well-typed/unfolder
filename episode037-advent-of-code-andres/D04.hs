{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Void
import System.Environment
import System.Exit
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

main :: IO ()
main = do
  [arg] <- getArgs
  input <- fetchInput arg
  -- print input
  print (part1 input)
  print (part2 input)

type Input = Map (Int, Int) Char

fetchInput :: FilePath -> IO Input
fetchInput file = do
  txt <- readFile file
  let
    ls :: [[Char]]
    ls = lines txt
  pure (Map.fromList [ ((i, j), c) | (i, l) <- zip [0 ..] ls, (j, c) <- zip [0 ..] l ])

checkInput :: Input -> Int
checkInput input =
  length (filter (checkCandidate input) (candidates input))

checkCandidate :: Input -> [(Int, Int)] -> Bool
checkCandidate input path = lookupCandidate input path == Just "XMAS"

lookupCandidate :: Input -> [(Int, Int)] -> Maybe String
lookupCandidate input path = traverse (\ p -> Map.lookup p input) path

candidates :: Input -> [[(Int, Int)]]
candidates input =
  concatMap candidatesFrom (Map.keys input)

-- (5, 3)  => [ [ (5, 3), (6, 3), (7, 3), (8, 3) ]
--            , [ (5, 3), (4, 3), (3, 3), (2, 3) ]
--            , ...
--            ]

-- >>> candidatesFrom (5, 3)
-- [[(5,3),(4,2),(3,1),(2,0)],[(5,3),(4,3),(3,3),(2,3)],[(5,3),(4,4),(3,5),(2,6)],[(5,3),(5,2),(5,1),(5,0)],[(5,3),(5,4),(5,5),(5,6)],[(5,3),(6,2),(7,1),(8,0)],[(5,3),(6,3),(7,3),(8,3)],[(5,3),(6,4),(7,5),(8,6)]]

candidatesFrom :: (Int, Int) -> [[(Int, Int)]]
candidatesFrom p =
  map (goFrom 4 p) directions

-- >>> directions
-- [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

directions :: [(Int, Int)]
directions = [ (i, j) | i <- [-1 .. 1], j <- [-1 .. 1], not (i == 0 && j == 0) ]

-- >>> goFrom 4 (5, 3) (-1, 0)
-- [(5,3),(4,3),(3,3),(2,3)]

goFrom :: Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
goFrom n (i1, j1) (i2, j2) = take n (iterate (.+. (i2, j2)) (i1, j1))

(.+.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(i1, j1) .+. (i2, j2) = (i1 + i2, j1 + j2)

(.-.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(i1, j1) .-. (i2, j2) = (i1 - i2, j1 - j2)

tripleAround :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
tripleAround p d = [ p .-. d, p, p .+. d ]

-- >>> directionPairs
-- [((-1,-1),(1,-1)),((-1,-1),(-1,1)),((1,1),(1,-1)),((1,1),(-1,1))]

directionPairs :: [((Int, Int), (Int, Int))]
directionPairs = [ (d1, d2) | d1 <- [(-1, -1), (1, 1)], d2 <- [(1, -1), (-1, 1)] ]

-- >>> xsFrom (5, 3)
-- [[(6,4),(5,3),(4,2),(4,4),(5,3),(6,2)],[(6,4),(5,3),(4,2),(6,2),(5,3),(4,4)],[(4,2),(5,3),(6,4),(4,4),(5,3),(6,2)],[(4,2),(5,3),(6,4),(6,2),(5,3),(4,4)]]

xsFrom :: (Int, Int) -> [[(Int, Int)]]
xsFrom p = map (\ (d1, d2) -> (tripleAround p d1 ++ tripleAround p d2)) directionPairs

xs :: Input -> [[(Int, Int)]]
xs input =
  concatMap xsFrom (Map.keys input)

checkInput' :: Input -> Int
checkInput' input =
  length (filter (checkX input) (xs input))

checkX :: Input -> [(Int, Int)] -> Bool
checkX input path = lookupCandidate input path == Just "MASMAS"

part1 :: Input -> Int
part1 = checkInput

part2 :: Input -> Int
part2 = checkInput'
