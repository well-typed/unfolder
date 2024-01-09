module Main where

import Prelude hiding (break, last, fst, snd)
import DoInline

-- @break@ from the Prelude doesn't seem to allow constant-space consumption.
-- I am not actually completely sure why.

break :: [Int] -> ([Int], [Int])
break []       = ([], [])
break (x : xs) = if x == 0 then
                   ([], xs)
                 else
                   let result = break xs
                   in (x : fst result, snd result)

last :: a -> [a] -> a
last x []     = x
last _ (x:xs) = last x xs

main :: IO ()
main = do
    let (before, after) = break $ [1, 3 .. 1_000_000] ++ 0 : [2, 4 .. 1_000_000]
    print $ last 0 before == last 0 after
