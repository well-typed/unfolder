{-
  Unfolder episode 13: Demonstration of open recursion with a focus on OOP

  Although we focus on OOP here, there are other applications of open
  recursion too. Typical examples include memoization and extensible data types
  (type level open recursion); a well-known example of the latter is the
  "Data types a la carte" approach by Wouter Swierstra.

  For an in-depth blog post on this topic, including a discussion of how to
  extent an object's state, see "Object Oriented Programming in Haskell",
  <https://www.well-typed.com/blog/2018/03/oop-in-haskell/>.
 -}

module Main where

import Version0 qualified
import Version1 qualified
import Version2 qualified

main :: IO ()
main = do
    putStrLn $ "Version0:   " ++ show Version0.example
    putStrLn $ "Version1.1: " ++ show Version1.example1
    putStrLn $ "Version1.2: " ++ show Version1.example2
    putStrLn $ "Version2.1: " ++ show Version2.example1
    putStrLn $ "Version2.1: " ++ show Version2.example2
