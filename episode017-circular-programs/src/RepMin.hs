-- | The classic circular program
--
-- Due to Richard Bird <https://dl.acm.org/doi/10.1007/BF00264249>
-- "Using circular programs to eliminate multiple traversals of data" from 1984.

module RepMin (
     repMin
   , Tree(..)
   , example
   ) where

data Tree = Leaf Int | Branch Tree Tree
  deriving (Show)

worker :: Int -> Tree -> (Int, Tree)
worker m (Leaf x)     = (x, Leaf m)
worker m (Branch l r) = let resultLeft  = worker m l
                            resultRight = worker m r
                            !mb = min (fst resultLeft) (fst resultRight)
                        in (mb, Branch (snd resultLeft) (snd resultRight))

repMin :: Tree -> Tree
repMin t = let (m, t') = worker m t in t'

example :: Tree
example = Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) (Leaf 4))
