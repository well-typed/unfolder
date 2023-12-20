-- | Breadth-first labelling / numbering
--
-- Due to Jones and Gibbons <https://researchspace.auckland.ac.nz/bitstream/handle/2292/3470/TR071.pdf>
-- "Linear-time breadth-first tree algorithms" from 1993.

module BFL where

data Tree =
    Leaf Int
  | Branch Tree Tree
  deriving (Eq, Show)

data Stream a =
    a :< Stream a
  deriving Show

infixr 5 :<

worker :: Tree -> Stream Int -> (Tree, Stream Int)
worker (Leaf _)     (c :< levels) =
  (Leaf c, c + 1 :< levels)
worker (Branch l r) (c :< levels) =
  let
    (ll, levels')  = worker l levels
    (lr, levels'') = worker r levels'
  in
    (Branch ll lr, c :< levels'')

labelTree :: Tree -> Tree
labelTree t =
  let
    (lt, outLevels) = worker t inLevels
    inLevels = 1 :< outLevels
  in
    lt

exampleTree :: Tree
exampleTree =
  Branch (Branch (Branch (Leaf 0)
                         (Branch  (Leaf 0)
                                  (Leaf 0)
                         )
                 )
                 (Branch (Leaf 0)
                         (Branch  (Leaf 0)
                                  (Leaf 0)
                         )
                 )
         )
         (Leaf 0)

resultTree :: Tree
resultTree =
-- input levels:
-- 1     :<  1   :<  2   :<  2    :<  4
  Branch (Branch (Branch (Leaf 2)
                         (Branch  (Leaf 4)
                                  (Leaf 5)
                         )
                 )
                 (Branch (Leaf 3)
                         (Branch  (Leaf 6)
                                  (Leaf 7)
                         )
                 )
         )
         (Leaf 1)

-- >>> labelTree exampleTree == resultTree
-- True

