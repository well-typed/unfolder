module Rendering where

type Block = String

width :: Block -> Int
width block =
  case lines block of
    [] -> 0
    ls -> maximum (length <$> ls)

height :: Block -> Int
height block =
  length block

nextTo :: Block -> Block -> Block
nextTo block1 block2 =
  let
    ls1  = lines block1
    ls2  = lines block2
    h    = max (height block1) (height block2)
    ls1' = map (\ l -> l ++ replicate (width block1 - length l) ' ') ls1 ++ replicate (h - height block1) (replicate (width block1) ' ')
    ls2' = ls2 ++ replicate (h - height block2) ""
  in
    unlines (zipWith (++) ls1' ls2')

sNextTo :: Block -> Block -> Block
sNextTo block1 block2 = block1 `nextTo` space `nextTo` block2

plain :: String -> Block
plain = (++"\n")

above :: Block -> Block -> Block
above block1 block2 = block1 ++ block2

space :: Block
space = " \n"

