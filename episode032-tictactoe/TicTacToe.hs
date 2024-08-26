{-# LANGUAGE DerivingStrategies #-}
module TicTacToe where

import Control.Applicative
import Data.Foldable
import Data.Function
import Data.List
import Text.Read
import Rendering

-- GRID and WINS

data Three a = MkThree a a a
  deriving stock (Foldable, Functor, Show)

newtype Grid a = MkGrid (Three (Three a))
  deriving stock (Foldable, Functor, Show)

data Player = X | O
  deriving stock (Eq, Show)

data Item = Empty | Full Player
  deriving stock Show

initialGrid :: Grid Item
initialGrid =
  MkGrid
    (MkThree
      (MkThree Empty Empty Empty)
      (MkThree Empty Empty Empty)
      (MkThree Empty Empty Empty)
    )

winThree :: Three Item -> Maybe Player
winThree (MkThree (Full X) (Full X) (Full X)) = Just X
winThree (MkThree (Full O) (Full O) (Full O)) = Just O
winThree _                                    = Nothing

candidateThrees :: Grid a -> [Three a]
candidateThrees
  (MkGrid
    (MkThree
      (MkThree a11 a12 a13)
      (MkThree a21 a22 a23)
      (MkThree a31 a32 a33)
    )
  ) =
  [ MkThree a11 a12 a13
  , MkThree a21 a22 a23
  , MkThree a31 a32 a33
  , MkThree a11 a21 a31
  , MkThree a12 a22 a32
  , MkThree a13 a23 a33
  , MkThree a11 a22 a33
  , MkThree a13 a22 a31
  ]

isWin :: Grid Item -> Maybe Player
isWin grid = asum (winThree <$> candidateThrees grid)


-- POSITIONS AND MOVES

positions :: Grid a -> Grid (a, a -> Grid a)
positions (MkGrid (MkThree (MkThree a11 a12 a13) (MkThree a21 a22 a23) (MkThree a31 a32 a33))) =
  MkGrid (MkThree (MkThree p11 p12 p13) (MkThree p21 p22 p23) (MkThree p31 p32 p33))
  where
    p11 = (a11, \ b11 -> MkGrid (MkThree (MkThree b11 a12 a13) (MkThree a21 a22 a23) (MkThree a31 a32 a33)))
    p12 = (a12, \ b12 -> MkGrid (MkThree (MkThree a11 b12 a13) (MkThree a21 a22 a23) (MkThree a31 a32 a33)))
    p13 = (a13, \ b13 -> MkGrid (MkThree (MkThree a11 a12 b13) (MkThree a21 a22 a23) (MkThree a31 a32 a33)))
    p21 = (a21, \ b21 -> MkGrid (MkThree (MkThree a11 a12 a13) (MkThree b21 a22 a23) (MkThree a31 a32 a33)))
    p22 = (a22, \ b22 -> MkGrid (MkThree (MkThree a11 a12 a13) (MkThree a21 b22 a23) (MkThree a31 a32 a33)))
    p23 = (a23, \ b23 -> MkGrid (MkThree (MkThree a11 a12 a13) (MkThree a21 a22 b23) (MkThree a31 a32 a33)))
    p31 = (a31, \ b31 -> MkGrid (MkThree (MkThree a11 a12 a13) (MkThree a21 a22 a23) (MkThree b31 a32 a33)))
    p32 = (a32, \ b32 -> MkGrid (MkThree (MkThree a11 a12 a13) (MkThree a21 a22 a23) (MkThree a31 b32 a33)))
    p33 = (a33, \ b33 -> MkGrid (MkThree (MkThree a11 a12 a13) (MkThree a21 a22 a23) (MkThree a31 a32 b33)))

data Next a = AlreadyPlayed Player | Next a
  deriving stock (Foldable, Functor, Show)

moves :: Player -> Grid Item -> Grid (Next (Grid Item))
moves player grid = go <$> positions grid
  where
    go :: (Item, Item -> Grid Item) -> Next (Grid Item)
    go (Empty, set) = Next (set (Full player))
    go (Full player', _) = AlreadyPlayed player'

-- GAME TREE AND EVALUATION

data Tree = Node Player (Grid Item) Score (Either Player (Grid (Next Tree)))
  deriving stock Show

otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X

gameTreeFrom :: Player -> Grid Item -> Tree
gameTreeFrom player grid =
  case isWin grid of
    Just player' -> Node player grid (winScore player') (Left player')
    Nothing ->
      let
        children = (gameTreeFrom (otherPlayer player) <$>) <$> (moves player grid)
        score =
          case choices children of
            [] -> 0
            candidates -> bestForOn player id (getScore <$> candidates)
      in
        Node player grid score (Right children)

choices :: Grid (Next a) -> [a]
choices = concatMap toList . toList

type Score = Int

winScore :: Player -> Score
winScore X = 1
winScore O = -1

bestForOn :: Ord b => Player -> (a -> b) -> [a] -> a
bestForOn X sel = maximumBy (compare `on` sel)
bestForOn O sel = minimumBy (compare `on` sel)

getScore :: Tree -> Score
getScore (Node _ _ score _) = score

fullTree :: Tree
fullTree = gameTreeFrom X initialGrid

applyBestPlay :: Player -> Tree -> Tree
applyBestPlay _ (Node player grid score (Left p)) =
  Node player grid score (Left p)
applyBestPlay aiPlayer (Node player grid score (Right children)) =
  let
    children' = (applyBestPlay aiPlayer <$>) <$> children
  in
    case choices children' of
      candidates@(_ : _) | player == aiPlayer -> bestForOn player getScore candidates
      _ -> Node player grid score (Right children')

xTree :: Tree
xTree = applyBestPlay X fullTree

oTree :: Tree
oTree = applyBestPlay O fullTree

results :: Tree -> [Maybe Player]
results (Node _ _ _ (Left p)) = [Just p]
results (Node _ _ _ (Right children)) =
  case choices children of
    [] -> [Nothing]
    candidates -> concatMap results candidates

-- RENDERING and PLAYING

renderGrid :: Grid Item -> String
renderGrid = renderGrid' renderItem

renderGrid' :: (a -> Block) -> Grid a -> Block
renderGrid' r (MkGrid (MkThree (MkThree a11 a12 a13) (MkThree a21 a22 a23) (MkThree a31 a32 a33))) =
  foldr above ""
    [ r a11 `nextTo` r a12 `nextTo` r a13 `nextTo` space
    , r a21 `nextTo` r a22 `nextTo` r a23 `nextTo` space
    , r a31 `nextTo` r a32 `nextTo` r a33 `nextTo` space
    , space
    ]

renderItem :: Item -> Block
renderItem Empty    = ".\n"
renderItem (Full p) = show p <> "\n"

renderChoices :: [Grid Item] -> IO ()
renderChoices xs = do
  let
    table = zip [1 :: Int ..] xs
    blocks = map (\ (i, x) -> (show i ++ ": ") `nextTo` renderGrid x) table
    msg = foldr nextTo "" (intersperse space blocks)
  putStr msg

chooser :: (a -> String) -> [a] -> IO a
chooser render xs = do
  let
    table = zip [1 :: Int ..] xs
    blocks = map (\ (i, x) -> (show i ++ ": ") `nextTo` render x) table
    msg = foldr nextTo "" (intersperse space blocks)
  putStr msg
  loop
  where
    loop = do
      c <- getChar
      case readMaybe [c] of
        Nothing -> loop
        Just i  ->
          case xs !? (i - 1) of
            Nothing -> loop
            Just x  -> pure x

play :: Tree -> IO ()
play (Node player grid _ sub) = do
  putStrLn "Current grid:"
  putStrLn (renderGrid grid)
  case sub of
    Left player' -> putStrLn (show player' <> " wins.")
    Right children -> do
      case choices children of
        [] -> putStrLn "Draw."
        candidates -> do
          putStrLn $ "Make a move for " <> show player <> ":"
          c <- interactiveStrategy (choices (moves player grid)) candidates
          putStrLn ""
          play c

type Strategy = [Grid Item] -> [Tree] -> IO Tree

interactiveStrategy :: Strategy
interactiveStrategy options nextStates =
  snd <$> chooser (renderGrid . fst) (zip options nextStates)

