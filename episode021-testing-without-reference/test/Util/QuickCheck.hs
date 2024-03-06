module Util.QuickCheck (
    dropSome
  , shrinkElems
  , dropEitherEnd
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Test.QuickCheck

-- | Drop some of the list elements, but don't try to shrink them
dropSome :: [a] -> [[a]]
dropSome = shrinkList (const [])

-- | Shrink one of the list elements, without dropping elements from the list
shrinkElems :: (a -> [a]) -> [a] -> [[a]]
shrinkElems _ []     = []
shrinkElems f (x:xs) = concat [
      -- Shrink the head of the list
      map (: xs) $ f x

      -- Shrink the tail of the list
    , map (x :) $ shrinkElems f xs
    ]

-- | Drop elements from either end of the list
dropEitherEnd :: NonEmpty a -> [NonEmpty a]
dropEitherEnd xs = concat [
      mapMaybe NE.nonEmpty . init . NE.toList $ NE.inits xs -- from the start
    , mapMaybe NE.nonEmpty . tail . NE.toList $ NE.tails xs -- from the end
    ]

