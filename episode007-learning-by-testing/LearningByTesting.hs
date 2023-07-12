{-# LANGUAGE DerivingVia #-}
module LearningByTesting where

import Control.Applicative
import Control.Monad.Trans.State
import Prelude hiding (filter, partition)

-- | Original 'filter'.
-- Difficult to see whether it keeps elements that
-- pass the test (that is what it does) or that fail
-- the test.
--
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs) =
  if p x
    then x : filter p xs
    else filter p xs

-- | Better 'filter', using 'Maybe'.
-- (This is available as 'mapMaybe' in the base package.)
--
filter' :: (a -> Maybe b) -> [a] -> [b]
filter' _ [] = []
filter' p (x : xs) =
  case p x of
    Just y -> y : filter' p xs
    Nothing -> filter' p xs

-- NOTE:
--
-- Swapping cases above is not possible, because
-- 'y' is not in scope in the 'Nothing' case, and 'x'
-- is not type-correct to be in the result list.

-- | Original 'filter' can be implemented in terms of
-- 'Maybe'-based 'filter'
--
filterOrig :: (a -> Bool) -> [a] -> [a]
filterOrig p =
  filter' $ \ x -> if p x then Just x else Nothing

-- Usage example 1:

checkEmail :: String -> Bool
checkEmail txt = '@' `elem` txt -- kept simple here

-- >>> filter checkEmail ["andres@well-typed.com", "foo", "bar@baz"]
-- ["andres@well-typed.com","bar@baz"]

-- Difficult to establish an actual boundary:
--
-- - Easy to forget
-- - Easy to run redundantly
--

newtype Email = MkEmail String
  deriving Show

checkEmail' :: String -> Maybe Email
checkEmail' txt =
  if '@' `elem` txt
    then Just (MkEmail txt)
    else Nothing

-- >>> filter' checkEmail' ["andres@well-typed.com", "foo", "bar@baz"]
-- [MkEmail "andres@well-typed.com",MkEmail "bar@baz"]

-- Still uses if-then-else, but it is now wrapped up
-- in a single place.
--
-- Cannot be forgotten, cannot easily be run redundantly.
--

-- Usage example 2:

isEvenLength :: [a] -> Bool
isEvenLength xs =
  even (length xs)

-- >>> filter isEvenLength ["aabb", "abcde"]
-- ["aabb"]

-- Focus on what you want to do! If, for example, we
-- want the even length so that we can form pairs of
-- adjacent elements, we can just do that:

isEvenLength' :: [a] -> Maybe [(a, a)]
isEvenLength' [] = Just []
isEvenLength' [_] = Nothing
isEvenLength' (x1 : x2 : xs) =
  ((x1, x2) :) <$> isEvenLength' xs

-- >>> filter' isEvenLength' ["aabb", "abcde"]
-- [[('a','a'),('b','b')]]

-- Easy to recover the original list if needed, but
-- also establishes a clear boundary and provides a
-- witness of having performed the test.

-- Simply renaming the Booleans, as sometimes suggested, is
-- not so great:
--

data KeepOrDrop = Keep | Drop

questionableFilter :: (a -> KeepOrDrop) -> [a] -> [a]
questionableFilter _ [] = []
questionableFilter p (x : xs) =
  case p x of
    Keep -> x : questionableFilter p xs
    Drop -> questionableFilter p xs

-- Still makes it possible to accidentally swap the cases.
-- Does not help with establishing boundaries.

-- Sometimes, we do not just want witnesses for success, but
-- also for failure:

-- | Normal version of 'partition'.
--
-- Again, it's rather unclear which component of the
-- pair is which.
--
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition p (x : xs) =
  let
    (ys, zs) = partition p xs
  in
    if p x
      then (x : ys, zs)
      else (ys, x : zs)

-- | Better version of 'partition'.
--
-- This is available as 'partitionEithers', but actually
-- with flipped result.
partition' :: (a -> Either b c) -> [a] -> ([b], [c])
partition' _ [] = ([], [])
partition' p (x : xs) =
  let
    (ys, zs) = partition' p xs
  in
    case p x of
      Right z -> (ys, z : zs)
      Left  y -> (y : ys, zs)

-- Credits:
-- Conor McBride, between 2005-2010, "learning by testing"
-- Bob Harper, 2011, "Boolean blindness"
-- Alexis King, 2019, "Parse don't validate"
-- [probably many others]

data Token =
    Keyword String
  | Identifier String
  | Literal Int
  deriving Show

-- satisfy :: (Token -> Bool) -> Parser Token

-- | 'lit' based on 'satisfy'.
--
-- If we use satisfy to define a parser that parses
-- any integer literal, we have to use some partial
-- function to extract the actual payload.
--
lit :: Parser Int
lit =
  (\ x -> case x of Literal i -> i; _ -> error "ugh") <$>
  satisfy (\ x -> case x of Literal _ -> True; _ -> False)

-- token :: (Token -> Maybe a) -> Parser a

-- | 'lit' based on 'token'.
--
-- >>> runParser (many lit') [Literal 3, Literal 4]
-- Just [3,4]
--
lit' :: Parser Int
lit' = token $ \ x ->
  case x of
    Literal i -> Just i
    _ -> Nothing

-- megaparsec does this right! It provides both functions,
-- 'satisfy' and 'token', and 'token' is the primitive one.

-- Helper code for parsers.

newtype Parser a = MkParser { unParser :: [Token] -> [(a, [Token])] }
  deriving (Functor, Applicative, Monad, Alternative)
    via StateT [Token] []

runParser :: Parser a -> [Token] -> Maybe a
runParser p toks =
  case filter' (\ (r, toks') -> if null toks' then Just r else Nothing) (unParser p toks) of
    [] -> Nothing
    (x : _) -> Just x

satisfy :: (Token -> Bool) -> Parser Token
satisfy p = MkParser $ \ tok ->
  case tok of
    [] -> []
    (t : ts)
      | p t -> [(t, ts)]
      | otherwise -> []

token :: (Token -> Maybe a) -> Parser a
token p = MkParser $ \ tok ->
  case tok of
    [] -> []
    (t : ts) ->
      case p t of
        Nothing -> []
        Just a -> [(a, ts)]

-- Pattern matching itself is learning by testing!
--

badMap :: (a -> b) -> [a] -> [b]
badMap f xs =
  if null xs
    then []
    else f (head xs) : badMap f (tail xs)

-- What happens if we swap the cases?

goodMap :: (a -> b) -> [a] -> [b]
goodMap f xs =
  case xs of
    [] -> []
    x : xs -> f x : goodMap f xs

-- Pattern matching combines the checking which branch
-- a value belongs to with providing the accessors to
-- the fields as a witness. So we are learning by testing.
