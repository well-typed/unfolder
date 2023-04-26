module Unfoldr where

import Debug.Trace

-- Definition of unfoldr (normally in Data.List)

unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr gen s =
  case gen s of
    Nothing -> []
    Just (a, s') -> a : unfoldr gen s'

-- Example 1: enumFromTo

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' lo hi =
  unfoldr
    (\ cur -> if cur > hi then Nothing else Just (cur, cur + 1))
    lo

-- Example 2: Fibonacci numbers

fibs :: [Int]
fibs =
  unfoldr
    (\ (x, y) -> traceShow (x, y) $ Just (x, (y, x + y)))
    (0, 1)

-- Example 3: map and filter
-- filter is not so easy to define using unfoldr

map' :: (a -> b) -> [a] -> [b]
map' f xs =
  unfoldr
    (\ l ->
      case l of
        [] -> Nothing
        y : ys -> Just (f y, ys)
    )
    xs

{-
filter :: (a -> Bool) -> [a] -> [a]
filter p xs =
  unfoldr
    (\ l ->
      case l of
        [] -> Nothing
        y : ys -> if p y then Just (y, ys) else _
    )
    xs
-}

-- Example 4: zip

zip' :: [a] -> [b] -> [(a, b)]
zip' xs ys =
  unfoldr
    (\ s ->
      case s of
        (x : xs, y : ys) -> Just ((x, y), (xs, ys))
        _ -> Nothing
    )
    (xs, ys)

-- Connection to recursion schemes / anamorphisms.
-- Explaining the type of unfoldr.

data List a = Nil | Cons a (List a)
data ListF a r = NilF | ConsF a r -- isomorphic to Maybe (a, r)

unfoldr' :: (s -> ListF a s) -> s -> [a]
unfoldr' = undefined

foldr :: (r -> a -> r) -> r -> [a] -> r
foldr = undefined

foldr' :: (ListF a r -> r) -> [a] -> r
foldr' = undefined
