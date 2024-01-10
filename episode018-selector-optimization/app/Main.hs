{-
  Unfolder Episode 18: The selector (thunk) optimization

  Main references:

  - "The Design and Implementation of Programming Languages", John Hughes (1984)
  - "Fixing some space leaks with a garbage collector", Philip Wadler (1987)

  Some other relevant posts:

  - "A Concurrent Garbage Collector for the Glasgow Haskell Compiler", Ben Gamari
    <https://well-typed.com/blog/aux/files/nonmoving-gc/design.pdf>
    Specifically section 2.5.7, "Selector optimization"

  - "Three runtime optimizations done by GHC's GC", Ömer Sinan Ağacan
    <https://osa1.net/posts/2018-03-16-gc-optimizations.html>
    Specifically section 3, "Selector thunk evaluation"

  - "GHC Commentary: The Layout of Heap Objects", section "Selector thunks"
    <https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects#selector-thunks>
-}

module Main (main) where

import System.Environment

{-------------------------------------------------------------------------------
  Version 1 of @break@

  This version obviously leaks: we only return a pair after the recursion
  returns, so this is not lazy enough.
-------------------------------------------------------------------------------}

break1 :: [Int] -> ([Int], [Int])
break1 []     = ([], [])
break1 (x:xs) = if x == 0 then
                  ([], xs)
                else
                  case break1 xs of
                    (before, after) -> (x:before, after)

main1 :: Bool
main1 =
    case break1 [-1_000_000 .. 1_000_000] of
      (before, after) -> last before == last after

{-------------------------------------------------------------------------------
  Version 2 of @break@

  This version works, but is a bit magical: /why/ does it work? What does that

  > let (before, after) = ..

  even /mean/?
-------------------------------------------------------------------------------}

break2 :: [Int] -> ([Int], [Int])
break2 []     = ([], [])
break2 (x:xs) = if x == 0 then
                  ([], xs)
                else
                  let (before, after) = break2 xs
                  in (x:before, after)

main2 :: Bool
main2 =
    let (before, after) = break2 [-1_000_000 .. 1_000_000]
    in last before == last after

{-------------------------------------------------------------------------------
  Version 3 of @break@

  In this version we expand the "magic" from version 2. This version also works.
-------------------------------------------------------------------------------}

break3 :: [Int] -> ([Int], [Int])
break3 []     = ([], [])
break3 (x:xs) = if x == 0 then
                  ([], xs)
                else
                  let broken = break3 xs
                  in (x:fst broken, snd broken)

main3 :: Bool
main3 =
    let broken = break3 [-1_000_000 .. 1_000_000]
    in last (fst broken) == last (snd broken)

{-------------------------------------------------------------------------------
  Version 4 of @break@

  In this version we can see that something magical is happening. When we
  abstract out those calls to @fst@ and @snd@, we get a memory leak again!
-------------------------------------------------------------------------------}

fst' :: (a, b) -> a
{-# NOINLINE fst' #-}
fst' = fst

snd' :: (a, b) -> b
{-# NOINLINE snd' #-}
snd' = snd

break4 :: [Int] -> ([Int], [Int])
break4 []     = ([], [])
break4 (x:xs) = if x == 0 then
                  ([], xs)
                else
                  let broken = break4 xs
                  in (x:fst' broken, snd' broken)

main4 :: Bool
main4 =
    let broken = break4 [-1_000_000 .. 1_000_000]
    in last (fst' broken) == last (snd' broken)

{-------------------------------------------------------------------------------
  (Contrived) example that illustrates the problem is not restricted to lists
-------------------------------------------------------------------------------}

-- | Swap the argument @n@ times
swapN :: Int -> (a, a) -> (a, a)
swapN 0 (x, y) = (x, y)
swapN n (x, y) = let result = swapN (n - 1) (x, y)
                 in (snd result, fst result)

swapN' :: Int -> (a, a) -> (a, a)
swapN' 0 (x, y) = (x, y)
swapN' n (x, y) = let result = swapN' (n - 1) (x, y)
                  in (snd' result, fst' result)

main5 :: (Bool, Bool)
main5 = swapN 1_000_000 (True, False)

main6 :: (Bool, Bool)
main6 = swapN' 1_000_000 (True, False)

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    experiment <- getArgs
    case experiment of
      ["main1"]  -> print main1
      ["main2"]  -> print main2
      ["main3"]  -> print main3
      ["main4"]  -> print main4
      ["main5"]  -> print main5
      ["main6"]  -> print main6
      _otherwise -> error "Invalid experiment"
