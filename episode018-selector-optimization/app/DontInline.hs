module DontInline (fst, snd) where

import Prelude hiding (fst, snd)

fst :: (a, b) -> a
{-# NOINLINE fst #-}
fst = \(x, _) -> x

snd :: (a, b) -> b
{-# NOINLINE snd #-}
snd = \(_, y) -> y
