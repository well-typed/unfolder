module DoInline (fst, snd) where

import Prelude hiding (fst, snd)

-- Written using explicit lambda to ensure inlining

fst :: (a, b) -> a
{-# INLINE fst #-}
fst = \(x, _) -> x

snd :: (a, b) -> b
{-# INLINE snd #-}
snd = \(_, y) -> y
