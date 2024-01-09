module DontInline (fst, snd) where

import Prelude hiding (fst, snd)
import qualified Prelude

fst :: (a, b) -> a
{-# NOINLINE fst #-}
fst = Prelude.fst

snd :: (a, b) -> b
{-# NOINLINE snd #-}
snd = Prelude.snd