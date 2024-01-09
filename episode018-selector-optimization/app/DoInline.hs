module DoInline (fst, snd) where

import Prelude hiding (fst, snd)
import qualified Prelude

-- Defining
--
-- > fst :: (a, b) -> a
-- > {-# INLINE fst #-}
-- > fst (x, _) = x
--
-- does not work. I am not sure why.

fst :: (a, b) -> a
{-# INLINE fst #-}
fst = Prelude.fst

snd :: (a, b) -> b
{-# INLINE snd #-}
snd = Prelude.snd