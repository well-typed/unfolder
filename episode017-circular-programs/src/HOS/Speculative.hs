-- | Speculative (non-circular) solution
--
-- This solution is @O(2^n)@, and therefore not really useable!
--
-- See "HOS.Circular" for additional notes.
module HOS.Speculative (
    -- * Higher-order interface
    lam
  , app
    -- * Example terms
  , identity
  , large
  ) where

import HOS.Exp

{-------------------------------------------------------------------------------
  Higher-order interface
-------------------------------------------------------------------------------}

lam :: (Exp -> Exp) -> Exp
lam f =
    let ph  = Var 0 -- Placeholder
        n   = maxBV (f ph)
        !n' = succ n
    in Lam n' (f (Var n'))

app :: Exp -> Exp -> Exp
app = App

{-------------------------------------------------------------------------------
  Example terms
-------------------------------------------------------------------------------}

identity :: Exp
identity = lam (\x -> app (app (lam (\y -> y)) (lam (\z -> z))) x)

-- | Construct term with @n@ nested lamdas
--
-- For the purposes of benchmarking, we have to avoid making this term /too/
-- simple. For example, consider
--
-- > large :: Word -> Exp
-- > large 0 = lam (\x -> x)
-- > large n = lam (\_ -> large (n - 1))
--
-- Consider what happens after we inline @lam@:
--
-- > large :: Word -> Exp
-- > large 0 = lam (\x -> x)
-- > large n =
-- >     let f   = \_ -> large (n - 1)
-- >         ph  = Var 0 -- Placeholder
-- >         i   = maxBV (f ph)
-- >         !i' = succ i
-- >     in Lam i' (f (Var i'))
--
-- In this case the ghc optimizer notices that the argument to @f@ is not used
-- and can remove it, so that we only do a /single/ recursive call, and the
-- program changes from @O(n^2) to @O(n)@, same as the circular version. In
-- more complicated examples such as the one below this does not happen.
large :: Word -> Exp
large 0 = lam (\x -> x)
large n = lam (\x -> App x (large (n - 1)))
