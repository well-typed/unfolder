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
large :: Word -> Exp
large 0 = lam (\x -> x)
large n = lam (\_ -> large (n - 1))
