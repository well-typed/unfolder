-- | Higher-order syntax
--
-- See "Using Circular Programs for Higher-Order Syntax"
-- by Emil Axelsson and Koen Claessen (ICFP 2013)
-- <https://emilaxelsson.github.io/documents/axelsson2013using.pdf>
module HOS.Circular (
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
    let body = f (Var n)
        !n   = succ (maxBV body)
    in Lam n body

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