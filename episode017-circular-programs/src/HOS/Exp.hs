-- | The basic (first-order) expression language
module HOS.Exp (
    -- * Definition
    Name
  , Exp(..)
    -- * Functions
  , maxBV
  ) where

import Control.DeepSeq
import GHC.Generics

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Name = Int

data Exp =
    -- | Variables
    Var Name

    -- | Application
  | App Exp Exp

    -- | Lambda abstraction
    --
    -- Invariant: in @Lam n e@, @n@ must be strictly greater than all bound
    -- variables in @e@.
  | Lam Name Exp

  deriving (Show, Generic, NFData)

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

maxBV :: Exp -> Name
maxBV (Var _)   = 0
maxBV (App f e) = max (maxBV f) (maxBV e)
maxBV (Lam n _) = n
