-- | Shorthand for constructing core expressions
module Shorthand (
    -- * Application, abstraction, variables
    Ap(..)
  , AbE(..)
  , AbT(..)
    -- * Specific constructions
  , (~>)
    -- * Debugging
  , NotThis(..)
  ) where

import Language.Core.TC.Lib
import Language.Haskell.TH.Syntax

{-------------------------------------------------------------------------------
  Application
-------------------------------------------------------------------------------}

class Ap q a b where
  (.$) :: q a -> q b -> q a

instance Applicative q => Ap q CoreExp  CoreExp  where (.$) = coreAppE
instance Applicative q => Ap q CoreExp  CoreType where (.$) = coreTyAppE
instance Applicative q => Ap q CoreExp  CoreCo   where (.$) = coreCastE
instance Applicative q => Ap q CoreType CoreType where (.$) = coreAppT
instance Applicative q => Ap q CoreCo   CoreCo   where (.$) = coreCoApp

{-------------------------------------------------------------------------------
  Abstraction
-------------------------------------------------------------------------------}

class AbE q a where
  abE ::  String -> q CoreType -> (q CoreExp -> q a) -> q a

class AbT q a where
  abT ::  String -> q CoreKind -> (q CoreType -> q a) -> q a

instance Quote q => AbE q CoreExp  where abE = coreLamE

instance Quote q => AbT q CoreExp  where abT = coreTyLamE
instance Quote q => AbT q CoreType where abT = coreForAllT

{-------------------------------------------------------------------------------
  Specific cases
-------------------------------------------------------------------------------}

(~>) :: Applicative q => q CoreType -> q CoreType -> q CoreType
(~>) = coreArrowT_saturated

infixr 4 ~>

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

data NotThis = NotThis

