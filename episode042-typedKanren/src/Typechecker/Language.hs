{-# LANGUAGE TemplateHaskell #-}

module Typechecker.Language where

import Data.Text (Text)
import GHC.Generics (Generic)

import Kanren.TH
import Kanren.Instances ()

{-------------------------------------------------------------------------------
  Language definition
-------------------------------------------------------------------------------}

type Var = Text

data Expr =
    EVar Var         -- ^ Variable (@x@)
  | EApp Expr Expr   -- ^ Application (@e1 e2@)
  | EAbs Var Expr    -- ^ Abstraction (@\\x -> e@)
  | EConst Constant  -- ^ Constants
  deriving stock (Show)

data Constant =
    CInt Int    -- ^ Integer constant (@1@, @2@, ..)
  | CBool Bool  -- ^ Boolean constant (@True@, @False@)
  | CIsZero     -- ^ Check for zero (@== 0@)
  | CIf         -- ^ Conditional (@if@)
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data Type =
    TInt
  | TBool
  | TFun Type Type
  deriving stock (Show, Generic)

-- Generate the "logic" equivalent of 'Type'
--
-- > data LogicType =
-- >     LogicTInt
-- >   | LogicTBool
-- >   | LogicTFun (Term Type) (Term Type)
-- >   deriving Generic
makeLogicals [''Type]

deriving stock instance Show LogicType

