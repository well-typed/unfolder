{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Typechecker.Relational where

import Control.Lens
import GHC.Generics

import Kanren.Core
import Kanren.Goal
import Kanren.Match
import Kanren.TH

import Typechecker.Language

{-------------------------------------------------------------------------------
  Some additional instances we need

  Defining these instances here instead of in "Typechecker.Language" to keep
  that module as clean as possible.
-------------------------------------------------------------------------------}

deriving stock instance Generic Constant
deriving stock instance Generic Expr

makeLogicals [''Expr, ''Constant]

makePrisms ''LogicExpr
makePrisms ''LogicConstant

deriving stock instance Show LogicExpr
deriving stock instance Show LogicConstant

{-------------------------------------------------------------------------------
  Environment

  This is a simple example of where disequality constraints are useful.
-------------------------------------------------------------------------------}

data Env =
    ENil
  | ECons Var Type Env
  deriving stock (Generic)

makeLogical ''Env
makePrisms ''LogicEnv
deriving stock instance Show LogicEnv

lookupo :: Term Var -> Term Type -> Term Env -> Goal ()
lookupo x t env = env & (matche
    & on _LogicECons (\(x', t', env') -> disjMany [
          do x === x'
             t === t'
        , do x =/= x'
             lookupo x t env'
        ])
    )

{-------------------------------------------------------------------------------
  Relational type inference

  The implementation of the rules is identical except for the variable lookup.
  Of course, we now need to use "relational pattern matching".
-------------------------------------------------------------------------------}

typeOf :: Term Expr -> Term Type -> Goal ()
typeOf = typeInEnv (Value LogicENil)

typeInEnv :: Term Env -> Term Expr -> Term Type -> Goal ()
typeInEnv env e t = e & (matche
    & on _LogicEVar (\x ->
          lookupo x t env
        )
    & on _LogicEApp (\(f, x) -> do
         a <- fresh
         typeInEnv env f (Value $ LogicTFun a t)
         typeInEnv env x a
       )
    & on _LogicEAbs (\(x, f) -> do
         (a, b) <- fresh
         t === Value (LogicTFun a b)
         typeInEnv (Value $ LogicECons x a env) f b
       )
    & on _LogicEConst (\c ->
         typeOfConstant c t
       )
    )

{-------------------------------------------------------------------------------
  Unfortunately we also need a relational version of 'typeOfConstant'.
  Here too the rules are identical to the funcitonal version.
-------------------------------------------------------------------------------}

typeOfConstant :: Term Constant -> Term Type -> Goal ()
typeOfConstant c t = c & (matche
    & on _LogicCInt (\_ ->
          t === Value LogicTInt
        )
    & on _LogicCBool (\_ ->
          t === Value LogicTBool
        )
    & on _LogicCIsZero (\() ->
          t === Value (LogicTFun
                         (Value LogicTInt)
                         (Value LogicTBool))
        )
    & on _LogicCIf (\() -> do
          a <- fresh
          t === Value (LogicTFun
                         (Value LogicTBool)
                         (Value (LogicTFun
                                   a
                                   (Value (LogicTFun
                                             a
                                             a)))))
        )
    )

{-------------------------------------------------------------------------------
  Examples

  These are the same examples as before, modulo an 'inject'' call
-------------------------------------------------------------------------------}

-- > 1
example1 :: [Term Type]
example1 = run $ \t -> typeOf (inject' $ EConst (CInt 1)) t

-- > \x -> x
example2 :: [Term Type]
example2 = run $ \t -> typeOf (inject' $ EAbs "x" (EVar "x")) t

-- > \x y -> if (isZero x) y
example3 :: [Term Type]
example3 = run . typeOf $ inject' $ EAbs "x" $ EAbs "y" $
     EApp
       (EApp (EConst CIf) (EApp (EConst CIsZero) (EVar "x")))
       (EVar "y")


-- > \x y -> if (isZero x) y True
example4 :: [Term Type]
example4 = run . typeOf $ inject' $ EAbs "x" $ EAbs "y" $
     EApp
       (EApp
          (EApp (EConst CIf) (EApp (EConst CIsZero) (EVar "x")))
          (EVar "y"))
       (EConst (CBool True))

{-------------------------------------------------------------------------------
  But with a relational type checker, we can also go the other way
-------------------------------------------------------------------------------}

example5 :: [Term Expr]
example5 = take 5 $ run $ \e -> typeOf e (inject' $ TBool)

example6 :: [Term Expr]
example6 = take 5 $ run $ \e -> typeOf e (inject' $ TFun TInt TBool)