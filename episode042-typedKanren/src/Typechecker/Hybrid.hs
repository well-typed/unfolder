module Typechecker.Hybrid where

import Data.Map (Map)
import Data.Map qualified as Map

import Kanren.Core
import Kanren.Goal
import Kanren.Instances ()

import Typechecker.Language

{-------------------------------------------------------------------------------
  Type inference
-------------------------------------------------------------------------------}

type Env = Map Var (Term Type)

typeOf :: Expr -> Term Type -> Goal ()
typeOf = typeInEnv mempty

typeInEnv :: Env -> Expr -> Term Type -> Goal ()
typeInEnv env e t =
    case e of
      EVar x ->
        (env Map.! x) === t
      EApp f x -> do
        a <- fresh
        typeInEnv env f (Value $ LogicTFun a t)
        typeInEnv env x a
      EAbs x f -> do
        (a, b) <- fresh
        t === Value (LogicTFun a b)
        typeInEnv (Map.insert x a env) f b
      EConst c ->
        typeOfConstant c t

typeOfConstant :: Constant -> Term Type -> Goal ()
typeOfConstant c t =
    case c of
      CInt  _ ->
        t === Value LogicTInt
      CBool _ ->
        t === Value LogicTBool
      CIsZero ->
        t === Value (LogicTFun
                       (Value LogicTInt)
                       (Value LogicTBool))
      CIf -> do
        a <- fresh
        t === Value (LogicTFun
                       (Value LogicTBool)
                       (Value (LogicTFun
                                 a
                                 (Value (LogicTFun
                                           a
                                           a)))))

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

-- > 1
example1 :: [Term Type]
example1 = run $ \t -> typeOf (EConst (CInt 1)) t

-- > \x -> x
example2 :: [Term Type]
example2 = run $ \t -> typeOf (EAbs "x" (EVar "x")) t

-- > \x y -> if (isZero x) y
example3 :: [Term Type]
example3 = run . typeOf $ EAbs "x" $ EAbs "y" $
     EApp
       (EApp (EConst CIf) (EApp (EConst CIsZero) (EVar "x")))
       (EVar "y")

-- > \x y -> if (isZero x) y True
example4 :: [Term Type]
example4 = run . typeOf $ EAbs "x" $ EAbs "y" $
     EApp
       (EApp
          (EApp (EConst CIf) (EApp (EConst CIsZero) (EVar "x")))
          (EVar "y"))
       (EConst (CBool True))
