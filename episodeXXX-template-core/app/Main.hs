{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -O0 -dno-typeable-binds #-}
{-# OPTIONS_GHC -ddump-splices -ddump-simpl -ddump-to-file -dumpdir=dumpdir #-}

module Main where

import Prelude hiding (const, id)

import Data.Kind
import Data.Proxy
import GHC.Base (IP(..))
import GHC.Exts hiding (List)
import GHC.Stack

import Language.Core.TC.Lib
import Language.Haskell.TH.Lib (coreE)

{-------------------------------------------------------------------------------
  Overview of Template Core

  "Language.Haskell.TH.Lib"

  > coreE :: (..) => m CoreExp -> m Exp

  "Language.Core.TC.Lib"

  BASIC TERMS

  * Type abstraction and application

  > coreTyLamE :: (..) => String -> q CoreKind -> (q CoreType -> q CoreExp) -> q CoreExp
  > coreTyAppE :: (..) => q CoreExp -> q CoreType -> q CoreExp

  Note: higher order abstract syntax (HOAS); no need for a "variable" constructor.

  * Term abstraction  and application

  > coreLamE :: (..) => String -> q CoreType -> (q CoreExp -> q CoreExp) -> q CoreExp
  > coreAppE :: (..) => q CoreExp -> q CoreExp -> q CoreExp

  Here too HOAS.

  BASIC TYPES

  * Abstraction and application

  > coreForAllT :: (..) => String -> q CoreKind -> (q CoreType -> q CoreType) -> q CoreType
  > coreAppT    :: (..) => q CoreType -> q CoreType -> q CoreType

  CONSTANTS

  > coreVarT :: (..) => Name -> q CoreType
  > coreVarE :: (..) => Name -> q CoreExp

  COERCIONS

  Apply a coercion:

  > coreCastE  :: (..) => q CoreExp -> q CoreCo -> q CoreExp

  Constructing coercions:

  > coreCoRefl :: (..) => q CoreType -> q CoreCo
  > coreCoApp  :: (..) => q CoreCo -> q CoreCo -> q CoreCo



-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Introduction

  "import Language.Haskell.TH.Lib" is extended with

  > coreE :: Quote m => m CoreExp -> m Exp
-------------------------------------------------------------------------------}

id :: forall a. a -> a
id = $(coreE $
      coreTyLamE "a" coreTypeT $ \a ->
      coreLamE   "x" a         $ \x ->
        x
    )

-- Note: variable names are just hints. We could call "y" also "x" and it would
-- still work just fine.
const :: forall a b. a -> b -> a
const = $(coreE $
      coreTyLamE "a" coreTypeT $ \a ->
      coreTyLamE "b" coreTypeT $ \b ->
      coreLamE   "x" a         $ \x ->
      coreLamE   "y" b         $ \_ ->
        x
    )

comma :: Char
comma = ','

-- Regular TH names refer to types (doubles quotes) and terms (single).
example1 :: Bool
example1 = $(coreE $
        coreVarE 'const
      `coreTyAppE`
        coreVarT ''Bool
      `coreTyAppE`
        coreVarT ''Char
      `coreAppE`
        coreVarE 'True
      `coreAppE`
        coreVarE 'comma
    )

{-------------------------------------------------------------------------------
  Coercions
-------------------------------------------------------------------------------}

newtype List a = WrapList [a]

unwrapList :: forall a. List a -> [a]
unwrapList = $(coreE $
      coreTyLamE "a"  coreTypeT                      $ \a  ->
      coreLamE   "xs" (coreVarT ''List `coreAppT` a) $ \xs ->
          xs
        `coreCastE`
          (coreCoNewtype ''List [] `coreCoApp` coreCoRefl a)
    )

{-------------------------------------------------------------------------------
  First application: when GHC's surface to core translation gets in the way.

  Ugh, implicit parameters are translated to

  > class IP (x :: Symbol) a | x -> a where
  >   ip :: a

  so this also requires dictionary manipulation.
-------------------------------------------------------------------------------}

mapCallStackAux :: forall cs r.
     (?callStack :: cs)
  => (cs -> cs)
  -> ((?callStack :: cs) => r)
  -> r
mapCallStackAux f k = let ?callStack = f ?callStack in k

mapCallStack :: forall r.
     HasCallStack
  => (CallStack -> CallStack)
  -> (HasCallStack => r)
  -> r
-- mapCallStack f k = let ?callStack = f callStack in k
-- mapCallStack f k = mapCallStackAux f k

-- This is eta-reduced, and results in an unwanted entry in the corestack
-- again. Unfortunately, we cannot give a type to @k@ (no syntax for .. => ..).
-- Just using @HasCallStack -> r@ results in a kind error (Type /= Constraint).
mapCallStack = $(coreE $
    coreTyLamE "r" coreTypeT $ \r ->
    coreLamE   "d" (coreVarT ''HasCallStack) $ \d ->
    coreLamE   "f" (coreArrowT `coreAppT` coreVarT ''CallStack `coreAppT` coreVarT ''CallStack) $ \f ->
--    coreLamE   "k" (coreArrowT `coreAppT` coreVarT ''HasCallStack `coreAppT` r) $ \k ->
        coreVarE 'mapCallStackAux
      `coreTyAppE`
        coreVarT ''CallStack
      `coreTyAppE`
        r
      `coreAppE`
        d
      `coreAppE`
        f
--      `coreAppE`
--        k
    )

insertHi :: CallStack -> CallStack
insertHi = fromCallSiteList . aux . getCallStack
  where
    aux []             = error "insert: empty callstack"
    aux cs@((_,loc):_) = ("hi", loc) : cs

f1 :: String
f1 = f2

f2 :: HasCallStack => String
f2 = mapCallStack insertHi f3

f3 :: HasCallStack => String
f3 = prettyCallStack callStack

{-------------------------------------------------------------------------------
  Third application: constructing dictionaries

  TODO: Unfortunately this newtype coercion does not seem to work.
-------------------------------------------------------------------------------}

class GetList a where
  getList :: [a]
  yo :: a

-- Main.$fGetListInt = Main.C:GetList @Int $cgetList1_r2Ox $cyo_r2Ov
instance GetList Int where
  getList = [1234]
  yo = 5678


{-

unwrapGetList :: forall a. GetList a => [a]
unwrapGetList = $(coreE $
      coreTyLamE "a" coreTypeT                         $ \a ->
      coreLamE   "d" (coreVarT ''GetList `coreAppT` a) $ \d ->
          d
        `coreCastE`
          (coreCoNewtype ''GetList [] `coreCoApp` coreCoRefl a)
    )
-}





main :: IO ()
main = putStrLn f1
