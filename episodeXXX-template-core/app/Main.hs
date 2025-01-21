{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -O0 -dno-typeable-binds #-}
{-# OPTIONS_GHC -ddump-splices -ddump-simpl -ddump-to-file -dumpdir=dumpdir #-}

module Main (main) where

import Prelude hiding (const, id)

import GHC.Base (IP(..))
import GHC.Exts (Symbol)
import GHC.Stack
import GHC.Show (showList__)

import Language.Core.TC.Lib
import Language.Haskell.TH.Lib (coreE)

import Shorthand

{-------------------------------------------------------------------------------
  Overview of Template Core

  "Language.Haskell.TH.Lib"

  > coreE :: (..) => m CoreExp -> m Exp

  "Language.Core.TC.Lib"

  BASIC TERMS

  * Term abstraction  and application

  > coreLamE :: (..) => String -> q CoreType -> (q CoreExp -> q CoreExp) -> q CoreExp
  > coreAppE :: (..) => q CoreExp -> q CoreExp -> q CoreExp

  Note: higher order abstract syntax (HOAS); no need for a "variable" constructor.

  * Type abstraction and application

  > coreTyLamE :: (..) => String -> q CoreKind -> (q CoreType -> q CoreExp) -> q CoreExp
  > coreTyAppE :: (..) => q CoreExp -> q CoreType -> q CoreExp

  Here too HOAS.

  BASIC TYPES

  * Abstraction and application

  > coreForAllT :: (..) => String -> q CoreKind -> (q CoreType -> q CoreType) -> q CoreType
  > coreAppT    :: (..) => q CoreType -> q CoreType -> q CoreType

  * Function space

  > coreArrowT           :: (..) => q CoreType
  > coreArrowT_saturated :: (..) => q CoreType -> q CoreType -> q CoreType

  CONSTANTS

  > coreVarT :: (..) => Name -> q CoreType
  > coreVarE :: (..) => Name -> q CoreExp

  Regular TH names refer to types (doubles quotes) and terms (single).

  COERCIONS

  Apply a coercion:

  > coreCastE  :: (..) => q CoreExp -> q CoreCo -> q CoreExp

  Constructing coercions:

  > coreCoRefl :: (..) => q CoreType -> q CoreCo
  > coreCoApp  :: (..) => q CoreCo -> q CoreCo -> q CoreCo
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Example: basics

  "import Language.Haskell.TH.Lib" is extended with

  > coreE :: Quote m => m CoreExp -> m Exp

  Note: variable names are just hints. We could call "y" also "x" and it would
  still work just fine.
-------------------------------------------------------------------------------}

const :: forall a b. a -> b -> a
const = $(coreE $
      abT "a" coreTypeT $ \a ->
      abT "b" coreTypeT $ \b ->
      abE "x" a         $ \x ->
      abE "y" b         $ \_ ->
        x
    )

comma :: Char
comma = ','

example_basics :: Bool
example_basics = $(coreE $
         coreVarE 'const
      .$ coreVarT ''Bool
      .$ coreVarT ''Char
      .$ coreVarE 'True
      .$ coreVarE 'comma
    )

{-------------------------------------------------------------------------------
  Example: higher order functions
-------------------------------------------------------------------------------}

apply :: (a -> b) -> a -> b
apply = $(coreE $
      abT "a" coreTypeT $ \a ->
      abT "b" coreTypeT $ \b ->
      abE "f" (a ~> b)  $ \f ->
      abE "x" a         $ \x ->
        f .$ x
    )

example_higherOrder :: Bool
example_higherOrder = apply not True

{-------------------------------------------------------------------------------
  Example: arguments that want dictionaries
-------------------------------------------------------------------------------}

withShow :: forall a. Show a => (Show a => a -> String) -> a -> String
withShow = $(coreE $
      abT "a"    coreTypeT              $ \a ->
      abE "dict" (coreVarT ''Show .$ a) $ \dict ->
      abE "f"    (    (coreVarT ''Show .$ a)
                   ~> a
                   ~> coreVarT ''String
                 )                      $ \f ->
      abE "x" a $ \x ->
        f .$ dict .$ x
    )

example_argWithDict :: String
example_argWithDict = withShow bracketed True

{-------------------------------------------------------------------------------
  Example: accepting dictionaries
-------------------------------------------------------------------------------}

justShow :: Show a => a -> String
justShow = $(coreE $
      abT "a"    coreTypeT              $ \a    ->
      abE "dict" (coreVarT ''Show .$ a) $ \dict ->
      abE "x"    a                      $ \x    ->
           coreVarE 'show
        .$ a
        .$ dict
        .$ x
    )

example_useDict :: String
example_useDict = justShow True

{-------------------------------------------------------------------------------
  Example: constructing dictionaries
-------------------------------------------------------------------------------}

data T = MkT

bracketed :: forall a. Show a => a -> String
bracketed x = "(" ++ show x ++ ")"

showsPrecT :: Int -> T -> ShowS
showsPrecT = undefined

showT :: T -> String
showT _ = "hey"

showListT :: [T] -> ShowS
showListT = undefined

example_constructDict :: String
example_constructDict = $(coreE $
         coreVarE 'bracketed
      .$ coreVarT ''T
      .$ (    coreDictConE ''Show
           .$ coreVarT ''T
           .$ coreVarE 'showsPrecT
           .$ coreVarE 'showT
           .$ coreVarE 'showListT
         )
      .$ coreVarE 'MkT
    )

{--------W-----------------------------------------------------------------------
  Example: implicit arguments

  > class IP (x :: Symbol) a | x -> a where
  >   ip :: a
-------------------------------------------------------------------------------}

showInt :: (?debug :: Bool) => Int -> String
showInt x
  | ?debug    = show x ++ " (" ++ if even x then "even" else "odd" ++ ")"
  | otherwise = show x

five :: Int
five = 5

true :: Bool
true = True

type SymbolDebug :: Symbol
type SymbolDebug = "debug"

example_implicitArgs :: String
example_implicitArgs = $(coreE $
        coreVarE 'showInt
      .$ (    coreDictConE ''IP
           .$ coreVarT ''SymbolDebug
           .$ coreVarT ''Bool
           .$ coreVarE 'true
         )
      .$ coreVarE 'five
    )

{-------------------------------------------------------------------------------
  Example: coercions
-------------------------------------------------------------------------------}

newtype List a = WrapList [a]

unwrapList :: forall a. List a -> [a]
unwrapList = $(coreE $
      abT "a"  coreTypeT              $ \a  ->
      abE "xs" (coreVarT ''List .$ a) $ \xs ->
           xs
        .$ (coreCoNewtype ''List [] .$ coreCoRefl a)
    )

example_coercions :: [Int]
example_coercions = unwrapList $ WrapList [1, 2, 3]

{-------------------------------------------------------------------------------
  First application: when GHC's surface to core translation gets in the way.
-------------------------------------------------------------------------------}

type ModifyCallStack = forall r. (HasCallStack => r) -> (HasCallStack => r)

mapCallStackWithoutCore :: (CallStack -> CallStack) -> ModifyCallStack
mapCallStackWithoutCore f k = let ?callStack = f ?callStack in k

type SymbolCallStack :: Symbol
type SymbolCallStack = "callStack"

mapCallStackWithCore :: (CallStack -> CallStack) -> ModifyCallStack
mapCallStackWithCore f = $(coreE $
      abT "r"  coreTypeT                      $ \r ->
      abE "k"  (coreVarT ''HasCallStack ~> r) $ \k ->
      abE "ip" (coreVarT ''HasCallStack)      $ \dict ->
        k .$ (    coreDictConE ''IP
               .$ coreVarT ''SymbolCallStack
               .$ coreVarT ''CallStack
               .$ (    coreVarE 'f
                    .$ (    coreVarE 'ip
                         .$ coreVarT ''SymbolCallStack
                         .$ coreVarT ''CallStack
                         .$ dict
                       )
                  )
             )
    )

insertHi :: CallStack -> CallStack
insertHi = fromCallSiteList . aux . getCallStack
  where
    aux []           = error "insert: empty callstack"
    aux ((_,loc):cs) = ("hi", loc) : cs

f1 :: HasCallStack => String
f1 = f2

f2 :: HasCallStack => String
f2 = mapCallStackWithoutCore insertHi f3

f3 :: HasCallStack => String
f3 = prettyCallStack callStack

g1 :: HasCallStack => String
g1 = g2

g2 :: HasCallStack => String
g2 = mapCallStackWithCore insertHi g3

g3 :: HasCallStack => String
g3 = prettyCallStack callStack

application_callStackWithoutCore :: String
application_callStackWithoutCore = f1

application_callStackWithCore :: String
application_callStackWithCore = g1

{-------------------------------------------------------------------------------
  Second application: constructing dictionaries

  We show a simple example here, that might be helpful for example during
  debugging. But many other examples are possible: reflection, singletons,
  checked exceptions, etc.
-------------------------------------------------------------------------------}

-- | Some datatype that does not have a 'Show' instance
data WithoutShow = MkWithoutShow

-- | Some function that needs a 'Show' instance
wantShow :: forall a. Show a => a -> String
wantShow x = show (x, x)

trivialShowsPrec :: forall a. Int -> a -> ShowS
trivialShowsPrec _ x s = trivialShow x ++ s

trivialShow :: forall a. a -> String
trivialShow _ = "undefined"

trivialShowList :: forall a. [a] -> ShowS
trivialShowList ls s = showList__ (trivialShowsPrec 0) ls s

withTrivialShow :: forall a r. (Show a => r) -> r
withTrivialShow k = $(coreE $
         coreVarE 'k
      .$ (    coreDictConE ''Show
           .$ coreVarT ''a
           .$ (coreVarE 'trivialShowsPrec .$ coreVarT ''a)
           .$ (coreVarE 'trivialShow      .$ coreVarT ''a)
           .$ (coreVarE 'trivialShowList  .$ coreVarT ''a)
         )
    )

application_withTrivialShow :: String
application_withTrivialShow =
    withTrivialShow @WithoutShow $ wantShow MkWithoutShow

{-------------------------------------------------------------------------------
  Collect all examples
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    putStrLn "# Examples"

    example "basics       " $ show example_basics
    example "higherOrder  " $ show example_higherOrder
    example "argWithDict  " $ show example_argWithDict
    example "useDict      " $ show example_useDict
    example "constructDict" $ show example_constructDict
    example "implicitArgs " $ show example_implicitArgs
    example "coercions    " $ show example_coercions

    putStrLn "# Applications"

    example "callStackWithoutCore" application_callStackWithoutCore
    example "callStackWithCore"    application_callStackWithCore
    example "withTrivialShow"      application_withTrivialShow

  where
    example :: String -> String -> IO ()
    example label output = putStrLn $ label ++ ": " ++ output
