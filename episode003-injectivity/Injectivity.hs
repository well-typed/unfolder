{-# LANGUAGE AllowAmbiguousTypes #-} -- enabled for some intermediate examples only
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-} -- only needed for the final example

module Injectivity where

import Data.Kind
import Data.Monoid

-- Definition:
--
-- A function `f` is called *injective* if
--
-- forall x, y,
--
--   f x == f y
--
-- implies
--
--   x == y
--

-- Example of an injective function:
--

double :: Integer -> Integer
double x = x + x

-- For example,
--
-- 8 == double x
--
-- implies
--
-- x == 4
--
-- (No other choices of x possible.)

-- Example of a non-injective function:
--
-- even :: Integer -> Bool
--
-- For
--
-- True == even x
--
-- there are plenty of possible choices for x:
--
-- x == 4
-- x == -4
-- x == 8
-- x == 42
-- ...

-- Paramterised Haskell datatypes are trivially injective: we can
-- read off the argument from the result.
--
-- For example, for
--
--   Maybe Int
--
-- we know the argument was Int.
--
-- A corner case:

data Proxy a = Proxy

-- Even though e.g. Proxy Int and Proxy Bool have the same inhabitants,
-- Haskell / GHC treats them as different types.


-- Propagating information through types:

showEmpty0 :: String
showEmpty0 = show (mempty :: [Bool])

showEmpty1 :: forall a. (Show a, Monoid a) => a -> String
showEmpty1 _ = show (mempty :: a)

showEmpty :: forall a. (Show a, Monoid a) => Maybe a -> String
showEmpty _ = show (mempty :: a)

-- The following works because we know the argument is of type Maybe [Bool],
-- so
--
--   a  must be [Bool]
--
-- This conclusion is possible because Maybe is injective.
--
example1 = showEmpty (Just [False, True])

-- The following requires a type annotation to disambiguate, but then
-- works for the same reasons as example1.
--
example2 = showEmpty (Nothing :: Maybe [Bool])

type family Fam a
type instance Fam [Bool] = Bool

-- The following does normally *not* work, because GHC rejects
-- the type signature as ambiguous.
--
-- We cannot conclude a from Fam a, because Fam does not need to
-- be injective. We can make it work with AllowAmbiguousTypes,
-- but then we cannot *call* it.

showEmpty' :: forall a. (Show a, Monoid a) => Fam a -> String
showEmpty' _ = show (mempty :: a)

-- example1' = showEmpty' (False :: Fam [Bool])
--
-- example1 doesn't work even with an extra type annotation.
-- For GHC, Fam [Bool] ~ Bool, and there could possibly be other
-- a's such that Fam a ~ Bool, so we cannot conclude the instantiation
-- of a in the type signature of showEmpty' from this.


-- Options for mediating the problem.
--
-- Option 1: Explicit type applications

example1' = showEmpty' @[Bool] False

type instance Fam All = Bool

example2' = showEmpty' @All False

-- Option 2: Use Proxy

showEmpty'' :: forall a. (Show a, Monoid a) => Proxy a -> Fam a -> String
showEmpty'' _ _ = show (mempty :: a)

-- Now the type signature is no longer ambiguous, and we would no longer
-- need AllowAmbiguousTypes.

example1'' = showEmpty'' (Proxy @[Bool]) False
example2'' = showEmpty'' (Proxy @All) False


-- Other examples for the use of proxies:
--
-- From Episode 2:

class Encryption e where
  type Key e :: Type
  type Enc e :: Type -> Type

  encrypt :: Proxy e -> Key e -> a -> Enc e a
  -- The Proxy on e is needed because both other occurences of e are as arguments
  -- of type families. For a, we do not need one because it appears bare as a
  -- function argument.

-- From Servant:

type family Server api :: Type
data Application

serve :: ({- ... -}) => Proxy api -> Server api -> Application
serve = undefined

-- The Proxy on api is needed because Server is a type family.


-- Option 3: Use an injective type family.
--
-- Big caveat: only works if the type family is actually injective!
--

type family FamInj a = r | r -> a  -- requires TypeFamilyDependencies
type instance FamInj [Bool] = Bool
-- type instance FamInj All = Bool -- now rejected because it violates injectivity

-- Now works without AllowAmbiguousTypes, and without a Proxy.
showEmptyInj :: forall a. (Show a, Monoid a) => FamInj a -> String
showEmptyInj _ = show (mempty :: a)

example1Inj = showEmptyInj False -- works as well!

-- Option 4: Define an auxiliary (partial) inverse.

type family FamInv r
type instance FamInv Bool = [Bool]

showEmptyInv :: forall a. (Show a, Monoid a, a ~ FamInv (Fam a)) => Fam a -> String
showEmptyInv _ = show (mempty :: a)

example1Inv = showEmptyInv False -- works (even though Fam isn't injective!)

-- Potential advantages of Option 4:
--
-- - does not actually require injectivity of the original type family
-- - FamInv can be defined separately from Fam
-- - you have the inverse available as a type family that can be used in code
-- - it can be more flexible

-- More advanced example where a manual inverse can be used, but type family
-- dependencies (currently) cannot:
--
-- Here is one way to represent perfect binary trees. Perfect binary
-- trees must be complete, so they always have a power of two elements.

data Nat = Zero | Suc Nat

type family Perf n a where
  Perf Zero a = a
  Perf (Suc n) a = (Perf n a, Perf n a)

-- Example tree of depth 2 with 2^2 == 4 elements:

examplePerf :: Perf (Suc (Suc Zero)) Bool
examplePerf = ((False, True), (True, False))

-- One interesting injectivity-like property here is that
-- we can infer n from a and r:
type family PerfInv a r where
  PerfInv a a = Zero
  PerfInv a (p, _) = Suc (PerfInv a p)

-- We require the partial inverse property to hold as a
-- superclass constraint:
class (n ~ PerfInv a (Perf n a)) => PerfToList n a where
  perfToList :: Perf n a -> [a]

instance PerfToList Zero a where
  perfToList x = [x]

instance (Suc n ~ PerfInv a (Perf (Suc n) a), PerfToList n a) => PerfToList (Suc n) a where
  perfToList (xs, ys) = perfToList xs ++ perfToList ys
    -- note that we do not have to specify n on the recursive calls; it's inferred ...

-- This one is not surprising, because examplePerf has a fully
-- annotated type:
--
exampleApp :: [Bool]
exampleApp = perfToList examplePerf

-- But this works without specifying the size of the tree, because it
-- can now be inferred:
--
exampleApp2 :: [Bool]
exampleApp2 = perfToList ((False, True), (True, False))
