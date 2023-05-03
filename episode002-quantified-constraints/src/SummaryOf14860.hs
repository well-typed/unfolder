{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module SummaryOf14860 where

import Data.Kind
import Data.Proxy

--
-- Problem statement:
-- We cannot refer to type families in quantified constraints.
--

class C a where

type family F x :: Type -> Type

-- This is not accepted:
-- fun :: (forall a. C a => C (F x a)) => Proxy x -> ()
-- fun = undefined

--
-- Partial first workaround:
-- Introduce type variable for the type family application.
--

fun' :: (F x ~ f, forall a. C a => C (f a)) => Proxy x -> ()
fun' = undefined

--
-- Problem statement (cont.):
-- The first workaround does not work for superclass constraints
--

-- This is not accepted, just as above:
-- class (forall a. C a => C (F x a)) => D x where

-- But the workaround also does not work ("not in scope: f")
-- class (F x ~ f, forall a. C a => C (f a)) => D x where

--
-- Better workaround: define a class alias
--

class    C (F x a) => CF x a
instance C (F x a) => CF x a

-- This works for functions
fun'' :: (forall a. C a => CF x a) => Proxy x -> ()
fun'' = undefined

-- As well as class superclass constraints
class (forall a. C a => CF x a) => D'' x where

--
-- Discussion: Type family of arity 2
--

-- Suppose we have a type family of arity 2:
type family F2 x y :: Type

-- And we're trying to define
--
-- > fun2 :: (forall a. C a => C (F2 x a)) => Proxy x -> ()
-- > fun2 = undefined
--
-- then it's very clear the first work-around does not apply:
--
-- > fun2' :: (F2 x ~ f, forall a. C a => C (f a)) => Proxy x -> ()
-- > fun2' = undefined
--
-- will result inn "F2 should have 2 arguments, but has been given 1".

-- It's less obvious what happens with the second workaround.
-- After all, we /can/ define

class    C (F2 x a) => CF2 x a
instance C (F2 x a) => CF2 x a

fun2'' :: (forall a. C a => CF2 x a) => Proxy x -> ()
fun2'' = undefined

-- However, this constraint is basically unsatisfiable.
-- I think this is reasonable, actually. It's not clear what
--
-- > forall a. C a => C (F2 x a)
--
-- even /means/ if F2 has arity two: how would we prove @F2 x a@ for a
-- universally quantified @a@? The type family would be stuck.