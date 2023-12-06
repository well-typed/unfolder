-- Obtaining from a "classic" monad via universal instances (not recommended).
{-# LANGUAGE UndecidableInstances #-}
module ErrorUniversalInstances where

import Control.Monad

data Error a =
    Ok a
  | Fail String

class ClassicMonad m where
  return' :: a -> m a
  bind' :: m a -> (a -> m b) -> m b

instance ClassicMonad Error where
  return' = Ok
  bind' (Ok a) k = k a
  bind' (Fail msg) _ = Fail msg

instance ClassicMonad m => Functor m where
  fmap = liftM

instance ClassicMonad m => Applicative m where
  pure = return'
  (<*>) = ap

instance ClassicMonad m => Monad m where
  (>>=) = bind'

-- The problem is that GHC will now *always* match instances for
-- these three classes to these instances, making any other instance
-- of these classes at best overlapping, but possibly even fundamentally
-- conflicting.
