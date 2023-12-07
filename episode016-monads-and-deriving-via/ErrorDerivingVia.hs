-- Obtaining from a "classic" monad via deriving-via (final solution).
{-# LANGUAGE DerivingVia #-}
module ErrorDerivingVia where

import Control.Monad
import Data.Coerce

data Error a =
    Ok a
  | Fail String
  deriving (Functor, Applicative, Monad) via FromClassicMonad Error

class ClassicMonad m where
  return' :: a -> m a
  bind' :: m a -> (a -> m b) -> m b

instance ClassicMonad Error where
  return' = Ok
  bind' (Ok a) k = k a
  bind' (Fail msg) _ = Fail msg

newtype FromClassicMonad m a = FromClassicMonad (m a)

instance ClassicMonad m => Functor (FromClassicMonad m) where
  fmap = liftM

instance ClassicMonad m => Applicative (FromClassicMonad m) where
  pure :: forall a . a -> FromClassicMonad m a
  pure = coerce (return' @m @a)
  (<*>) = ap

instance ClassicMonad m => Monad (FromClassicMonad m) where
  (>>=) :: forall a b . FromClassicMonad m a -> (a -> FromClassicMonad m b) -> FromClassicMonad m b
  (>>=) = coerce (bind' @m @a @b)
