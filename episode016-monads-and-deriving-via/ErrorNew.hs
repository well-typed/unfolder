-- Hand-written instances for recent (post-AMP) GHCs.
module ErrorNew where

import Control.Monad

data Error a =
    Ok a
  | Fail String

instance Functor Error where
  fmap = liftM

-- liftM :: Monad m => (a -> b) -> m a -> m b
-- liftM f ma = do
--   a <- ma
--   return (f a)

instance Applicative Error where
  pure = Ok
  (<*>) = ap

-- ap :: Monad m => m (a -> b) -> m a -> m b
-- ap mf ma = do
--   f <- mf
--   a <- ma
--   return (f a)

instance Monad Error where
  Ok a     >>= k = k a
  Fail msg >>= _ = Fail msg
