module MonadBracket1 where

import Control.Exception
import Control.Monad.State

{-------------------------------------------------------------------------------
  Generalizing 'bracket', step 1

  ('generalBracket' is defined in the @exceptions@ library)
-------------------------------------------------------------------------------}

class Monad m => MonadBracket m where
  generalBracket :: m a -> (a -> m ()) -> (a -> m b) -> m b

instance MonadBracket IO where
  generalBracket = bracket

instance MonadBracket m => MonadBracket (StateT s m) where
  generalBracket (StateT acquire) _release _use = StateT $ \s ->
      generalBracket
        (acquire s)
        undefined -- problem! we have no choice but to throw the state out
        undefined
