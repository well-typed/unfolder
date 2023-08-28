module MonadBracket2 where

import Control.Exception
import Control.Monad.Except
import Control.Monad.State

{-------------------------------------------------------------------------------
  Improving the type of 'bracket' for IO
-------------------------------------------------------------------------------}

-- | Version of 'bracket' for IO with a better type
--
-- Note that the type now tells us cleanup must be called.
generalBracketIO_bad :: IO a -> (b -> IO c) -> (a -> IO b) -> IO c
generalBracketIO_bad acquire release use = do
    mask $ \unmask -> do
      a  <- acquire
      mb <- try $ unmask $ use a
      case mb of
        Right b ->
          release b
        Left (_e :: SomeException) ->
          undefined -- we don't have a b!

generalBracketIO ::
     IO a
  -> (a -> Either SomeException b -> IO c)
  -> (a -> IO b)
  -> IO c
generalBracketIO acquire release use = do
    mask $ \unmask -> do
      a  <- acquire
      mb <- try $ unmask $ use a
      case mb of
        Right b ->
          release a (Right b)
        Left e -> do
          void $ release a (Left e)
          throwIO e

{-------------------------------------------------------------------------------
  Generalizing 'bracket', step 2
-------------------------------------------------------------------------------}

class Monad m => MonadBracket m where
  generalBracket ::
       m a
    -> (a -> Either SomeException b -> m c)
    -> (a -> m b)
    -> m c

instance MonadBracket IO where
  generalBracket = generalBracketIO

instance MonadBracket m => MonadBracket (StateT s m) where
  generalBracket (StateT acquire) release use = StateT $ \s ->
      generalBracket
        (acquire s)
        (\(a, s') -> \case
          Right (b, s'') -> runStateT (release a (Right b)) s''
          Left  e        -> runStateT (release a (Left  e)) s'
        )
        (\(a, s') -> runStateT (use a) s')

instance MonadBracket m => MonadBracket (ExceptT e m) where
  generalBracket (ExceptT acquire) _release _use = ExceptT $
      generalBracket
        acquire
        undefined -- problem: /two/ exit cases, one of which is not an exception
        undefined

