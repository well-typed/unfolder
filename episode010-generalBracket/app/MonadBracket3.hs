module MonadBracket3 where

import Control.Concurrent hiding (modifyMVar)
import Control.Exception
import Control.Monad.Except
import Control.Monad.State

{-------------------------------------------------------------------------------
  'ExitCase'
-------------------------------------------------------------------------------}

data ExitCase a =
    ExitCaseSuccess a
  | ExitCaseException SomeException
  | ExitCaseAbort
  deriving Show

generalBracketIO ::
     IO a
  -> (a -> ExitCase b -> IO c)
  -> (a -> IO b)
  -> IO c
generalBracketIO acquire release use = do
    mask $ \unmask -> do
      a  <- acquire
      mb <- try $ unmask $ use a
      case mb of
        Right b ->
          release a (ExitCaseSuccess b)
        Left e -> do
          void $ release a (ExitCaseException e)
          throwIO e

{-------------------------------------------------------------------------------
  Generalizing 'bracket', step 3
-------------------------------------------------------------------------------}

class Monad m => MonadBracket m where
  generalBracket ::
       m a
    -> (a -> ExitCase b -> m c)
    -> (a -> m b)
    -> m c

instance MonadBracket IO where
  generalBracket = generalBracketIO

instance MonadBracket m => MonadBracket (StateT s m) where
  generalBracket (StateT acquire) release use = StateT $ \s ->
      generalBracket
        (acquire s)
        (\(a, s') -> \case
          ExitCaseSuccess   (b, s'') -> runStateT (release a (ExitCaseSuccess   b)) s''
          ExitCaseException e        -> runStateT (release a (ExitCaseException e)) s'
          ExitCaseAbort              -> runStateT (release a  ExitCaseAbort)        s'
        )
        (\(a, s') -> runStateT (use a) s')

instance MonadBracket m => MonadBracket (ExceptT e m) where
  generalBracket (ExceptT acquire) release use = ExceptT $
      generalBracket
        acquire
        (\case
          Left  e -> const $ return (Left e)
          Right a -> \case
            ExitCaseSuccess (Right b) -> runExceptT $ release a (ExitCaseSuccess b)
            ExitCaseException e       -> runExceptT $ release a (ExitCaseException e)
            ExitCaseAbort             -> runExceptT $ release a  ExitCaseAbort

            -- This last case is a bit tricky:
            ExitCaseSuccess (Left e1) -> do
              mc <- runExceptT $ release a ExitCaseAbort
              case mc of
                Left e2 -> return $ Left e2 -- matches the IO behaviour
                Right _ -> return $ Left e1
        )
        (\case
          Left  e -> return (Left e)
          Right a -> runExceptT $ use a
        )

{-------------------------------------------------------------------------------
  Taking advantage of the more general type
-------------------------------------------------------------------------------}

modifyMVar1 :: MVar a -> (a -> IO a) -> IO ()
modifyMVar1 v _f =
    bracket
      (takeMVar v)
      undefined -- problem: we don't get the updated value!
      undefined

modifyMVar :: MVar a -> (a -> IO a) -> IO ()
modifyMVar v f =
    generalBracket
      (takeMVar v)
      (\old -> \case
        ExitCaseSuccess new -> putMVar v new
        _otherwise          -> putMVar v old
      )
      f
