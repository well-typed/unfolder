module Bracket where

import Control.Exception hiding (bracket)
import Control.Concurrent hiding (withMVar)

{-------------------------------------------------------------------------------
  Example: re-implement 'withMVar'

  Reminder:

  > takeMVar :: MVar a -> IO a        -- blocks if MVar already taken
  > putMVar  :: MVar a -> a -> IO ()
-------------------------------------------------------------------------------}

withMVar1 :: MVar a -> (a -> IO b) -> IO b
withMVar1 v f = do
    a <- takeMVar v
    b <- f a
    putMVar v a
    return b

withMVar2 :: MVar a -> (a -> IO b) -> IO b
withMVar2 v f = do
    a  <- takeMVar v
    mb <- try $ f a
    putMVar v a
    case mb of
      Right b -> return b
      Left (e :: SomeException) -> throwIO e

withMVar3 :: MVar a -> (a -> IO b) -> IO b
withMVar3 v f = mask $ \unmask ->  do
    a  <- takeMVar v
    mb <- try $ unmask $ f a  -- unmask on the /inside/!
    putMVar v a
    case mb of
      Right b -> return b
      Left (e :: SomeException) -> throwIO e

{-------------------------------------------------------------------------------
  bracket
-------------------------------------------------------------------------------}

-- | Bracket resource allocation/release
--
-- Caution: 'acquire' and 'release' are run with exceptions masked!
-- (For example, @bracket (forkIO ..)@ is a bad idea.)
bracket :: IO a -> (a -> IO ()) -> (a -> IO b) -> IO b
bracket acquire release use = mask $ \unmask -> do
    a  <- acquire
    mb <- try $ unmask $ use a
    release a
    case mb of
      Right b -> return b
      Left (e :: SomeException) -> throwIO e

withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar v f = bracket (takeMVar v) (putMVar v) f
