module StrictMVar (
    MVar -- opaque
  , newMVar
  , modifyMVar_
  ) where

import Control.Concurrent qualified as Lazy
import NoThunks.Class

newtype MVar a = Wrap {
      unwrap :: Lazy.MVar a
    }

newMVar :: a -> IO (MVar a)
newMVar = fmap Wrap . Lazy.newMVar

modifyMVar_ :: NoThunks a => MVar a -> (a -> IO a) -> IO ()
modifyMVar_ v f = Lazy.modifyMVar_ (unwrap v) $ \x -> do
    !x' <- f x
    mThunk <- noThunks [] x'
    case mThunk of
      Nothing   -> return x'
      Just info -> fail $ "Thunk: " ++ show info

