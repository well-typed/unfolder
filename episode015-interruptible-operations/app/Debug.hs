module Debug (
    -- * Logging
    putLogMsg
  , printLogMsg
  , withTimer
    -- * Thread management
  , forkDebug
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import GHC.Conc (labelThread)
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)

{-------------------------------------------------------------------------------
  Logging

  NOTE: Logging is /not/ an interuptible operation. This is useful, as it means
  that adding logging to a function will not change its interruptibility. It is
  also safe, because individual calls to 'putLogMsg' will not take indefinite
  amounts of time (unless we are printing something infinite).
-------------------------------------------------------------------------------}

logger :: MVar (String -> IO ())
{-# NOINLINE logger #-}
logger = unsafePerformIO $ newMVar putStrLn

putLogMsg :: String -> IO ()
putLogMsg msg = uninterruptibleMask_ $ withMVar logger ($ msg)

printLogMsg :: Show a => a -> IO ()
printLogMsg = putLogMsg . show

withTimer :: IO a -> IO a
withTimer body = bracket (forkIO timer) killThread $ \_ -> do
    -- Delay the body by 10ms so that the timer dot shows first
    threadDelay 10_000
    body
  where
    timer :: IO ()
    timer = forever $ do
        threadDelay 1_000_000
        putLogMsg "."

{-------------------------------------------------------------------------------
  Thread management
-------------------------------------------------------------------------------}

forkDebug :: HasCallStack => String -> IO () -> IO ThreadId
forkDebug label body = mask $ \_ ->
    forkIOWithUnmask $ \unmask -> do
      tid <- myThreadId
      labelThread tid label
      unmask body `finally` threadExit tid
  where
    threadExit :: ThreadId -> IO ()
    threadExit tid = do
        putStrLn $ concat [
            "Thread "
          , show label
          , " terminated ("
          , show tid
          , ", spawned at "
          , prettySrcLoc . snd $ head (getCallStack callStack)
          , ")"
          ]
