module Test4_StopTheWorld (main) where

import Control.Concurrent
import System.Environment
import System.Mem

import Tests

-- | FFI calls vs GC
--
-- Try
--
-- > cabal run stoptheworld -- NativeSleepy 0.1 +RTS -N
-- > cabal run stoptheworld -- UnsafeSleepy 0.1 +RTS -N
-- > cabal run stoptheworld -- SafeSleepy   0.1 +RTS -N
--
-- (Interruptible calls are safe, but no difference in this test case.)
main :: IO ()
main = do
    [test, len] <- getArgs
    withLotsOfLiveData 50_000_000 $ do
      putStrLn "Start test"
      tid <- forkIO $ runSimpleTest (read test) (read len) 100 '*'

      threadDelay 3_000_000
      putChar '<'
      performMajorGC
      putChar '>'

      threadDelay 3_000_000
      killThread tid
      putStrLn "\nEnd test"

