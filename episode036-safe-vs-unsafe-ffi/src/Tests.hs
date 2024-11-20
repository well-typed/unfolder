module Tests (
    -- * Tests
    SimpleTest(..)
  , runSimpleTest
    -- * Experimenting with GC
  , withLotsOfLiveData
  ) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import System.IO

import CBits.Interruptible qualified as Interruptible
import CBits.Safe          qualified as Safe
import CBits.Unsafe        qualified as Unsafe
import Native              qualified as Native

{-------------------------------------------------------------------------------
  Run any of the various versions
-------------------------------------------------------------------------------}

data SimpleTest =
    NativeSleepy
  | NativeBusy
  | UnsafeSleepy
  | UnsafeBusy
  | SafeSleepy
  | SafeBusy
  | InterruptibleSleepy
  | InterruptibleBusy
  deriving stock (Show, Read)

runSimpleTest :: SimpleTest -> Double -> Int -> Char -> IO ()
runSimpleTest NativeSleepy        = Native.sleepy
runSimpleTest NativeBusy          = Native.busy
runSimpleTest UnsafeSleepy        = Unsafe.sleepy
runSimpleTest UnsafeBusy          = Unsafe.busy
runSimpleTest SafeSleepy          = Safe.sleepy
runSimpleTest SafeBusy            = Safe.busy
runSimpleTest InterruptibleSleepy = Interruptible.sleepy
runSimpleTest InterruptibleBusy   = Interruptible.busy

{-------------------------------------------------------------------------------
  Experimenting with GC
-------------------------------------------------------------------------------}

withLotsOfLiveData :: Int -> IO () -> IO ()
withLotsOfLiveData amount k = do
    hSetBuffering stdout NoBuffering
    putStr "Generating live data"
    generateGarbage amount []
  where
    generateGarbage :: Int -> [(Int, Int, Int)] -> IO ()
    generateGarbage 0 acc = do
        putStrLn " OK"
        k
        -- make sure GC can't clean up the live data too early
        -- (so that every GC is expensive)
        evaluate $ rnf acc
    generateGarbage n acc = do
        when (n `mod` (amount `div` 10) == 0) $ putStr "."
        generateGarbage (pred n) ((n, n + 1, n + 2) : acc)
