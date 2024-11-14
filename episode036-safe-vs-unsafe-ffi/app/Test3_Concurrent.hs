module Test3_Concurrent (main) where

import Control.Concurrent
import Control.Monad
import System.Environment

import Tests

-- | Concurrent execution
--
-- Try
--
-- > cabal run concurrent -- NativeSleepy 1 +RTS -N1
-- > cabal run concurrent -- UnsafeSleepy 1 +RTS -N1
-- > cabal run concurrent -- UnsafeSleepy 1 +RTS -N4
-- > cabal run concurrent -- SafeSleepy   1 +RTS -N1
--
-- The behaviour of the native "busy" version is again difficult to predict, as
-- it depends on GC.
--
-- > cabal run concurrent -- NativeBusy 1 +RTS -N1
-- > cabal run concurrent -- NativeBusy 1 +RTS -N4
--
-- (For the purposes of this test there is no difference between the safe and
-- interruptible FFI versions, nor between the busy and sleepy FFI versions.)
main :: IO ()
main = do
    [test, len] <- getArgs
    forM_ ['0' .. '9'] $ \i ->
      forkIO $ runSimpleTest (read test) (read len) 10 i
    threadDelay 20_000_000
