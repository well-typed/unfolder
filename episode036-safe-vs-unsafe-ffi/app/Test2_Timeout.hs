module Test2_Timeout (main) where

import Control.Monad
import System.Environment
import System.Timeout

import Tests

-- | Try to timeout FFI
--
-- Compare behaviour in
--
-- > cabal run timeout -- UnsafeSleepy        2
-- > cabal run timeout -- InterruptibleSleepy 2
-- > cabal run timeout -- InterruptibleBusy   2
--
-- It is also interesting to compare the CTRL-C behaviour of these programs.
--
-- Interruptibility of the /native/ "busy" version is quite tricky to predict,
-- because it depends on garbage collection:
--
-- > cabal run timeout -- NativeSleepy 2
-- > cabal run timeout -- NativeBusy   2
main :: IO ()
main = do
    [test, len] <- getArgs
    void $ timeout 4_100_000 $ runSimpleTest (read test) (read len) 10 '*'
