module Test1_CPU (main) where

import System.Environment

import Tests

-- | Compare CPU usage
--
-- > /usr/bin/time cabal run cpu -- UnsafeSleepy 1
-- > /usr/bin/time cabal run cpu -- UnsafeBusy   1
-- > /usr/bin/time cabal run cpu -- NativeSleepy 1
-- > /usr/bin/time cabal run cpu -- NativeBusy   1
main :: IO ()
main = do
    [test, len] <- getArgs
    runSimpleTest (read test) (read len) 5 '*'
