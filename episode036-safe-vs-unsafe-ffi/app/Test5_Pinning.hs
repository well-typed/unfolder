module Test5_Pinning (main) where

import Control.Concurrent
import Control.Exception
import Data.Primitive
import System.Environment
import System.Mem

import Tests

import CBits.Safe qualified as Safe

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Pinned versus unpinned memory
--
-- Try
--
-- > cabal run pinning -- simple
-- > cabal run pinning -- unpinned
-- > cabal run pinning -- pinned
-- > cabal run pinning -- premature
main :: IO ()
main = do
    [test] <- getArgs
    case test of
      "simple"    -> simpleExample
      "unpinned"  -> withUnpinnedData
      "pinned"    -> withPinnedData
      "premature" -> prematureGC
      _otherwise  -> fail "unknown test case"

{-------------------------------------------------------------------------------
  Individual tests
-------------------------------------------------------------------------------}

simpleExample :: IO ()
simpleExample = do
    arr <- newByteArray 100
    withMutableByteArrayContents arr $ \ptr ->
      Safe.sleepyBuf ptr 0 100 '*'
    frozen <- freezeByteArray arr 0 100
    print frozen

withUnpinnedData :: IO ()
withUnpinnedData = do
    arr <- newByteArray 100
    withLotsOfLiveData 1_000_000 $ do
      putStrLn "Start test"
      result <- newEmptyMVar
      _tid   <- forkIO $ do
          withMutableByteArrayContents arr $ \ptr ->
            Safe.sleepyBuf ptr 0.1 100 '*'
          frozen <- freezeByteArray arr 0 100
          putMVar result frozen

      threadDelay 3_000_000
      putChar '<'
      performMajorGC
      putChar '>'

      print =<< readMVar result
      putStrLn "\nEnd test"

withPinnedData :: IO ()
withPinnedData = do
    arr <- newPinnedByteArray 100
    withLotsOfLiveData 1_000_000 $ do
      putStrLn "Start test"
      result <- newEmptyMVar
      _tid   <- forkIO $ do
          withMutableByteArrayContents arr $ \ptr ->
            Safe.sleepyBuf ptr 0.1 100 '*'
          frozen <- freezeByteArray arr 0 100
          putMVar result frozen

      threadDelay 3_000_000
      putChar '<'
      performMajorGC
      putChar '>'

      print =<< readMVar result
      putStrLn "\nEnd test"

prematureGC :: IO ()
prematureGC = do
    arr <- newPinnedByteArray 100
    ptr <- evaluate $ mutableByteArrayContents arr
    withLotsOfLiveData 1_000_000 $ do
      putStrLn "Start test"
      result <- newEmptyMVar
      _tid   <- forkIO $ do
          Safe.sleepyBuf ptr 0.1 100 '*'
          putMVar result ()

      threadDelay 3_000_000
      putChar '<'
      performMajorGC
      putChar '>'

      readMVar result
      putStrLn "\nEnd test"

