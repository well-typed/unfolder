module Native (
    sleepy
  , busy
  ) where

import Control.Concurrent
import Control.Monad
import Data.Word
import System.IO

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

sleepy :: Double -> Int -> Char -> IO ()
sleepy s n c =
    replicateM_ n $ do
      putChar c
      hFlush stdout
      threadDelay (round $ s * 1_000_000)

busy :: Double -> Int -> Char -> IO ()
busy len n c = do
    replicateM_ n $ do
      putChar c
      hFlush stdout
      spin len

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

spin :: Double -> IO ()
spin s = go (round $ s * 4_750_000_000)
  where
    go :: Word64 -> IO ()
    go 0 = return ()
    go n = go (n - 1)

