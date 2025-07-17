module Timing (time) where

import Data.Time
import System.IO
import Text.Printf

{-------------------------------------------------------------------------------
  Timing

  Benchmark packages like @criterion@ are much sophisticated; here we use
  something very simple, because we want a /single/ execution where we can
  easily monitor CPU usage.
-------------------------------------------------------------------------------}

-- | Time action (in seconds)
time :: String -> IO a -> IO a
time label act = do
    putStr label >> hFlush stdout

    start  <- getCurrentTime
    result <- act
    end    <- getCurrentTime

    let elapsed :: Double
        elapsed = realToFrac (end `diffUTCTime` start)

    putStrLn $ printf "%0.3fs" elapsed
    return result
