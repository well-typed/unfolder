module CBits.Safe where

import Data.Word
import Foreign

foreign import capi safe "ep36.h sleepy"
  sleepy :: Double -> Int -> Char -> IO ()

foreign import capi safe "ep36.h busy"
  busy :: Double -> Int -> Char -> IO ()

foreign import capi safe "ep36.h sleepyBuf"
  sleepyBuf :: Ptr Word8 -> Double -> Int -> Char -> IO ()
