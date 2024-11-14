module CBits.Interruptible where

foreign import capi interruptible "ep36.h sleepy"
  sleepy :: Double -> Int -> Char -> IO ()

foreign import capi interruptible "ep36.h busy"
  busy :: Double -> Int -> Char -> IO ()