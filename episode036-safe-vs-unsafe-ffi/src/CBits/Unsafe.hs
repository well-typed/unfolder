module CBits.Unsafe where

-- Compare to the signatures of the corresponding C functions:
--
-- > void sleepy(double s, int n, char c);
-- > void busy(double s, int n, char c);

foreign import capi unsafe "ep36.h sleepy"
  sleepy :: Double -> Int -> Char -> IO ()

foreign import capi unsafe "ep36.h busy"
  busy :: Double -> Int -> Char -> IO ()