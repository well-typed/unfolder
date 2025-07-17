-- | Write points in format understood by gnuplot
--
-- Intended for qualified import.
--
-- > import Gnuplot qualified
module Gnuplot (
    writePoints
  ) where

import Points (Point(..))
import System.IO

writePoints :: FilePath -> ((Point -> IO ()) -> IO ()) -> IO ()
writePoints fp k = do
    withFile fp WriteMode $ \h -> do
      hPutStrLn h "# x y"

      let onPoint :: Point -> IO ()
          onPoint p = hPutStrLn h $ show p.x ++ "\t" ++ show p.y

      k onPoint
