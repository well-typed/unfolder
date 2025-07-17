-- | Haskell Unfolder episode 47: pure parallelism
--
-- "Pure parallelism" refers to the execution of pure Haskell functions on
-- multiple CPU cores, (hopefully) speeding up the computation. Since we are
-- still dealing with pure functions, however, we get none of the problems
-- normally associated with concurrent execution: no non-determinism, no need
-- for locks, etc.
--
-- In this episode we will develop a pure but parallel implementation of linear
-- regression. We will briefly recap how linear regression works, before
-- discussing the two primitive functions that Haskell offers for pure
-- parallelism: `par` and `pseq`. We will conclude by taking a glimpse at
-- parallel strategies.
--
-- References:
--
-- * "Seq no more: Better Strategies for Parallel Haskell"
--   Simon Marlow et al., Haskell Symposium 2010
--   <https://simonmar.github.io/bib/papers/strategies.pdf>
-- * "Proof: Ordinary least squares for simple linear regression"
--   Joram Soch et al.
--   <https://statproofbook.github.io/P/slr-ols.html>
-- * "What Textbooks Don't Tell You About Curve Fitting"
--   Artem Kirsanov
--   <https://www.youtube.com/watch?v=q7seckj1hwM>
module Main where

import Control.Exception
import Control.Monad
import Data.List qualified as List
import System.IO

import Cmdline
import Gnuplot qualified
import Line (Line(..))
import Line qualified
import LinearRegression
import Points qualified
import Timing

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmdline <- getCmdline
    case cmdline.command of
      Analyse input algo ->
        runAnalyse input algo
      GenRandomData output params ->
        runGenRandomData output params
      GenRegressionSurface input params ->
        runGenRegressionSurface input params

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

runAnalyse :: FilePath -> Algorithm -> IO ()
runAnalyse input alg = do
    case alg of
      Baseline    -> unchunked baseline
      Parallel1   -> unchunked parallel1
      Parallel2 n -> chunked n parallel2
      Parallel3 n -> chunked n parallel3
      Parallel4 n -> chunked n parallel4
      Parallel5 n -> chunked n parallel5
      Parallel6 n -> chunked n parallel6
  where
    unchunked :: (Points.Points -> LinearRegression.Sums) -> IO ()
    unchunked = go (Points.readFile input) . linearRegressionUsing

    chunked :: Int -> ([Points.Points] -> LinearRegression.Sums) -> IO ()
    chunked n = go (Points.readChunked input n) . chunkedRegressionUsing

    go :: IO a -> (a -> Line) -> IO ()
    go readData f = do
        points <- time "Reading data.. " $ readData
        model  <- time "Analysing..    " $ evaluate $ f points
        print model

runGenRandomData :: Output -> Line.RandomDataParams -> IO ()
runGenRandomData output params = do
    putStrLn $ "Generating data with parameters " ++ show params

    let writeData :: ((Points.Point -> IO ()) -> IO ()) -> IO ()
        writeData =
          case output of
            Gnuplot fp -> Gnuplot.writePoints fp
            Raw     fp -> Points.writeFile fp params.numPoints

    writeData $ Line.genRandomData params

runGenRegressionSurface :: FilePath -> SurfaceParams -> IO ()
runGenRegressionSurface input params = do
    points <- Points.readFile input

    withFile "surface.data" WriteMode $ \h ->
      forM_ guessIntercept $ \intercept ->
      forM_ guessSlope     $ \slope     -> do
        let rss = Line.computeRSS points (Line intercept slope)
        hPutStrLn h $ List.intercalate "\t" $
          map show $ [intercept, slope, rss]

    withFile "surface-fixSlope.data" WriteMode $ \h ->
      forM_ guessIntercept $ \intercept -> do
        let rss = Line.computeRSS points (Line intercept params.slopeActual)
        hPutStrLn h $ List.intercalate "\t" $
          map show $ [intercept, rss]

    withFile "surface-fixIntercept.data" WriteMode $ \h ->
      forM_ guessSlope $ \slope -> do
        let rss = Line.computeRSS points (Line params.interceptActual slope)
        hPutStrLn h $ List.intercalate "\t" $
          map show $ [slope, rss]
  where
    guessIntercept, guessSlope :: [Double]
    guessIntercept = [
           params.interceptMin
        ,  params.interceptMin + params.interceptStep
        .. params.interceptMax
        ]
    guessSlope =[
           params.slopeMin
        ,  params.slopeMin + params.slopeStep
        .. params.slopeMax
        ]
