module LinearRegression (
    Sums
  , linearRegressionUsing
  , chunkedRegressionUsing
    -- * Implementations
  , baseline
  , parallel1
  , parallel2
  , parallel3
  , parallel4
  , parallel5
  , parallel6
  ) where

import Control.Parallel.Strategies qualified as Parallel
import Data.List qualified as List
import GHC.Conc

import Line (Line(..))
import Points (Points, Point(..), (!))
import Points qualified

{-------------------------------------------------------------------------------
  General infrastructure
-------------------------------------------------------------------------------}

data Sums = Sums {
      x  :: !Double
    , y  :: !Double
    , xx :: !Double
    , xy :: !Double
    }

onePoint :: Point -> Sums
onePoint p = Sums p.x p.y (p.x * p.x) (p.x * p.y)

instance Semigroup Sums where
  p <> q = Sums (p.x + q.x) (p.y + q.y) (p.xx + q.xx) (p.xy + q.xy)

instance Monoid Sums where
  mempty = Sums 0 0 0 0

toModel :: Int -> Sums -> Line
toModel numPoints sums = Line{intercept, slope}
  where
    avg_x, avg_y :: Double
    avg_x = sums.x / fromIntegral numPoints
    avg_y = sums.y / fromIntegral numPoints

    intercept, slope :: Double
    slope     = (sums.xy - avg_y * sums.x)
              / (sums.xx - avg_x * sums.x)
    intercept = avg_y - slope * avg_x

linearRegressionUsing :: (Points -> Sums) -> Points -> Line
linearRegressionUsing f points =
    toModel numPoints $ f points
  where
    numPoints :: Int
    numPoints = Points.length points

chunkedRegressionUsing :: ([Points] -> Sums) -> [Points] -> Line
chunkedRegressionUsing f pointss =
    toModel numPoints $ f pointss
  where
    numPoints :: Int
    numPoints = sum $ map Points.length pointss

{-------------------------------------------------------------------------------
  Baseline
-------------------------------------------------------------------------------}

baseline :: Points -> Sums
baseline points = go mempty 0
  where
    go :: Sums -> Int -> Sums
    go !acc n
      | n < Points.length points
      = let one = onePoint (points ! n)
        in go (acc <> one) (n + 1)

      | otherwise
      = acc

{-------------------------------------------------------------------------------
  Various attempts at a parallel version
-------------------------------------------------------------------------------}

-- | Way too many sparks
parallel1 :: Points -> Sums
parallel1 points =
    go mempty 0
  where
    go :: Sums -> Int -> Sums
    go !acc n
      | n < Points.length points
      = let one = onePoint (points ! n)
        in one `par` go (acc <> one) (n + 1)

      | otherwise
      = acc

-- | Sparks don't get a chance to be converted: value immediately needed
parallel2 :: [Points] -> Sums
parallel2 =
    go mempty
  where
    go :: Sums -> [Points] -> Sums
    go !acc []       = acc
    go !acc (ps:pss) = let sums = baseline ps
                       in sums `par` go (acc <> sums) pss


-- | Still not many sparks converted: sums1 is evaluated before sums2
parallel3 :: [Points] -> Sums
parallel3 =
    go
  where
    go :: [Points] -> Sums
    go []       = mempty
    go (ps:pss) = let sums1 = baseline ps
                      sums2 = go pss
                  in sums1 `par` sums1 <> sums2

-- | This one works, but this is brittle: Haskell has no guaranteed ordering
parallel4 :: [Points] -> Sums
parallel4 =
    go
  where
    go :: [Points] -> Sums
    go []       = mempty
    go (ps:pss) = let sums1 = baseline ps
                      sums2 = go pss
                  in sums1 `par` sums2 <> sums1

-- | Guaranteed ordering only through 'pseq': this is the correct version
parallel5 :: [Points] -> Sums
parallel5 =
    go
  where
    go :: [Points] -> Sums
    go []       = mempty
    go (ps:pss) = let sums1 = baseline ps
                      sums2 = go pss
                  in sums1 `par` sums2 `pseq` sums1 <> sums2

{-------------------------------------------------------------------------------
  Sneak peak at strategies: equivalent code using the @parallel@ library
-------------------------------------------------------------------------------}

parallel6 :: [Points] -> Sums
parallel6 = List.foldl' (<>) mempty . Parallel.parMap Parallel.rseq baseline
