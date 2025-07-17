-- | Straight lines
--
-- Intended for qualified import
--
-- > import Line (Line(..))
-- > import Line qualified
module Line (
    Line(..)
    -- * Queries
  , computeRSS
    -- * Generate random data
  , RandomDataParams(..)
  , genRandomData
  ) where

import System.Random

import Points (Points, Point(..))
import Points qualified

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parameters for a straight line
data Line = Line {
      intercept :: !Double
    , slope     :: !Double
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Compute @y@ given @x@
at :: Line -> Double -> Double
at model x = model.intercept + model.slope * x

-- | Residual sum of squares
computeRSS :: Points -> Line -> Double
computeRSS points model =
    sum $ map squaredError (Points.toList points)
  where
    squaredError :: Point -> Double
    squaredError p =
        (p.y - predicted) ** 2
      where
        predicted :: Double
        predicted = at model p.x

{-------------------------------------------------------------------------------
  Generate random data
-------------------------------------------------------------------------------}

data RandomDataParams = RandomDataParams {
      numPoints :: Int
    , model     :: Line
    , deviation :: (Double, Double) -- ^ Bounds for deviation from the model
    , distance  :: (Double, Double) -- ^ Bounds for the distance between points
    }
  deriving stock (Show)

genRandomData :: RandomDataParams -> (Point -> IO ()) -> IO ()
genRandomData params onPoint =
    go 0 0
  where
    go ::
         Int      -- Number of accumulated points
      -> Double   -- Next @x@ value to generate
      -> IO ()
    go acc x
      | acc == params.numPoints
      = return ()

      | otherwise = do
          dev <- randomRIO params.deviation
          dst <- randomRIO params.distance
          let y = at params.model x + dev
          onPoint $ Point x y
          go (acc + 1) (x + dst)
