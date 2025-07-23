-- | Collection of points
--
-- Intended for qualified import.
--
-- > import Points (Points, Point(..), (!))
-- > import Points qualified
module Points (
    Points -- opaque
  , Point(..)
    -- * Simple queries
  , length
  , (!)
    -- * Conversion
  , fromList
  , toList
    --  * I/O
  , writeFile
  , readFile
  , readChunked
  ) where

import Prelude hiding (writeFile, readFile, length)

import Control.Concurrent.Async (mapConcurrently)
import Data.ByteString qualified as BS.Strict
import Data.List qualified as List
import Data.Store (Store)
import Data.Store qualified as Store
import Data.Store.Core qualified as Store
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as Vector
import Data.Vector.Storable.Mutable qualified as MSV
import Foreign
import System.IO hiding (writeFile, readFile)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Point = Point {
      x :: {-# UNPACK #-} !Double
    , y :: {-# UNPACK #-} !Double
    }
  deriving stock (Show, Eq)

newtype Points = Points (Vector Point)
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

{-------------------------------------------------------------------------------
  Simple queries
-------------------------------------------------------------------------------}

length :: Points -> Int
length = Vector.length . toVector

(!) :: Points -> Int -> Point
points ! i = toVector points Vector.! i

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

toVector :: Points -> Vector Point
toVector (Points ps) = ps

fromVector :: Vector Point -> Points
fromVector = Points

fromList :: [Point] -> Points
fromList = fromVector . Vector.fromList

toList :: Points -> [Point]
toList = Vector.toList . toVector

{-------------------------------------------------------------------------------
  I/O
-------------------------------------------------------------------------------}

readFile :: FilePath -> IO Points
readFile fp = fmap Points $
    BS.Strict.readFile fp >>= Store.decodeIO

readChunked :: FilePath -> Int -> IO [Points]
readChunked fp = \chunkSize -> do
    chunks <- chunksOf chunkSize <$> getNumElems
    putStr $ "(" ++ show (List.length chunks) ++ " chunks) "
    hFlush stdout

    mapConcurrently getChunk chunks
  where
    elemSize :: Int
    elemSize = 2 * sizeOf (undefined :: Double)

    getNumElems :: IO Int
    getNumElems = withFile fp ReadMode $ \h -> do
        bs <- BS.Strict.hGetSome h (sizeOf (undefined :: Int))
        Store.decodeIO bs

    getChunk :: (Int, Int) -> IO Points
    getChunk (offset, chunkSize) = withFile fp ReadMode $ \h -> do
        hSeek h AbsoluteSeek offsetInFile
        bs <- BS.Strict.hGetSome h (elemSize * chunkSize)
        Points <$> Store.decodeIOWith peekChunk bs
      where
        offsetInFile :: Integer
        offsetInFile = fromIntegral $
              sizeOf (undefined :: Int) -- skip over the header
            + offset * elemSize

        peekChunk :: Store.Peek (Vector Point)
        peekChunk = do
            fptr <- Store.peekToPlainForeignPtr "Vector" (elemSize * chunkSize)
            Vector.unsafeFreeze (MSV.MVector chunkSize fptr)

writeFile :: FilePath -> Int -> ((Point -> IO ()) -> IO ()) -> IO ()
writeFile fp numPoints k =
    withFile fp WriteMode $ \h -> do
      BS.Strict.hPut h $ Store.encode numPoints
      let onPoint :: Point -> IO ()
          onPoint = BS.Strict.hPut h . Store.encode
      k onPoint

{-------------------------------------------------------------------------------
  Storable
-------------------------------------------------------------------------------}

instance Storable Point where
  sizeOf    _ = 2 * sizeOf (undefined :: Double)
  alignment _ = alignment  (undefined :: Double)

  peek ptr =
      pure Point
        <*> peek (ptr `plusPtr` 0)
        <*> peek (ptr `plusPtr` (sizeOf (undefined :: Double)))

  poke ptr p = do
      poke (ptr `plusPtr` 0)                              p.x
      poke (ptr `plusPtr` (sizeOf (undefined :: Double))) p.y

instance Store Point where
  size = Store.ConstSize (sizeOf (undefined :: Point))
  poke = Store.pokeStorable
  peek = Store.peekStorable

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Compute offsets and chunk size
--
-- > chunksOf 6 18 == [(0,6),(6,6),(12,6)]
-- > chunksOf 7 18 == [(0,7),(7,7),(14,4)]
-- > chunksOf 8 18 == [(0,8),(8,8),(16,2)]
-- > chunksOf 9 18 == [(0,9),(9,9)]
chunksOf :: Int -> Int -> [(Int, Int)]
chunksOf chunkSize numElements = go 0
  where
    go :: Int -> [(Int, Int)]
    go offset
      | offset >= numElements
      = []

      | offset + chunkSize >= numElements
      = [(offset, numElements - offset)]

      | otherwise
      = (offset, chunkSize) : go (offset + chunkSize)
