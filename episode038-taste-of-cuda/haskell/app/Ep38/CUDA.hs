-- | Bindings to the CUDA component
--
-- Intended for qualified import.
module Ep38.CUDA (
    mapAdd
  , foldAdd
  , scanAdd
  ) where

import Data.Vector.Storable qualified as Storable (Vector)
import Data.Vector.Storable qualified as Vector
import Foreign
import System.IO.Unsafe (unsafePerformIO)

foreign import capi safe "ep38-cuda.h ep38_map_add"
  ep38_map_add :: Float -> Ptr Float -> Ptr Float -> Int -> IO ()

foreign import capi safe "ep38-cuda.h ep38_fold_add"
  ep38_fold_add :: Ptr Float -> Int -> IO Float

foreign import capi safe "ep38-cuda.h ep38_scan_add"
  ep38_scan_add :: Ptr Float -> Ptr Float -> Int -> IO ()

mapAdd :: Float -> Storable.Vector Float -> Storable.Vector Float
mapAdd x inp = unsafePerformIO $
    Vector.unsafeWith inp $ \inp_ptr -> do
      out_fptr <- mallocForeignPtrArray n
      withForeignPtr out_fptr $ \out_ptr ->
        ep38_map_add x inp_ptr out_ptr n
      return $ Vector.unsafeFromForeignPtr0 out_fptr n
  where
    n :: Int
    n = Vector.length inp

foldAdd :: Storable.Vector Float -> Float
foldAdd inp = unsafePerformIO $
    Vector.unsafeWith inp $ \inp_ptr ->
      ep38_fold_add inp_ptr n
  where
    n :: Int
    n = Vector.length inp

scanAdd :: Storable.Vector Float -> Storable.Vector Float
scanAdd inp = unsafePerformIO $
    Vector.unsafeWith inp $ \inp_ptr -> do
      out_fptr <- mallocForeignPtrArray n
      withForeignPtr out_fptr $ \out_ptr ->
        ep38_scan_add inp_ptr out_ptr n
      return $ Vector.unsafeFromForeignPtr0 out_fptr n
  where
    n :: Int
    n = Vector.length inp
