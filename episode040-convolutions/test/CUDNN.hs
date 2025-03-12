{-# OPTIONS_GHC -Wno-orphans #-}

module CUDNN (convolve) where

import Control.Monad.State
import Data.Foldable qualified as Foldable
import Data.List (uncons)
import Data.Maybe (fromJust)
import Data.Type.Nat
import Data.Vec.Lazy (Vec(..))
import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import Ep40.Tensor (Tensor)
import Ep40.Tensor qualified as Tensor

{-------------------------------------------------------------------------------
  Convolutions using cuDNN
-------------------------------------------------------------------------------}

convolve ::
     (Real a, Fractional a)
  => Tensor Nat4 a -> Tensor Nat4 a -> Tensor Nat4 a
convolve kernels input = unsafePerformIO $
    withArray (Foldable.toList (realToFrac <$> kernels)) $ \kernelsPtr ->
    withArray (Foldable.toList (realToFrac <$> input))   $ \inputPtr   ->
    alloca $ \outputHeightPtr ->
    alloca $ \outputWidthPtr  -> do
      outputPtr <-
        c_test_cudnn_convolve
          (fromIntegral k)
          (fromIntegral kh)
          (fromIntegral kw)
          kernelsPtr
          (fromIntegral n)
          (fromIntegral c)
          (fromIntegral ih)
          (fromIntegral iw)
          inputPtr
          outputHeightPtr
          outputWidthPtr
      oh <- fromIntegral <$> peek outputHeightPtr
      ow <- fromIntegral <$> peek outputWidthPtr
      let outputSize  = n ::: k ::: oh ::: ow ::: VNil
      fmap realToFrac . traversableFromList (Tensor.replicate outputSize) <$>
        peekArray (product outputSize) outputPtr
  where
    n ::: c ::: ih ::: iw ::: VNil = Tensor.size input
    k ::: _ ::: kh ::: kw ::: VNil = Tensor.size kernels

foreign import capi unsafe "test-cudnn.h test_cudnn_convolve"
  c_test_cudnn_convolve ::
       CInt       -- ^ num_kernels
    -> CInt       -- ^ kernel_height
    -> CInt       -- ^ kernel_width
    -> Ptr Float  -- ^ kernel
    -> CInt       -- ^ num_images
    -> CInt       -- ^ input_channels
    -> CInt       -- ^ input_height
    -> CInt       -- ^ input_width
    -> Ptr Float  -- ^ input
    -> Ptr CInt   -- ^ output_height
    -> Ptr CInt   -- ^ output_width
    -> IO (Ptr Float)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

traversableFromList :: Traversable f => (forall x. x -> f x) -> [a] -> f a
traversableFromList setup xs =
    fromJust . flip evalStateT xs . sequenceA $ setup (StateT uncons)
