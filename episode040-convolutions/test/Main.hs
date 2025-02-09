module Main (main) where

import Data.Type.Nat
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tensor.TestValue

import Ep40.Tensor (Tensor(..))
import Ep40.Tensor qualified as Tensor

import CUDNN qualified

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain $ testGroup "ep40" [
      testProperty "4d" prop_4d
    , testProperty "2d" prop_2d
    , testProperty "3d" prop_3d
    ]

{-------------------------------------------------------------------------------
  Tests

  It /looks/ like cuDNN offers a 4D convolution, but actually that's not the
  case. The size of the first dimension is the number of input images, each of
  which is processed independently; the size of the first dimension of the
  kernel essentially corresponds to multiple independent convolutions. For
  example, if the "4D kernel" has two 3D kernels, then that does _not_ mean it
  can use results from more than one image.

  Thus, this approach to understanding an existing system allows us to form
  a conjecture, then /test/ that conjecture, and just as needed.
-------------------------------------------------------------------------------}

-- | Failing property: cuDNN does not provide 4D convolutions
--
-- Note that (without the call to 'expectFailure') this property (nearly?)
-- always fails:
--
-- > cabal run test-ep40 -- -p 4d --quickcheck-tests=1
--
-- This indicates that this is a fundamental misunderstanding of what cuDNN
-- is doing; it's not a subtle mistake in the computation somewhere.
prop_4d :: Tensor Nat4 TestValue -> Property
prop_4d input = expectFailure $
    counterexample ("size input: " ++ show (Tensor.size input)) $
    counterexample ("size usingModel: " ++ show (Tensor.size usingModel)) $
    counterexample ("size usingCUDNN: " ++ show (Tensor.size usingCUDNN)) $
    usingModel === usingCUDNN
  where
    kernel, usingModel, usingCUDNN :: Tensor Nat4 TestValue
    kernel     = Tensor.inject . Tensor.inject $ Tensor.blur
    usingModel = Tensor.convolve kernel input
    usingCUDNN = CUDNN.convolve kernel input

prop_2d :: Tensor Nat2 TestValue -> Property
prop_2d input =
    usingModel === usingCUDNN
  where
    usingModel, usingCUDNN :: Tensor Nat2 TestValue
    usingModel = Tensor.convolve Tensor.blur input
    usingCUDNN =
        Tensor.project . Tensor.project $
          CUDNN.convolve
            (Tensor.inject . Tensor.inject $ Tensor.blur)
            (Tensor.inject . Tensor.inject $ input)

prop_3d :: Tensor Nat3 TestValue -> Property
prop_3d input =
    usingModel === usingCUDNN
  where
    usingModel, usingCUDNN :: Tensor Nat3 TestValue
    usingModel = Tensor.map (Tensor.convolve Tensor.blur) input
    usingCUDNN =
        Tensor.map Tensor.project $
          CUDNN.convolve
            (Tensor.inject . Tensor.inject $ Tensor.blur)
            (Tensor.map Tensor.inject $ input)

{-------------------------------------------------------------------------------
  QuickCheck

  This is a simple instance of 'Arbitrary'. See the @testing-tensor@ library
  for more sophisticated QuickCheck support.
-------------------------------------------------------------------------------}

instance (SNatI n, Arbitrary a) => Arbitrary (Tensor n a) where
  arbitrary = sized $ \n -> do
      --  cuDNN doesn't like it when the kernel is larger than the input.
      -- For simplicity we therefore generate a large enough input.
      sz :: Tensor.Size n <- liftArbitrary (choose (3, 3 + n))
      sequenceA $ Tensor.replicate sz arbitrary

  -- For a good definition of shrinking, see @testing-tensor@
  shrink _ = []

