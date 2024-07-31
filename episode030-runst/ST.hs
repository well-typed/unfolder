module ST where

import Control.Monad.ST
import Data.STRef
import Unsafe.Coerce -- for unsafeRunST below

classify :: [Int] -> (Int, Int)
classify xs = runST $ do
  neg <- newSTRef 0
  pos <- newSTRef 0
  let
    step x = case compare x 0 of
      LT -> modifySTRef' neg (+1)
      GT -> modifySTRef' pos (+1)
      EQ -> pure ()
  mapM_ step xs
  pure (,) <*> readSTRef neg <*> readSTRef pos

-- >>> classify [1, 3, 0, -2, 4, -5]
-- (2,3)

-- Interface:
--
-- newSTRef     :: forall a s. a -> ST s (STRef s a)
-- readSTRef    :: forall s a. STRef s a -> ST s a
-- writeSTRef'  :: forall s a. STRef s a -> a -> ST s ()
-- modifySTRef' :: forall s a. STRef s a -> (a -> a) -> ST s ()
--
-- >>> :t runST
-- runST :: forall a. (forall s. ST s a) -> a

-- Move to STOwn at this point, return later.
--
-- Trying to reproduce bad behaviour with the built-in ST type:

unsafeRunST :: ST s a -> a
unsafeRunST m = runST (unsafeCoerce m)

evil :: STRef s Int
evil = unsafeRunST (newSTRef 17)

eviler :: ST s Int
eviler = do
  x <- readSTRef evil
  writeSTRef evil (x + 1)
  return x

test1, test2 :: Int
test1 = runST eviler
test2 = runST eviler

-- >>> test1
-- 17
-- >>> test2
-- 18

