{-# LANGUAGE DerivingVia #-}
module STOwn where

import Control.Monad.Identity
import Control.Monad.State
import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- Simplified interface:
--
-- - No "s" argument.
-- - Let's assume all refs are for type Int.
--
-- newSTRef     :: a -> ST STRef
-- readSTRef    :: STRef -> ST Int
-- writeSTRef'  :: STRef -> Int -> ST ()
-- modifySTRef' :: STRef -> (Int -> Int) -> ST ()

newtype STRef = MkSTRef Offset

newtype ST a = MkST (Store -> (a, Store))
  deriving (Functor, Applicative, Monad) via State Store

data Store =
  MkStore
    { memory :: !(Vector Int)
    , next   :: !Offset
    }

type Offset = Int

newSTRef :: Int -> ST STRef
newSTRef i =
  MkST $ \ (MkStore memory next) ->
    (MkSTRef next, MkStore (Vector.snoc memory i) (next + 1))

readSTRef :: STRef -> ST Int
readSTRef (MkSTRef offset) =
  MkST $ \ store@(MkStore memory _next) ->
    (memory Vector.! offset, store)

writeSTRef' :: STRef -> Int -> ST ()
writeSTRef' (MkSTRef offset) !x =
  MkST $ \ (MkStore memory next) ->
    ((), MkStore (memory Vector.// [(offset, x)]) next)

modifySTRef' :: STRef -> (Int -> Int) -> ST ()
modifySTRef' r f = do
  x <- readSTRef r
  writeSTRef' r (f x)

runST :: ST a -> a
runST (MkST f) =
  fst (f (MkStore Vector.empty 0))

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

evil :: STRef
evil = runST (newSTRef 17)

eviler :: ST Int
eviler = do
  x <- readSTRef evil
  writeSTRef' evil (x + 1)
  return x

test :: Int
test = runST eviler

-- >>> test
-- index out of bounds (0,0)
