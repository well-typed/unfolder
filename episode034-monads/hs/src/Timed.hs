module Timed where

import Control.Concurrent
import Control.Exception
import System.Timeout

{-------------------------------------------------------------------------------
  Breaking the laws
-------------------------------------------------------------------------------}

-- | A \"monad\" in which every action can take no longer than 1sec
--
-- This is not actually a monad! Breaks the laws!
newtype Timed a = WrapTimed {
      unwrapTimed :: IO a
    }
  deriving newtype (Functor, Applicative)

instance Monad Timed where
  f >>= g = WrapTimed $ do
      ma <- timeout 1_000_000 $ unwrapTimed f
      case ma of
        Nothing -> throwIO Timeout
        Just a  -> unwrapTimed (g a)

data Timeout = Timeout
  deriving stock (Show)
  deriving anyclass (Exception)

runTimed :: Timed a -> IO a
runTimed = unwrapTimed

{-------------------------------------------------------------------------------
  Example application using 'Timed'
-------------------------------------------------------------------------------}

funA :: Int -> Timed Int
funA x = WrapTimed $ do
    putStr "Hello "
    threadDelay 750_000
    pure (x + 1)

funB :: Int -> Timed Int
funB x = WrapTimed $ do
    putStr "world\n"
    threadDelay 750_000
    pure (x + 2)

-- Try
--
-- > runTimed $ example1 1
--
-- Note that this is essentially
--
-- > (funC <=< funA) <=< funA
example1 :: Int -> Timed Int
example1 x0 = do
     x1 <- funA x0
     x2 <- funA x1
     x3 <- funB x2
     pure x3

{-------------------------------------------------------------------------------
  Let's simply abstract out that double call to 'funA'. We would expect this
  not to change the behaviour of the program.
-------------------------------------------------------------------------------}

funAA :: Int -> Timed Int
funAA x0 = do
     x1 <- funA x0
     x2 <- funA x1
     pure x2

-- Try
--
-- > runTimed $ example1 1
--
-- Note that this is essentially
--
-- > funC <=< (funA <=< funA)
example2 :: Int -> Timed Int
example2 x0 = do
     x2 <- funAA x0
     x3 <- funB  x2
     pure x3
