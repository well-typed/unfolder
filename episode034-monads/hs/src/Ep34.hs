{-# OPTIONS_GHC -fdefer-type-errors #-}

-- | Unfolder episode 34: You already understand monads
--
-- Function composition is the idea that we can take two functions and create a
-- new function, which applies the two functions one after the other. When
-- viewed from the right angle, monads just generalize this idea from functions
-- to programs: construct new programs by running other programs one after the
-- other. In this episode we make this simple idea precise. We will also see
-- what the monad laws look like in this setting, and we will discuss an example
-- of what goes wrong when the monad laws are broken.
module Ep34 where

import Prelude hiding ((>>=))

import Control.Monad hiding ((>>=))
import Control.Monad.IO.Class

import Background

{-------------------------------------------------------------------------------
  Can't add programs!
-------------------------------------------------------------------------------}

funA :: Int -> IO Int
funA x = do
    putStr "Hello "
    pure (x + 1)

funB :: Int -> IO Int
funB x = do
    putStr "world\n"
    pure (x + 2)

addAB :: IO ()
addAB = undefined (funA 1 + funB 2)

{-------------------------------------------------------------------------------
  Composition of functions
-------------------------------------------------------------------------------}

-- | Simply example (polynomial) function: 3x^2 + 4
poly :: Double -> Double
poly = (+ 4) . (3 *) . (^ 2)

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)

{-------------------------------------------------------------------------------
  Composition of programs
-------------------------------------------------------------------------------}

-- (.)   :: (b ->    c) -> (a ->    b) -> (a ->    c)
-- (<=<) :: (b -> IO c) -> (a -> IO b) -> (a -> IO c)

askName :: String -> IO String
askName prompt = do
    putStr prompt
    getLine

sayHi :: String -> IO ()
sayHi name = putStrLn $ "hi " ++ name

-- try
--
-- > meet "What is your name? "
meet :: String -> IO ()
meet = sayHi <=< askName

{-------------------------------------------------------------------------------
  Laws

  f  .  (g  .  h) = (f  .  g)  .  h = f  .  g  .  h
  f <=< (g <=< h) = (f <=< g) <=< h = f <=< g <=< h

  pure :: a -> IO a    program that just returns its input

  f  .  id   = id   .   f = f
  f <=< pure = pure <=< f = f
-------------------------------------------------------------------------------}

meet', meet'' :: String -> IO ()
meet'  = meet <=< pure
meet'' = pure <=< meet

{-------------------------------------------------------------------------------
  Another example: callback hell
-------------------------------------------------------------------------------}

-- fetchUrlCallback :: String -> (String -> IO ()) -> IO ()

-- try
--
-- > callbackHell "url1"
exampleCallbackHell :: String -> IO ()
exampleCallbackHell url1 =
    fetchUrlCallback url1 $ \url2 ->
      fetchUrlCallback url2 $ \url3 ->
        fetchUrlCallback url3 $ print

-- fetchUrlBackground :: String -> Background String

threeRedirects :: String -> Background String
threeRedirects =
       fetchUrlBackground
   <=< fetchUrlBackground
   <=< fetchUrlBackground

printBackground :: String -> Background ()
printBackground = liftIO . print

-- try
--
-- > callBackground "url1"
exampleBackground :: String -> IO ()
exampleBackground url1 = background (printBackground <=< threeRedirects) url1

{-------------------------------------------------------------------------------
  Back to @>>=@
-------------------------------------------------------------------------------}

-- (<=<) :: (b -> IO c) -> (a -> IO b) -> (a -> IO c)

(>>=) :: forall m a b. Monad m => m a -> (a -> m b) -> m b
f >>= g = (g <=< f') ()
  where
    f' :: () -> m a
    f' () = f

threeRedirects' :: String -> Background String
threeRedirects' url1 =
   fetchUrlBackground url1 >>= \url2 ->
   fetchUrlBackground url2 >>= \url3 ->
   fetchUrlBackground url3

threeRedirects'' :: String -> Background String
threeRedirects'' url1 = do
   url2 <- fetchUrlBackground url1
   url3 <- fetchUrlBackground url2
   fetchUrlBackground url3
