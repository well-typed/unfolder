-- | Mock implementation of a monad for background actions
module Background (
    -- * Callback-style API
    fetchUrlCallback
    -- * Monadic API
  , Background -- opaque
  , background
  , fetchUrlBackground
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

{-------------------------------------------------------------------------------
  Callback-style API
-------------------------------------------------------------------------------}

fetchUrlCallback :: String -> (String -> IO ()) -> IO ()
fetchUrlCallback url callback = void . forkIO $ do
    threadDelay 1_000_000
    callback $ contentsOf url

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

newtype Background a = WrapBackground {
      unwrapBackground :: IO a
    }
  deriving newtype (Functor, Applicative, Monad)

instance MonadIO Background where
  liftIO = WrapBackground

fetchUrlBackground :: String -> Background String
fetchUrlBackground url = WrapBackground $ do
    threadDelay 2_000_000 -- Mock delay as we fetch the URL
    pure $ contentsOf url

background :: (a -> Background ()) -> a -> IO ()
background f a = void . forkIO $ unwrapBackground (f a)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

contentsOf :: String -> String
contentsOf "url1" = "url2"
contentsOf "url2" = "url3"
contentsOf "url3" = "contents of url3"
contentsOf _ = error "contentsOf"

