module Util.Transport (
    Transport(..)
  , mockTransport
    -- * JSON support
  , sendValue
  , recvValue
  , sendValueWith
  , recvValueWith
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (LazyByteString)
import GHC.TopHandler (reportError)

import Util.Aeson

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Transport = Transport{
      send :: LazyByteString -> IO ()
    , recv :: IO LazyByteString
    }

{-------------------------------------------------------------------------------
  Mock
-------------------------------------------------------------------------------}

mockTransport ::
     (Transport -> IO ()) -- ^ Server
  -> (Transport -> IO r)  -- ^ Client
  -> IO r
mockTransport server client = do
    serverToClient <- newEmptyMVar
    clientToServer <- newEmptyMVar

    let transportServer, transportClient :: Transport
        transportServer = Transport{
              send = putMVar  serverToClient
            , recv = takeMVar clientToServer
            }
        transportClient = Transport{
              send = putMVar  clientToServer
            , recv = takeMVar serverToClient
            }

    withAsync (handle reportError $ server transportServer) $ \_server ->
      client transportClient

{-------------------------------------------------------------------------------
  JSON
-------------------------------------------------------------------------------}

sendValue :: ToJSON a => Transport -> a -> IO ()
sendValue transport value = transport.send (encode value)

recvValue :: FromJSON a => Transport -> IO a
recvValue transport = decodeFail =<< transport.recv

sendValueWith :: Transport -> (a -> Value) -> a -> IO ()
sendValueWith transport f = sendValue transport . f

recvValueWith :: FromJSON a => Transport -> (a -> Parser b) -> IO b
recvValueWith transport f = decodeFailWith f =<< transport.recv