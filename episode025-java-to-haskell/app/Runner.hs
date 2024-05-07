-- | Unfolder episode 25: from Java to Haskell
--
-- In this episode, we will translate a gRPC server written in Java to Haskell.
-- We will use it as an example to demonstrate some of the conceptual
-- differences of the two languages, but also observe that the end result of the
-- translation looks perhaps more similar to the Java version than one might
-- expect. Unlike most of our episodes, we hope that this one is understandable
-- to any software developer, even people without any previous exposure to
-- Haskell. Of course, we won’t be able to explain everything, but the example
-- used should help to establish an idea of the look and feel of Haskell code,
-- and perhaps learn a bit more about the relationship between the
-- object-oriented and functional programming paradigms.
--
-- References:
--
-- * The blog post introducing the Java code we're working with:
--   <https://grpc.io/blog/optimizing-grpc-part-1/>
--
-- * This episode is _not_ about modelling inheritance in Haskell. This is
--   possible, but not that easy. See
--   <https://well-typed.com/blog/2018/03/oop-in-haskell/>
--   and episode 13, "open recursion" of the Haskell Unfolder
--   <https://www.youtube.com/watch?v=pfwP4hXM5hA>.
module Runner (main) where

import Control.Concurrent
import Network.GRPC.Common
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType

import Proto.Kvstore

import Service

{-------------------------------------------------------------------------------
  The server can in principle service more than one API, but we serve only one.
-------------------------------------------------------------------------------}

allServices :: MVar Store -> Services IO (ProtobufServices '[KeyValueService])
allServices kvService =
      Service (keyValueService kvService)
    $ NoMoreServices

{-------------------------------------------------------------------------------
  * The exact way that the server is started up is different from how it's done
    in Java, but this is just different library design.
  * `ServerConfig` is a record with fields for a server without and with TLS
-------------------------------------------------------------------------------}

startServer :: IO ()
startServer = do
    let config = ServerConfig {
            serverInsecure = Just (InsecureConfig Nothing defaultInsecurePort)
          , serverSecure   = Nothing
          }
    kvServiceState <- constructServiceState
    runServerWithHandlers config def (fromServices (allServices kvServiceState))

main :: IO ()
main = startServer