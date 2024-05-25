{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Demonstration of the use of varargs in grapesy
--
-- Episode 26 of the Unfolder talks about implementing functions with a variable
-- number of arguments in Haskell, and mentions that we could use this as an
-- alternative way to list the methods of a gRPC server. In this module we
-- show how we can apply this technique in @grapesy@. This is supported in the
-- upstream library as of <https://github.com/well-typed/grapesy/pull/152>, but
-- in order to keep this repository match the actual episode, we have pinned
-- the version of @grapesy@ that was used there.
module VarArgs where

import Data.Kind
import Control.Concurrent

import Network.GRPC.Common
import Network.GRPC.Common.StreamType
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.StreamType

import Proto.Kvstore
import Service

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

class SimpleMethods m (rpcs :: [Type]) (rpcs' :: [Type]) a | a -> m rpcs rpcs' where
  simpleMethods' :: (Methods m rpcs -> Methods m rpcs') -> a

instance SimpleMethods m '[] rpcs (Methods m rpcs) where
  simpleMethods' f = f NoMoreMethods

instance
  ( -- Requirements inherited from the 'Method' constructor
    StreamingRpcHandler h
  , SupportsServerRpc rpc
  , Default (ResponseInitialMetadata rpc)
  , Default (ResponseTrailingMetadata rpc)
  , h ~ HandlerFor (RpcStreamingType rpc)
    -- Requirements for the vararg construction
  , b ~ h m rpc
  , SimpleMethods m rpcs rpcs' a
  ) => SimpleMethods m (rpc ': rpcs) rpcs' (b -> a) where
  simpleMethods' f h = simpleMethods' (f . Method h)

-- | Alternative way to construct 'Methods'
--
-- == Example usage
--
-- Listing the handlers for the gRPC routeguide server using 'Methods' directly
-- looks like this:
--
-- >   Method (mkNonStreaming    $ getFeature   db)
-- > $ Method (mkServerStreaming $ listFeatures db)
-- > $ Method (mkClientStreaming $ recordRoute  db)
-- > $ Method (mkBiDiStreaming   $ routeChat    db)
-- > $ NoMoreMethods
--
-- Since we only use 'Method' here, we can instead write this as
--
-- > simpleMethods
-- >   (mkNonStreaming    $ getFeature   db)
-- >   (mkServerStreaming $ listFeatures db)
-- >   (mkClientStreaming $ recordRoute  db)
-- >   (mkBiDiStreaming   $ routeChat    db)
--
-- Which API you prefer is mostly just a matter of taste.
simpleMethods :: SimpleMethods m rpcs rpcs a => a
simpleMethods = simpleMethods' id

{-------------------------------------------------------------------------------
  Example
-------------------------------------------------------------------------------}

keyValueService' :: MVar Store -> Methods IO (ProtobufMethodsOf KeyValueService)
keyValueService' this =
    simpleMethods
      (mkNonStreaming $ create   this)
      (mkNonStreaming $ delete   this)
      (mkNonStreaming $ retrieve this)
      (mkNonStreaming $ update   this)

{-------------------------------------------------------------------------------
  Handlers

  These can be implemented the same way as in "Service" (or indeed exported
  from there, though typically individual handlers don't need to be exported,
  only the service definition).
-------------------------------------------------------------------------------}

create :: MVar Store -> CreateRequest -> IO CreateResponse
create = undefined

retrieve :: MVar Store -> RetrieveRequest -> IO RetrieveResponse
retrieve = undefined

update :: MVar Store -> UpdateRequest -> IO UpdateResponse
update = undefined

delete :: MVar Store -> DeleteRequest -> IO DeleteResponse
delete = undefined



