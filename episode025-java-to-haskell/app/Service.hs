{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax       #-}

module Service (
    Store
  , constructServiceState
  , keyValueService
  ) where

import Prelude

import Control.Concurrent
import Data.ByteString (ByteString)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf (Protobuf, defMessage)
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.StreamType

import Proto.Kvstore
import RecordDot

{-------------------------------------------------------------------------------
  Private static variables

  Private static variables are values that are only available inside the class,
  but not associated with any particular object. In Haskell, this kind of
  "implementation hiding" is done through modules. Any value not explicitly
  exported is only visible within the module.
-------------------------------------------------------------------------------}

readDelayMillis, writeDelayMillis :: Int
readDelayMillis  = 10
writeDelayMillis = 50

{-------------------------------------------------------------------------------
  Private static functions

  * Much like private static variables, these are functions that are visible
    inside the module but not outside.
  * The `IO` in the type of this function indicates that
    this function is allowed side effects (write to files, change memory
    locations, etc.); in Java of course this is true for _all_ functions.
  * Haskell has exceptions, but no _checked_ exceptions.

  Background: this code is part of a gRPC benchmark of a key/value store; we
  simulate the actual work done by the the key/value store by simply waiting.
-------------------------------------------------------------------------------}

simulateWork :: Int -> IO ()
simulateWork millis = threadDelay (millis * 1_000)

{-------------------------------------------------------------------------------
  Non-static class members

  There are no objects in Haskell, so non-static variables must be passed
  explicitly to all functions (as if you are explicitly passing a `this`
  parameter).
-------------------------------------------------------------------------------}

type Store = Map ByteString ByteString

{-------------------------------------------------------------------------------
  Constructor

  * Since there are no objects, a constructor is just a normal function.
  * An IORef is a thread-safe mutable variable.
-------------------------------------------------------------------------------}

constructServiceState :: IO (MVar Store)
constructServiceState = newMVar Map.empty

{-------------------------------------------------------------------------------
  Public methods

  * Like we already said, these become functions that take `this` as an argument
    argument.
  * There is no need for an explicit `synchronized` marker; synchronization
    is taken care of by the `MVar` abstraction.
  * There is no concept of inheritance in Haskell.
  * Haskell's strong type system can compute for this particular route that it
    will generate only a single response, so there is no 'responseObserver'
    argument. Instead, we return the single response as the function result.
  * Every `then` of an `if`-statement _must_ have a corresponding `else`;
    there is no explicit control flow in Haskell. The `return` function in
    Haskell only behaves like `return` in Java when it is the last statement
    in the function (or a control block); it does _not_ cause an immediate
    return from the function if used elsewhere. (You can think of it as
    the return value of that "block".)
  * The argument to modifyMVar is an anonymous function; the syntax in Java
    would be something like

    ```
    (store) -> { ... }
    ```

  Background: `CreateRequest` and `CreateResponse` are generated from the
  `.proto` file.
-------------------------------------------------------------------------------}

create :: MVar Store -> CreateRequest -> IO CreateResponse
create this request = do
    let key   = request.key
        value = request.value
    simulateWork writeDelayMillis
    modifyMVar this (\store ->
        if Map.notMember key store then
          return (Map.insert key value store, defMessage)
        else
          throwGrpcError GrpcAlreadyExists
      )

{-------------------------------------------------------------------------------
  * The retrieve handler is much the same: the only difference is that we see
    for the first time the syntax "<-", which means "call this effectful
    function, and then give a name to the result". This name is not a variable:
    we cannot change it's value afterwards.
  * We do not need to provide a type annotation for `key` or `mValue`: it is
    inferred automatically.
  * There is no `null` value in Haskell; instead, in cases where there is the
    possibility of a `null`-like value, this is explicitly marked in the type
    system (and only then). Since this has a different type, you cannot forget
    to check for null!
-------------------------------------------------------------------------------}

retrieve :: MVar Store -> RetrieveRequest -> IO RetrieveResponse
retrieve this request = do
    let key = request.key
    simulateWork readDelayMillis
    mValue <- withMVar this (\store ->
        return (Map.lookup key store)
      )
    case mValue of
      Just value ->
        return defMessage{value = value}
      Nothing ->
        throwGrpcError GrpcNotFound

{-------------------------------------------------------------------------------
  The other handlers are much the same
-------------------------------------------------------------------------------}

update :: MVar Store -> UpdateRequest -> IO UpdateResponse
update this request = do
    let key      = request.key
        newValue = request.value
    simulateWork writeDelayMillis
    modifyMVar this (\store ->
        if Map.notMember key store then
          throwGrpcError GrpcNotFound
        else
          return (Map.insert key newValue store, defMessage)
      )

delete :: MVar Store -> DeleteRequest -> IO DeleteResponse
delete this request = do
    let key = request.key
    simulateWork writeDelayMillis
    modifyMVar_ this (\store ->
        return (Map.delete key store)
      )
    return defMessage

{-------------------------------------------------------------------------------
  Declaring the service

  Background: "from the point of view of gRPC, a service is a collection of
  methods" <https://grpc.io/blog/grpc-with-json/>. When using Protobuf, this
  is embodied by the generated `KeyValueServiceImplBase` base. In Haskell, we
  need to declare this explicitly (like you'd also have to do in Java when
  using JSON instead of Protobuf).
-------------------------------------------------------------------------------}

keyValueService :: MVar Store -> Methods IO (ProtobufMethodsOf KeyValueService)
keyValueService this =
      Method (mkNonStreaming $ create   this)
    $ Method (mkNonStreaming $ delete   this)
    $ Method (mkNonStreaming $ retrieve this)
    $ Method (mkNonStreaming $ update   this)
    $ NoMoreMethods

{-------------------------------------------------------------------------------
  Declaring metadata types

  Background: the Haskell @grapesy@ gRPC library is precise about metadata, so
  we also have to declare that for this particular service, we make no use of
  metadata.
-------------------------------------------------------------------------------}

type instance RequestMetadata          (Protobuf KeyValueService meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf KeyValueService meth) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf KeyValueService meth) = NoMetadata


