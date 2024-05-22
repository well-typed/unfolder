{-# OPTIONS_GHC -Wno-orphans #-}

-- | Definitions required to make record dot syntax work
--
-- This is supported in a more systematic manner in the main library as of
-- <https://github.com/well-typed/grapesy/pull/148>, but in order to keep this
-- repository match the actual episode, we have pinned the version of @grapesy@
-- that was used there.
module RecordDot (
    module RecordDot
  , module GHC.Records.Compat
  ) where

import Data.ByteString (ByteString)
import GHC.Records.Compat
import Network.GRPC.Common.Protobuf ((^.), (.~), (&))

import Proto.Kvstore

{-------------------------------------------------------------------------------
  HasField instances
-------------------------------------------------------------------------------}

instance HasField "key" CreateRequest ByteString where
  hasField x = (\y -> x & #key .~ y, x ^. #key)

instance HasField "key" RetrieveRequest ByteString where
  hasField x = (\y -> x & #key .~ y, x ^. #key)

instance HasField "key" UpdateRequest ByteString where
  hasField x = (\y -> x & #key .~ y, x ^. #key)

instance HasField "key" DeleteRequest ByteString where
  hasField x = (\y -> x & #key .~ y, x ^. #key)

instance HasField "value" CreateRequest ByteString where
  hasField x = (\y -> x & #value .~ y, x ^. #value)

instance HasField "value" RetrieveResponse ByteString where
  hasField x = (\y -> x & #value .~ y, x ^. #value)

instance HasField "value" UpdateRequest ByteString where
  hasField x = (\y -> x & #value .~ y, x ^. #value)

{-------------------------------------------------------------------------------
  Definitions required because of RebindableSyntax
-------------------------------------------------------------------------------}

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _ = t
ifThenElse False _ f = f


