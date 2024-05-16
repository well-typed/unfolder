{-# OPTIONS_GHC -Wno-orphans #-}

-- | Definitions required to make record dot syntax work
--
-- We could conceivably do this better in @grapesy@ proper, but for the sake
-- of the Unfolder episode, these definitions suffice.
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


