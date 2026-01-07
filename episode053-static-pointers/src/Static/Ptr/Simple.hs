-- | Type-safe wrapper around 'GHC.StaticPtr' with some extra instances
--
-- Intended for qualified import.
--
-- > import Static.Ptr.Simple (StaticPtr)
-- > import Static.Ptr.Simple qualified as Static
module Static.Ptr.Simple (
    StaticPtr -- opaque
  , deref
  ) where

import Control.Monad ((>=>), guard)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson qualified as Aeson
import Data.String
import Data.Typeable
import GHC.Fingerprint
import GHC.Generics (Generic)
import GHC.StaticPtr qualified as GHC
import System.IO.Unsafe

import Util.Base64 qualified as Base64

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

newtype StaticPtr a = Wrap { unwrap :: GHC.StaticPtr a }

instance GHC.IsStatic StaticPtr where
  fromStaticPtr = Wrap

deref :: StaticPtr a -> a
deref = GHC.deRefStaticPtr . unwrap

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

instance Typeable a => Show (StaticPtr a) where
  show = show . Aeson.encode

instance Typeable a => IsString (StaticPtr a) where
  fromString = either error id . Aeson.eitherDecode . fromString

instance Typeable a => ToJSON (StaticPtr a) where
  toJSON = toJSON . toEncoded

instance Typeable a => FromJSON (StaticPtr a) where
  parseJSON = parseJSON >=> maybe (fail "could not decode") return . fromEncoded

{-------------------------------------------------------------------------------
  Internal auxiliary: serialization

  NOTE: We do a type check only for the outer type, not for any of the inner
  existentials. The outer type is by far the most critical: it is quite easy
  to try and deserialize a 'StaticPtr' at the wrong type at call sites, but
  the inner existentials are by definition not visible at call sites.
-------------------------------------------------------------------------------}

data Encoded = Encoded{
      typ :: Base64.AsStorable Fingerprint
    , key :: Base64.AsStorable GHC.StaticKey
    }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

toEncoded :: forall a. Typeable a => StaticPtr a -> Encoded
toEncoded (Wrap x) = Encoded{
      typ = Base64.WrapAsStorable $ typeRepFingerprint (typeRep (Proxy @a))
    , key = Base64.WrapAsStorable $ GHC.staticKey x
    }

fromEncoded :: forall a. Typeable a => Encoded -> Maybe (StaticPtr a)
fromEncoded encoded = do
    guard $ typ == typeRepFingerprint (typeRep (Proxy @a))
    unsafePerformIO $ fmap Wrap <$> GHC.unsafeLookupStaticPtr key
  where
    Encoded{
        typ = Base64.WrapAsStorable typ
      , key = Base64.WrapAsStorable key
      } = encoded
