-- | Compositional and type-safe layer around @GHC.StaticPtr@
--
-- Intended for qualified import.
--
-- > import Static.Ptr (StaticPtr)
-- > import Static.Ptr qualified as Static
module Static.Ptr (
    StaticPtr -- opaque
    -- * Introduction
  , ap
    -- * Elimination
  , deref
    -- * Static dictionaries
  , HasInstance(..)
  , useInstance
  ) where

import Control.Monad ((>=>), guard)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson qualified as Aeson
import Data.Constraint (Dict(..))
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

data StaticPtr a where
  Pure :: GHC.StaticPtr a -> StaticPtr a
  Ap   :: StaticPtr (a -> b) -> StaticPtr a -> StaticPtr b

{-------------------------------------------------------------------------------
  Introduction
-------------------------------------------------------------------------------}

instance GHC.IsStatic StaticPtr where
  fromStaticPtr = Pure

ap :: StaticPtr (a -> b) -> StaticPtr a -> StaticPtr b
ap = Ap

{-------------------------------------------------------------------------------
  Elimination
-------------------------------------------------------------------------------}

deref :: StaticPtr a -> a
deref = go
  where
    go :: StaticPtr a -> a
    go (Pure x) = GHC.deRefStaticPtr x
    go (Ap f x) = go f $ go x

{-------------------------------------------------------------------------------
  Static class dictionaries
-------------------------------------------------------------------------------}

class HasInstance c where
  dict :: StaticPtr (Dict c)

useInstance :: forall c r. HasInstance c => (c => r) -> r
useInstance k = case deref (dict @c) of Dict -> k

{-------------------------------------------------------------------------------
  Semigroup
-------------------------------------------------------------------------------}

instance (Typeable a, HasInstance (Semigroup a)) => Semigroup (StaticPtr a) where
  a <> b = (static concatWithDict `ap` dict) `ap` a `ap` b

concatWithDict :: Dict (Semigroup a) -> a -> a -> a
concatWithDict Dict = (<>)

{-------------------------------------------------------------------------------
  Example instances
-------------------------------------------------------------------------------}

instance Typeable env => HasInstance (Semigroup (env -> IO ())) where
  dict = static Dict

instance HasInstance (FromJSON ())     where dict = static Dict
instance HasInstance (FromJSON String) where dict = static Dict

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
      typ  :: Base64.AsStorable Fingerprint
    , tree :: EncodedTree
    }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data EncodedTree =
    Leaf (Base64.AsStorable GHC.StaticKey)
  | Branch EncodedTree EncodedTree
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

toEncoded :: forall a. Typeable a => StaticPtr a -> Encoded
toEncoded ptr = Encoded{
      typ  = Base64.WrapAsStorable $ typeRepFingerprint (typeRep (Proxy @a))
    , tree = toEncodedTree ptr
    }

toEncodedTree :: StaticPtr a -> EncodedTree
toEncodedTree = \case
    Pure x -> Leaf $ Base64.WrapAsStorable $ GHC.staticKey x
    Ap f x -> Branch (toEncodedTree f) (toEncodedTree x)

fromEncoded :: forall a. Typeable a => Encoded -> Maybe (StaticPtr a)
fromEncoded encoded = do
    guard $ typ == typeRepFingerprint (typeRep (Proxy @a))
    unsafeFromEncodedTree tree
  where
    Encoded{
        typ = Base64.WrapAsStorable typ
      , tree
      } = encoded

unsafeFromEncodedTree :: EncodedTree -> Maybe (StaticPtr a)
unsafeFromEncodedTree = \case
    Leaf (Base64.WrapAsStorable x) ->
      unsafePerformIO $ fmap Pure <$> GHC.unsafeLookupStaticPtr x
    Branch f x ->
      Ap <$> unsafeFromEncodedTree f <*> unsafeFromEncodedTree x

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

_example :: StaticPtr (Int -> Int)
_example = static (+ 1)
