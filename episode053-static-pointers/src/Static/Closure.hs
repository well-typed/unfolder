-- | Closures
--
-- Intended for unqualified import.
module Static.Closure (
    Closure -- opaque
  , closure
  , fromClosure
  ) where

import Data.Constraint (Dict(..))
import Data.Text (Text)
import Data.Typeable
import GHC.Generics (Generic)
import GHC.StaticPtr qualified as GHC

import Static.Ptr (StaticPtr)
import Static.Ptr qualified as Static
import Util.Aeson

{-------------------------------------------------------------------------------
  Closures

  NOTE: Here and elsewhere we use a textual (JSON rather than binary
  representation, for educational purposes. In a "real" implementation you'll
  probably want a more compact representation.
-------------------------------------------------------------------------------}

data Closure a = Closure (StaticPtr (Text -> a)) Text
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{-------------------------------------------------------------------------------
  Elimination
-------------------------------------------------------------------------------}

fromClosure :: Closure a -> a
fromClosure (Closure f env) = Static.deref f env

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

instance GHC.IsStatic Closure where
  fromStaticPtr = trivialClosure . GHC.fromStaticPtr

closure :: forall env a.
     (Typeable env, Typeable a, Static.HasInstance (FromJSON env), ToJSON env)
  => StaticPtr (env -> a) -> env -> Closure a
closure f env =
    Closure
      (static withDecodedEnv `Static.ap` Static.dict `Static.ap` f)
      (encodeText env)

trivialClosure :: Typeable a => StaticPtr a -> Closure a
trivialClosure ptr = closure (static withTrivialEnv `Static.ap` ptr) ()

{-------------------------------------------------------------------------------
  Internal auxiliary: construction
-------------------------------------------------------------------------------}

withDecodedEnv :: Dict (FromJSON env) -> (env -> a) -> (Text -> a)
withDecodedEnv Dict f encodedEnv =
    case decodeText encodedEnv of
      Left  err -> error err
      Right env -> f env

withTrivialEnv :: a -> () -> a
withTrivialEnv x () = x

{-------------------------------------------------------------------------------
  Semigroup
-------------------------------------------------------------------------------}

instance (Typeable a, Static.HasInstance (Semigroup a))
      => Semigroup (Closure a) where
  Closure f1 env1 <> Closure f2 env2 =
      Closure
        (   static withCombinedEnv
          `Static.ap`
            Static.dict
          `Static.ap`
            f1
          `Static.ap`
            f2
        )
        (encodeText (env1, env2))

withCombinedEnv ::
     Dict (Semigroup a)
  -> (Text -> a)
  -> (Text -> a)
  -> (Text -> a)
withCombinedEnv Dict f1 f2 combinedEnv =
    case decodeText combinedEnv of
      Left  err          -> error err
      Right (env1, env2) -> f1 env1
                         <> f2 env2
