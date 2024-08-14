module Remarks where

import Data.Time
import GHC.Generics
import NoThunks.Class

{-------------------------------------------------------------------------------
  Suppose we have some datatype defined in a 3rdparty library, which does not
  have a 'NoThunks' instance:
-------------------------------------------------------------------------------}

data SomeOtherInfo

{-------------------------------------------------------------------------------
  We can omit checks for 'otherInfo', but be careful with this! A value of any
  time could potentially retain (in its computation) a value of another type:
  we cannot in general predict what the consequences of such an omission are.
-------------------------------------------------------------------------------}

data UserInfo = UserInfo {
      lastActive :: !UTCTime
    , visits     :: !Word
    , otherInfo  :: !SomeOtherInfo
    }
  deriving stock (Generic)
  deriving NoThunks via AllowThunksIn '["otherInfo"] UserInfo

{-------------------------------------------------------------------------------
  The other alternative is to provide instances for the type. If generics are
  available, this is often not difficult, if perhaps cumbersome (if there are
  lots of nested types). Alternative, we can use 'InspectHeap'; the downside is
  that we get less information about the precise nature of the thunk.
-------------------------------------------------------------------------------}

deriving
  via InspectHeap SomeOtherInfo
  instance NoThunks SomeOtherInfo

{-------------------------------------------------------------------------------
  The other reason for the class mechanism (rather than just inspecting the
  heap) is that some specialized data types /rely/ on thunks for their amortized
  performance; 'FingerTree' is one example. Such datatypes will need to be given
  careful 'NoThunks' instances. Hand-writing instances is not hard:
-------------------------------------------------------------------------------}

data FooBar = Foo Int Int | Bar Int

instance NoThunks FooBar where
  -- The 'noThunks' method uses some dark magic to figure out if the argumet is
  -- in WHNF. If it isn't, it reports the thunk; if it is, it calls wNoThunks.
  -- Since we know the argument is in WHNF, we can pattern match on it (it would
  -- be bad if checking for thunks would itself force thunks!).
  --
  -- A handwritten instance gives us an opportunity to custom information into
  -- the context.
  wNoThunks ctxt (Foo a b) = allNoThunks [
        noThunks ("a" : "Foo" : ctxt) a
      , noThunks ("b" : "Foo" : ctxt) b
      ]
  wNoThunks ctxt (Bar a) = noThunks ("Bar" : ctxt) a

  showTypeOf _ = "FooBar"

testHandwritten :: IO ()
testHandwritten = do
    -- Note that this works just fine, the thunk is reported in "a".
    -- This shows that 'noThunks' indeed does not force any thunks.
    let fooBar = Foo undefined 2
    print =<< noThunks [] fooBar

