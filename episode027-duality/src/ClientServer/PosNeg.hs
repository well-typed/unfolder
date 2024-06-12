module ClientServer.PosNeg where

import Control.Monad (forM_)
import Control.Monad.Loops (unfoldM, whileJust_)
import Test.HUnit (assertEqual)

{-------------------------------------------------------------------------------
  Same preliminaries
-------------------------------------------------------------------------------}

data Input  = SomeInput  deriving (Show, Eq)
data Output = SomeOutput deriving (Show, Eq)

{-------------------------------------------------------------------------------
  Our second version looks a bit better than the first, but the duality is
  still not obvious:

  > type Send a = Maybe a -> IO ()
  > type Recv a = IO (Maybe a)
  >
  > type ServerN = Input -> IO Output
  > type ClientN = Input -> IO Output
  >
  > type ServerC = Recv Input -> IO Output
  > type ClientC = (Send Input -> IO ()) -> IO Output
  >
  > type ServerS = Input -> Send Output -> IO ()
  > type ClientS = Input -> (Recv Output -> IO ()) -> IO ()
  >
  > type ServerB = (Recv Input, Send Output) -> IO ()
  > type ClientB = ((Send Input, Recv Output) -> IO ()) -> IO ()

  Let's try to abstract out that inversion of control that we do in the client.
  When we do, suddenly the duality emerges from obscurity: we can now understand
  "across the duality", leading to a much improved user experience of our
  library, and giving us peace of mind the interface we offer makes sense.

  Side note: this example is a simplified version of what we use in `grapesy`
  to define the types of server and client handlers. There we use a slightly
  different definition:

  > type    Positive m a b = a -> m b
  > newtype Negative m a b = Negative (forall r. (a -> m r) -> m (b, r))

  The advantage of this more general definition is that it allows the client
  to return a result once it is is done with the server; in the version we
  discuss in the episode, the client can only return unit:

  > type ClientC = (Send Input -> IO ()) -> IO Output
  >                               ^^^^^ this is restrictive
-------------------------------------------------------------------------------}

type Send a = Maybe a -> IO ()
type Recv a = IO (Maybe a)

type Neg a = a -> IO ()
type Pos a = a

type ServerN = Input -> IO Output
type ClientN = Input -> IO Output

type ServerC = Pos (Recv Input) -> IO Output
type ClientC = Neg (Send Input) -> IO Output

type ServerS = Input -> Pos (Send Output) -> IO ()
type ClientS = Input -> Neg (Recv Output) -> IO ()

type ServerB = Pos (Recv Input, Send Output) -> IO ()
type ClientB = Neg (Send Input, Recv Output) -> IO ()

{-------------------------------------------------------------------------------
  Just to show that we haven't changed anything, here are the example clients
  and servers again from the previous version, with no modifications at all.
-------------------------------------------------------------------------------}

exampleServerN :: ServerN
exampleServerN SomeInput = do
    -- handle the request
    return SomeOutput

exampleClientN :: ClientN -> IO ()
exampleClientN handler = do
    SomeOutput <- handler SomeInput
    -- do something with the result
    return ()

exampleServerC :: ServerC
exampleServerC recvInput = do
    _inputs <- unfoldM recvInput
    -- compute output from all inputs
    return SomeOutput

exampleClientC :: ClientC -> IO ()
exampleClientC handler = do
    SomeOutput <- handler $ \sendInput -> do
      let allInputs :: [Input]
          allInputs = undefined
      mapM_ (sendInput . Just) allInputs
      sendInput Nothing
    return ()

exampleServerS :: ServerS
exampleServerS input sendOutput = do
    let allOutputs :: [Output]
        allOutputs = undefined input
    mapM_ (sendOutput . Just) allOutputs
    sendOutput Nothing

exampleClientS :: ClientS -> IO ()
exampleClientS handler = do
    handler SomeInput $ \recvOutput -> do
      _allOutputs <- unfoldM recvOutput
      -- Process all outputs
      return ()

exampleServerB :: ServerB
exampleServerB (recvInput, sendOutput) = do
    whileJust_ recvInput $ \SomeInput -> sendOutput (Just SomeOutput)
    sendOutput Nothing

exampleClientB :: ClientB -> IO ()
exampleClientB handler =
    handler $ \(sendInput, recvOutput) -> do
      let allInputs :: [Input]
          allInputs = undefined

      -- Verify that each message is echoed
      forM_ allInputs $ \inp -> do
        sendInput (Just inp)
        mOut <- recvOutput
        assertEqual "echo" mOut $ Just SomeOutput

      -- Verify that the server handler terminates when we do
      sendInput Nothing
      mOut <- recvOutput
      assertEqual "terminate" mOut $ Nothing
