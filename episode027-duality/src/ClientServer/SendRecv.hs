module ClientServer.SendRecv where

import Control.Monad (forM_)
import Control.Monad.Loops (unfoldM, whileJust_)
import Test.HUnit (assertEqual)

{-------------------------------------------------------------------------------
  Same preliminaries
-------------------------------------------------------------------------------}

data Input  = SomeInput  deriving (Show, Eq)
data Output = SomeOutput deriving (Show, Eq)

{-------------------------------------------------------------------------------
  So far we have

  > type ServerN = Input -> IO Output
  > type ClientN = Input -> IO Output
  >
  > type ServerC = IO (Maybe Input) -> IO Output
  > type ClientC = ((Maybe Input -> IO ()) -> IO ()) -> IO Output
  >
  > type ServerS = Input -> (Maybe Output -> IO ()) -> IO ()
  > type ClientS = Input -> (IO (Maybe Output) -> IO ()) -> IO ()
  >
  > type ServerB = (IO (Maybe Input), Maybe Output -> IO ()) -> IO ()
  > type ClientB = ((Maybe Input -> IO (), IO (Maybe Output)) -> IO ()) -> IO ()

  But this is a mess. It's very hard to be sure that this actually makes sense.
  There should be some kind of duality between client and server, and it's not
  at all obvious from this definition what that is. Let's try to improve this.

  As a first step, let's make the distinction between sending and receiving
  clearer.

  Side note: We can see the duality between 'Send' and 'Recv' more clearly
  when we define

  > type Send a = Maybe a -> IO ()
  > type Recv a = () -> IO (Maybe a)

  or perhaps even

  > Send a = Kleisli IO (Maybe a) ()
  > Recv a = Kleisli IO () (Maybe a)

  (<https://hackage.haskell.org/package/base/docs/Control-Arrow.html#t:Kleisli>)
-------------------------------------------------------------------------------}

type Send a = Maybe a -> IO ()
type Recv a = IO (Maybe a)

type ServerN = Input -> IO Output
type ClientN = Input -> IO Output

type ServerC = Recv Input -> IO Output
type ClientC = (Send Input -> IO ()) -> IO Output

type ServerS = Input -> Send Output -> IO ()
type ClientS = Input -> (Recv Output -> IO ()) -> IO ()

type ServerB = (Recv Input, Send Output) -> IO ()
type ClientB = ((Send Input, Recv Output) -> IO ()) -> IO ()

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
