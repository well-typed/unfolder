module ClientServer.FirstAttempt where

import Control.Monad (forM_)
import Control.Monad.Loops (unfoldM, whileJust_)
import Test.HUnit (assertEqual)

{-------------------------------------------------------------------------------
  Preliminaries
-------------------------------------------------------------------------------}

data Input  = SomeInput  deriving (Show, Eq)
data Output = SomeOutput deriving (Show, Eq)

{-------------------------------------------------------------------------------
  First version where the duality is not obvious

  N: non-streaming
  C: client-side streaming
  S: server-side streaming
  B: bidirectional streaming
-------------------------------------------------------------------------------}

type ServerN = Input -> IO Output
type ClientN = Input -> IO Output

{-------------------------------------------------------------------------------
  Example

  Some duality already here: on the server side we must /provide/ a handler,
  and on the client side we /get/ a handler.
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

{-------------------------------------------------------------------------------
  Client-side streaming: the client can send any number of Inputs, then the
  server responds with a single output.

  To understand the ClientC: we want to run an action which can send outputs
  whenever it wants, and when that action is done, we want to get the output
  from the server. So as a first approximation we might write

  > type ClientC = IO () -> IO Output

  But of course, that action must have a means to send inputs to the server,
  so it must be given an argument that allows it to do so; that argument has
  type

  > Maybe Input -> IO ()

  Putting those two things together gets us to 'ClientC'. Compare also to these
  two functions from the io-streams package (Gregory Collins and others):

  > makeInputStream  :: IO (Maybe a)       -> IO (InputStream a)
  > makeOutputStream :: (Maybe a -> IO ()) -> IO (OutputStream a)

  As we do in the episode, it may be instructive to /first/ make 'ServerC' and
  'ClientC' look more alike, and then use that to /derive/ 'ClientS' from
  'ServerS' and 'ClientB' from 'ServerB'.
-------------------------------------------------------------------------------}

type ServerC = IO (Maybe Input) -> IO Output

exampleServerC :: ServerC
exampleServerC recvInput = do
    _inputs <- unfoldM recvInput
    -- compute output from all inputs
    return SomeOutput

type ClientC = ((Maybe Input -> IO ()) -> IO ()) -> IO Output

exampleClientC :: ClientC -> IO ()
exampleClientC handler = do
    SomeOutput <- handler $ \sendInput -> do
      let allInputs :: [Input]
          allInputs = undefined
      mapM_ (sendInput . Just) allInputs
      sendInput Nothing
    return ()

{-------------------------------------------------------------------------------
  Server-side streaming
-------------------------------------------------------------------------------}

type ServerS = Input -> (Maybe Output -> IO ()) -> IO ()

exampleServerS :: ServerS
exampleServerS input sendOutput = do
    let allOutputs :: [Output]
        allOutputs = undefined input
    mapM_ (sendOutput . Just) allOutputs
    sendOutput Nothing

type ClientS = Input -> (IO (Maybe Output) -> IO ()) -> IO ()

exampleClientS :: ClientS -> IO ()
exampleClientS handler = do
    handler SomeInput $ \recvOutput -> do
      _allOutputs <- unfoldM recvOutput
      -- Process all outputs
      return ()

{-------------------------------------------------------------------------------
  Bidirectional streaming
-------------------------------------------------------------------------------}

type ServerB = (IO (Maybe Input), Maybe Output -> IO ()) -> IO ()
type ClientB = ((Maybe Input -> IO (), IO (Maybe Output)) -> IO ()) -> IO ()

-- Simple echo server
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
