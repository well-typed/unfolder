{-
  Demo 2. This demo illustrates two papers:

  * An Exceptional Actor System (Patrick Redmond, Lindsey Kuper)

    - Presentation: https://icfp23.sigplan.org/details/haskellsymp-2023/3/An-Exceptional-ActorId-System-Functional-Pearl-
    - Paper: https://dl.acm.org/doi/10.1145/3609026.3609728

  * GHC Plugin for Setting Breakpoints (Aaron Allen)

    - Library: <https://hackage.haskell.org/package/breakpoint>
    - Presentation: <https://icfp23.sigplan.org/details/hiw-2023/4/GHC-Plugin-for-Setting-Breakpoints>
-}

{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Data.Dynamic
import Type.Reflection

import Debug.Breakpoint

{-------------------------------------------------------------------------------
  Implementation

  We follow the spirit of the paper, but not the letter.
-------------------------------------------------------------------------------}

-- | Actor ID
--
-- Actions will be represented by threads.
type ActorId = ThreadId

-- | Message sent to an actor
data Envelope = Envelope {
      sender  :: ActorId
    , message :: Dynamic
    }
  deriving (Show, Exception)

-- | Exception raised when actor receives an unsupported message type
data WrongMsg = WrongMsg SomeTypeRep
  deriving (Show, Exception)

-- | Process incoming messages
newtype Intent = Intent { runIntent :: ActorId -> Dynamic -> IO () }

-- | Send message
send :: Typeable msg => ActorId -> msg -> IO ()
send recipient msg = do
    sender <- myThreadId
    throwTo recipient $ Envelope sender (toDyn msg)

-- | Construct 'Intent'
--
-- Multiple message types can be handled with the 'Intent' 'Semigroup' instance.
mkIntent :: Typeable a => (ActorId -> a -> IO ()) -> Intent
mkIntent f = Intent $ \sender dyn ->
    case fromDynamic dyn of
      Just x  -> f sender x
      Nothing -> throwIO $ WrongMsg (dynTypeRep dyn)

instance Semigroup Intent where
  i1 <> i2 = Intent $ \sender dyn ->
      catch (runIntent i1 sender dyn) $ \(WrongMsg _) ->
        runIntent i2 sender dyn

-- | Spawn a new actor
--
-- This is slightly different from the paper: we /ensure/ that the new thread
-- is started with async exceptions masked.
--
-- We don't use the @async@ library to stay closer to the intent of the paper:
-- use base only.
spawnActor :: Intent -> IO ActorId
spawnActor intent =
    mask_ $ forkIO $ loop []
  where
    loop :: [Envelope] -> IO ()
    loop inbox =
        handle (\env -> loop (inbox ++ [env])) $
          case inbox of
            [] -> do
              threadDelay 60_000_000
              loop inbox
            env:inbox' -> do
              catch (runIntent intent (sender env) (message env)) $ \err ->
                throwTo (sender env) (err :: WrongMsg)
              loop inbox'

{-------------------------------------------------------------------------------
  Reimplementation of the protocol of demo1, but using actors
-------------------------------------------------------------------------------}

data Instruction = Get | Incr
  deriving (Show, Read)

data Response = Current Int | Done
  deriving (Show, Read)

data ClientStart = ClientStart

server :: MVar Int -> Intent
server serverState =
    processInstruction
  where
    processInstruction :: Intent
    processInstruction = mkIntent $ \sender instr -> do
        -- breakpointIO
        response <- modifyMVar serverState $ \i -> return $
          case instr of
            Get  -> (i, Current i)
            Incr -> {- let i_even = even i in breakpoint -} (succ i, Done)
        send sender response

client :: MVar Bool -> ActorId -> Intent
client clientState serverId =
    processStart <> processResponse
  where
    processStart :: Intent
    processStart = mkIntent $ \_sender ClientStart ->
        go

    processResponse :: Intent
    processResponse = mkIntent $ \_sender (response :: Response) -> do
        print response
        go

    go :: IO ()
    go = do
        threadDelay 1_000_000
        instr <- modifyMVar clientState $ \b -> return $
                   case b of
                     False -> (True, Get)
                     True  -> (False, Incr)
        send serverId instr

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    clientState <- newMVar False
    serverState <- newMVar 0

    serverId <- spawnActor $ server serverState
    clientId <- spawnActor $ client clientState serverId

    send clientId ClientStart
    threadDelay 100_000_000
