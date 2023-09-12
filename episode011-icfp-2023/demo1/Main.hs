{-
  Demo 1. This demo illustrates the presentation:

  * HasChor: Functional Choreographic Programming for All
    (Gan Shen, Shun Kashiwa, Lindsey Kuper)

    - Library: <https://github.com/gshen42/HasChor>
    - Paper: <https://dl.acm.org/doi/10.1145/3607849>
    - Presentation: <https://icfp23.sigplan.org/details/icfp-2023-papers/19/HasChor-Functional-Choreographic-Programming-for-All-Functional-Pearl->
-}

{-# LANGUAGE DataKinds #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Proxy

import Choreography
import Choreography.Location

{-------------------------------------------------------------------------------
  Roles
-------------------------------------------------------------------------------}

server :: Proxy "server"
client :: Proxy "client"

server = Proxy
client = Proxy

{-------------------------------------------------------------------------------
  Messages
-------------------------------------------------------------------------------}

data Instruction = Get | Incr
  deriving (Show, Read)

data Response = Current Int | Done
  deriving (Show, Read)

{-------------------------------------------------------------------------------
  Choreography proper
-------------------------------------------------------------------------------}

choreography :: MVar Bool @ "client" -> MVar Int @ "server" -> Choreo IO ()
choreography clientState serverState =
    loop
  where
    loop :: Choreo IO ()
    loop = do
        msg :: Instruction @ "client" <-
          client `locally` \un -> do
            threadDelay 1_000_000
            modifyMVar (un clientState) $ \b -> return $
              case b of
                False -> (True, Get)
                True  -> (False, Incr)

        cond (client, msg) $ \instr -> do
          response :: Response @ "server" <-
            server `locally` \un -> do
              modifyMVar (un serverState) $ \i -> return $
                case instr of
                  Get  -> (i, Current i)
                  Incr -> (succ i, Done)

          response' :: Response @ "client" <-
            (server, response) ~> client
          void $ client `locally` \un ->
            print (un response')

          loop

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    clientState <- newMVar False
    serverState <- newMVar 0

    let clientProcess, serverProcess :: IO ()
        clientProcess =
          runChoreography
            cfg
            (choreography (Wrap clientState) Empty)
            "client"
        serverProcess =
          runChoreography
            cfg
            (choreography Empty (Wrap serverState))
            "server"

    withAsync serverProcess $ \_ ->
      withAsync clientProcess $ \_ ->
        threadDelay 100_000_000
  where
    cfg :: HttpConfig
    cfg = mkHttpConfig [
        ("server", ("localhost", 12000))
      , ("client", ("localhost", 12001))
      ]


