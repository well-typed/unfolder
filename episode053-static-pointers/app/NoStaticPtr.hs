module NoStaticPtr (main) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Data.IORef
import GHC.Generics (Generic)

import Util.Aeson

{-------------------------------------------------------------------------------
  Application state
-------------------------------------------------------------------------------}

data AppState =
    Tick
  | Tock
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type App a = IORef AppState -> IO a

resume :: AppState -> App ()
resume Tick = tick
resume Tock = tock

{-------------------------------------------------------------------------------
  Application proper
-------------------------------------------------------------------------------}

tick :: App ()
tick appState = do
    writeIORef appState $ Tick
    putStrLn "Tick"
    threadDelay 1_000_000
    tock appState

tock :: App ()
tock appState = do
    writeIORef appState $ Tock
    putStrLn "Tock"
    threadDelay 1_000_000
    tick appState

main :: IO ()
main = do
    appState <- newIORef Tick
    finally
      ( tryReadJSON
         "suspended.json"
         (flip resume appState)
         (tick appState)
      )
      ( do st <- readIORef appState
           writeJSON "suspended.json" st
      )
