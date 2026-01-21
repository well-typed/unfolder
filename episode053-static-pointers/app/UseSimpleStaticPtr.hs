module UseSimpleStaticPtr (main) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Data.IORef
import GHC.Generics (Generic)

import Static.Ptr.Simple (StaticPtr)
import Static.Ptr.Simple qualified as Static
import Util.Aeson

{-------------------------------------------------------------------------------
  Application state
-------------------------------------------------------------------------------}

newtype AppState = AppState (StaticPtr (App ()))
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type App a = IORef AppState -> IO a

resume :: AppState -> App ()
resume (AppState action) appStateRef = Static.deref action appStateRef

{-------------------------------------------------------------------------------
  Application proper
-------------------------------------------------------------------------------}

tick :: App ()
tick appState = do
    writeIORef appState $ AppState $ static tick
    putStrLn "Tick"
    threadDelay 1_000_000
    tock appState

tock :: App ()
tock appState = do
    writeIORef appState $ AppState $ static tock
    putStrLn "Tock"
    threadDelay 1_000_000
    tick appState

main :: IO ()
main = do
    appState <- newIORef $ AppState $ static tick
    finally
      ( tryReadJSON
         "suspended.json"
         (flip resume appState)
         (tick appState)
      )
      ( do st <- readIORef appState
           writeJSON "suspended.json" st
      )
