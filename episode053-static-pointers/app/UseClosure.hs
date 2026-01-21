module UseClosure (main) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Data.IORef
import Data.Time
import GHC.Generics (Generic)

import Static.Closure
import Util.Aeson

{-------------------------------------------------------------------------------
  Application state
-------------------------------------------------------------------------------}

newtype AppState = AppState (Closure (App ()))
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type App a = IORef AppState -> IO a

resume :: AppState -> App ()
resume (AppState action) appStateRef = fromClosure action appStateRef

{-------------------------------------------------------------------------------
  Modify application state
-------------------------------------------------------------------------------}

markAppState :: App ()
markAppState appState = do
    currentTime <- show <$> getCurrentTime
    modifyIORef appState $ \(AppState app) -> AppState $
         closure (static printHere) currentTime
      <> app

printHere :: String -> App ()
printHere currentTime _appState = putStrLn $ "HERE at " ++ currentTime

{-------------------------------------------------------------------------------
  Application proper
-------------------------------------------------------------------------------}

tick :: App ()
tick appState = do
    writeIORef appState $ AppState $ static tick
    putStrLn "Tick"
    markAppState appState
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
