module Version1 (simulateWorkload) where

import Control.Monad
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Time
import System.Mem

-- import Control.Concurrent.MVar
import StrictMVar

type AppState = MVar (Map User UserInfo)
type User     = Text
type UserInfo = UTCTime

markActive :: AppState -> User -> IO ()
markActive appState user = modifyMVar_ appState $ \st -> do
    currentTime <- getCurrentTime
    return $ Map.insert user currentTime st

simulateWorkload :: IO ()
simulateWorkload = do
    appState :: AppState <- newMVar Map.empty
    forM_ (take 20000 $ cycle exampleUsers) $ \user -> do
      markActive appState user
      performMajorGC
  where
    exampleUsers :: [User]
    exampleUsers = ["John", "Mary", "Bill"]
