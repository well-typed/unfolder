module Version2 (simulateWorkload) where

import Control.DeepSeq
import Control.Monad
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time
import GHC.Generics (Generic)
import NoThunks.Class
import System.Mem

import StrictMVar

{-------------------------------------------------------------------------------
  Application state

  Progression:

  * Use Map.Strict
  * Put bang on 'lastActive'
  * Use 'force' on 'getCurrentTime'
  * Put bang on 'visits'
-------------------------------------------------------------------------------}

type AppState = MVar (Map User UserInfo)
type User     = Text

data UserInfo = UserInfo {
      lastActive :: !UTCTime
    , visits     :: !Word
    }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

markActive :: AppState -> User -> IO ()
markActive appState user = modifyMVar_ appState $ \st -> do
    -- The fact that 'force' is necessary is arguably a bug in @base@.
    currentTime <- force <$> getCurrentTime

    let firstVisit :: UserInfo
        firstVisit = UserInfo {
              lastActive = currentTime
            , visits     = 1
            }

        subsequent :: UserInfo -> UserInfo
        subsequent info = UserInfo {
            lastActive = currentTime
          , visits     = visits info + 1
          }

    return $ Map.alter (Just . maybe firstVisit subsequent) user st

simulateWorkload :: IO ()
simulateWorkload = do
    appState :: AppState <- newMVar Map.empty
    forM_ (take 20000 $ cycle exampleUsers) $ \user -> do
      markActive appState user
      performMajorGC
  where
    exampleUsers :: [User]
    exampleUsers = ["John", "Mary", "Bill"]
