-- | System under test
--
-- Intended for qualified import.
--
-- > import Test.SUT (SystemMonad, SystemPort)
-- > import Test.SUT qualified as SUT
module Test.SUT (
    -- * Monad
    SystemMonad
    -- * Command line arguments
  , SystemPort -- opaque
    -- * Initialization and termination
  , initialize
  , terminate
    -- * Interaction
  , showInstruction
  , getEvent
  , delay
  ) where

import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT(..))
import System.Console.ANSI
import System.Timeout
import Test.Tasty.Options

import EurekaPROM.IO.ALSA qualified as ALSA
import EurekaPROM.IO.Input

{-------------------------------------------------------------------------------
  Monad
-------------------------------------------------------------------------------}

type SystemMonad = ReaderT ALSA.Handle IO

{-------------------------------------------------------------------------------
  Command line arguments
-------------------------------------------------------------------------------}

data SystemPort =
    NoSystemPort
  | SystemPort ALSA.PortSpec

instance IsOption SystemPort where
  defaultValue = NoSystemPort
  optionName   = pure "alsa-port"
  parseValue   = Just . SystemPort .  ALSA.PortInputOnly
  optionHelp   = pure "ALSA input port"

{-------------------------------------------------------------------------------
  Initialization and termination
-------------------------------------------------------------------------------}

initialize :: SystemPort -> IO ALSA.Handle
initialize NoSystemPort = do
    ports <- bracket ALSA.open ALSA.close $ \h -> ALSA.getPortNames h
    error $ "No port specified. Available:\n" ++ unlines ports
initialize (SystemPort portSpec) = do
    alsaHandle <- ALSA.open
    ALSA.resolve alsaHandle portSpec
    ALSA.dropInput alsaHandle
    putStrLn "System initialized"
    return alsaHandle

terminate :: ALSA.Handle -> IO ()
terminate alsaHandle = do
    setSGR [SetColor Foreground Dull Red]
    putStrLn "Please release all pedals."
    setSGR [Reset]
    delay

    ALSA.dropInput alsaHandle
    ALSA.close alsaHandle

    putStrLn "System terminated"

{-------------------------------------------------------------------------------
  Interaction
-------------------------------------------------------------------------------}

getEvent :: SystemMonad PedalEvent
getEvent = ReaderT $ \alsaHandle ->
    ALSA.waitInputUsing pedalEventFromMIDI alsaHandle

showInstruction :: MonadIO m => PedalEvent -> m ()
showInstruction action = liftIO $ do
    case action of
      PedalEvent pedal Press -> do
        setSGR [SetColor Foreground Dull Green]
        putStrLn $ "Please press " ++ show pedal
      PedalEvent pedal Release -> do
        setSGR [SetColor Foreground Dull Red]
        putStrLn $ "Please release " ++ show pedal
    setSGR [Reset]

delay :: MonadIO m => m ()
delay = liftIO $ do
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn "Continuing after delay or key press."
    setSGR [Reset]
    void $ timeout waitTime $ getLine
  where
    waitTime :: Int
    waitTime = 3_000_000
