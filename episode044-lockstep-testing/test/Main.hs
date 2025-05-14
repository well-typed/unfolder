{-# OPTIONS_GHC -Wno-orphans #-}

-- | Haskell Unfolder episode 44: state-based testing with @quickcheck-lockstep@
--
-- Many Haskell programmers will be familiar with property based testing of pure
-- functions (for those who are not, various episodes of the Haskell Unfolder
-- have discussed this: #4, #21, #38 and #40). Property based testing for
-- _stateful_ systems ("IO code") is however much less well-known, which is a
-- pity as it is just as useful! In this episode we will demonstrate how we can
-- use `quickcheck-lockstep` to verify the responses we get from a simple
-- stateful API; as we will see, all of the lessons from property based testing
-- for pure functions can be applied in this stateful setting also.
module Main (main) where

import Control.Monad.Reader (runReaderT)
import Data.Proxy
import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.QuickCheck

import Test.QuickCheck.StateModel
import Test.QuickCheck.StateModel.Lockstep
import Test.QuickCheck.StateModel.Lockstep.Defaults qualified as Lockstep
import Test.QuickCheck.StateModel.Lockstep.Run      qualified as Lockstep

import EurekaPROM.IO.Input

import Test.SUT (SystemMonad, SystemPort)
import Test.SUT qualified as SUT
import Test.Model

{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

instance StateModel (Lockstep Model) where
  data Action (Lockstep Model) a where
    ActPedal :: PedalEvent -> Action (Lockstep Model) (Maybe PedalEvent)

  initialState    = Lockstep.initialState initModel
  nextState       = Lockstep.nextState
  arbitraryAction = Lockstep.arbitraryAction
  shrinkAction    = Lockstep.shrinkAction
  precondition    = Lockstep.precondition

instance InLockstep Model where
  data ModelValue Model a where
    MEvent :: Maybe PedalEvent -> ModelValue Model (Maybe PedalEvent)
  data Observable Model a where
    OEvent :: Maybe PedalEvent -> Observable Model (Maybe PedalEvent)

  usedVars _action = []
  observeModel (MEvent es) = OEvent es

  modelNextState (ActPedal action) _ctxt old =
      let new = update action old in (MEvent $ emitted new action, new)
  arbitraryWithVars _ctxt state = elements $
      map (Some . ActPedal) $ possibleActions state

instance RunModel (Lockstep Model) SystemMonad where
  postcondition = Lockstep.postcondition
  monitoring    = Lockstep.monitoring (Proxy @SystemMonad)

  perform model (ActPedal action) _ctxt = do
      SUT.showInstruction action
      if visible (getModel model) action
        then Just <$> SUT.getEvent
        else SUT.delay >> return Nothing

instance RunLockstep Model SystemMonad where
  observeReal _ (ActPedal _) res = OEvent res

deriving stock instance Show (Action (Lockstep Model) a)
deriving stock instance Eq   (Action (Lockstep Model) a)

deriving stock instance Show (ModelValue Model a)
deriving stock instance Show (Observable Model a)
deriving stock instance Eq   (Observable Model a)

{-------------------------------------------------------------------------------
  Application top-level
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMainWithIngredients ingredients $
    -- We need the MIDI port to listen to
    askOption $ \mPortSpec ->
    -- Disable shrinking (too slow due to human involvement in test execution)
    localOption (QuickCheckMaxShrinks 0) $
    -- A few tests are sufficient (each test is itself many tests)
    localOption (QuickCheckTests 2) $
    -- Setup the actual lockstep test
    testGroup "ep44" [
        testProperty "lockstep" $
          Lockstep.runActionsBracket
            (Proxy @Model)
            (SUT.initialize mPortSpec)
            SUT.terminate
            runReaderT
      ]
  where
    ingredients :: [Ingredient]
    ingredients =
          includingOptions [Option (Proxy @SystemPort)]
        : defaultIngredients
