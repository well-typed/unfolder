-- | Model
--
-- Intended for unqualified import.
module Test.Model (
    -- * Model
    Model
  , initModel
    -- * Execute actions
  , update
  , naive
  , emitted
  , visible
    -- * Generating actions
  , possibleActions
  ) where

import Data.Set (Set)
import Data.Set qualified as Set

import EurekaPROM.IO.Input (PedalEvent(..), Pedal(..), PedalState(..))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Our model simply records which pedals are currently pressed
type Model = Set Pedal

initModel :: Model
initModel = Set.empty

{-------------------------------------------------------------------------------
  Execute actions
-------------------------------------------------------------------------------}

update :: PedalEvent -> Model -> Model
update (PedalEvent pedal Press)   = Set.insert pedal
update (PedalEvent pedal Release) = Set.delete pedal

naive :: Model -> PedalEvent -> Maybe PedalEvent
naive _ = Just

emitted :: Model -> PedalEvent -> Maybe PedalEvent
emitted new (PedalEvent pedal Press)
  | pedal < maximum new = Nothing
  | otherwise           = Just $ PedalEvent pedal Press
emitted new (PedalEvent pedal Release)
  | Set.null new        = Just $ PedalEvent pedal Release
  | pedal < maximum new = Nothing
  | otherwise           = Just $ PedalEvent (maximum new) Press

visible :: Model -> PedalEvent -> Bool
visible old (PedalEvent pedal _)
  | Set.null old = True
  | otherwise    = pedal >= maximum old

{-------------------------------------------------------------------------------
  Generating actions
-------------------------------------------------------------------------------}

canPerform :: Model -> PedalEvent -> Bool
canPerform old event@(PedalEvent pedal state) = and[
      case state of
        Press   -> Set.notMember pedal old
        Release -> Set.member    pedal old
    , easyForHuman old event
    ]

possibleActions :: Model -> [PedalEvent]
possibleActions pressed = filter (canPerform pressed) . concat $ [
      [ PedalEvent pedal Press
      , PedalEvent pedal Release
      ]
    | pedal <- [minBound .. maxBound]
    ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Only execute events that are easy to do for the human test runner
easyForHuman :: Model -> PedalEvent -> Bool
easyForHuman old event =
    permitted (update event old)
  where
    permitted :: Set Pedal -> Bool
    permitted = flip elem . map Set.fromList . concat $ [
          [ [] ]
        , [ [Pedal1]
          , [Pedal2]
          , [Pedal4]
          , [Pedal5]
          ]
        , [ [Pedal1, Pedal4]
          , [Pedal1, Pedal5]
          , [Pedal2, Pedal4]
          , [Pedal2, Pedal5]
          ]
        ]
