-- | Haskell Unfolder episode 49: Shrinking
--
-- Shrinking is a critical step in property based testing. When we generate
-- random inputs for functions in order to test them, those random inputs often
-- contain unnecessary and distracting detail. The purpose of shrinking is to
-- remove that noise, so that minimal test cases emerge. In this episode we will
-- see how to write shrinkers, discuss some of the pitfalls, and explore how we
-- can tackle some of the more subtle difficulties in writing good shrinkers.
module Main where

import Data.Map (Map)
import Data.Map qualified as Map
import Test.QuickCheck

import KeyValueStore (KeyValueStore)
import KeyValueStore qualified as KVS

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- Intentionally small domain: higher chance of collisions
data Key = A | B | C
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data Cmd =
    Insert Key Int
  | Lookup Key
  deriving stock (Show)

newtype Cmds = Cmds [Cmd]
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Property
-------------------------------------------------------------------------------}

prop :: Cmds -> Property
prop (Cmds cmds) = go Map.empty KVS.empty cmds
  where
    go :: Map Key Int -> KeyValueStore Key Int -> [Cmd] -> Property
    go _     _   []     = property True
    go model sut (c:cs) =
        case c of
          Insert key val ->
            go (Map.insert key val model) (KVS.insert key val sut) cs
          Lookup key -> conjoin [
              Map.lookup key model === KVS.lookup key sut
            , go model sut cs
            ]

{-------------------------------------------------------------------------------
  Arbitrary instances
-------------------------------------------------------------------------------}

allKeys :: [Key]
allKeys = [minBound .. maxBound]

instance Arbitrary Key where
  arbitrary = elements allKeys

  -- Without this, we might get counter-examples like
  --
  -- > Cmds [Insert A 0,Insert A 0,Insert C 1,Lookup A]
  --
  -- It's essential that that second key (C) is different from A, but it should
  -- ideally be B, not C.
  shrink x = filter (< x) allKeys

instance Arbitrary Cmd where
  arbitrary = oneof [
        Insert <$> arbitrary <*> arbitrary
      , Lookup <$> arbitrary
      ]

  -- The goal of shrinking is to ensure only relevant detail remains
  --
  -- NOTE: Avoid this pitfall:
  --
  -- > shrink (Insert key val) =
  -- >     Insert <$> shrink key <*> shrink val
  --
  -- We must shrink fields independently (otherwise we can only shrink if /both/
  -- fields are shrunk).
  shrink (Insert key val) = mconcat [
        [ Insert key' val  | key' <- shrink key ]
      , [ Insert key  val' | val' <- shrink val ]
      , [ Lookup key ]
      ]
  -- Here we could simplify the code; this is mostly for consistency.
  shrink (Lookup key) = mconcat [
        [ Lookup key' | key' <- shrink key ]
      ]

instance Arbitrary Cmds where
  arbitrary = Cmds <$> arbitrary
  -- Try with and without 'remap'
  shrink (Cmds cmds) = remap . Cmds <$> shrink cmds

remap :: Cmds -> Cmds
remap (Cmds cmds) = Cmds $ go Map.empty cmds
  where
    go :: Map Key Key -> [Cmd] -> [Cmd]
    go _ []     = []
    go m (c:cs) =
        case c of
          Lookup key ->
            let (m', key') = remapKey m key
            in Lookup key' : go m' cs
          Insert key val ->
            let (m', key') = remapKey m key
            in Insert key' val : go m' cs

    remapKey :: Map Key Key -> Key -> (Map Key Key, Key)
    remapKey m old =
        case Map.lookup old m of
          Just new -> (m, new)
          Nothing  ->
            let new = toEnum (Map.size m)
            in (Map.insert old new m, new)

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = quickCheck prop
