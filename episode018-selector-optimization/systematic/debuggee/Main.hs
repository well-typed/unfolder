module Main where

import GHC.Debug.Stub

import LastBeforeBreak qualified

main :: IO ()
main = withGhcDebug LastBeforeBreak.main
