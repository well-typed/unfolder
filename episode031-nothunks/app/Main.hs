module Main (main) where

import System.Environment

import Version1 qualified
import Version2 qualified
import Remarks qualified

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["version1"] -> Version1.simulateWorkload
      ["version2"] -> Version2.simulateWorkload
      ["remarks"]  -> Remarks.testHandwritten
      _otherwise -> putStrLn "Unknown version"