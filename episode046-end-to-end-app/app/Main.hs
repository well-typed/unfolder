-- | Haskell Unfolder episode 46: developing an application from scratch
--
-- In this episode targeted at beginners, we show the end-to-end application
-- development process, starting from an empty directory. We'll consider package
-- configuration, taking advantage of editor integration, how to deal with
-- dependencies, organizing code into modules, and parsing command line
-- arguments. We will use this to write a simple but useful application.
module Main (main) where

import Cmdline

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmdline <- getCmdline
    interact $ friendly cmdline

{-------------------------------------------------------------------------------
  Core logic
-------------------------------------------------------------------------------}

friendly :: Cmdline -> String -> String
friendly cmdline = friendlyAt 0
  where
    friendlyAt :: Int -> [Char] -> [Char]
    friendlyAt _ []     = []
    friendlyAt i (c:cs)
      | elem c "{[" = [c] ++ newline (i + 1) ++ friendlyAt (i + 1) cs
      | elem c "}]" = newline (i - 1) ++ [c] ++ friendlyAt (i - 1) cs
      | c == ','    = newline i ++ [c] ++ friendlyAt i cs
      | otherwise   = c : friendlyAt i cs

    newline :: Int -> String
    newline i = "\n" ++ replicate (i * cmdline.indent) ' '
