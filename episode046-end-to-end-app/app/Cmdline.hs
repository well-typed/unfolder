module Cmdline (
    Cmdline(..)
  , getCmdline
  ) where

import Options.Applicative

data Cmdline = Cmdline{
      -- | Number of spaces to add at each indentation level
      indent :: Int

      -- | Which characters should we regard as open brackets?
      --
      -- EXERCISE 1: We have this option in the 'Cmdline', but it is not
      -- actually used anywhere. Modify the definition of 'friendly' to use it.
      --
      -- EXERCISE 2: Add a similar command line option for 'closeBrackets' and
      -- 'separators'.
      --
      -- EXERCISE 3: If the user adds parenthesis ("(", ")") to to the
      -- 'openBrackets' and 'closeBrackets', then @OpenDoc()@ in the example
      -- JSON file will be rendered with a linebreak in between the two
      -- brackets. Add a special case to 'friendly' so that if the closing
      -- bracket immediately follows the open bracket, no linebreak is added.
    , openBrackets :: [Char]
    }
  deriving (Show)

parseCmdline :: Parser Cmdline
parseCmdline =
    pure Cmdline
      <*> option auto (short 'i' <> value 2)
      <*> option str (long "open" <> value "{[")

getCmdline :: IO Cmdline
getCmdline = execParser (info parseCmdline mempty)

