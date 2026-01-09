module Main where

import Control.Monad
import Control.Selective
import System.Directory
import System.FilePath

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data FileAction a where
  Rename :: FilePath -> FilePath -> FileAction ()
  Exists :: FilePath -> FileAction Bool
  Done   :: a -> FileAction a
  FMap   :: (a -> b) -> FileAction a -> FileAction b
  Select :: FileAction (Either a b) -> FileAction (a -> b) -> FileAction b

whenAction :: FileAction Bool -> FileAction () -> FileAction ()
whenAction check action =
    Select
      ((\x -> if x then Left () else Right ()) <$> check)
      ((\() () -> ()) <$> action)

instance Functor FileAction where
  fmap = FMap

instance Applicative FileAction where
  pure  = Done
  (<*>) = apS

instance Selective FileAction where
  select = Select

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

example1 :: FileAction ()
example1 = Rename "./sandbox/a.txt" "./sandbox/b.txt"

example2 :: FileAction ()
example2 =
       Rename "./sandbox/a.txt" "./sandbox/b.txt"
    *> Rename "./sandbox/b.txt" "./sandbox/a.txt"

example3 :: FileAction Bool
example3 = Exists "./sandbox/a.txt"

example3b :: FileAction Bool
example3b = not <$> example3

example4 :: FileAction Bool
example4 = pure (&&) <*> Exists "./sandbox/a.txt" <*> Exists "./sandbox/README.md"

example5 :: FileAction ()
example5 =
    whenS
      (Exists "./sandbox/a.txt")
      (Rename "./sandbox/a.txt" "./sandbox/b.txt")

example6 :: FileAction ()
example6 =
    ifS
      (Exists "./sandbox/a.txt")
      (Rename "./sandbox/a.txt" "./sandbox/b.txt")
      (Rename "./sandbox/b.txt" "./sandbox/a.txt")

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

runFileAction :: FileAction a -> IO a
runFileAction = \case
    Rename fr to ->
      renameFile fr to
    Exists path ->
      doesFileExist path
    Done x ->
      return x
    FMap f action -> do
      a <- runFileAction action
      return $ f a
    Select l r -> do
      ab <- runFileAction l
      case ab of
        Right b -> return b
        Left  a -> do
          f <- runFileAction r
          return (f a)

{-------------------------------------------------------------------------------
  Audit
-------------------------------------------------------------------------------}

checkIsSafe :: FilePath -> FileAction a -> IO ()
checkIsSafe sandbox = check
  where
    check :: FileAction a -> IO ()
    check = \case
        Rename fr to -> do
          checkInSandbox fr
          checkInSandbox to
        Exists path ->
          checkInSandbox path
        Done _ ->
          return ()
        FMap _ action ->
          check action
        Select l r -> do
          check l
          check r

    checkInSandbox :: FilePath -> IO ()
    checkInSandbox path = do
        path' <- canonicalizePath path
        when (takeDirectory path' /= sandbox) $
          fail $ "Not in sandbox: " ++ path

{-------------------------------------------------------------------------------
  Main application entry point
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    sandbox <- canonicalizePath "./sandbox"
    checkIsSafe sandbox example
    print =<< runFileAction example
  where
    example = example6

