{-
  Unfolder Episode 10: generalBracket

  References/notes:

  - https://www.fpcomplete.com/blog/monadmask-vs-monadbracket/
  - https://markkarpov.com/tutorial/exceptions.html
  - https://www.yesodweb.com/blog/2014/06/exceptions-transformers
  - https://ro-che.info/articles/2014-07-30-bracket
  - https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/asynch-exns.pdf
    "Asynchronous Exceptions in Haskell"
  - https://simonmar.github.io/bib/papers/ext-exceptions.pdf
    (not /directly/ relevant, discusses the exception hierarchy)
  - https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/
    "Parallel and Concurrent Programming in Haskell"
    specifically https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch09.html
    "Chapter 9. Cancellation and Timeouts";
    we do not discuss interruptible operations in this episode.
  - Also outside the scope of this episode: atomicity of throwTo
-}

module Main where

import Control.Monad.Except
import Control.Monad.Catch (throwM)
import System.Environment

import Control.Monad.Catch qualified as Exceptions
import MonadBracket3       qualified as OurDefinition

demoA :: Exceptions.MonadMask m => m b -> m c -> m (b, c)
demoA use release =
    Exceptions.generalBracket
      (return ())
      (\() _ -> release)
      (\()   -> use)

demoB :: OurDefinition.MonadBracket m => m b -> m c -> m c
demoB use release =
    OurDefinition.generalBracket
      (return ())
      (\() _ -> release)
      (\()   -> use)

demoA1, demoA2, demoA3, demoA4, demoA5, demoA6, demoA7, demoA8, demoA9 :: ExceptT String IO ((), ())
demoA1 = demoA (return ())                              (return ())
demoA2 = demoA (return ())                              (throwError "release error")
demoA3 = demoA (return ())                              (throwM $ userError "release exception")
demoA4 = demoA (throwError "release error")             (return ())
demoA5 = demoA (throwError "release error")             (throwError "release error")
demoA6 = demoA (throwError "release error")             (throwM $ userError "release exception")
demoA7 = demoA (throwM $ userError "release exception") (return ())
demoA8 = demoA (throwM $ userError "release exception") (throwError "release error")
demoA9 = demoA (throwM $ userError "release exception") (throwM $ userError "release exception")

demoB1, demoB2, demoB3, demoB4, demoB5, demoB6, demoB7, demoB8, demoB9 :: ExceptT String IO ()
demoB1 = demoB (return ())                              (return ())
demoB2 = demoB (return ())                              (throwError "release error")
demoB3 = demoB (return ())                              (throwM $ userError "release exception")
demoB4 = demoB (throwError "release error")             (return ())
demoB5 = demoB (throwError "release error")             (throwError "release error")
demoB6 = demoB (throwError "release error")             (throwM $ userError "release exception")
demoB7 = demoB (throwM $ userError "release exception") (return ())
demoB8 = demoB (throwM $ userError "release exception") (throwError "release error")
demoB9 = demoB (throwM $ userError "release exception") (throwM $ userError "release exception")

main :: IO ()
main = do
    [demo] <- getArgs
    case demo of
      "A1" -> print =<< runExceptT demoA1
      "B1" -> print =<< runExceptT demoB1

      "A2" -> print =<< runExceptT demoA2
      "B2" -> print =<< runExceptT demoB2

      "A3" -> print =<< runExceptT demoA3
      "B3" -> print =<< runExceptT demoB3

      "A4" -> print =<< runExceptT demoA4
      "B4" -> print =<< runExceptT demoB4

      "A5" -> print =<< runExceptT demoA5
      "B5" -> print =<< runExceptT demoB5

      "A6" -> print =<< runExceptT demoA6
      "B6" -> print =<< runExceptT demoB6

      "A7" -> print =<< runExceptT demoA7
      "B7" -> print =<< runExceptT demoB7

      "A8" -> print =<< runExceptT demoA8
      "B8" -> print =<< runExceptT demoB8

      "A9" -> print =<< runExceptT demoA9
      "B9" -> print =<< runExceptT demoB9

      _    -> putStrLn "Unknown demo"
