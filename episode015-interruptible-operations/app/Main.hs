{-
  Unfolder episode 15: Interruptible operations

  In episode 10 on `generalBracket` we discussed _asynchronous exceptions_:
  exceptions that can be thrown to a thread at any point. In that episode we
  saw that correct exception handling in the presence of asynchronous exceptions
  relies on carefully controlling precisely when they delivered by _masking_
  (temporarily postponing) asynchronous exceptions at specific points. However,
  even when asynchronous exceptions are masked, _some_ specific instructions can
  _still_ be interrupted by asynchronous exceptions (technically, these are now
  synchronous). In this episode we will see how this works, why it is important,
  and how to take interruptibility into account.

  Original design described in "Asynchronous Exceptions in Haskell" (Simon
  Marlow, Simon Peyton Jones, and Andrew Moran), but modern Haskell deviates from
  this paper in a few places.
  <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/asynch-exns.pdf>

  See "Dealing with Asynchronous Exceptions during Resource Acquisition"
  <https://www.well-typed.com/blog/97/> for some additional thoughts.
-}

module Main (
    -- Main demos
    main

    -- withMVar example
  , withMVar_broken
  , withMVar_correct
  , withMVar_bracket

    -- cleanup example
  , cleanup_broken1
  , cleanup_broken2
  , cleanup_correct1
  , cleanup_correct2

    -- acquisition example
  , withMVarPair_broken1
  , withMVarPair_broken2
  , withSocketPair_broken1
  , withSocketPair_broken2
  , withSocketPair_correct
  , withSocketPair_explicit
  ) where

import Control.Concurrent hiding (withMVar)
import Control.Exception
import Control.Monad
import System.Environment

import Debug

{-------------------------------------------------------------------------------
  Reminder: asynchronous exceptions and masking
-------------------------------------------------------------------------------}

withMVar_broken :: MVar a -> (a -> IO b) -> IO b
withMVar_broken v f = do
    a <- takeMVar v
    result :: Either SomeException b <- try $ f a
    putMVar v a
    either throwIO return result

withMVar_correct :: MVar a -> (a -> IO b) -> IO b
withMVar_correct v f = mask $ \unmask -> do                 -- (*)
    a <- takeMVar v
    result :: Either SomeException b <- try $ unmask $ f a  -- (*)
    putMVar v a
    either throwIO return result

withMVar_bracket :: MVar a -> (a -> IO b) -> IO b
withMVar_bracket v f = bracket (takeMVar v) (putMVar v) f

{-------------------------------------------------------------------------------
  Interruptibility of takeMVar

  Key take-away: never mask exceptions indefinitely.
-------------------------------------------------------------------------------}

example0_threadA :: MVar Int -> IO ()
example0_threadA v = do
    i <- takeMVar v
    printLogMsg i

example0_threadB :: MVar Int -> IO ()
example0_threadB v = do
    threadDelay 6_000_000
    putMVar v 1

example0 :: IO ()
example0 = do
    v <- newEmptyMVar
    _ <- forkDebug "a" $ example0_threadA v
    _ <- forkDebug "b" $ example0_threadB v
    threadDelay 8_000_000

--------------------------------------------------------------------------------

example1_threadA :: MVar Int -> IO ()
example1_threadA v = do
    i <- takeMVar v
    printLogMsg i

example1_threadB :: MVar Int -> IO ()
example1_threadB v = do
    threadDelay 6_000_000
    putMVar v 1

example1 :: IO ()
example1 = do
    v <- newEmptyMVar
    a <- forkDebug "a" $ example1_threadA v
    _ <- forkDebug "b" $ example1_threadB v
    threadDelay 4_000_000 >> killThread a  -- (*)
    threadDelay 8_000_000

--------------------------------------------------------------------------------

example2_threadA :: MVar Int -> IO ()
example2_threadA v = mask_ $ do  -- (*)
    i <- takeMVar v
    printLogMsg i

example2_threadB :: MVar Int -> IO ()
example2_threadB v = do
    threadDelay 6_000_000
    putMVar v 1

example2 :: IO ()
example2 = do
    v <- newEmptyMVar
    a <- forkDebug "a" $ example2_threadA v
    _ <- forkDebug "b" $ example2_threadB v
    threadDelay 4_000_000 >> killThread a
    threadDelay 8_000_000

--------------------------------------------------------------------------------

example3_threadA :: MVar Int -> IO ()
example3_threadA v = uninterruptibleMask_ $ do  -- (*)
    i <- takeMVar v
    printLogMsg i

example3_threadB :: MVar Int -> IO ()
example3_threadB v = do
    threadDelay 6_000_000
    putMVar v 1

example3 :: IO ()
example3 = do
    v <- newEmptyMVar
    a <- forkDebug "a" $ example3_threadA v
    _ <- forkDebug "b" $ example3_threadB v
    threadDelay 4_000_000 >> killThread a
    threadDelay 8_000_000

{-------------------------------------------------------------------------------
  Mask makes asynchronous exceptions synchronous

  Note: there are some subtleties related to re-throwing async exceptions as
  synchronous examples (as 'logInterrupt' does). For some details, see
  <http://edsko.net/2013/06/11/throwTo/>.
-------------------------------------------------------------------------------}

logInterrupt :: IO a -> IO a
logInterrupt = handle aux
  where
    aux :: SomeAsyncException -> IO a
    aux e = printLogMsg e >> throwIO e

example4_threadA :: MVar Int -> IO ()
example4_threadA v = mask_ $ do     -- (*)
    i <- logInterrupt $ takeMVar v  -- (*)
    printLogMsg i

example4_threadB :: MVar Int -> IO ()
example4_threadB v = do
    threadDelay 6_000_000
    putMVar v 1

example4 :: IO ()
example4 = do
    v <- newEmptyMVar
    a <- forkDebug "a" $ example4_threadA v
    _ <- forkDebug "b" $ example4_threadB v
    threadDelay 4_000_000 >> killThread a
    threadDelay 8_000_000

{-------------------------------------------------------------------------------
  Synchronicity of 'killThread'

  Synchronicity of killThread means that once 'killThread' returns, the thread
  has stopped doing the thing it was doing (although it may still be in some
  kind of cleanup phase). For example, if the thread was logging to the console,
  it may now be safe to log something else without confusing the output.
  (Though all of this will depend on the exact implementation of the thread.)

  (NOTE: Deviates from the paper.)
-------------------------------------------------------------------------------}

example5_threadA :: MVar Int -> IO ()
example5_threadA v = uninterruptibleMask_ $ do  -- (*)
    i <- takeMVar v
    printLogMsg i

example5_threadB :: MVar Int -> IO ()
example5_threadB v = do
    threadDelay 6_000_000
    putMVar v 1

example5 :: IO ()
example5 = do
    v <- newEmptyMVar
    a <- forkDebug "a" $ example5_threadA v
    _ <- forkDebug "b" $ example5_threadB v
    _ <- forkDebug "c" $ threadDelay 2_000_000 >> killThread a  -- (*)
    threadDelay 8_000_000

{-------------------------------------------------------------------------------
  Interruptibility of killThread/throwTo

  (NOTE: This too deviates from the paper, but is a direct consequence of the
  fact that it is synchronous.)
-------------------------------------------------------------------------------}

example6_threadA :: MVar Int -> IO ()
example6_threadA v = uninterruptibleMask_ $ do
    i <- takeMVar v
    printLogMsg i

example6_threadB :: MVar Int -> IO ()
example6_threadB v = do
    threadDelay 6_000_000
    putMVar v 1

example6 :: IO ()
example6 = do
    v <- newEmptyMVar
    a <- forkDebug "a" $ example6_threadA v
    _ <- forkDebug "b" $ example6_threadB v
    c <- forkDebug "c" $ threadDelay 2_000_000 >> killThread a
    threadDelay 4_000_000 >> killThread c  -- (*)
    threadDelay 8_000_000

--------------------------------------------------------------------------------

example7_threadA :: MVar Int -> IO ()
example7_threadA v = uninterruptibleMask_ $ do
    i <- takeMVar v
    printLogMsg i

example7_threadB :: MVar Int -> IO ()
example7_threadB v = do
    threadDelay 6_000_000
    putMVar v 1

example7 :: IO ()
example7 = do
    v <- newEmptyMVar
    a <- forkDebug "a" $ example7_threadA v
    _ <- forkDebug "b" $ example7_threadB v
    c <- forkDebug "c" $ mask_ $ threadDelay 2_000_000 >> killThread a  -- (*)
    threadDelay 4_000_000 >> killThread c
    threadDelay 8_000_000

{-------------------------------------------------------------------------------
  Interruptibility versus resource cleanup

  Note: nested acquisition is better (see below), but the general technique of
  spawning a thread to protect it against async exceptions is a useful one.
-------------------------------------------------------------------------------}

helperThread1, helperThread2, someApp :: IO ()
helperThread1 = undefined
helperThread2 = undefined
someApp       = undefined

spawnHelpers :: IO (ThreadId, ThreadId)
spawnHelpers = do
    helper1 <- forkIO helperThread1
    helper2 <- forkIO helperThread2
    return (helper1, helper2)

cleanupHelpers :: (ThreadId, ThreadId) -> IO ()
cleanupHelpers (helper1, helper2) = do
    killThread helper1
    killThread helper2

cleanup_broken1 :: IO ()
cleanup_broken1 = bracket spawnHelpers cleanupHelpers (\_ -> someApp)

cleanup_broken2 :: IO ()
cleanup_broken2 = mask $ \unmask -> do
    helpers <- spawnHelpers
    result :: Either SomeException () <- try $ unmask $ someApp
    cleanupHelpers helpers
    either throwIO return result

cleanup_correct1 :: IO ()
cleanup_correct1 = mask $ \unmask -> do
    helpers <- spawnHelpers
    result :: Either SomeException () <- try $ unmask $ someApp
    uninterruptibleMask_ $ cleanupHelpers helpers  -- (*)
    either throwIO return result

cleanup_correct2 :: IO ()
cleanup_correct2 = mask $ \unmask -> do
    helpers <- spawnHelpers
    result :: Either SomeException () <- try $ unmask $ someApp
    void $ forkIO $ cleanupHelpers helpers  -- (*)
    either throwIO return result

{-------------------------------------------------------------------------------
  Interruptibility versus resource acquisition

  Note: the calls to @putMVar@ in @withMVarPair_broken1@ are fine: they are not
  interruptible, since we know that they cannot block (assuming to concurrent
  stray calls to @putMVar@, which would anyway break the @withMVar@ pattern).
-------------------------------------------------------------------------------}

withMVarPair_broken1 :: MVar a -> MVar b -> (a -> b -> IO c) -> IO c
withMVarPair_broken1 v1 v2 f = mask $ \unmask ->  do
    a <- takeMVar v1
    b <- takeMVar v2
    result :: Either SomeException c <- try $ unmask $ f a b
    putMVar v1 a
    putMVar v2 b
    either throwIO return result

withMVarPair_broken2 :: MVar a -> MVar b -> (a -> b -> IO c) -> IO c
withMVarPair_broken2 v1 v2 f = mask $ \unmask ->  do
    a  <- takeMVar v1
    b  <- uninterruptibleMask_ $ takeMVar v2
    result :: Either SomeException c <- try $ unmask $ f a b
    putMVar v1 a
    putMVar v2 b
    either throwIO return result

{-------------------------------------------------------------------------------
  Same example, with but sockets

  If @openSocket@ is interruptible, with have the same problem as above with
  @takeMVar@. If @openSocket@ is /not/ interruptible, we may have a different
  problem: now we might block indefinitely with async exceptions masked. In
  general, resource acqusition functions should either return relatively
  promptly or be interruptible.

  In general, we must assume all unknown code is /potentially/ interruptible.
  (And yet cannot assume unknown code /definitely/ is.)
-------------------------------------------------------------------------------}

withSocketPair_broken1 :: (Socket -> Socket -> IO ()) -> IO ()
withSocketPair_broken1 f = mask $ \unmask ->  do
    a  <- openSocket
    b  <- openSocket
    result :: Either SomeException () <- try $ unmask $ f a b
    closeSocket a
    closeSocket b
    either throwIO return result

{-------------------------------------------------------------------------------
  Nested resource acquisition and release

  This address both the problems with interruptibility during acquisition and
  during release.

  However, if resources cannot be statically nested like this, we need a
  different solution, such as @ResourceT@
  <https://hackage.haskell.org/package/resourcet>.
-------------------------------------------------------------------------------}

withSocketPair_correct :: (Socket -> Socket -> IO ()) -> IO ()
withSocketPair_correct f =
    bracket openSocket closeSocket $ \a ->
    bracket openSocket closeSocket $ \b ->
    f a b

withSocketPair_explicit :: (Socket -> Socket -> IO ()) -> IO ()
withSocketPair_explicit f =
    mask $ \unmask -> do
      a <- openSocket
      result :: Either SomeException () <- try $ unmask $ do
        mask $ \unmask' -> do
          b <- openSocket
          result' :: Either SomeException () <- try $ unmask' $ f a b
          closeSocket b
          either throwIO return result'
      closeSocket a
      either throwIO return result

{-------------------------------------------------------------------------------
  Careful: print is interruptible!

  This is because a @Handle@ contains an @MVar@:

  > data Handle   = FileHandle FilePath (MVar Handle__) | ..
  > data Handle__ = ..
-------------------------------------------------------------------------------}

closeSocket' :: Socket -> IO ()
closeSocket' s = do
    putStrLn "Going to close the socket!"
    closeSocket s

withSocketPair_broken2 :: (Socket -> Socket -> IO ()) -> IO ()
withSocketPair_broken2 f =
    mask $ \unmask -> do
      a <- openSocket
      result :: Either SomeException () <- try $ unmask $ do
        mask $ \unmask' -> do
          b <- openSocket
          result' :: Either SomeException () <- try $ unmask' $ f a b
          closeSocket' b                  -- (*)
          either throwIO return result'
      closeSocket' a                      -- (*)
      either throwIO return result

{-------------------------------------------------------------------------------
  Application driver
-------------------------------------------------------------------------------}

main :: IO ()
main = withTimer $ do
    args <- getArgs
    case args of
      ["example0"]     -> example0
      ["example1"]     -> example1
      ["example2"]     -> example2
      ["example3"]     -> example3
      ["example4"]     -> example4
      ["example5"]     -> example5
      ["example6"]     -> example6
      ["example7"]     -> example7
      ["printExample"] -> printExample
      _otherwise       -> throwIO $ userError "Invalid arguments"

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

data Socket

openSocket :: IO Socket
openSocket = undefined

closeSocket :: Socket -> IO ()
closeSocket = undefined

printExampleThread :: IO ()
printExampleThread = mask_ $ do
    forever $ putStrLn "Printing"

printExample :: IO ()
printExample = do
    tid <- forkIO $ printExampleThread
    threadDelay 1_000_000
    killThread tid
    threadDelay 8_000_000

