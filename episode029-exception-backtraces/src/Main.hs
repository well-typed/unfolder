-- | Haskell Unfolder episode 29: exception annotations and backtraces
--
-- Version 9.10 of `ghc` introduces an extremely useful new feature: exception
-- annotations and automatic exception backtraces. This new feature, four
-- years in the making, can be a life-saver when debugging code and has not
-- received nearly as much attention as it deserves. In this episode of the
-- Haskell Unfolder we therefore give an overview of the changes and discuss how
-- we can advantage of them.

-- See
--
-- * <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0330-exception-backtraces.rst>
--   for the full proposal.
-- * <https://github.com/haskell/core-libraries-committee/issues/202>
--   for the part of the proposal that has not yet been accepted.
-- * <https://github.com/haskell/core-libraries-committee/issues/164>
--   for the CLC proposal for the changes to `base`.
-- * <https://hackage.haskell.org/package/base-4.20.0.0>
--   for the changes to base (specifically in @Control.Exception.*@).
module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Exception.Annotation
import Control.Exception.Backtrace
import Control.Exception.Context
import Data.Typeable
import GHC.Generics (Generic)
import GHC.Internal.Exception.Type (NoBacktrace(..))
import GHC.Stack
import System.Environment
import Text.Show.Pretty

import Control.Exception.Annotated qualified as Before910

{-------------------------------------------------------------------------------
  Motivation
-------------------------------------------------------------------------------}

type StreamId = Int
data StreamClosed = StreamClosed StreamId
  deriving stock (Show)
  deriving anyclass (Exception)

exampleThrow :: IO ()
exampleThrow = throwIO $ StreamClosed 1

{-------------------------------------------------------------------------------
  Manually adding annotations (not yet using the new features of ghc 9.10).

  This does not work: 'catch1' does not manage to catch the exception, because
  the exception thrown by 'annotate1' has a different type.
-------------------------------------------------------------------------------}

data Annotated = Annotated String StreamClosed
  deriving stock (Show)
  deriving anyclass (Exception)

annotate1 :: IO ()
annotate1 = do
    exampleThrow `catch` \e@(StreamClosed _) ->
      throwIO $ Annotated "Hi" e

catch1 :: IO ()
catch1 =
    annotate1 `catch` \(StreamClosed i) ->
      putStrLn $ "Stream closed: " ++ show i

{-------------------------------------------------------------------------------
  Making use of the new exception annotations
-------------------------------------------------------------------------------}

data Sending = Sending String
  deriving stock (Show)
  deriving anyclass (ExceptionAnnotation)

annotate2 :: IO ()
annotate2 = do
    annotateIO (Sending "Hi") $
      exampleThrow

catch2 :: IO ()
catch2 =
    annotate2 `catch` \(StreamClosed i) ->
      putStrLn $ "Stream closed: " ++ show i

{-------------------------------------------------------------------------------
  Rethrowing exceptions

  Note that we lose the annotation in 'rethrow2'.
-------------------------------------------------------------------------------}

rethrow1 :: IO ()
rethrow1 =
    annotate2 `catch` \(e :: IOException) -> do
      putStrLn "Caught IOException"
      throwIO e

rethrow2 :: IO ()
rethrow2 =
    annotate2 `catch` \(e :: StreamClosed) -> do
      putStrLn "Caught StreamClosed"
      throwIO e

rethrow3 :: IO ()
rethrow3 =
    annotate2 `catch` \(ExceptionWithContext ctxt (e :: StreamClosed)) -> do
      putStrLn "Caught StreamClosed"
      throwIO $ ExceptionWithContext ctxt e

rethrow4 :: IO ()
rethrow4 =
    annotate2 `catch202` \(e :: StreamClosed) -> do
      putStrLn "Caught StreamClosed"
      throwIO e

{-------------------------------------------------------------------------------
  Backtraces

  Without the 'HasCallStack' constraint, the only part of the callstack we
  see is the call to call to 'collectBacktraces' itself; not very useful!

  You will probably not manually call 'collectBacktraces' like this (see next
  section), but it occassionally be useful. For example, suppose we get a
  'StreamClosed' exception because we are trying to write to a stream that
  was previously closed: we might also want to know the callstack that tells us
  /how/ that stream was closed.
-------------------------------------------------------------------------------}

bottom :: HasCallStack => IO ()
bottom = do
    backtraces <- collectBacktraces
    putStrLn $ displayBacktraces backtraces

middle :: HasCallStack => IO ()
middle = bottom

top1 :: HasCallStack => IO ()
top1 = middle

-- > cabal run ep29 --enable-profiling -- top2
top2 :: HasCallStack => IO ()
top2 = do
    setBacktraceMechanismState CostCentreBacktrace True
    middle

{-------------------------------------------------------------------------------
  Default backtraces:

  In @base@, 'throwIO' has been modified to add a backtrace as a default
  annotation to every thrown exception.

  Disabling backtrace collection is probably only useful for exceptions you
  expect to be thrown in non-exceptional circumstances, as part of the regular
  control flow of your program. In the @base@ libraries, this is primarily
  asynchronous exceptions such as 'ThreadKilled'.
-------------------------------------------------------------------------------}

backtrace1 :: IO ()
backtrace1 = throwIO $ StreamClosed 1

data SomeOtherException = SomeOtherException
  deriving stock (Show)

instance Exception SomeOtherException where
  backtraceDesired _ = False

backtrace2 :: IO ()
backtrace2 = throwIO $ SomeOtherException

{-------------------------------------------------------------------------------
  Main application

  For the examples that are not explicitly about backtraces, we disable all
  backtrace collection. You would not normally do this in regular code, but it
  makes the output of the program a bit easier to read when we are discussing
  annotations in the episode.
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["annotate1"]      -> disableAllBacktraces >> annotate1
      ["annotate2"]      -> disableAllBacktraces >> annotate2
      ["async1"]         -> async1
      ["async2"]         -> async2
      ["backtrace1"]     -> backtrace1
      ["backtrace2"]     -> backtrace2
      ["before910_1"]    -> before910_1
      ["before910_2"]    -> before910_2
      ["before910_3"]    -> before910_3
      ["catch1"]         -> disableAllBacktraces >> catch1
      ["catch2"]         -> disableAllBacktraces >> catch2
      ["exampleThrow"]   -> disableAllBacktraces >> exampleThrow
      ["rethrow1"]       -> disableAllBacktraces >> rethrow1
      ["rethrow2"]       -> disableAllBacktraces >> rethrow2
      ["rethrow3"]       -> disableAllBacktraces >> rethrow3
      ["rethrow4"]       -> disableAllBacktraces >> rethrow4
      ["rethrowNested1"] -> rethrowNested1
      ["rethrowNested2"] -> rethrowNested2
      ["rethrowNested3"] -> rethrowNested3
      ["top1"]           -> top1
      ["top2"]           -> top2
      ["top2"]           -> top2
      ["top3"]           -> top3
      ["top4"]           -> top4
      _otherwise         -> putStrLn "Unknown example"

disableAllBacktraces :: IO ()
disableAllBacktraces = do
    setBacktraceMechanismState CostCentreBacktrace   False
    setBacktraceMechanismState HasCallStackBacktrace False
    setBacktraceMechanismState ExecutionBacktrace    False
    setBacktraceMechanismState IPEBacktrace          False

{-------------------------------------------------------------------------------
  AUXILIARY MATERIAL

  The rest of this module contains some auxiliary material that we do not
  discuss in detail in the episode.
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Prior art (before ghc 9.10): the @annotated-exception@ package

  We have

  * 'Before910.checkpoint' is similar to 'annotateIO'
  * 'Before910.throw' is similar to 'throwIO'
  * 'Before910.catch' is similar to 'catch202', except that it merges the list
    of annotations, rather than introducing the 'WhileHandling' hierarchical
    annotation.

  This relies on throwing a /new/ exception type ('AnnotatedException'), so we
  /must/ use 'catch' from the library, making this non-compositional (if you
  annotate an exception, /all/ places were it might be caught must be modified).
  This is one of the problems solved by the new infrastructure in ghc 9.10.
-------------------------------------------------------------------------------}

before910_1 :: IO ()
before910_1 = do
    Before910.checkpoint (Before910.Annotation $ Sending "Hi") $
      Before910.throw $ StreamClosed 1

before910_2 :: IO ()
before910_2 = do
    before910_1 `Before910.catch` \(StreamClosed s) ->
      Before910.throw $ StreamClosed s

before910_3 :: IO ()
before910_3 = do
    before910_1 `catch` \(StreamClosed _) ->
      putStrLn "Caught StreamClosed!"

{-------------------------------------------------------------------------------
  Annotations vs async exceptions

  This demonstrates that if you throw an exception /from/ the scope of
  'annotateIO' nothing happens, but if you throw an exception /into/ the scope
  of 'annotateIO', you get the annotation.

  Note: the call to 'mask_' ensures that we cannot throw the exception until the
  exception handler is installed (we unmask implicitly in the call to
  'takeMVar', because 'takeMVar' is interruptible).
-------------------------------------------------------------------------------}

-- | An async exception thrown /from/ the scope of 'annotateIO'
async1 :: IO ()
async1 = do
    start  <- newEmptyMVar
    finish <- newEmptyMVar

    otherThread <- mask_ $ forkIO $ do
      takeMVar start `catch` \e ->
        putStrLn $ "Caught " ++ displayException (e :: SomeException)
      putMVar finish ()

    annotateIO (Sending "Hi") $
      throwTo otherThread (StreamClosed 1)
    takeMVar finish

-- | An async exception thrown /into/ the scope of 'annotateIO'
async2 :: IO ()
async2 = do
    start  <- newEmptyMVar
    finish <- newEmptyMVar

    otherThread <- mask_ $ forkIO $ do
      annotateIO (Sending "Hi") (takeMVar start) `catch` \e ->
        putStrLn $ "Caught " ++ displayException (e :: SomeException)
      putMVar finish ()

    throwTo otherThread (StreamClosed 1)
    takeMVar finish

{-------------------------------------------------------------------------------
  Exception backtrace proposal: Part 4: Rethrowing
  <https://github.com/haskell/core-libraries-committee/issues/202>

  This is the only part of the exception backtrace proposal that has not yet
  been approved. We reproduce the current proposal here (somewhat simplified).
-------------------------------------------------------------------------------}

newtype WhileHandling = WhileHandling SomeException
  deriving stock (Generic)

instance ExceptionAnnotation WhileHandling where
  displayExceptionAnnotation (WhileHandling e) =
    "While handling " ++ displayException e

catch202 :: forall e a. Exception e => IO a -> (e -> IO a) -> IO a
catch202 io handler = catch io handler'
  where
    handler' :: SomeException -> IO a
    handler' e =
        case fromException e of
          Just e' -> annotateIO (WhileHandling e) $ handler e'
          Nothing -> throwIO $ NoBacktrace e

{-------------------------------------------------------------------------------
  More types of backtraces

  Note 1: the use of 'HasCallStack' in these examples is not directly relevant
  to these types of backtraces, but it prevents ghc from inlining.

  Note 2: in order to get DWARF backtraces, you need a ghc compiled with @libdw@
  support, and you probably also want debug symbols in the base libraries. The
  version of ghc 9.10 shipped by ghcup does /not/ include this. You can make
  your own binary distribution with DWARF support by downloading the GHC sources
  and then running

  > ./configure --enable-dwarf-unwind
  > ./hadrian/build binary-dist -j --flavour=release+debug_info

  This constructs a binary distribution which you can then install like any
  other. See also
  <https://downloads.haskell.org/ghc/latest/docs/users_guide/debug-info.html>.
-------------------------------------------------------------------------------}

-- > cabal run ep29 --ghc-options=-finfo-table-map -- top3
top3 :: HasCallStack => IO ()
top3 = do
    setBacktraceMechanismState IPEBacktrace True
    middle

-- > cabal run ep29 --enable-debug-info=3 -- top4
top4 :: HasCallStack => IO ()
top4 = do
    setBacktraceMechanismState ExecutionBacktrace True
    middle

{-------------------------------------------------------------------------------
  Discussion on core-libraries-committee:#202

  If we start wiht

  > SomeException
  >   [Annotation]
  >   OriginalException

  then the hierarchical structure results in something like

  > SomeException
  >   [WhileHandling (SomeException [Annotation] OriginalException)]
  >   (NestSomeException OriginalException)

  whereas the flat structure results in something like

  > SomeException
  >   [Annotation]
  >   (NestSomeException OriginalException)
-------------------------------------------------------------------------------}

data OriginalException = OriginalException
  deriving stock (Show, Generic)
  deriving anyclass (Exception, PrettyVal)

data NestSomeException = NestSomeException SomeException
  deriving stock (Show, Generic)
  deriving anyclass (Exception, PrettyVal)

data Annotation = Annotation String
  deriving stock (Show, Generic)
  deriving anyclass (ExceptionAnnotation, PrettyVal)

throwOriginal :: IO ()
throwOriginal =
    annotateIO (Annotation "hi") $
      throwIO OriginalException

rethrowNested1 :: IO ()
rethrowNested1 = showExceptionStructure $
    throwOriginal `catch202` \e@OriginalException ->
      throwIO $ NestSomeException (toException e)

rethrowNested2 :: IO ()
rethrowNested2 = showExceptionStructure $
    throwOriginal `catchFlat` \e@OriginalException ->
      throwIO $ NestSomeException (toException e)

rethrowNested3 :: IO ()
rethrowNested3 = showExceptionStructure $
    throwOriginal `catchExceptionNoAnnotation` \e ->
      throwIO $ NestSomeException e

catchFlat :: forall e a. Exception e => IO a -> (e -> IO a) -> IO a
catchFlat io handler = catch io handler'
  where
    handler' :: SomeException-> IO a
    handler' e =
        case fromException e of
          Just e' -> catch (handler e') (handleRethrows (someExceptionContext e))
          Nothing -> throwIO $ NoBacktrace e

    handleRethrows :: ExceptionContext -> SomeException -> IO a
    handleRethrows origCtxt e@(SomeException e') =
        throwIO $ NoBacktrace $
          ExceptionWithContext (origCtxt <> someExceptionContext e) e'

catchExceptionNoAnnotation :: Exception e => IO a -> (e -> IO a) -> IO a
catchExceptionNoAnnotation = catch

{-------------------------------------------------------------------------------
  Auxiliary: show exception structure

  Using 'displayException' makes it a bit difficult to see what's happening,
  so we define some instances here to clarity the structure. These instances
  are for demonstration purposes only.
-------------------------------------------------------------------------------}

instance PrettyVal SomeException where
  prettyVal e@(SomeException e') = Rec "SomeException" [
        ("someExceptionContext", prettyVal (someExceptionContext e))
      , ("fromException", prettyFromException e')
      ]

instance PrettyVal ExceptionContext where
  prettyVal (ExceptionContext anns) =
      Con "ExceptionContext" [prettyVal anns]

instance PrettyVal SomeExceptionAnnotation where
  prettyVal (SomeExceptionAnnotation ann) = prettyExceptionAnnotation ann

instance PrettyVal Backtraces where
  prettyVal = String . last . lines . displayBacktraces

deriving anyclass instance PrettyVal WhileHandling

prettyFromException :: forall e. Exception e => e -> Value
prettyFromException e
  | Just (e' :: OriginalException) <- cast e
  = prettyVal e'

  | Just (e' :: NestSomeException) <- cast e
  = prettyVal e'

  | Just (e' :: SomeException) <-  cast e
  = prettyVal e'

  | otherwise
  = Con ("<" ++ show (typeRep (Proxy @e)) ++ ">") []

prettyExceptionAnnotation :: forall ann. ExceptionAnnotation ann => ann -> Value
prettyExceptionAnnotation ann
  | Just (ann' :: Backtraces) <- cast ann
  = prettyVal ann'

  | Just (ann' :: WhileHandling) <- cast ann
  = prettyVal ann'

  | Just (ann' :: Annotation) <- cast ann
  = prettyVal ann'

  | otherwise
  = Con ("<" ++ show (typeRep (Proxy @ann)) ++ ">") []

showExceptionStructure :: IO () -> IO ()
showExceptionStructure = handle $ \(e :: SomeException) ->
    dumpIO e
