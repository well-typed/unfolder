{-# OPTIONS_GHC -fno-full-laziness -fno-state-hack -O0 #-}

{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

{-
  CPP flags expected to be set:

  * One of: BESPOKE_LAST, PRELUDE_LAST
  * One of: BESPOKE_BREAK, PRELUDE_BREAK
  * One of:
    - TEST_PRINT_EQ
    - TEST_SEQ_EQ
    - TEST_ONE_PRINT
    - TEST_TWO_PRINTS
  * ELEM should be Int or Integer (or any other Num instance)
  * One of: SAME_ELEM_INLINE, SAME_ELEM_NOINLINE
 -}

module LastBeforeBreak (main) where

import Prelude hiding (last, break)
import GHC.Exts
import GHC.IO

#if ENABLE_GHC_DEBUG
import System.Mem
#endif

#if defined(PRELUDE_LAST) || defined(PRELUDE_BREAK)
import Prelude qualified
#endif

{-------------------------------------------------------------------------------
  Priminaries: raw IO (so that we really know which code is executed)
-------------------------------------------------------------------------------}

type RawIO a = State# RealWorld -> (# State# RealWorld, a #)

{-------------------------------------------------------------------------------
  Integration with ghc-debug

  This is enabled by default in the cabal file (and then disabled for the
  tests that don't use the cabal file).

  TODO: Currently we only pause in the two-prints test.
-------------------------------------------------------------------------------}

#ifdef TEST_TWO_PRINTS

pauseForGhcDebug :: RawIO ()
#ifdef ENABLE_GHC_DEBUG
pauseForGhcDebug = \w0 ->
    case unIO performMajorGC w0 of { (# w1, () #) ->
    case unIO getLine        w1 of { (# w2, _  #) ->
    (# w2, () #)
    }}
#else
pauseForGhcDebug = \w0 -> (# w0, () #)
#endif

#endif

{-------------------------------------------------------------------------------
  Type specific operations

  These are the only functions that are different depending on the choice of
  @ELEM@; we abstract them out and make them non-inlineable to make the various
  examples more easy to compare.
-------------------------------------------------------------------------------}

-- We break at 5, so that this works both with the long and the short example
shouldBreak :: ELEM -> Bool
{-# NOINLINE shouldBreak #-}
shouldBreak x = x == 5

-- May want to enable the shorter example when using ghc-debug
constructList :: () -> RawIO [ELEM]
{-# NOINLINE constructList #-}
constructList () = \w -> (# w, [-1_000_000 .. 1_000_000] #)
-- constructList () = \w -> (# w, [0 .. 10] #)

#if defined(TEST_PRINT_EQ) || defined(TEST_SEQ_EQ)
sameElem :: ELEM -> ELEM -> Bool
sameElem = (==)

#if SAME_ELEM_INLINE
{-# INLINE sameElem #-}
#endif
#if SAME_ELEM_NOINLINE
{-# NOINLINE sameElem #-}
#endif
#endif

{-------------------------------------------------------------------------------
  @last@
-------------------------------------------------------------------------------}

#ifdef BESPOKE_LAST
last :: [ELEM] -> ELEM
last = lastWithDefault undefined

lastWithDefault :: ELEM -> [ELEM] -> ELEM
lastWithDefault def []     = def
lastWithDefault _   (x:xs) = lastWithDefault x xs
#endif

#ifdef PRELUDE_LAST
last :: [ELEM] -> ELEM
last = Prelude.last
#endif

{-------------------------------------------------------------------------------
  @break@
-------------------------------------------------------------------------------}

#ifdef BESPOKE_BREAK
break :: [ELEM] -> ([ELEM], [ELEM])
break []     = ([], [])
break (x:xs) = if shouldBreak x then
                 ([], xs)
               else
                 let result = break xs
                 in ( x : case result of (a, _) -> a
                    ,     case result of (_, b) -> b
                    )
#endif

#ifdef PRELUDE_BREAK
break :: [ELEM] -> ([ELEM], [ELEM])
break = Prelude.break shouldBreak
#endif

{-------------------------------------------------------------------------------
  Main test
-------------------------------------------------------------------------------}

testIO :: () ->  RawIO ()
{-# NOINLINE testIO #-}
testIO () = \w0 ->
    case constructList () w0 of { (# w1, inputList #) ->

    -- Problems:
    --
    -- - With -O1, the definition of @last_after@ gets modified to
    --
    --   > case result of (_, b) -> last b
    --
    --   which GHC then does not recognize as a selector thunk.
    --
    -- - With -O0,
    --
    --   > last (case result of (_, b) -> b)
    --
    --   is a thunk /which has not yet allocated the selector thunk/.
    --   If ghc would not insist on changing our code (even with -O0), perhaps
    --   we could write this as
    --
    --   > let after      = case result of (_, b) -> b
    --   >     last_after = last after
    --
    --   but unfortunately the Very Simple Optimizer seems to make it very very
    --   difficult to make this actually happen.

    let result      = break inputList
        last_before = last (case result of (a, _) -> a)
        last_after  = last (case result of (_, b) -> b) in

#ifdef TEST_PRINT_EQ
    unIO (print (sameElem last_before last_after)) w1
#endif

#ifdef TEST_SEQ_EQ
    last_before `seq`
    last_after  `seq`
    unIO (print (sameElem last_before last_after)) w1
#endif

#ifdef TEST_ONE_PRINT
    unIO (print (last_before, last_after)) w1
#endif

#ifdef TEST_TWO_PRINTS
    case unIO (print last_before) w1 of { (# w2, () #) ->
    case       pauseForGhcDebug   w2 of { (# w3, _  #) ->
    case unIO (print last_after)  w3 of { (# w4, () #) ->
    (#w4, () #)
    }}}
#endif

    }

{-------------------------------------------------------------------------------
  Application driver
-------------------------------------------------------------------------------}

main :: IO ()
main = IO $ testIO ()
