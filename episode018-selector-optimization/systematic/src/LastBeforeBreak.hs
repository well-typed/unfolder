{-# OPTIONS_GHC -fno-full-laziness -fno-state-hack #-}
{-# LANGUAGE CPP #-}

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

#if defined(PRELUDE_LAST) || defined(PRELUDE_BREAK)
import Prelude qualified
#endif

{-------------------------------------------------------------------------------
  Type specific operations

  These are the only functions that are different depending on the choice of
  @ELEM@; we abstract them out and make them non-inlineable to make the various
  examples more easy to compare.
-------------------------------------------------------------------------------}

isZero :: ELEM -> Bool
{-# NOINLINE isZero #-}
isZero x = x == 0

constructList :: () -> IO [ELEM]
{-# NOINLINE constructList #-}
constructList () = return [-1_000_000 .. 1_000_000]

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
break (x:xs) = if isZero x then
                 ([], xs)
               else
                 let result = break xs
                 in ( x : case result of (a, _) -> a
                    ,     case result of (_, b) -> b
                    )
#endif

#ifdef PRELUDE_BREAK
break :: [ELEM] -> ([ELEM], [ELEM])
break = Prelude.break isZero
#endif

{-------------------------------------------------------------------------------
  Main test
-------------------------------------------------------------------------------}

test :: () ->  IO ()
{-# NOINLINE test #-}
test () = do
    inputList <- constructList ()

    let result      = break inputList
        last_before = last (case result of (a, _) -> a)
        last_after  = last (case result of (_, b) -> b)

#ifdef TEST_PRINT_EQ
    print (sameElem last_before last_after)
#endif

#ifdef TEST_SEQ_EQ
    last_before `seq` last_after `seq` print (sameElem last_before last_after)
#endif

#ifdef TEST_ONE_PRINT
    print (last_before, last_after)
#endif

#ifdef TEST_TWO_PRINTS
    print last_before
    print last_after
#endif

{-------------------------------------------------------------------------------
  Application driver
-------------------------------------------------------------------------------}

main :: IO ()
main = test ()
