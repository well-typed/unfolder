module Version2 where

import Version0 (Counter(..))

{-------------------------------------------------------------------------------
  Modelling OOP "super" concept

  See "The Different Aspects of Monads and Mixins" by Bruno Oliveira.
-------------------------------------------------------------------------------}

type Object o = o -> o -> o

object :: Object o -> o
object f = f undefined (object f)

(.+.) :: Object o -> Object o -> Object o
f .+. g = \super this -> f (g super this) this

{-------------------------------------------------------------------------------
  Redoing the counter example again
-------------------------------------------------------------------------------}

mkCounter :: Object (Int -> Counter)
mkCounter _super this n = Counter {
      tick    = this (n + 1)
    , value   = n
    }

faster :: Object (Int -> Counter)
faster super this n = (super n) {
      tick = this (n + 2)
    }

example1 :: Int
example1 = value . tick . tick $ object (faster .+. mkCounter) 0

{-------------------------------------------------------------------------------
  Side note

  We might attempt to try and define 'faster' by saying "do the tick operation
  from the superclass twice"; if we could, then we could repeat calls to
  'faster' to tick faster and faster. However, that does not work. If we define

  > faster :: Object (Int -> Counter)
  > faster super this n = (super n) {
  >       tick = tick . tick $ super n
  >     }

  then evaluation proceeds something like this:

  >   tick (tick (super 0))
  > = tick (tick (mkCounter undefined (object ..) 0))
  > = tick (object .. 1)
  > = tick (tick (super 1)
  >  = ..

  The problem is that 'tick' in 'mkCounter' is defined in terms of 'this', which
  then points back to the faster counter; this results in an infinite loop.
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Second example

  Chain applications of '(.+.)'
-------------------------------------------------------------------------------}

upTo :: Int -> Object (Int -> Counter)
upTo m super _this n = (super n) {
      value = n `mod` m
    }

example2 :: Int
example2 = value . tick . tick $ object (upTo 3 .+. faster .+. mkCounter) 0
