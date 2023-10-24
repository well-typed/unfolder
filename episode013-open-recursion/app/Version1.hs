module Version1 where

import Version0 (Counter(..))

{-------------------------------------------------------------------------------
  OOP approach
-------------------------------------------------------------------------------}

type Object o = o -> o

object :: Object o -> o
object f = f (object f)

mkCounter :: Object (Int -> Counter)
mkCounter this n = Counter {
      tick    = this (n + 1)
    , value   = n
    }

example1 :: Int
example1 = value . tick . tick $ object mkCounter 0

{-------------------------------------------------------------------------------
  Second attempt at overriding the behaviour of the counter
-------------------------------------------------------------------------------}

faster :: Object (Int -> Counter)
faster this n = (mkCounter this n) {
      tick = this (n + 2)
    }

example2 :: Int
example2 = value . tick . tick $ object faster 0

