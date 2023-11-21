module Version0 where

{-------------------------------------------------------------------------------
  Simple definition of a counter
-------------------------------------------------------------------------------}

data Counter = Counter {
      tick    :: Counter
    , value   :: Int
    }

mkCounter :: Int -> Counter
mkCounter n = Counter {
      tick    = mkCounter (n + 1)
    , value   = n
    }

{-------------------------------------------------------------------------------
  (Failed) attempt at overriding part of the behaviour of the counter

  Attempt to predict what the value of 'example' is!
-------------------------------------------------------------------------------}

faster :: Counter -> Counter
faster c = c {
      tick = mkCounter (value c + 2)
    }

example :: Int
example = value . tick . tick $ faster (mkCounter 0)

