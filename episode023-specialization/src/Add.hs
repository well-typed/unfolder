module Add where

add :: (Ord a, Num a) => a -> a -> a
add !n !m =
    if m <= 0 then
      n
    else
      add (n + 1) (m - 1)
