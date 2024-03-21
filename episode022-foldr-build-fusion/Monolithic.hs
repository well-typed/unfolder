module Monolithic where

example :: Int -> Int
example n = sum (map (\ x -> x * x) (enumFromTo 1 n))

build :: (forall r. (a -> r -> r) -> r -> r) -> [a]
build builder = builder (:) []

-- inline the foldr-build versions of the functions:
monolithic1 :: Int -> Int
monolithic1 n =
  let
    r0 =
      build $ \ cons nil ->
      let
        go i =
          i `cons`
          if i == n
            then nil
            else go (i + 1)
      in
        if 1 > n then nil else go 1
    r1 = build $ \ cons nil -> foldr (\ x r -> (x * x) `cons` r) nil r0
    r2 = foldr (\ x r !acc -> r (acc + x)) id r1 0
  in
    r2

-- fuse r0 into r1:
monolithic2 :: Int -> Int
monolithic2 n =
  let
    r1 =
      build $ \ cons nil ->
      let
        go i =
          (i * i) `cons`
          if i == n
            then nil
            else go (i + 1)
      in
        if 1 > n then nil else go 1
    r2 = foldr (\ x r !acc -> r (acc + x)) id r1 0
  in
    r2

-- fuse r1 into r2:
monolithic3 :: Int -> Int
monolithic3 n =
  let
    r2 =
      let
        cons = \ x r !acc -> r (acc + x)
        nil = id
        go i =
          (i * i) `cons`
          if i == n
            then nil
            else go (i + 1)
      in
        (if 1 > n then nil else go 1) 0
  in
    r2

-- simplify:
monolithic4 :: Int -> Int
monolithic4 n =
  let
    r0 =
      let
        go i !acc =
          if i == n
           then acc + i * i
           else go (i + 1) (acc + i * i)
      in
        if 1 > n then 0 else go 1 0
  in
    r0


-- >>> monolithic1 10000
-- 333383335000
-- >>> monolithic2 10000
-- 333383335000
-- >>> monolithic3 10000
-- 333383335000
-- >>> monolithic4 10000
-- 333383335000

