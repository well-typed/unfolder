{-# LANGUAGE CPP #-}

module Swap (main) where

#ifdef PREVENT_INLINING
fst' :: (a, b) -> a
{-# NOINLINE fst' #-}
fst' = fst

snd' :: (a, b) -> b
{-# NOINLINE snd' #-}
snd' = snd
#endif

-- | Swap the argument @n@ times
swapN :: Int -> (a, a) -> (a, a)
swapN 0 (x, y) = (x, y)
swapN n (x, y) = let result = swapN (n - 1) (x, y)
#ifdef CASE
                 in ( case result of (_, b) -> b
                    , case result of (a, _) -> a
                    )
#endif
#ifdef FST_SND
                 in ( snd result
                    , fst result
                    )
#endif
#ifdef PREVENT_INLINING
                 in ( snd' result
                    , fst' result
                    )
#endif

main :: IO ()
main = print $ swapN 1_000_000 (True, False)