module Main (main) where

import Criterion.Main

import HOS.Speculative qualified as Speculative
import HOS.Circular    qualified as Circular

main :: IO ()
main = defaultMain [
      bgroup "speculative" $
        [ bench (show sz) $ nf Speculative.large sz
        | sz <- [100, 200 .. 1_000]
        ]
    , bgroup "circular" $
        [ bench (show sz) $ nf Circular.large sz
        | sz <- [100, 200 .. 1_000]
        ]
    ]
