module Compression where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S

{-------------------------------------------------------------------------------
  Compression and decompression of single "frames" (assumed given)
-------------------------------------------------------------------------------}

type Compressed   = S.ByteString
type Uncompressed = S.ByteString
type Leftover     = S.ByteString

compress :: Uncompressed -> Compressed
compress = undefined

decompress :: Compressed -> (Uncompressed, Leftover)
decompress = undefined

{-------------------------------------------------------------------------------
  Streaming API: simply concatenate all frames

  This version of `decompressLazy` has a subtle bug; the point is to discover
  this through duality.
-------------------------------------------------------------------------------}

compressLazy :: L.ByteString -> L.ByteString
compressLazy = L.fromChunks . map compress . L.toChunks

decompressLazy :: L.ByteString -> L.ByteString
decompressLazy = L.fromChunks . go mempty . L.toChunks
  where
    go :: S.ByteString -> [S.ByteString] -> [S.ByteString]
    go leftover [] =
        if S.null leftover
          then []
          else error "Leftover data"
    go leftover (bs:bss) =
        let (frame, leftover') = decompress (leftover <> bs)
        in frame : go leftover' bss

{-------------------------------------------------------------------------------
  Looking at these two definitions, we might wonder why `decompressLazy` is
  so much more complicated than `compressLazy`. Could `decompressLazy` be
  simpler if we thought harder, or is `compressLazy` _too_ simple because we
  did not think hard enough?

  Could we make these two definitions look more dual to each other?

  During decompression we have leftover data after processing each chunk from
  the lazy bytestring, whereas during compression we're just processing each
  chunk as it comes. This could result in frames of widely varying size. It
  would be better to construct frames of a specific size.
-------------------------------------------------------------------------------}

idealFrameSize :: Int
idealFrameSize = undefined

compressFrames :: S.ByteString -> ([S.ByteString], S.ByteString)
compressFrames = go []
  where
    go :: [S.ByteString] -> S.ByteString -> ([S.ByteString], S.ByteString)
    go acc bs =
        if S.length bs < idealFrameSize
          then (reverse acc, bs)
          else let (frame, leftover) = S.splitAt idealFrameSize bs
               in go (frame:acc) leftover

compressLazy' :: L.ByteString -> L.ByteString
compressLazy' = L.fromChunks . go mempty . L.toChunks
  where
    go :: S.ByteString -> [S.ByteString] -> [S.ByteString]
    go leftover [] =
        if S.null leftover
          then []
          else [compress leftover]
    go leftover (bs:bss) =
        let (frames, leftover') = compressFrames (leftover <> bs)
        in frames ++ go leftover' bss

{-------------------------------------------------------------------------------
  This is a useful generalization. Moreover `compressLazy` and `decompressLazy`
  now look much more similar. There is still a difference though: we get
  _multiple_ frames from `splitIntoFrames`, but we have only a _single_ frame in
  `decompresssLazy`. Turns out, this is a bug! A single chunk of the bytestring
  _could_ contain multiple frames; if we don't decompress all of them, then by
  the time we get to the end of the list of chunks, we might incorrectly report
  a decompression error.
-------------------------------------------------------------------------------}

decompressFrames :: S.ByteString -> ([S.ByteString], S.ByteString)
decompressFrames = go []
  where
    go :: [S.ByteString] -> S.ByteString -> ([S.ByteString], S.ByteString)
    go acc bs =
        let (frame, leftover) = decompress bs
        in if S.null frame
             then (reverse acc, leftover)
             else go (frame : acc) leftover

decompressLazy' :: L.ByteString -> L.ByteString
decompressLazy' = L.fromChunks . go mempty . L.toChunks
  where
    go :: S.ByteString -> [S.ByteString] -> [S.ByteString]
    go leftover [] =
        if S.null leftover
          then []
          else error "Leftover data"
    go leftover (bs:bss) =
        let (frames, leftover') = decompressFrames (leftover <> bs)
        in frames ++ go leftover' bss
