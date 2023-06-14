
import qualified Control.Foldl as F
import Data.Char
import Data.Word

data Stats =
  MkStats
    { _numWords         :: !Int
    , _checksum         :: !Word8
    , _wordsTotalLength :: !Int
    }

main :: IO ()
main = do
  ws <- words <$> readFile "foo.txt"
  let
    MkStats numWords checksum wordsTotalLength =
      F.fold (MkStats <$> numWordsFold <*> checksumFold <*> wordsTotalLengthFold) ws

    -- defining the individual folds manaually
    numWordsFold :: F.Fold String Int
    numWordsFold = F.Fold (\ nw _ -> nw + 1) 0 id
    checksumFold :: F.Fold String Word8
    checksumFold = F.Fold (\ cs w -> cs + sum (map (fromIntegral . ord) w)) 0 id
    wordsTotalLengthFold :: F.Fold String Int
    wordsTotalLengthFold = F.Fold (\ wtl w -> wtl + length w) 0 id

    -- defining the individual folds via library functions
    _numWordsFold :: F.Fold String Int
    _numWordsFold = F.length
    _checksumFold :: F.Fold String Word8
    _checksumFold = F.handles traverse (F.premap (fromIntegral . ord) F.sum)
    _wordsTotalLengthFold :: F.Fold String Int
    _wordsTotalLengthFold = F.premap length F.sum

    averageLength :: Int
    averageLength = wordsTotalLength `div` numWords

  putStrLn ("Number of words:     " ++ show numWords)
  putStrLn ("Checksum:            " ++ show checksum)
  putStrLn ("Words total length:  " ++ show wordsTotalLength)
  putStrLn ("Average word length: " ++ show averageLength)

