
import Data.Char
import Data.List (foldl')
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
      foldl' update (MkStats 0 0 0) ws

    update :: Stats -> String -> Stats
    update (MkStats nw cs wtl) w =
      MkStats
        (nw + 1)
        (cs + sum (map (fromIntegral . ord) w))
        (wtl + length w)

    averageLength :: Int
    averageLength = wordsTotalLength `div` numWords

  putStrLn ("Number of words:     " ++ show numWords)
  putStrLn ("Checksum:            " ++ show checksum)
  putStrLn ("Words total length:  " ++ show wordsTotalLength)
  putStrLn ("Average word length: " ++ show averageLength)

