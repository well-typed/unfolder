
import Data.Char
import Data.Word

main :: IO ()
main = do
  ws1 <- words <$> readFile "foo.txt"
  ws2 <- words <$> readFile "foo.txt"
  ws3 <- words <$> readFile "foo.txt"
  let
    numWords :: Int
    numWords = length ws1

    checksum :: Word8
    checksum = sum $ concatMap (map (fromIntegral . ord)) ws2

    wordsTotalLength :: Int
    wordsTotalLength = sum $ map length ws3

    averageLength :: Int
    averageLength = wordsTotalLength `div` numWords

  putStrLn ("Number of words:     " ++ show numWords)
  putStrLn ("Checksum:            " ++ show checksum)
  putStrLn ("Words total length:  " ++ show wordsTotalLength)
  putStrLn ("Average word length: " ++ show averageLength)

