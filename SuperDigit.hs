import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

compressDigits :: Integer -> Integer
compressDigits = fromIntegral . sum . map digitToInt . show

-- this never works despite giving the correct answer for the big test that
-- failed on HR but worked on my computer
superDigit :: Integer -> Integer
superDigit x
  | x > 10 = superDigit $ compressDigits x
  | otherwise = x

reader :: (Read a) => String -> a
reader = let msg = "error" in fromMaybe (error msg) . readMaybe

parseLine :: String -> Integer
parseLine xs =
  let [n, k] = words xs
   in reader $ concat $ replicate (reader k) n

main :: IO ()
main =
  interact $
    show
      . superDigit
      . parseLine
