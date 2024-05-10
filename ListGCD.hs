import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Text.Read (readMaybe)

listGCD :: (Integral a) => [a] -> a
listGCD (l1 : rest) = foldl gcd l1 rest

reader :: (Read a) => String -> a
reader = let msg = "error" in fromMaybe (error msg) . readMaybe

readInts :: String -> [Int]
readInts = map (\s -> reader s :: Int) . words

main :: IO ()
main =
  putStrLn
    . unwords
    . map (show . listGCD . readInts)
    . drop 1
    . lines
    =<< getContents
