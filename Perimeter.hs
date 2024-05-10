import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Text.Read (readMaybe)

msg :: String
msg = "error"

type Point a = (a, a)

dist :: (Floating a) => Point a -> Point a -> a
dist (x1, y1) (x2, y2) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2

rotate :: Int -> [b] -> [b]
rotate n xs = take (length xs) (drop n (cycle xs))

perimeter :: (Floating a) => [Point a] -> a
perimeter = sum . (zipWith dist <*> rotate 1)

bundle :: (a -> b) -> [a] -> (b, b)
bundle f xs =
  let [a, b] = map f $ take 2 xs
   in (a, b)

readPairs :: (Read a) => String -> [(a, a)]
readPairs input =
  let (_ : ts) = (map words . lines) input
   in map (bundle (fromMaybe (error msg) . readMaybe)) ts

main :: IO ()
main = putStrLn . printf "%f\n" . perimeter . readPoints =<< getContents
  where
    readPoints s = readPairs s :: [Point Double]
