import Control.Exception (assert)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

type Point a = (a, a)

slope :: (Eq a, Floating a) => Point a -> Point a -> a
slope (x1, y1) (x2, y2) =
  case dx of
    0 -> 0
    _ -> dy / dx
  where
    dx = x2 - x1
    dy = y2 - y1

pushBack :: [a] -> a -> [a]
pushBack xs e = xs ++ [e]

rotate :: Int -> [b] -> [b]
rotate n xs = take (length xs) (drop n (cycle xs))

-- isConcavePolygon :: (Ord a, Floating a) => [Point a] -> Bool
isConcavePolygon =
  map atan . (zipWith slope <*> reverse)

reader :: (Read a) => String -> a
reader = let msg = "reader" in fromMaybe (error msg) . readMaybe

readPairs :: (Read a) => String -> [(a, a)]
readPairs input =
  let (_ : ts) = (map words . lines) input
   in map (\[a, b] -> (reader a, reader b)) ts

-- main :: IO ()
-- main =
--   interact $
--     (\b -> if b then "YES" else "NO")
--       . isConcavePolygon
--       . readPoints
--   where
--     readPoints s = readPairs s :: [Point Double]
