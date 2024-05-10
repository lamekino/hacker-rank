import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

foldBimap :: (y1 -> y2 -> y) -> (x1 -> y1) -> (x2 -> y2) -> (x1, x2) -> y
foldBimap op f g (a, b) = f a `op` g b

isOpposite :: (Foldable t) => (a -> Bool) -> (t a, t a) -> Bool
isOpposite p = foldBimap (&&) (all p) (not . any p)

isValidPreorder :: (Ord a) => [a] -> Bool
isValidPreorder [] = True
isValidPreorder (r : cs) =
  let children@(smaller, larger) = span (< r) cs
   in isOpposite (< r) children
        && isValidPreorder smaller
        && isValidPreorder larger

reader :: (Read a) => String -> a
reader = let msg = "error" in fromMaybe (error msg) . readMaybe

readInts :: String -> [Int]
readInts = map (\s -> reader s :: Int) . words

yesNo :: Bool -> String
yesNo b = if b then "YES" else "NO"

main :: IO ()
main =
  mapM_ (putStrLn . yesNo . isValidPreorder . readInts)
    . (\lines -> [line | (num, line) <- zip [1 ..] lines, even num])
    . drop 1
    . lines
    =<< getContents
