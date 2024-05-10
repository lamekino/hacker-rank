import Data.List (foldl', inits)

foldBimap :: (y1 -> y2 -> y) -> (x1 -> y1) -> (x2 -> y2) -> (x1, x2) -> y
foldBimap op f g (a, b) = f a `op` g b

bimap :: (x1 -> y1) -> (x2 -> y2) -> (x1, x2) -> (y1, y2)
bimap = foldBimap (,)

bias :: (Eq a, Num b) => a -> a -> a -> b
bias down up x
  | x == down = -1
  | x == up = 1
  | otherwise = 0

redGreenBias :: (Num b) => Char -> b
redGreenBias = bias 'R' 'G'

yellowBlueBias :: (Num b) => Char -> b
yellowBlueBias = bias 'Y' 'B'

countColors :: (Num a) => (a, a) -> Char -> (a, a)
countColors ts x = bimap (+ redGreenBias x) (+ yellowBlueBias x) ts

colorDifference :: (Num a) => [Char] -> (a, a)
colorDifference = foldl' countColors (0, 0)

isValidScheme :: [Char] -> Bool
isValidScheme = foldBimap (&&) (== 0) (== 0) . colorDifference

hasValidPrefixes :: [Char] -> Bool
hasValidPrefixes =
  all (foldBimap (&&) (<= 1) (<= 1))
    . scanl countColors (0, 0)

solution :: String -> Bool
solution colors = isValidScheme colors && hasValidPrefixes colors

trueFalse :: Bool -> String
trueFalse b = if b then "True" else "False"

main :: IO ()
main =
  mapM_ (putStrLn . trueFalse . solution)
    . drop 1
    . lines
    =<< getContents
