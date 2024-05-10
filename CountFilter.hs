import Data.List (elemIndices, nub)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

occurances :: (Eq a) => Int -> [a] -> [a]
occurances k xs = nub $ filter ((>= k) . numOccurances xs) xs
  where
    numOccurances xs = length . flip elemIndices xs

reader :: (Read a) => String -> a
reader = let msg = "error" in fromMaybe (error msg) . readMaybe

parseList :: [[a]] -> [(a, [a])]
parseList xss =
  case xss of
    [] -> []
    ([_, k] : xs : rest) -> (k, xs) : parseList rest
    _fail -> error "parse error"

formatShow :: (Show a) => [a] -> String
formatShow [] = show (-1)
formatShow xs = unwords $ map show xs

main :: IO ()
main =
  mapM_ (putStrLn . formatShow . uncurry occurances)
    . parseList
    . map (map readInt . words)
    . drop 1
    . lines
    =<< getContents
  where
    readInt s = reader s :: Int
