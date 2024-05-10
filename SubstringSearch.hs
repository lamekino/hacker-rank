import Data.Array
import Data.Bifunctor (first)

arrayOf :: [a] -> Array Int a
arrayOf xs = listArray (0, length xs) xs

pairNeighbors :: [a] -> [(a, a)]
pairNeighbors (a : b : cs) = (a, b) : pairNeighbors cs
pairNeighbors _ = []

kmpTable :: Array Int Char -> Array Int Int
kmpTable word =
  let table =
        array
          (start, finish + 1)
          ((start, -1) : [(i + 1, testChar table i) | i <- indices word])
   in table
  where
    (start, finish) = bounds word
    testChar t idx
      | word ! idx == word ! (idx + 1) = t ! idx
      | t ! idx >= 0 = t ! idx
      | otherwise = idx {- WARN: -}

kmpSearch :: String -> String -> Bool
kmpSearch haystack needle = True
  where
    hs = arrayOf haystack
    ns = arrayOf needle
    table = kmpTable ns

yesNo :: Bool -> String
yesNo b = if b then "YES" else "NO"

main :: IO ()
main =
  mapM_ (putStrLn . yesNo . uncurry kmpSearch)
    . pairNeighbors
    . drop 1
    . lines
    =<< getContents
