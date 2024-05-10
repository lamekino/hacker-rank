import Data.Bifunctor (bimap, first)
import Data.List (find, group)
import Data.Maybe (listToMaybe)

dup a = (a, a)

-- solution :: Int -> [Int] -> Int
solution n cs =
  maximum
    . map (uncurry (+) . (`divMod` 2) . sum)
    . group
    . map (fromEnum . not . (`elem` cs))
    $ [0 .. n - 1]

main :: IO ()
main =
  print
    . uncurry solution
    . (\(x : xs) -> (read x, map read . drop 1 $ xs) :: (Int, [Int]))
    . words
    =<< getContents
