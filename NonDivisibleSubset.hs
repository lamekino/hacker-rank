import Data.Bifunctor (second)
import Data.List (delete, nub, sort, (\\))

-- Given a set of *distinct* integers, print the size of a maximal subset of S
-- where the sum of any 2 numbers in S' is not evenly divisible by k.
-- solution :: (Integral a) => a -> [a] -> Int
solution k xs =
  -- length
  --   . nub
  -- concatMap (\(n, ns) -> if null ns then [] else n : ns)
  -- remove the elements which don't divide k in current list
  map (\p -> second (filter (\aj -> (fst p + aj) `mod` k /= 0)) p)
    -- [1,7,4,2] -> [(1,[7,4,2]), (7, [4, 2]), (4, [2])]
    . (zip <*> init . zipWith drop [0 ..] . map (`delete` xs))
    $ xs

main :: IO ()
main =
  print
    . ( \[one, two] ->
          let k = read . (!! 1) . words $ one :: Int
              xs = map read . words $ two :: [Int]
           in solution k xs
      )
    . take 2
    . lines
    =<< getContents
