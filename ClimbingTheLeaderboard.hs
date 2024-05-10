import Data.Array
import Data.List (foldl', group)

data Tree a
  = Tree a (Tree a) (Tree a)
  | Leaf
  deriving (Show, Eq)

readWords :: (Read a) => String -> [a]
readWords = map read . words

compress :: [a] -> [Bool] -> [a]
compress as bs = [a | (a, b) <- zip as bs, b]

pairwise :: (a -> a -> b) -> [a] -> [b]
pairwise op = zipWith op <*> drop 1

uniquesFromSorted :: (Eq a) => [a] -> [a]
uniquesFromSorted = compress <*> (++ [True]) . pairwise (/=)

-- binary search tree of ranks
-- if found equal, take the right branch while == e, the number of nodes
-- touched in founding e's new position should be the new rank i think
-- and since they are sorted you could do this in place in an array i think
-- using heap left right... no

buildBST :: [a] -> Tree a
buildBST xs =
  let arr = (listArray =<< (,) 1 . length) xs
      (start, finish) = bounds arr
      helper lo hi
        | lo > hi = Leaf
        | let mid = (hi + lo) `div` 2,
          otherwise =
            Tree (arr ! mid) (helper lo (mid - 1)) (helper (mid + 1) hi)
   in helper start finish

treeSolution :: (Ord a) => Tree a -> a -> Int
treeSolution Leaf _ = 0
treeSolution (Tree root left right) e
  | root >= e = 1 + treeSolution left e
  | otherwise = fromEnum (root < e)

solution' :: (Num a, Ord a) => [a] -> [a] -> [Int]
solution' rs = let bst = buildBST rs in map (treeSolution bst)

solution :: (Num a, Ord a) => [a] -> a -> Int
solution rs ps =
  foldl' (\acc x -> acc + fromEnum (x > ps)) 1 $
    uniquesFromSorted rs

solution3 :: (Num a, Ord a) => [a] -> a -> Int
solution3 rs p = fromEnum (null gts) + fromEnum (null lts) + length gts
  where
    (gts, lts) = span (>= p) $ uniquesFromSorted rs

main :: IO ()
main =
  mapM_ print
    . (\(rs, ps) -> map (solution3 rs) ps)
    . (\(rs, ps) -> (readWords rs, readWords ps) :: ([Int], [Int]))
    . (\[_, l2, _, l4] -> (l2, l4))
    . lines
    =<< getContents
