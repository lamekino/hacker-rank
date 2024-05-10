import Data.Array
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

data Tree a
  = Tree a (Tree a) (Tree a)
  | Leaf
  deriving (Show, Eq)

myRead :: (Read a) => String -> a
myRead = let msg = "reader" in fromMaybe (error msg) . readMaybe

readFlat :: (Read a) => [String] -> [a]
readFlat = map myRead . words . unwords

-- readTree :: (Read a, Eq a) => a -> a -> [String] -> Tree a
readTree :: (Read a) => a -> [String] -> Array Int a
readTree root xs = listArray (1, length xs + 1) $ root : readFlat xs

-- TODO: make this return an array, see:
-- https://hackage.haskell.org/package/array-0.5.6.0/docs/Data-Array.html#v:-47--47-
swapAtDepthArrayArray :: Array Int Int -> Int -> Array Int Int
swapAtDepthArrayArray tree target =
  let helper idx
        | idx > finish = tree
        | floor $ logBase 2 idx >= target =
            swapAtDepthArrayArray (tree // swap idx) target (idx + 1)
        | otherwise =
            swapAtDepthArrayArray tree target (idx + 1)
   in helper start
  where
    (start, finish) = bounds tree
    swap idx = [(left idx, tree ! right idx), (right idx, tree ! left idx)]
    left i = 2 * i -- WARN: 1 indexed
    right i = left i + 1

swapAtDepthArray :: Array Int Int -> Int -> Tree Int
swapAtDepthArray tree target =
  let (start, finish) = bounds tree
      left i = 2 * i -- WARN: 1 indexed
      right i = left i + 1
      helper cur idx
        | idx > finish || tree ! idx == -1 = Leaf
        | cur >= target =
            Tree
              (tree ! idx)
              (helper (cur + 1) (right idx))
              (helper (cur + 1) (left idx))
        | otherwise =
            Tree
              (tree ! idx)
              (helper (cur + 1) (left idx))
              (helper (cur + 1) (right idx))
   in helper 1 start

swapAtDepth :: Tree a -> Int -> Tree a
swapAtDepth Leaf _ = Leaf
swapAtDepth tree target = swapAtDepthIdx 1 tree
  where
    swapAtDepthIdx _ Leaf = Leaf
    swapAtDepthIdx cur (Tree root left right) =
      let next = swapAtDepthIdx (cur + 1)
       in if target <= cur
            then Tree root (next right) (next left)
            else Tree root (next left) (next right)

inorderTraversalArray :: Array Int Int -> [Int]
inorderTraversalArray tree =
  let (start, finish) = bounds tree
      helper idx
        | idx > finish || tree ! idx == -1 = []
        | otherwise =
            helper (left idx) ++ [tree ! idx] ++ helper (right idx)
   in helper start
  where
    left i = 2 * i
    right i = left i + 1

inorderTraversal :: Tree a -> [a]
inorderTraversal Leaf = []
inorderTraversal (Tree root left right) =
  inorderTraversal left ++ [root] ++ inorderTraversal right

solution tree = scanl swapAtDepthArray tree

main :: IO ()
main =
  mapM_ (putStrLn . unwords . map show)
    -- print
    . ( \(n : rest) ->
          let nodes = myRead n :: Int
              -- tree = readTree (-1) $ take nodes rest :: Array Int Int
              depths = readFlat $ drop (nodes + 1) rest :: [Int]
           in undefined
      )
    . lines
    =<< getContents
