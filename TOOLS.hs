pairwisePermutation :: (Eq a) => (a -> a -> b) -> [a] -> [b]
pairwisePermutation op xs =
  concatMap (\x -> [op x a | a <- xs, a /= x]) xs

pairwise :: (a -> a -> b) -> [a] -> [b]
pairwise op = zipWith op <*> drop 1
