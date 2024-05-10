import Data.Bifunctor (bimap)
import Data.List (singleton)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Text.Read (readMaybe)

type Point a = (a, a)

symmetricCombine :: (a -> a -> b) -> [a] -> [b]
symmetricCombine op = (zipWith op <*> tail) . ((++) <*> (singleton . head))

polygonArea :: (Floating a) => [Point a] -> a
polygonArea =
  (/ 2)
    . sum
    . uncurry (zipWith (*))
    . bimap
      (pairwise (-) . frontToBack) -- x_2-x_1, x_3-x_2, ... x_n-x_n-1, x_n-x1
      (pairwise (+) . frontToBack) -- x_2+x_1, x_3+x_2, ... x_n+x_n-1, x_n+x1
    . unzip
  where
    -- (zipWith op <*> tail) does op on adjacent elements in a list
    pairwise op = zipWith op <*> tail
    -- ((++) <*> (singleton . head)) appends the first element of a list to the
    -- list
    frontToBack = (++) <*> (singleton . head)

reader :: (Read a) => String -> a
reader = let msg = "error" in fromMaybe (error msg) . readMaybe

readPairs :: (Read a) => String -> [(a, a)]
readPairs input =
  let (_ : ts) = (map words . lines) input
   in map (\[a, b] -> (reader a, reader b)) ts

main :: IO ()
main =
  interact $
    printf "%f"
      . polygonArea
      . (\s -> readPairs s :: [Point Double])
