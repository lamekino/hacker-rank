import Data.Bifunctor (first)

solution :: Int -> [Int] -> Bool
solution k = (k >) . sum . map (fromEnum . (<= 0))

yesNo :: Bool -> String
yesNo yes = if yes then "YES" else "NO"

groupAdj :: [a] -> [(a, a)]
groupAdj [] = []
groupAdj (a : b : cs) = (a, b) : groupAdj cs

main :: IO ()
main =
  mapM_ (putStrLn . yesNo . uncurry solution . first (!! 1))
    . groupAdj
    . map (map (\x -> read x :: Int) . words)
    . drop 1
    . lines
    =<< getContents
