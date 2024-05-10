import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

taylorSeries :: Double -> [Double]
taylorSeries x = [x ^ n / fromIntegral (fact n) | n <- [0 ..]]
  where
    fact n = product [1 .. n]

solution :: Double -> Double
solution = sum . take 10 . taylorSeries

reader :: (Read a) => String -> a
reader = let msg = "reader" in fromMaybe (error msg) . readMaybe

main :: IO ()
main =
  mapM_ (print . solution . (\x -> reader x :: Double))
    . drop 1
    . lines
    =<< getContents
