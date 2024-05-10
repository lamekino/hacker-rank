import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

fibonacci :: (Integral a) => [a]
fibonacci =
  let f 0 = 0
      f 1 = 1
      f n = f (n - 1) + f (n - 2)
   in [f x | x <- [0 ..]]

reader :: (Read a) => String -> a
reader = fromMaybe (error msg) . readMaybe
  where
    msg = "text"

main :: IO ()
main =
  interact $
    show
      . (!!) fibonacci
      . subtract 1
      . (\s -> reader s :: Int)
      . head
      . take 1
      . lines
