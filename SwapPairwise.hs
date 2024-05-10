import Data.Array
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

myRead :: (Read a) => String -> a
myRead = let msg = "reader" in fromMaybe (error msg) . readMaybe

solution :: [a] -> [a]
solution (a : b : cs) = b : a : solution cs
solution [a] = [a]
solution [] = []

main :: IO ()
main =
  mapM_ (putStrLn . solution)
    . drop 1
    . lines
    =<< getContents
