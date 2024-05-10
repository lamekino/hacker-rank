import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

fact :: (Integral a) => a -> a
fact n = product [1 .. n]

coefficents :: (Integral a) => a -> [a]
coefficents r = [fact r `div` (fact c * fact (r - c)) | c <- [1 .. r]]

pascalTriangle :: (Integral a) => [[a]]
pascalTriangle = [coefficents row | row <- [1 ..]]

msg :: String
msg = "error"

reader :: (Read a) => String -> a
reader = fromMaybe (error msg) . readMaybe

-- Every bind on a monad needs to be composed with a function that gives back
-- the same monadic type, in this case we can just re-wrap that value in IO with
-- return, ie
-- getNum = return . reader' . head' . take 1 . words =<< getLine
-- the fmap works also and is less redundant
getWord :: (Read a) => IO a
getWord = reader . head' . take 1 . words <$> getLine
  where
    head' xs =
      case xs of
        (h : _) -> h
        _fail -> error msg

main :: IO ()
main =
  let getInt = (getWord :: IO Int)
   in mapM_ (putStrLn . unwords . map show)
        . (`take` pascalTriangle)
        =<< getInt
