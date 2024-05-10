import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Text.Read (readMaybe)

msg :: String
msg = "text"

myGcd :: (Integral a) => a -> a -> a
myGcd x y =
  case compare x y of
    GT -> myGcd (x - y) y
    LT -> myGcd x (y - x)
    EQ -> x

readPair :: (Read a) => String -> (a, a)
readPair s =
  let reader = fromMaybe (error msg) . readMaybe
   in case words s of
        [a, b] -> (reader a, reader b)
        _fail -> error msg

main :: IO ()
main =
  interact $
    show
      . uncurry gcd
      . (\s -> readPair s :: (Int, Int))
      . head
      . take 1
      . lines
