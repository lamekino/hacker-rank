import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Text.Read (readMaybe)

resolution :: (Num a) => a
resolution = 1000

polynomial :: (Floating a) => [(a, a)] -> (a -> a)
polynomial ((a1, n1) : terms) =
  foldr (\f g x -> f x + g x) (form a1 n1) [form a n | (a, n) <- terms]
  where
    form a n x = a * x ** n

riemann :: (Enum a, Floating a) => a -> a -> a -> (a -> a) -> a
riemann espilon lo hi f =
  let domain = [lo, lo + espilon .. hi]
      dx = (hi - lo) / fromIntegral (length domain)
   in sum [dx * f x | x <- domain]

solution :: (Enum a, Floating a) => a -> a -> [(a, a)] -> (a, a)
solution l r terms =
  let p = polynomial terms
      integrate = riemann (recip resolution) l r
      volume f = pi * integrate ((** 2) . f)
   in (integrate p, volume p)

reader :: (Read a) => IO [a]
reader = map (fromMaybe (error msg) . readMaybe) . words <$> getLine
  where
    msg = "error msg"

main :: IO ()
main = do
  as <- reader :: IO [Double]
  bs <- reader :: IO [Double]
  [l, r] <- reader :: IO [Double]

  let (area, volume) = solution l r (zip as bs)
  putStrLn $ printf "%.1f" area
  putStrLn $ printf "%.1f" volume
