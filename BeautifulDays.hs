import Data.Function (on)

divides k = (== 0) . (`mod` k)

reverseNum :: Int -> Int
reverseNum =
  let pushLastDigit n x place = n + (10 ^ place) * (x `mod` 10)
   in snd
        . foldr (\d (pos, acc) -> (pos + 1, pushLastDigit acc d pos)) (0, 0)
        . takeWhile (/= 0)
        . iterate (`div` 10)

solution :: Int -> Int -> Int -> Int
solution k lo hi =
  sum . map (fromEnum . divides k . abs . uncurry (-) . ((,) =<< reverseNum)) $
    [lo .. hi]

main :: IO ()
main =
  print
    . (\[lo, hi, k] -> solution k lo hi)
    . map (\x -> read x :: Int)
    . take 3
    . words
    . (!! 0)
    . lines
    =<< getContents
