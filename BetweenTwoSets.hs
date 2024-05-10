divides :: (Integral a) => a -> a -> Bool
divides a b = a `mod` b == 0

isBetween :: (Integral a) => [a] -> [a] -> a -> Bool
isBetween as bs x = all (x `divides`) as && all (`divides` x) bs

solution :: (Integral a) => [a] -> [a] -> Int
solution as bs =
  sum . map (fromEnum . isBetween as bs) $ [1 .. maximum (as ++ bs)]

main :: IO ()
main =
  print
    . (\[as, bs] -> solution as bs)
    . map (map (\x -> read x :: Int) . words)
    . take 2
    . drop 1
    . lines
    =<< getContents
