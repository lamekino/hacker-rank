solution :: String -> String -> String
solution as bs = foldr (\(x, y) acc -> x : y : acc) [] $ zip as bs

main :: IO ()
main =
  putStrLn
    . uncurry solution
    . (\[l1, l2] -> (l1, l2))
    . lines
    =<< getContents
