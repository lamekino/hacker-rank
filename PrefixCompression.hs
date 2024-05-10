import Text.Printf (printf)

compress :: [a] -> [Bool] -> [a]
compress as bs = [a | (a, b) <- zip as bs, b]

lengthPair :: (Foldable t) => t a -> (Int, t a)
lengthPair = (,) =<< length

solution :: String -> String -> [(Int, String)]
solution as bs =
  let pre@(idx, _) = lengthPair . map fst . takeWhile (uncurry (==)) $ zip as bs
      ya = lengthPair $ drop idx as
      yb = lengthPair $ drop idx bs
   in [pre, ya, yb]

main :: IO ()
main =
  mapM_ (putStrLn . uncurry (printf "%d %s"))
    . (\[xs, ys] -> solution xs ys)
    . take 2
    . lines
    =<< getContents
