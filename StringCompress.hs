import Data.List (group)

solution :: String -> String
solution =
  concatMap (\s@(h : t) -> if null t then s else h : show (length s))
    . group

main :: IO ()
main =
  putStrLn
    . solution
    . (\(s : _) -> s)
    . lines
    =<< getContents
