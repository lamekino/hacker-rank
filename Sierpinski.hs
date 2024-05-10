import Data.Array

nullMatrix :: Int -> Int -> [[Bool]]
nullMatrix m n = replicate m $ replicate n False

sierpinski :: [[Bool]]
sierpinski = nullMatrix height width
  where
    startLeft = (0, 0) -- (1, 1) -> (2,2) -> ... -> (height, width/2)
    startRight = (0, width) -- (1, width - 1) -> ... -> (height, width/2)
    width = 32
    height = 32

main :: IO ()
main = interact $ show
