module Auxiliar where

import System.IO.Unsafe                                         
import System.Random 
 
type Choord = [Int]

origen :: Choord
origen = [0, 1]

getRandom :: Int -> Int -> Int 
getRandom min max = unsafePerformIO (getStdRandom (randomR (min, max)))

sumPos (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

toInt :: Float -> Int
toInt x = round x

percent perc total = perc * total / 100

inRange x y totalx totaly = x >= 0 && y >=0 && x < totalx && y < totaly

moveNorth x y = (x - 1, y)
moveSouth x y = (x + 1, y)
moveEast  x y = (x, y + 1)
moveWest  x y = (x, y - 1)

 