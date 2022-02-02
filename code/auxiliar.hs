module Auxiliar where

import System.IO.Unsafe                                         
import System.Random 
 
getRandom :: Int -> Int -> Int 
getRandom min max = unsafePerformIO (getStdRandom (randomR (min, max)))

sumPos (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

toInt :: Float -> Int
toInt x = round x

percent perc total = perc * total / 100

