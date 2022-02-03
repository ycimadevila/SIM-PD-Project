module Auxiliar where

import System.IO.Unsafe                                         
import System.Random 

type Choord = (Int, Int)

getRandom :: Int -> Int -> Int 
getRandom min max = unsafePerformIO (getStdRandom (randomR (min, max)))

sumPos (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

toInt :: Float -> Int
toInt x = round x

percent perc total = perc * total / 100

inRange (x, y) totalx totaly = x >= 0 && y >=0 && x < totalx && y < totaly

moveNorth x y = (x-1, y)
moveEast x y = (x, y+1)
moveSouth x y= (x+1, y)
moveWest x y = (x, y-1)

notInList :: [Choord] -> [Choord] -> [Choord]
notInList [] _ = []
notInList (x:xr) list = if x `notElem` list 
                            then x : notInList xr list 
                            else notInList xr list
                     
findNeighbors (x, y) = concatList (moveNorth x y) (concatList (moveEast x y) (concatList (moveSouth x y) (concatList (moveWest x y) [])))  

concatList x [] = x : []
concatList x (x1:xs) = x1 : concatList x xs

concatChoordList :: Choord -> [Choord] -> [Choord]
concatChoordList x [] = x : []
concatChoordList x (x1:xs) = x1 : concatChoordList x xs

hola = print("hola")

-- bfs :: Choord -> [[Choord]] -> [Choord] -> [Choord] -> Choord
-- bfs _ [] _ _ = (-1,-1)
-- bfs pos queue seen elements = do {
--                                     let current = (queue!!0)!!0
--                                     ;if current `elem` elements
--                                         then current
--                                         else (-1,-1)
--                                     ;let temp_fatherchild = concatChoordList current (concatChoordList pos [])
--                                     ;let temp_queue = concatList temp_fatherchild (tail queue)
--                                     ;let temp_seen = concatList current seen
--                                     ;bfs current temp_queue temp_seen elements
--                                 }