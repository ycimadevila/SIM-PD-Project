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

lookForFather :: Choord -> [[Choord]] -> [Choord]
lookForFather _ [] = [(-1, -1), (-1, -1)] -- break
lookForFather ch (x:xr) | ch == (x!!0) = x
                        | otherwise = lookForFather ch xr

lookBack :: Choord -> [Choord] -> [[Choord]] -> Choord
lookBack destiny current elems | (current!!1) == destiny = current!!0
                               | otherwise = lookBack destiny (lookForFather (current!!1) elems) elems

tupleToList (x, y) = concatList y (concatList x [])

generateUniList _ 0 = []
generateUniList x n = do{
                    let ant = n - 1
                    ;x : generateUniList x ant
                }

relationChildParent x y = concatList y (concatList x [])

asosiateParent :: [Choord] -> Choord -> [[Choord]]
asosiateParent [] _ = []
asosiateParent (x: xr) ch = (relationChildParent x ch) : asosiateParent xr ch


-- testing area
-- hola = print("hola")
-- lista :: [[Choord]]
-- lista = [[(2, 0), (1, 0)], [(1, 0), (0, 0)], [(0, 0), (0, 0)], [(3, 0),(2, 0)], [(3, 1), (3, 0)]]


-- bfs :: Choord -> [[Choord]] -> [Choord] -> [Choord] -> Choord
-- bfs _ [] _ _ = (-1,-1)
-- bfs pos queue seen elements = do {
--                     let current = (queue!!0)!!0
--                     if current `elem` elements
--                         then current
--                         else {let temp_fatherchild = concatChoordList current (concatChoordList pos [])
--                             let temp_queue = concatList temp_fatherchild (tail queue)
--                             let temp_seen = concatList current seen
--                             bfs current temp_queue temp_seen elements
--                     }
--                 }


bfs :: Choord -> [[Choord]] -> [[Choord]] -> [Choord] -> Choord
bfs _ [] _ _ = (-1,-1)
bfs pos seen queue elements | pos `elem` elements = pos
                            | otherwise = do {
                                let queue_tail = tail queue
                                ;let queue_head = head queue
                                ;let h = findNeighbors pos
                                ;let neighb = notInList h elements
                                ;let temp_queue = queue_tail++(asosiateParent neighb pos)
                                ;let temp_pos = (head temp_queue)!!0
                                ;let temp_seen = concatList queue_head seen
                                ;bfs temp_pos temp_seen temp_queue elements
                            }