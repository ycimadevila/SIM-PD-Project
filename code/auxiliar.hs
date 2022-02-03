module Auxiliar where

import System.IO.Unsafe                                         
import System.Random 

-- type that defines the coordinates on the board
type Coord = (Int, Int)

-- random
getRandom :: Int -> Int -> Int 
getRandom min max = unsafePerformIO (getStdRandom (randomR (min, max)))

-- coordinate sum
sumPos (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

-- approximation to integers
toInt :: Float -> Int
toInt x = round x

-- calculates the percentage of a number x with respect to a total y
percent perc total = perc * total / 100

-- returns if a position does not go off the board
inRange (x, y) totalx totaly = x >= 0 && y >=0 && x < totalx && y < totaly

-- surrounding positions
moveNorth x y = (x-1, y)
moveEast x y = (x, y+1)
moveSouth x y= (x+1, y)
moveWest x y = (x, y-1)

-- surrounding moves with respect to an integer 
moveDirection :: Coord -> Int -> Coord
moveDirection (x, y) i  | i == 0 = (x+1, y) -- south
                        | i == 1 = (x, y+1) -- east
                        | i == 2 = (x-1, y) -- north
                        | i == 3 = (x, y-1) -- west
                        
-- returns the elements that belong to the first list that do not belong to the second
notInList :: [Coord] -> [Coord] -> [Coord]
notInList [] _ = []
notInList (x:xr) list = if x `notElem` list 
                            then x : notInList xr list 
                            else notInList xr list

-- returns a list with the surrounding positions               
findNeighbors (x, y) = concatList (moveSouth x y) (concatList (moveNorth x y) (concatList (moveEast x y) (concatList (moveSouth x y) [])))  

-- validates that the provided position does not leave the board
validPos :: [Coord] -> Int -> Int -> [Coord]
validPos [] _ _ = []
validPos (ch : xr) n m  | inRange ch n m = ch : validPos xr n m
                        | otherwise = validPos xr n m


-- concatenates an element to a list of elements of the same type
concatList x [] = x : []
concatList x (x1:xs) = x1 : concatList x xs


-- look for the father in the relationship tree
lookForFather :: Coord -> [[Coord]] -> [Coord]
lookForFather _ [] = [(-1, -1), (-1, -1)] -- break
lookForFather ch (x:xr) | ch == (x!!0) = x
                        | otherwise = lookForFather ch xr


-- finds the closest ancestor to the initial in the relationship tree
lookBack :: Coord -> [Coord] -> [[Coord]] -> Coord
lookBack destiny current elems | (current!!1) == destiny = current!!0
                               | otherwise = lookBack destiny (lookForFather (current!!1) elems) elems

tupleToList (x, y) = concatList y (concatList x [])

-- generates a list of n equal elements
generateUniList _ 0 = []
generateUniList x n = x : generateUniList x (n - 1)

__relationChildParent__ x y = concatList y (concatList x [])

-- creates the list that contains the element with its parent
asosiateParent :: [Coord] -> Coord -> [[Coord]]
asosiateParent [] _ = []
asosiateParent (x: xr) ch = (__relationChildParent__ x ch) : asosiateParent xr ch

-- take the first element of a list for each element of the list
getInitials :: [[Coord]] -> [Coord]
getInitials [] = []
getInitials (x:xr) = x!!0 : getInitials xr 

-- ###########
-- ### BFS ###
-- ###########
    -- ### PARAMS ###
        -- queue_tail@(tail queue)
        -- queue_head@(head queue)
        -- h@(findNeighbors pos)
        -- neighb@(notInList h elements)
        -- temp_queue@(queue_tail++(asosiateParent neighb pos))
        -- temp_pos@((head temp_queue)!!0)
        -- temp_seen@(concatList queue_head seen)
    -- ### RETURN ###
        -- bfs temp_pos temp_seen temp_queue elements
bfs :: Coord -> Coord -> [[Coord]] -> [[Coord]] -> [Coord] -> Int -> Int-> [Coord]
bfs _ _ _ [] _ _ _ = [(-1,-1), (-1, -1)]
bfs init pos seen queue elements n m  | pos `elem` elements =  concatList pos (concatList (lookBack init (head queue) seen) []) 
                            | otherwise = bfs init 
                                                ((head ((tail queue)++(asosiateParent (notInList (validPos (findNeighbors pos) n m) (getInitials seen)) pos)))!!0) 
                                                (concatList (head queue) seen) --
                                                ((tail queue)++(asosiateParent (notInList (validPos (findNeighbors pos) n m) (getInitials seen)) pos)) 
                                                elements
                                                n m

seen::[[Coord]]
seen = []
queue:: [[Coord]]
queue = [[(0, 1), (0, 1)]]
init::Coord 
init = (queue!!0)!!0
elements :: [Coord]
elements=[(2, 0), (1, 2)]
