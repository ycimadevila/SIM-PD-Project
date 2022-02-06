module Auxiliar where

import System.IO.Unsafe                                         
import System.Random 

-- type that defines the coordinates on the board
type Coord = (Int, Int)

-- random
getRandom :: Int -> Int -> Int -- getStdRandom
getRandom min max = unsafePerformIO (getStdRandom (randomR (min, max)))

-- coordinate sum
sumPos (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)


-- calculates the percentage of a number x with respect to a total y
percent_ :: Int -> Int -> Int
percent_ perc total = floor (realToFrac((perc * total) `div` 100))

-- returns if a position does not go off the board
inRange (x, y) totalx totaly = x >= 0 && y >=0 && x < totalx && y < totaly

-- surrounding positions
moveNorth x y = (x-1, y)
moveEast x y  = (x, y+1)
moveSouth x y = (x+1, y)
moveWest x y  = (x, y-1)

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
findNeighbors (x, y) = concatList (moveWest x y) (concatList (moveNorth x y) (concatList (moveEast x y) (concatList (moveSouth x y) [])))  

-- validates that the provided positions does not leave the board
validPos :: [Coord] -> Int -> Int -> [Coord]
validPos [] _ _ = []
validPos (ch : xr) n m  | inRange ch n m = ch : validPos xr n m
                        | otherwise = validPos xr n m


-- concatenates an element to a list of elements of the same type
concatList x [] = x : []
concatList x (x1:xs) = x1 : concatList x xs

-- get surrounding position list
getGrid :: Coord -> [Coord]
getGrid (x, y) = [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]


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

-- get a new list from another without an x element
__listWithoutElem__ _ list [] = list
__listWithoutElem__ i list (x:xr) = if i == x
                                    then list ++ xr
                                    else __listWithoutElem__ i (concatList x list) xr
listWithoutElem i list = __listWithoutElem__ i [] list

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
bfs :: Coord -> Coord -> [[Coord]] -> [[Coord]] -> [Coord] -> [Coord] -> Int -> Int-> [Coord]
bfs _ _ _ _ [] _ _ _ = [(-1,-1), (-1, -1)]
bfs _ _ _ [] _ _ _ _ = [(-1,-1), (-1, -1)]
bfs init pos seen queue elements notemty n m  | pos `elem` elements =  concatList pos (concatList (lookBack init (head queue) seen) []) 
                            | otherwise = bfs init 
                                                ((head ((tail queue)++(asosiateParent (notInList (notInList (validPos (findNeighbors pos) n m) (getInitials seen)) notemty) pos)))!!0) 
                                                ((head queue): seen) --
                                                ((tail queue)++(asosiateParent (notInList (notInList (validPos (findNeighbors pos) n m) (getInitials seen)) notemty) pos)) 
                                                elements
                                                notemty
                                                n m

getListToPrint :: Coord -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [String]
getListToPrint current [] robots dirt child objects babypen babypenused robotbaby  
                            | elem current robotbaby   = ["_R_"]  
                            | elem current robots      = ["_r_"] 
                            | elem current dirt        = ["_d_"] 
                            | elem current child       = ["_c_"] 
                            | elem current objects     = ["_m_"]
                            | elem current babypenused = ["_X_"] 
                            | elem current babypen     = ["_v_"]
                            | otherwise                = ["_._"] 
getListToPrint current (x:xr) robots dirt child objects babypen babypenused robotbaby 
                            | elem current robotbaby    = "_R_" : getListToPrint x xr robots dirt child objects babypen babypenused robotbaby
                            | elem current robots       = "_r_" : getListToPrint x xr robots dirt child objects babypen babypenused robotbaby
                            | elem current dirt         = "_d_" : getListToPrint x xr robots dirt child objects babypen babypenused robotbaby
                            | elem current child        = "_c_" : getListToPrint x xr robots dirt child objects babypen babypenused robotbaby
                            | elem current objects      = "_m_" : getListToPrint x xr robots dirt child objects babypen babypenused robotbaby
                            | elem current babypenused  = "_X_" : getListToPrint x xr robots dirt child objects babypen babypenused robotbaby
                            | elem current babypen      = "_v_" : getListToPrint x xr robots dirt child objects babypen babypenused robotbaby
                            | otherwise                 = "_._" : getListToPrint x xr robots dirt child objects babypen babypenused robotbaby
-- current x, current y, total x, total y
getBoxes :: Int -> Int -> Int -> Int -> [Coord]
getBoxes x y totalx totaly  | (x /= totalx && y /= totaly) = (x, y) : getBoxes x (y+1) totalx totaly
                            |  y == totaly = getBoxes (x + 1) 0 totalx totaly
                            |  x == totalx = [] 
                            |  otherwise = []

-- the rows of elements with respect to the m columns 
createListWordFromString :: [String] -> Int -> [String]
createListWordFromString [] _ = []
createListWordFromString list n = (unwords (take n list)) : createListWordFromString (drop n list) n

-- receives a list of strings and prints them
myPrinter :: [String] -> String
myPrinter [] = "\n"
myPrinter (x:xr) = x++"\n"++myPrinter xr

-- print the board
    -- robots dirt child objects babypen robotbaby n m
printBoard :: [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> Int -> Int -> String
printBoard robots dirt child objects babypen babypenused robotbaby n m = myPrinter (createListWordFromString (tail (getListToPrint (0,0) (getBoxes 0 0 n m) robots dirt child objects babypen babypenused robotbaby)) m)

-- list = tail (getListToPrint (0,0) (getBoxes 0 0 3 3) [(0, 1)] [] [(1,0),(2,2)] [(1, 2)] [] [])

createBabyPPen :: Int -> Int -> Int -> Int  -> Coord -> [Coord]
createBabyPPen dir i n m (x, y) 
                            | i == 0 || not (inRange (x, y) n m) = []
                            | dir == 0 = (x, y) : createBabyPPen dir (i-1) n m (moveEast x y) --horizontal
                            | otherwise = (x, y) : createBabyPPen dir (i-1) n m (moveSouth x y)

showTupleRobot :: Int -> (Coord, [Coord], [Coord], [Coord], [Coord], [Coord], [Coord]) -> [Coord]
showTupleRobot i (c1,c2,c3,c4, c5, c6, c7) 
                    | i == 1 = [c1]
                    | i == 2 = c2
                    | i == 3 = c3
                    | i == 4 = c4
                    | i == 5 = c5
                    | i == 6 = c6
                    | i == 7 = c7
                    

__getRandomElementList__ :: Int -> Int -> Int -> [Coord] -> [Coord]
__getRandomElementList__ current n total list 
            | current == n = []
            | otherwise = let x = list!!(getRandom 0 (total - current - 1))
                            in x : __getRandomElementList__ (current + 1) n total (listWithoutElem x list)

getRandomElementList :: Int -> [Coord] -> [Coord]
getRandomElementList i list = __getRandomElementList__ 0 i (length list) list

concatTwoLists :: [Coord] -> [Coord] -> [Coord]
concatTwoLists [] l2 = l2
concatTwoLists l1 l2 = concatTwoLists (init l1) ((last l1): l2)

duplicateList :: [Coord] -> [Coord]
duplicateList [] = []
duplicateList (x:xr) = x : duplicateList xr