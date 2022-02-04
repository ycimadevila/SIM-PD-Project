module Child where
    
import Auxiliar as Ax

-- the child is define as a coordinate with a direction
type Child = (Coord, Int)

-- search for the child that corresponds to the coordinate
searchChildByCoord :: Coord -> [Child] -> Child
searchChildByCoord _ [] = ((-1, -1), -1)
searchChildByCoord ch (child: xr) | ch == (fst child) = child
                                  | otherwise = searchChildByCoord ch xr

-- search for the direction of the child that corresponds to the coordinate
getChildDir :: Coord -> [Child] -> Int
getChildDir coord childs = snd (searchChildByCoord coord childs)

-- object coordinate direction 
  -- my coordinate -> direction -> [coordinates used] -> coordinate I'm gonna move
consecutiveIsObj :: Coord -> Int -> [Coord]  -> Bool
consecutiveIsObj ch i list = elem (moveDirection ch i) list

-- returns the next position to the object at that direction
surroungingPosDir :: Coord -> Int -> [Coord] -> Coord
surroungingPosDir ch i elems | consecutiveIsObj ch i elems = surroungingPosDir (moveDirection ch i) i  elems
                             | otherwise = (moveDirection ch i)

-- return if the next position isn't dirty
isNotDirty :: Coord -> Int -> [Coord] -> Bool
isNotDirty ch i list = notElem (moveDirection ch i) list

-- return if the next position isn't a babypen
isNotBabypen :: Coord -> Int -> [Coord] -> Bool
isNotBabypen ch i list = notElem (moveDirection ch i) list

-- return if the next position isn't a babypen
isNotRobot :: Coord -> Int -> [Coord] -> Bool
isNotRobot ch i list = notElem (moveDirection ch i) list

-- return if the next position is an object
isObject :: Coord -> Int -> [Coord] -> Bool
isObject ch i list = elem (moveDirection ch i) list

-- returns the updated list of objects
    -- last obj pos, next obj pos
__moveObject__ :: Coord -> Coord -> [Coord] -> [Coord]
__moveObject__ c1 c2 objects = concatList c2 (listWithoutElem c1 objects)

-- current coord, objects, childs, direction, n, m -> (Coord child, new obj list)
moveObject :: Coord -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> Int -> Int -> Int -> (Coord, [Coord])
moveObject ch objects childs dirt robots i n m = if (inRange (surroungingPosDir ch i objects) n m) && (notElem (surroungingPosDir ch i objects) dirt) && (notElem (surroungingPosDir ch i objects) robots)
                                then ((moveDirection ch i), ( __moveObject__ (moveDirection ch i) (surroungingPosDir ch i objects) objects))
                                else (ch, objects)
---
---
---
---
---
---

moveByRandom :: (Coord, [Coord], [Child], [Coord]) -> (Coord, [Coord], [Child], [Coord]) -> (Coord, [Coord], [Child], [Coord])
moveByRandom x y | (getRandom 1 2) == 1 = y
                 | otherwise = x

--grid = 
poopByRandom :: Coord -> [Coord] -> Coord
poopByRandom ch grid = if (getRandom 1 100) < 20 && (length grid > 0)
                        then grid!!(getRandom 1 ((length grid)-1))
                        else (-1, -1)

-- current coord, direction, childs list, dirt list, n, m -> (next coord , newObj, newChild, newDirt)
__moveChild__ :: Coord -> Int -> [Child] -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> Int -> Int -> [Coord] -> (Coord, [Coord], [Child], [Coord])
__moveChild__ ch i childs dirt robots objects babypen n m grid = if (isNotDirty ch i dirt) && (isNotBabypen ch i babypen) && (isNotRobot ch i robots) -- if is not filled
                                                then (if isObject ch i objects 
                                                        -- if is an object
                                                        then(if (getRandom 1 100) < 20 && (length (notInList (notInList (notInList (notInList grid dirt) robots) objects) babypen) > 0) -- poop
                                                                then (
                                                                        moveDirection ch i,
                                                                        snd (moveObject ch objects childs dirt robots i n m), 
                                                                        concatList (moveDirection ch i) (listWithoutElem (ch, i) childs),
                                                                        concatList ((notInList (notInList (notInList (notInList grid dirt) robots) objects) babypen)
                                                                            !!(getRandom 1 ((length (notInList (notInList (notInList (notInList grid dirt) robots) objects) babypen))-1))) dirt
                                                                        )
                                                                else (
                                                                        moveDirection ch i,
                                                                        snd (moveObject ch objects childs dirt robots i n m), 
                                                                        concatList (moveDirection ch i) (listWithoutElem (ch, i) childs),
                                                                        dirt
                                                                        )
                                                                ) 
                                                        -- if is not an object
                                                        else (if (getRandom 1 100) < 20 && (length (notInList (notInList (notInList (notInList grid dirt) robots) objects) babypen) > 0) -- poop
                                                                then (
                                                                        moveDirection ch i, 
                                                                        objects, 
                                                                        concatList ((moveDirection ch i), i) (listWithoutElem (ch, i) childs), 
                                                                        concatList ((notInList (notInList (notInList (notInList grid dirt) robots) objects) babypen))
                                                                            !!(getRandom 1 ((length (notInList (notInList (notInList (notInList grid dirt) robots) objects) babypen))-1)) dirt
                                                                        )
                                                                else (
                                                                        moveDirection ch i, 
                                                                        objects, 
                                                                        concatList ((moveDirection ch i), i) (listWithoutElem (ch, i) childs), 
                                                                        dirt
                                                                        )
                                                                )
                                                    )
                                                -- if is not empty
                                                else (if (getRandom 1 100) < 20 && (length (notInList (notInList (notInList (notInList grid dirt) robots) objects) babypen) > 0) -- poop
                                                        then (ch, objects, childs, 
                                                                concatList ((notInList (notInList (notInList (notInList grid dirt) robots) objects) babypen))
                                                                    !!(getRandom 1 ((length (notInList (notInList (notInList (notInList grid dirt) robots) objects) babypen))-1)) dirt
                                                                                )
                                                        else (ch, objects, childs, dirt)
                                                        )

moveChild :: Coord -> [Child] -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> Int -> Int -> (Coord, [Coord], [Child], [Coord])
moveChild ch childs dirt robots objects babypen n m = __moveChild__ ch (getRandom 1 4) childs dirt robots objects babypen n m (getGrid ch) 



