module Child where
    
import Auxiliar as Ax


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

moveByRandom :: (Coord, [Coord], [Coord], [Coord]) -> (Coord, [Coord], [Coord], [Coord]) -> (Coord, [Coord], [Coord], [Coord])
moveByRandom x y | (getRandom 1 2) == 1 = y
                 | otherwise = x


moveChild :: Coord -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> Int -> Int -> (Coord, [Coord], [Coord], [Coord])
moveChild ch childs dirt robots objects babypen n m = __moveChild__ ch (getRandom 0 3) childs dirt robots objects babypen n m (validPos (notInList (notInList (notInList (notInList (getGrid ch) dirt) robots) objects) babypen) n m) 

-- (next coord , newObj, newChild, newDirt)
__moveChild__ :: Coord -> Int -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> Int -> Int -> [Coord] -> (Coord, [Coord], [Coord], [Coord])
__moveChild__ ch i childs dirt robots objects babypen n m grid = 
        let next = moveDirection ch i
        in ( if inRange next n m && (notElem next childs) && (notElem next dirt) && (notElem next robots) && (notElem next babypen) 
			then ( -- can move
				if (elem next objects)
					then (--obj
						let (mov, obj) = moveObject ch objects childs dirt robots i n m
						in (mov, obj, mov:(listWithoutElem ch childs), dirt)
					)
					else ( -- empty
						next, 
						objects, 
						(next:(listWithoutElem ch childs)), 
						(
							if (getRandom 1 100) < 40 && (length (notInList (ch:(grid)) [next]) > 0)
								then (getRandomElementList 1 (notInList (ch:(grid)) [next]))++dirt
								else dirt
						)
					)
			)
			else ( -- cant move
				ch, 
				objects, 
				childs, 
				(
					if (getRandom 1 100) < 20 && (length (notInList (ch:(grid)) [next]) > 0)
						then (getRandomElementList 1 grid)++dirt
						else dirt
				)
			)
		)


