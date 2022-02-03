module Child where
    
import Auxiliar as Ax

-- the child is define as a coordinate with a direction
type Child = (Coord, Int)

-- search for the child that corresponds to the coordinate
searchChildByCoord :: Coord -> [Child] -> Child
searchChildByCoord _ [] = ((-1, -1), -1)
searchChildByCoord coord (child: xr) | ch == (fst child) = child
                                  | otherwise = searchChildByCoord ch xr

-- search for the direction of the child that corresponds to the coordinate
getChildDir :: Coord -> [Child] -> Int
getChildDir coord childs = snd (searchChildByCoord coord childs)

-- object coordinate direction 
  -- my coordinate -> direction -> [coordinates used] -> coordinate I'm gonna move
consecutiveIsObj :: Coord -> Int -> [Coord]  -> Bool
consecutiveIsObj ch i list = elem (moveDirection ch i) list

-- returns the next position to the object at that direction
surroungingPosDir :: Coord -> Int -> [Coord] -> Choord
surroungingPosDir ch i elems | consecutiveIsObj ch i elems = surroungingPosDir (moveDirection ch i) elems
                             | otherwise = (moveDirection ch i)

