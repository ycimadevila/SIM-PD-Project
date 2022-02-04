module Robot where

import Auxiliar


-- current coord, direction, childs list, dirt list, obj list,bbpen list, robots list, robotChild list, n, m -> (next coord , newRobot, newRobotChild, newChild, newDirt, newbabypen, newbabyPenUsed)
moveRobot :: Coord -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord]  -> [Coord] -> Int -> Int -> (Coord, [Coord], [Coord], [Coord], [Coord], [Coord], [Coord])
moveRobot ch childs dirt objects babypen babypenused robots robchi n m = if (ch `elem` dirt) && (ch `notElem` robchi) -- if its dirty and not carry a child
                                                                then (
                                                                    if (getRandom 1 2 == 1) -- 1 to clean, 2 to move
                                                                        then (ch, robots, robchi, childs, (listWithoutElem ch dirt), babypen, babypenused)
                                                                        else ( -- it moves if it can
                                                                            let next = (bfs ch ch [] [[ch, ch]] childs (objects++babypenused) n m)!!0
                                                                            in (if next == (-1,-1) then (ch, robots, robchi, childs, dirt, babypen, babypenused)
                                                                                else (next,
                                                                                [next],
                                                                                (if elem next robchi then (next:robchi) else robchi),
                                                                                childs,
                                                                                dirt,
                                                                                babypen, babypenused
                                                                                )
                                                                            )
                                                                            
                                                                        )
                                                                )
                                                                else (
                                                                    if (ch `elem` robchi) -- if its carring a child
                                                                        then (if (ch `elem` babypen) -- if it's in the playpen
                                                                            then (ch, robots, (notInList robchi [ch]), (notInList childs [ch]), dirt, (notInList babypen [ch]), (ch : babypenused))---
                                                                            else ( -- look for a babyplaypen
                                                                                let next = (bfs ch ch [] [[ch, ch]] babypen (objects++babypenused) n m)!!0
                                                                                in (if next == (-1,-1) 
                                                                                    then (ch, robots, robchi, childs, dirt,  babypen, babypenused)
                                                                                    else(next,
                                                                                        [next],
                                                                                        next:(notInList robchi [ch]),
                                                                                        childs,
                                                                                        dirt, babypen, babypenused
                                                                                        )
                                                                                )
                                                                            )
                                                                        )
                                                                        else( -- is not cacrrying a child
                                                                            if notElem ch childs
                                                                                then ( -- look for the closest child and return the step
                                                                                    let next = (bfs ch ch [] [[ch, ch]] childs (objects++babypenused) n m)!!0
                                                                                    in (if next == (-1,-1) 
                                                                                        then (ch, robots, robchi, childs, dirt, babypen, babypenused)
                                                                                        else(next,
                                                                                            [next],
                                                                                            robchi,
                                                                                            childs,
                                                                                            dirt, babypen, babypenused
                                                                                            )
                                                                                        )
                                                                                )
                                                                                else( -- get a child
                                                                                    let next = (bfs ch ch [] [[ch, ch]] babypen (objects++babypenused) n m)!!0
                                                                                    in (if next == (-1,-1) 
                                                                                        then (ch, robots, [ch], listWithoutElem ch childs, dirt, babypen, babypenused)
                                                                                        else(next,
                                                                                            [next],
                                                                                            [next],
                                                                                            listWithoutElem ch childs,
                                                                                            dirt, babypen, babypenused
                                                                                            )
                                                                                        )  
                                                                                )
                                                                        )
                                                                    )
childs::[Coord]
childs = [(1, 2), (2, 1)] 
dirt::[Coord]
dirt = [(3, 1), (2, 2)] 
objects::[Coord]
objects = [(1, 3), (3, 1)] 
babypen::[Coord]
babypen = [(0, 0), (0, 1)]
babypenused::[Coord]
babypenused = [] 
robots::[Coord]
robots = [(1, 2)] 
ch::Coord 
ch = (1, 2)
robchi::[Coord]
robchi = [] 
n::Int
n = 4
m::Int 
m = 4   

result = moveRobot ch childs dirt objects babypen babypenused robots robchi n m