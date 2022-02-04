module Robot where

import Auxiliar


-- current coord, direction, childs list, dirt list, obj list,bbpen list, robots list, robotChild list, n, m -> (next coord , newRobot, newRobotChild, newChild, newDirt, newbabypen, newbabyPenUsed)
moveRobot :: Coord -> Int -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord]  -> [Coord] -> Int -> Int -> [Coord] -> (Coord, [Coord], [Coord], [Coord], [Coord], [Coord], [Coord])
moveRobot ch i childs dirt objects babypen babypenused robots robchi n m = if (ch `elem` dirt) && (ch `notElem` robchi) -- if its dirty and not carry a child
                                                                then (
                                                                    if (getRandom 1 2 == 1) -- 1 to clean, 2 to move
                                                                        then (ch, robots, robchi, childs, listWithoutElem ch dirt)
                                                                        else ( -- it moves if it can
                                                                            let next = (bfs ch ch [] [[(ch, ch)]] childs objects)!!0
                                                                            in (if next == (-1,-1) then (ch, robots, robchi, childs, dirt, babypen, babypenused)
                                                                                else(next,
                                                                                [next,
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
                                                                            then (ch, robots, (listWithoutElem robchi [ch]), (listWithoutElem childs [ch]), dirt, (listWithoutElem babypen [ch]), (ch : babypenused))---
                                                                            else ( -- look for a babyplaypen
                                                                                let next = (bfs ch ch [] [[(ch, ch)]] babypen objects++babypenused n m)!!0
                                                                                in (if next == (-1,-1) 
                                                                                    then (ch, robots, robchi, childs, dirt,  babypen, babypenused)
                                                                                    else(next,
                                                                                        [next],
                                                                                        next:(listWithoutElem robchi [ch]),
                                                                                        next:(listWithoutElem childs [ch]),
                                                                                        dirt, babypen, babypenused
                                                                                        )
                                                                                )
                                                                            )
                                                                        )
                                                                        else( -- is not cacrrying a child
                                                                            let next = (bfs ch ch [] [[(ch, ch)]] childs objects)!!0
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
                                                                    )
                                                                )