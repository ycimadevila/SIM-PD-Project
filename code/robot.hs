module Robot where

import Auxiliar

-- (next coord , newRobot, newRobotChild, newChild, newDirt, newbabypen, newbabyPenUsed)
moveRobot :: Coord -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord]  -> [Coord] -> Int -> Int -> (Coord, [Coord], [Coord], [Coord], [Coord], [Coord], [Coord])
moveRobot ch childs dirt objects babypen babypenused robots robchi n m = 
    if (elem ch robchi) -- if its carry a child
        then ( -- go to babypen
            if (elem ch babypen) -- i'm on a baby pen
                then( --leave child
                    (ch, robots, [], childs, dirt, (listWithoutElem ch babypen), (ch : babypenused))
                )
            else (-- look for babypen
                let next = (bfs ch ch [] [[ch, ch]] babypen (objects++babypenused) n m)!!0
                in (if next == (-1,-1)
                    then (ch, robots, robchi, childs, dirt, babypen, babypenused)
                    else (next, [next], [next], childs, dirt, babypen, babypenused)
                )
            )
        )
        else ( -- not carrying a child
            if (length childs) > 0 -- if there's a child
                then ( 
                    if (elem ch childs)
                        then( -- get child and become a robotsWithChild
                            ch, [ch], [ch], (listWithoutElem ch childs), dirt, babypen, babypenused
                        )
                        else( -- look for a child
                            let next = (bfs ch ch [] [[ch, ch]] childs (objects++babypenused) n m)!!0
                            in (if next == (-1,-1)
                                then (ch, robots, robchi, childs, dirt, babypen, babypenused)
                                else (next, [next], robchi, childs, dirt, babypen, babypenused)
                            )
                        ) 
                )
                else ( -- dirt?
                    if (length dirt) > 0
                        then (-- look for dirt
                            if (elem ch dirt) -- im on a dirt place
                                then(
                                    (ch, robots, robchi, childs, (listWithoutElem ch dirt), babypen, babypenused)
                                )
                                else(
                                    let next = (bfs ch ch [] [[ch, ch]] dirt (objects++babypenused) n m)!!0
                                    in (if next == (-1,-1)
                                        then (ch, robots, robchi, childs, dirt, babypen, babypenused)
                                        else (next, [next], robchi, childs, dirt, babypen, babypenused)
                                    )
                                )
                        )
                        else -- do nothing, stand in the position, return same env
                            (ch, robots, robchi, childs, dirt, babypen, babypenused)
                )
        )





















--

        --     if ((getRandom 1 2) == 1) -- 1 to clean, 2 to move
        --         then (ch, robots, robchi, childs, (listWithoutElem ch dirt), babypen, babypenused)
        --         else ( -- it moves if it can
        --             let next = (bfs ch ch [] [[ch, ch]] childs (objects++babypenused) n m)!!0
        --             in (if next == (-1,-1)
        --                 then (ch, robots, robchi, childs, dirt, babypen, babypenused)
        --                 else (
        --                     next,
        --                     [next],
        --                     (if elem next robchi then (next:robchi) else robchi),
        --                     childs,
        --                     dirt,
        --                     babypen, babypenused
        --                 )
        --             )
        --         )
        -- )
        -- else (
        --     if (ch `elem` robchi) -- if its carring a child
        --         then (if (ch `elem` babypen) -- if it's in the playpen
        --             then (ch, robots, (notInList robchi [ch]), (notInList childs [ch]), dirt, (notInList babypen [ch]), (ch : babypenused))---
        --             else ( -- look for a babyplaypen
        --                 let next = (bfs ch ch [] [[ch, ch]] babypen (objects++babypenused) n m)!!0
        --                 in (if next == (-1,-1) 
        --                     then (ch, robots, robchi, childs, dirt,  babypen, babypenused)
        --                     else(next,
        --                         [next],
        --                         next:(notInList robchi [ch]),
        --                         childs,
        --                         dirt, babypen, babypenused
        --                         )
        --                 )
        --             )
        --         )
        --         else( -- is not cacrrying a child
        --             if notElem ch childs
        --                 then ( -- look for the closest child and return the step
        --                     let next = (bfs ch ch [] [[ch, ch]] childs (objects++babypenused) n m)!!0
        --                     in (if next == (-1,-1) 
        --                         then (ch, robots, robchi, childs, dirt, babypen, babypenused)
        --                         else(next,
        --                             [next],
        --                             robchi,
        --                             childs,
        --                             dirt, babypen, babypenused
        --                             )
        --                         )
        --                 )
        --                 else( -- get a child
        --                     -- let next = (bfs ch ch [] [[ch, ch]] babypen (objects++babypenused) n m)!!0
        --                     -- in (if next == (-1,-1) 
        --                         -- then (ch, robots, [ch], listWithoutElem ch childs, dirt, babypen, babypenused)
        --                         -- else
        --                             (
        --                                 ch,
        --                                 [ch],
        --                                 [ch],
        --                                 listWithoutElem ch childs,
        --                                 dirt, babypen, babypenused
        --                             )
        --                         -- )  
        --                     )
        --         )
        --     )


-- current coord, direction, childs list, dirt list, obj list,bbpen list, robots list, robotChild list, n, m -> (next coord , newRobot, newRobotChild, newChild, newDirt, newbabypen, newbabyPenUsed)
-- moveRobot :: Coord -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord] -> [Coord]  -> [Coord] -> Int -> Int -> (Coord, [Coord], [Coord], [Coord], [Coord], [Coord], [Coord])
-- moveRobot ch childs dirt objects babypen babypenused robots robchi n m = 
--     if (ch `elem` dirt) && (ch `notElem` robchi) -- if its dirty and not carry a child
--         then (
--             if ((getRandom 1 2) == 1) -- 1 to clean, 2 to move
--                 then (ch, robots, robchi, childs, (listWithoutElem ch dirt), babypen, babypenused)
--                 else ( -- it moves if it can
--                     let next = (bfs ch ch [] [[ch, ch]] childs (objects++babypenused) n m)!!0
--                     in (if next == (-1,-1)
--                         then (ch, robots, robchi, childs, dirt, babypen, babypenused)
--                         else (
--                             next,
--                             [next],
--                             (if elem next robchi then (next:robchi) else robchi),
--                             childs,
--                             dirt,
--                             babypen, babypenused
--                         )
--                     )
--                 )
--         )
--         else (
--             if (ch `elem` robchi) -- if its carring a child
--                 then (if (ch `elem` babypen) -- if it's in the playpen
--                     then (ch, robots, (notInList robchi [ch]), (notInList childs [ch]), dirt, (notInList babypen [ch]), (ch : babypenused))---
--                     else ( -- look for a babyplaypen
--                         let next = (bfs ch ch [] [[ch, ch]] babypen (objects++babypenused) n m)!!0
--                         in (if next == (-1,-1) 
--                             then (ch, robots, robchi, childs, dirt, babypen, babypenused)
--                             else(next,
--                                 [next],
--                                 next:(notInList robchi [ch]),
--                                 listWithoutElem ch childs,
--                                 dirt, babypen, babypenused
--                                 )
--                         )
--                     )
--                 )
--                 else( -- is not cacrrying a child
--                     if notElem ch childs
--                         then ( -- look for the closest child and return the step
--                             let next = (bfs ch ch [] [[ch, ch]] childs (objects++babypenused) n m)!!0
--                             in (if next == (-1,-1) 
--                                 then (ch, robots, robchi, childs, dirt, babypen, babypenused)
--                                 else(next,
--                                     [next],
--                                     robchi,
--                                     childs,
--                                     dirt, babypen, babypenused
--                                     )
--                                 )
--                         )
--                         else( -- get a child
--                             -- let next = (bfs ch ch [] [[ch, ch]] babypen (objects++babypenused) n m)!!0
--                             -- in (if next == (-1,-1) 
--                                 -- then (ch, robots, [ch], listWithoutElem ch childs, dirt, babypen, babypenused)
--                                 -- else
--                                     (
--                                         ch,
--                                         [ch],
--                                         [ch],
--                                         listWithoutElem ch childs,
--                                         dirt, babypen, babypenused
--                                     )
--                                 -- )  
--                             )
--                 )
--             )

----- testing ----- 
-- childs::[Coord]
-- childs = [(1, 2), (2, 1)] 
-- dirt::[Coord]
-- dirt = [(3, 1), (2, 2)] 
-- objects::[Coord]
-- objects = [(1, 3), (3, 1)] 
-- babypen::[Coord]
-- babypen = [(0, 0), (0, 1)]
-- babypenused::[Coord]
-- babypenused = [] 
-- robots::[Coord]
-- robots = [(2, 2)] 
-- ch::Coord 
-- ch = (2, 2)
-- robchi::[Coord]
-- robchi = [] 
-- n::Int
-- n = 4
-- m::Int 
-- m = 4   

-- result = moveRobot ch childs dirt objects babypen babypenused robots robchi n m