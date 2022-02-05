module Environment where

import Auxiliar as Ax
import Child
import Robot

data Env s = Env {
    t :: Int, 
    height :: Int,
    width :: Int, 
    robots :: [Coord],
    childs :: [Coord],
    robotsWithChild :: [Coord],
    object :: [Coord],
    dirt :: [Coord],
    babypen :: [Coord],
    babypenused :: [Coord]
}

start = do 
    putStrLn "_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"
    putStrLn "_-_-_   ROBOT DE LIMPIEZA   _-_-_"
    putStrLn "_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"

    putStrLn "   __,_,"
    putStrLn "  [_|_/ "
    putStrLn "   //"
    putStrLn " _//    __"
    putStrLn "(_|)   |@@|"
    putStrLn " \\ \\__ \\--/ __"
    putStrLn "  \\___|----|  |   __"
    putStrLn "      \\ }{ /\\ )_ / _\\"
    putStrLn "      /\\__/\\ \\__O (__"
    putStrLn "     (--/\\--)    \\__/"
    putStrLn "     _)(  )(_"
    putStrLn "    `---''---`"


    putStrLn "_-_-_-_-_-_-_-_-_-_-_-_"
    putStrLn "_-_-_   LEYENDA   _-_-_"
    putStrLn "_-_-_-_-_-_-_-_-_-_-_-_"
    putStrLn "niño:             \"_C_\""
    putStrLn "robot             \"_r_\""
    putStrLn "robot con niño:   \"_R_\""
    putStrLn "basura:           \"_d_\""
    putStrLn "pbjeto:           \"_m_\""
    putStrLn "corral ocupado:   \"_X_\""
    putStrLn "corral:           \"_v_\""
    putStrLn "vacio:            \"_._\""
    putStrLn "_-_-_-_-_-_-_-_-_-_-_-_"


-- n, m, all positions, robot, robchi, objects, childs, dirt, babypen, babypenused

-- generateNewEnv list n m nxm = 
--     let playpen = createBabyPPen (getRandom 1 2) (getRandom 1 (min n m)) n m ((getRandomElementList 0 1 list)!!0)
--     in (
--         let child = (getRandomElementList 0 (length playpen) (notInList list playpen))
--         in (
--             let objs = getRandomElementList 0 (getRandom 0 (round(nxm/6))) (notInList list playpen++child)
--             in (
--                 let dirt_ = getRandomElementList 0 (getRandom 0 (round(nxm/6))) (notInList list playpen++child++objs)
--                 in (
--                     let robot = getRandomElementList 0 1 (notInList list playpen++child++objs++dirt_)
--                     in (
--                         -- Env {
--                         --     t = 0, 
--                         --     height = n,
--                         --     width = m, 
--                         --     robots = robot,
--                         --     childs = child,
--                         --     robotsWithChild = [],
--                         --     object = objs,
--                         --     dirt = dirt_,
--                         --     babypen = playpen,
--                         --     babypenused = []
--                         -- }
--                         (0, n, m, robot, child, [], objs, dirt_, playpen, [])
--                     )
--                 )
--             )
--         )
--     )

generateNewEnv_ list n m nxm rd = do
    playpen <- createBabyPPen rd (getRandom 1 (min n m)) n m ((getRandomElementList 0 1 list)!!0)
    child <- getRandomElementList 0 (length playpen) (notInList list playpen)
    objs <- getRandomElementList 0 (getRandom 0 (round(nxm/6))) (notInList list playpen++child)
    dirt_ <- getRandomElementList 0 (getRandom 0 (round(nxm/6))) (notInList list playpen++child++objs)
    robot <- getRandomElementList 0 1 (notInList list playpen++child++objs++dirt_)
    (robot, child)


-- printEnv i env  | i == 0 = env.t
--                 | i == 1 = env.height
--                 | i == 2 = env.width
--                 | i == 3 = env.robots
--                 | i == 4 = env.childs
--                 | i == 5 = env.robotsWithChild
--                 | i == 6 = env.object
--                 | i == 7 = env.dirt
--                 | i == 8 = env.babypen
--                 | i == 9 = env.babypenused
