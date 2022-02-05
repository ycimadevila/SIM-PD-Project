module Environment where

import Auxiliar as Ax
import Child
import Robot

-- n = getRandom 3 20
-- m = getRandom 3 20

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

init = do 
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

generateEnv list n m = let playpen = createBabyPPen (getRandom 1 2) (getRandom 1 (min n m)) n m ((getRandomElementList 0 1 list)!!0)
                            in (
                                let child = (getRandomElementList 0 (length playpen) (notInList list playpen))
                                in (
                                    let objs = getRandomElementList 0 (round((25 *((length list) - 2 * (length child)))   )) (notInList list playpen++child)
                                    in (
                                        let dirt_ = getRandomElementList 0 (round((25*((length list) - 2 * (length child)))    )) (notInList list playpen++child++objs)
                                        in (
                                            let robot = getRandomElementList 0 1 (notInList list playpen++child++objects++dirt_)
                                            in (
                                                Env {
                                                    t = 0, 
                                                    height = n,
                                                    width = m, 
                                                    robots = robot,
                                                    childs = child,
                                                    robotsWithChild = [],
                                                    object = objs,
                                                    dirt = dirt_,
                                                    babypen = playpen,
                                                    babypenused = []
                                                }
                                            )
                                        )
                                    )
                                )
                            )
    
