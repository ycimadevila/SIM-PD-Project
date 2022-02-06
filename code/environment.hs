module Environment where

import Auxiliar 
import Child
import Robot
import Text.Printf

-------------------------------------
-----------  Data Section -----------
-------------------------------------

data Env = Env {
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

--------------------------------------
-----------  Print Section -----------
--------------------------------------

instance Show (Env) where {
    show env = let Env {
                        t = t_ , 
                        height = height_ ,
                        width = width_ , 
                        robots = robots_ ,
                        childs = childs_ ,
                        robotsWithChild = robotsWithChild_ ,
                        object = object_ ,
                        dirt = dirt_ ,
                        babypen = babypen_ ,
                        babypenused = babypenused_ 
                    } = env
        in printf "\nDimensión del tablero: (%d, %d)\n\nTurn: %d\n%s" height_ width_ t_ (printBoard robots_ dirt_ childs_ object_ babypen_ babypenused_ robotsWithChild_ height_ width_)        --in printf "Turn: %d\n(%d, %d)\n%s\nRobots:%s\nNiños:%s\nSuciedad:%s\nObjetos:%s\nCorral:%s\nCorralUsado:%s\n" t_ height_ width_ (printBoard robots_ dirt_ childs_ object_ babypen_ babypenused_ robotsWithChild_ height_ width_) (coordListToStr(robots_)) (coordListToStr(childs_)) (coordListToStr(dirt_)) (coordListToStr(object_)) (coordListToStr(babypen_)) (coordListToStr(babypenused_))
    }  

envToString (Env t_ height_ width_ robots_ childs_ robotsWithChild_ object_ dirt_ babypen_ babypenused_) = 
    "\nTurn: "++(show t_)++"\n"++(printBoard robots_ dirt_ childs_ object_ babypen_ babypenused_ robotsWithChild_ height_ width_)
        -- ++"\nRobot:   "++(coordListToStr robots_)++"\nNino:   "++(coordListToStr childs_)++"\nRobotNino:   "++(coordListToStr robotsWithChild_)
        --     ++"\nObjetos:   "++(coordListToStr object_)++"\nSuciedad:   "++(coordListToStr dirt_)++"\nCorral:   "++(coordListToStr babypen_)
        --         ++"\nCorralNino:   "++(coordListToStr babypenused_)


coordListToStr [] = ""
coordListToStr ((a,b):xs) = "(" ++ show a ++", "++ show b++ ") " ++ coordListToStr xs


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
    putStrLn "niño:             \"_c_\""
    putStrLn "robot             \"_r_\""
    putStrLn "robot con niño:   \"_R_\""
    putStrLn "basura:           \"_d_\""
    putStrLn "pbjeto:           \"_m_\""
    putStrLn "corral ocupado:   \"_X_\""
    putStrLn "corral:           \"_v_\""
    putStrLn "vacio:            \"_._\""
    putStrLn "_-_-_-_-_-_-_-_-_-_-_-_"

--------------------------------------------
-----------  Environment Section -----------
--------------------------------------------

baseEnv n m =  Env {
            t = 0,
            height = n,
            width = m,
            robots = [],
            childs = [],
            robotsWithChild = [],
            object = [],
            dirt = [],
            babypen = [],
            babypenused = []
            }

egEnv =  Env {
            t = 0,
            height = 3,
            width = 3,
            robots = [(0,0)],
            childs = [(2,0)],
            robotsWithChild = [],
            object = [(1,1)],
            dirt = [],
            babypen = [(2,2)],
            babypenused = []
            }

_generateNewEnvCon_ :: Env -> [[Coord]]
_generateNewEnvCon_ (Env t_ height_ width_ robots_ childs_ robotsWithChild_ object_ dirt_ babypen_ babypenused_) = do
    let rest_ = getRandomElementList (height_*width_) (getBoxes 0 0 height_ width_)
    let ppen = createBabyPPen (getRandom 1 2) (getRandom 1 ((min height_ width_)-1)) height_ width_ ((getRandomElementList 1 rest_)!!0)
    let rest = notInList rest_ ppen
    let ch = take (length ppen) rest
    let obj = take (getRandom 0 (percent_ 20 (height_*width_))) (drop ((length ch)*2) rest) 
    let dir = take (getRandom 0 (percent_ 20 (height_*width_))) (drop (((length ch)*2)+ (length obj)) rest)
    let rob = take 1 (drop (((length ch)*2) + (length obj) + (length dir)) rest)
    [ppen, ch, obj, dir, rob]

_generateNewEnv_ :: Env -> [[Coord]] -> Env
_generateNewEnv_ env list = 
        let Env { t = t_, height = height_, width = width_, robots = robots_, 
                    childs = childs_, robotsWithChild = robotsWithChild_, object = object_, 
                        dirt = dirt_, babypen = babypen_, babypenused = babypenused_
            } = env
            in 
                Env {
                    t = t_,
                    height = height_,
                    width = width_,
                    robots = list!!4,
                    childs = list!!1,
                    robotsWithChild = robotsWithChild_,
                    object = list!!2,
                    dirt = list!!3,
                    babypen = list!!0,
                    babypenused = babypenused_
                } 

generateNewEnv n m = _generateNewEnv_ (baseEnv n m) (_generateNewEnvCon_ (baseEnv n m))

endCurrentSim (Env t_ height_ width_ robots_ childs_ robotsWithChild_ object_ dirt_ babypen_ babypenused_) turn = 
    t_ == turn 

moveAgentsConect (Env t_ height_ width_ robots_ childs_ robotsWithChild_ object_ dirt_ babypen_ babypenused_) = do
    let (_ , newRobot, newRobotChild, newChild, newDirt, newbabypen, newbabyPenUsed) = moveRobot (robots_!!0) childs_ dirt_ object_ babypen_ babypenused_ robots_ robotsWithChild_ height_ width_
    let (_ , newObj_, newChild_, newDirt_) = moveListChild newChild newChild newDirt newRobot object_ newbabypen height_ width_
    [newRobot, newChild_, newRobotChild, newObj_, newDirt_, newbabypen, newbabyPenUsed]
    

_moveAgents_ (Env t_ height_ width_ robots_ childs_ robotsWithChild_ object_ dirt_ babypen_ babypenused_) list = 
    Env {
        t = t_ + 1,
        height = height_,
        width = width_,
        robots = list!!0,
        childs = list!!1,
        robotsWithChild = list!!2,
        object = list!!3,
        dirt = list!!4,
        babypen = list!!5,
        babypenused = list!!6
    }

moveAgents env = _moveAgents_ env (moveAgentsConect env)

moveListChild [] childs_ dirt_ robots_ object_ babypen_ height_ width_ = (0, object_, childs_, dirt_)
moveListChild (ch : chr) childs_ dirt_ robots_ object_ babypen_ height_ width_ = 
    let (_, obj, child, dirt) = moveChild ch childs_ dirt_ robots_ object_ babypen_ height_ width_
    in moveListChild chr child dirt robots_ obj babypen_ height_ width_

  
