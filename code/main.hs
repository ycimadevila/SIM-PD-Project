module Main where
    
import Environment
import Control.Concurrent
import Sleep 
import Auxiliar
import Text.Printf

main = do
    start
    _main_ 4 10

_main_ 0 _ = putStrLn "Simulaciones Terminadas!!"
_main_ simCount turn = do
    sleep 1
    putStrLn "\nComenzando Simulación..."
    -- let turn = (getRandom 5 36)
    putStrLn "\nTurnos para lograr el objetivo: "
    printf "%d\n" turn
    sleep 1
    let env = generateNewEnv (getRandom 3 6) (getRandom 3 6)
    putStrLn (show env)
    putStrLn (doSimulation env turn)
    sleep 2
    _main_ (simCount-1) turn
    

doSimulation env@(Env t_ height_ width_ robots_ childs_ robotsWithChild_ object_ dirt_ babypen_ babypenused_) turn = 
    if (endCurrentSim env turn)  
        then (
            "Simulación terminada "++(
                do
                    let perc = (percent_ 60 (width_*height_))
                    if ((length dirt_ ) < perc)
                        then "Exitosamente!\n\n"
                        else "de forma fallida."++(coordListToStr dirt_)++", "++(show (length dirt_))++", "++(show perc)++", "++(show (width_))
                    )
            )
        else let newEnv = (moveAgents env) in (envToString newEnv)++doSimulation newEnv turn
                
            
