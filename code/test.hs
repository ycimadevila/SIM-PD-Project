module Test where
    
potencia :: Int -> (Int -> Int)
potencia n x = x^n

cuadrado :: Int -> Int
cuadrado = potencia 2

take_ :: Int -> [a] -> [a]
take_ 0 _ = []
take_ (n + 1) [] = []
take_ (n + 1) (x:xs) = x : take_ n xs