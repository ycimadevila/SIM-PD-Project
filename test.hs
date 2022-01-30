potencia :: Int -> (Int -> Int)
potencia n x = x^n

cuadrado :: Int -> Int
cuadrado = potencia 2

take :: Int -> [a] -> [a]
take 0 _ = []
take (n + 1) [] = []
take (n + 1) (x:xs) = x : take n xs