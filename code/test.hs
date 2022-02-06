module Test where
    
potencia :: Int -> (Int -> Int)
potencia n x = x^n

cuadrado :: Int -> Int
cuadrado = potencia 2

take_ :: Int -> [a] -> [a]
take_ 0 _ = []
take_ (n + 1) [] = []
take_ (n + 1) (x:xs) = x : take_ n xs

name = [(1, 2)]
all_ = [(1, 2), (2, 3)]
r = dropWhile (== (1, 2)) all_
r1 = 3/=2
r2 = 3==2

ress :: Int -> (Int, Int,Int)
ress x = ((x+1),
            x, (if x > 0 then 1 else -1))

a::Num a => a
a = 3

name_ t@(a, b, c) = t
    