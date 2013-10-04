-- Ejercicio 1: Cuenta dentro de la sumatoria

primero :: Int -> Int 
primero i = (2 * i - 1) ^ 2

-- Ejercicio 2: Cuenta dentro de la sumatoria

segundo :: Int -> Int
segundo i = (-1) ^ i * 2 ^ i

-- Sumatoria para resolver Ejercicios 1 y 2 

sumatoria :: Int -> (Int -> Int) -> Int
sumatoria 0 funcion = 0
sumatoria n funcion = funcion n + sumatoria (n - 1) funcion

-- Ejercicio 3: Cantidad de elementos de A que se encuentran en B

cantidad :: Ord a => [a] -> [a] -> Int
cantidad [] _ = 0
cantidad _ [] = 0
cantidad (x : xs) (y : ys) = comparar (x : xs) (y : ys) + cantidad xs (y : ys)

comparar :: Ord a => [a] -> [a] -> Int
comparar _ [] = 0
comparar (x : xs) (y : ys) | x == y = 1
                           | otherwise = comparar (x : xs) ys

-- Ejercicio 4: Lista triangular. 

triangular :: Ord a => [a] -> Bool
triangular [] = True
triangular [x] = True
triangular (x : y : xs) | x <= y = triangular (y : xs)
                        | otherwise = decreciente (x : y : xs)

decreciente :: Ord a => [a] -> Bool
decreciente [] = True
decreciente [x] = True
decreciente (x : y : xs) | x >= y = decreciente (y : xs)
                         | otherwise = False 

-- Ejercicio 5: Factores primos de n

primos :: Int -> [Int]
primos n = factores n 2

factores :: Int -> Int -> [Int]
factores 1 d = []
factores n d | mod n d == 0 = d : factores (div n d) d
           | otherwise = factores n (d + 1)