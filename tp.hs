sumatoria :: Num a => Int -> Int -> (Int -> a) -> a
sumatoria i n f | i == n = f n
sumatoria i n f = f i + sumatoria (succ i) n f

-- Ejercicio 1. Función principal: "puntoUno".

primero :: Int -> Int 
primero i = (2 * i - 1) ^ 2

puntoUno :: Int -> Int
puntoUno n = sumatoria 1 n primero

-- Ejercicio 2. Función principal: "puntoDos".

segundo :: Int -> Int
segundo i = (-1) ^ i * 2 ^ i

puntoDos :: Int -> Int
puntoDos n = sumatoria 1 n segundo

-- Ejercicio 3: Cantidad de elementos de A que se encuentran en B. Función principal: "cantidad".

cantidad :: Ord a => [a] -> [a] -> Int
cantidad [] _ = 0
cantidad _ [] = 0
cantidad (x : xs) (y : ys) = comparar (x : xs) (y : ys) + cantidad xs (y : ys)

comparar :: Ord a => [a] -> [a] -> Int
comparar _ [] = 0
comparar (x : xs) (y : ys) | x == y = 1
                           | otherwise = comparar (x : xs) ys

-- Ejercicio 4: Lista triangular. Función principal: "triangular".

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

-- Ejercicio 5: Factores primos de n. Función principal: "factoresPrimos".

factoresPrimos :: Int -> [Int]
factoresPrimos n = factorizar n 2

factorizar :: Int -> Int -> [Int]
factorizar 1 d = []
factorizar n d | mod n d == 0 = d : factorizar (div n d) d
               | otherwise = factorizar n (d + 1)
