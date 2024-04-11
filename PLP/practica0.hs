-- Ejercicio 1

-- null :: [a] -> Bool Toma una lista y se fija si es vacia
-- head :: [a] -> a Toma una lista y devuelve la cabeza de la lista
-- tail :: [a] -> [a] Toma una lista y devuelve esa misma lista sin la cabeza
-- init :: [a] -> [a] Toma una lista y devuelve esa misma lista sin el ultimo elemento
-- last ::[a] -> a Toma una lista y devuelve el ultimo elemento de esa lista
-- take :: Int -> [a] -> [a] Toma un numero n y una lista s y genera una lista que toma los 
-- primeros n elementos de s
-- drop :: Int -> [a] -> [a] Toma un numero n y una lista s y devuelve una lista sin los primeros n elementos

-- Ejercicio 2

-- a) valorAbsoluto :: Int -> Int
valorAbsoluto :: Int -> Int
valorAbsoluto x | x >= 0 = x
                | otherwise = -x

-- b) bisiesto:: Int -> Bool

bisiesto :: Int -> Bool
bisiesto year | mod year 400 == 0 = True
              | mod year 100 == 0 = False
              | mod year 4 == 0 = True
              | otherwise = False

 --c) factorial

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- d) cantDivisoresPrimos :: Int -> Int
esPrimo :: Int -> Bool
esPrimo n = [x | x <- [1..n], mod n x == 0] == [1,n]

cantDivisoresPrimos:: Int -> Int
cantDivisoresPrimos n = length [x | x <- [1..n], mod n x == 0, esPrimo x]


-- Ejercicio 4:
-- a) limpiar :: String -> String -> String
esta :: Char -> String -> Bool
esta c [] =  False
esta c (x:xs) | c == x = True
              | otherwise = esta c xs


limpiar :: String -> String -> String
limpiar s [] = []
limpiar s (y:ys) | esta y s =  limpiar s ys
                 | otherwise = y : limpiar s ys

-- b) difPromedio:: [Float] -> [Float]


average:: [Float] -> Float
average s = sum s /fromIntegral (length s)

difPromedio:: [Float] -> [Float]
difPromedio [] = []
difPromedio s = difPromedioAux s (average s)

difPromedioAux:: [Float] -> Float -> [Float]
difPromedioAux s promedio = map (\x -> x-promedio) s

-- c) todosIguales:: [Int] -> Bool 

todosIguales::  [Int] -> Bool
todosIguales (x:xs) = all (\y -> y == x) xs

--Ejercicio 5

data AB a = Nil | Bin (AB a) a (AB a)
-- a) vacioAB:: AB a -> Bool
vacioAB:: AB a -> Bool
vacioAB Nil = True
vacioAB (Bin _ _ _) = False

--b) negacionAB :: AB Bool -> AB Bool
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin i r d) = Bin (negacionAB i) (not r) (negacionAB d)

--c) productoAB AB Int -> Int

productoAB:: AB Int -> Int
productoAB Nil = 1
productoAB (Bin i r d) = (productoAB i)* r *(productoAB d)


