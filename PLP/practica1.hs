
-- Ejercicio1 
 
-- max2:: (Float, Float) -> Float
-- normaVectorial:: (Float, Float) -> Float
-- substract:: Float -> Float -> FLoat
-- predecesor:: Float -> FLoat
-- evaluarEnCero:: (Float -> Float) -> Float
-- dosVeces:: (Float -> Float) -> Float -> Float
-- EXPLICACION DE DOSVECES: Tenemos una funcion que toma una funcion f que toma un parametro
-- de tipo FLoat y devuelve otro de tipo float, pero a su vez como es una funcion compuesta, tambien
-- toma otro parametro que seria el resultado de hacer f (la que esta a la derecha del .) entonces tambien
-- toma otro parametro

-- flipAll:: (Float -> Float -> FLoat) -> [FLoat] -> [FLoat]

--b) las que no estan currificadas son las primeras 2, max2 y normaVectorial

max2Curry:: Float -> Float -> Float
max2Curry x y | x >= y = x
              | otherwise = y

normaVectorialCurry:: Float -> Float -> Float
normaVectorialCurry x y = sqrt (x^2 + y^2)

-- Ejercicio 2
--a) definir curry:: ((a,b) -> c) -> (a -> b -> c)

curryV2:: ((a,b) -> c) -> (a -> b -> c)
curryV2 f x y = f(x,y)

--b) uncurry:: (a -> b -> c) -> ((a,b) -> c) 

uncurryV2:: (a -> b -> c) -> ((a,b) -> c)
uncurryV2 f (x,y) = f x y

--Ejercicio 3

--a) hacer sum, elem, (++), filter y map con foldr

--foldr f z [] = z
--foldr f z (x:xs) = f x ( foldr f z xs )

sumv2 :: Int -> Int -> Int
sumv2 x y = foldr (+) 0 [x,y]

elemv2:: Eq a => a -> [a] -> Bool
elemv2 e l = foldr (\x recc -> recc || x == e) False l

plusv2:: [a] -> [a] -> [a]
plusv2 l s = foldr (:) s l -- voy appendeando los elementos de la primera lista, y al final le queda 
-- por ejemplo [1,2] [3] = 1:2:[3] = 1,2,3

filterv2:: Eq a => (a -> Bool) -> [a] -> [a]
filterv2 f l = foldr (\x recc -> if f x then x:recc else recc) [] l

mapv2:: (a -> b) -> [a] -> [b]
mapv2 f l = foldr (\x recc -> f x : recc) [] l

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec)

sumasParciales:: Num a => [a] -> [a]
sumasParciales l = foldl (\rec x -> rec ++ [last rec + x]) [0] l

--Ejercicio 4

-- a) permutaciones:: [a] -> [[a]] que dada una lista devuelve todas las permutaciones posibles de la misma


insertarEnTodasLasPosiciones:: a -> [a] -> [[a]]
insertarEnTodasLasPosiciones x [] = [[x]]
insertarEnTodasLasPosiciones x (y:ys) = (x:y:ys) : map (y:) (insertarEnTodasLasPosiciones x ys)

permutaciones:: [a] -> [[a]]
permutaciones s = foldr (\x rec -> concatMap (insertarEnTodasLasPosiciones x) rec) [[]] s

partes:: [a] -> [[a]]
partes = foldr(\x rec -> rec ++ map (x:) rec) [[]]

prefijos:: [a] -> [[a]]
prefijos = foldr(\x rec -> [] : map (x:) rec) [[]]

--5) indicar si cada funcion usa recursion estructural

-- elementosEnPosicionesPares :: [a] -> [a]
--elementosEnPosicionesPares [] = []
--elementosEnPosicionesPares (x:xs) = if null xs
--then [x]
--else x : elementosEnPosicionesPares (tail xs)

-- no usa recursion estructural, porque usa el valor de cola para ver si es vacia


entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
then x : entrelazar xs []
else x : head ys : entrelazar xs (tail ys)

-- en este caso si usa recursion estructural, porque la recursion esta hecha sobre la 
-- primer lista, y solo consulta el valor de la segunda lista y usa el valor de la cabeza
-- de la primer lista

entrelazarVersionFoldr:: [a] -> [a] -> [a]
entrelazarVersionFoldr = foldr (\x rec -> \li -> if null li then x: (rec []) else x: head li:rec (tail li)) (id)


-- Ejercicio 6

--  a) sacarUna

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna:: Eq a => a -> [a] -> [a]
sacarUna elem = recr (\x xs rec -> if x == elem then xs else x:rec) []

-- b) foldr no sirve aca porque necesitamos ademas poder cortar antes la recursion usando 
-- la cola de la lista

--c) insertarOrdenado::Ord a => a -> [a] -> [a]


insertarOrdenado::Ord a => a -> [a] -> [a]
insertarOrdenado elem = recr(\x xs rec -> if x >= elem then elem:x:xs else x:rec) [elem]

--Ejercicio 7: 
--Definir la función genLista :: a -> (a -> a) -> Integer -> [a], que genera una lista de una cantidad dada de elementos, a partir de un elemento inicial y de una función de incremento entre los elementos
--de la lista. Dicha función de incremento, dado un elemento de la lista, devuelve el elemento siguiente.

genLista :: a -> (a -> a) -> Integer -> [a]
genLista elem f n = foldr(\x rec -> f (head rec) : rec) [elem] [1..n-1]


desdeHasta:: Integer -> Integer -> [Integer]
desdeHasta x = genLista x (+1)

--Ejercicio 8

-- a) mapPares

mapPares:: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f = map (uncurry f)