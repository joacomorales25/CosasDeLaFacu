
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