import Test.HUnit

{-- Tipos --}

import Data.Either
import Data.List

data Dirección = Norte | Sur | Este | Oeste
  deriving (Eq, Show)
type Posición = (Float, Float)

data Personaje = Personaje Posición String  -- posición inicial, nombre
  | Mueve Personaje Dirección               -- personaje que se mueve, dirección en la que se mueve
  | Muere Personaje                         -- personaje que muere
  deriving (Eq, Show)
data Objeto = Objeto Posición String        -- posición inicial, nombre
  | Tomado Objeto Personaje                 -- objeto que es tomado, personaje que lo tomó
  | EsDestruido Objeto                      -- objeto que es destruido
  deriving (Eq, Show)
type Universo = [Either Personaje Objeto]

{-- Observadores y funciones básicas de los tipos --}

siguiente_posición :: Posición -> Dirección -> Posición
siguiente_posición p Norte = (fst p, snd p + 1)
siguiente_posición p Sur = (fst p, snd p - 1)
siguiente_posición p Este = (fst p + 1, snd p)
siguiente_posición p Oeste = (fst p - 1, snd p)

--posición :: Either Personaje Objeto -> Posición
--posición (Left p) = posición_personaje p
--posición (Right o) = posición_objeto o

--posición_objeto :: Objeto -> Posición
--posición_objeto = foldObjeto const (const posición_personaje) id

nombre :: Either Personaje Objeto -> String
nombre (Left p) = nombre_personaje p
nombre (Right o) = nombre_objeto o

nombre_personaje :: Personaje -> String
nombre_personaje = foldPersonaje (const id) const id

está_vivo :: Personaje -> Bool
está_vivo = foldPersonaje (const (const True)) (const (const True)) (const False)

fue_destruido :: Objeto -> Bool
fue_destruido = foldObjeto (const (const False)) (const (const False)) (const True)

universo_con :: [Personaje] -> [Objeto] -> [Either Personaje Objeto]
universo_con ps os = map Left ps ++ map Right os

es_un_objeto :: Either Personaje Objeto -> Bool
es_un_objeto (Left o) = False
es_un_objeto (Right p) = True

es_un_personaje :: Either Personaje Objeto -> Bool
es_un_personaje (Left o) = True
es_un_personaje (Right p) = False

-- Asume que es un personaje
personaje_de :: Either Personaje Objeto -> Personaje
personaje_de (Left p) = p

-- Asume que es un objeto
objeto_de :: Either Personaje Objeto -> Objeto
objeto_de (Right o) = o

en_posesión_de :: String -> Objeto -> Bool
en_posesión_de n = foldObjeto (const (const False)) (\ r p -> nombre_personaje p == n) (const False)

objeto_libre :: Objeto -> Bool
objeto_libre = foldObjeto (const (const True)) (const (const False)) (const False)

norma2 :: (Float, Float) -> (Float, Float) -> Float
norma2 p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

cantidad_de_objetos :: Universo -> Int
cantidad_de_objetos = length . objetos_en

cantidad_de_personajes :: Universo -> Int
cantidad_de_personajes = length . personajes_en

--distancia :: (Either Personaje Objeto) -> (Either Personaje Objeto) -> Float
--distancia e1 e2 = norma2 (posición e1) (posición e2)

--objetos_libres_en :: Universo -> [Objeto]
--objetos_libres_en u = filter objeto_libre (objetos_en u)

está_el_personaje :: String -> Universo -> Bool
está_el_personaje n = foldr (\x r -> es_un_personaje x && nombre x == n && (está_vivo $ personaje_de x) || r) False

--está_el_objeto :: String -> Universo -> Bool
--está_el_objeto n = foldr (\x r -> es_un_objeto x && nombre x == n && not (fue_destruido $ objeto_de x) || r) False

-- Asume que el personaje está
personaje_de_nombre :: String -> Universo -> Personaje
personaje_de_nombre n u = foldr1 (\x1 x2 -> if nombre_personaje x1 == n then x1 else x2) (personajes_en u)

-- Asume que el objeto está
objeto_de_nombre :: String -> Universo -> Objeto
objeto_de_nombre n u = foldr1 (\x1 x2 -> if nombre_objeto x1 == n then x1 else x2) (objetos_en u)

es_una_gema :: Objeto -> Bool
es_una_gema o = isPrefixOf "Gema de" (nombre_objeto o)

{-Ejercicio 1-}
            --  los tipos son los de f , z, p y el de la salida, la funcion  es f que toma un personaje y el result
            -- de la recursión y devuelve el resultado, z es el resultado de la recursión cuando el personaje muere
foldPersonaje:: (Personaje -> a -> a) -> (a -> Dirección -> a) -> (a -> a) -> Personaje -> a
foldPersonaje fPersonaje fMueve fMuere p = case p of
  Personaje pos nom -> fPersonaje p (foldPersonaje fPersonaje fMueve fMuere p)
  Mueve p' d -> fMueve (foldPersonaje fPersonaje fMueve fMuere p') d
  Muere p' -> fMuere (foldPersonaje fPersonaje fMueve fMuere p')

foldObjeto :: (Objeto -> a -> a) -> (a -> Personaje -> a) -> (a -> a) -> Objeto -> a
foldObjeto fObjeto fTomado fDestruido o = case o of
  Objeto pos nom -> fObjeto o (foldObjeto fObjeto fTomado fDestruido o)
  Tomado o' p -> fTomado (foldObjeto fObjeto fTomado fDestruido o') p
  EsDestruido o' -> fDestruido (foldObjeto fObjeto fTomado fDestruido o')
{-Ejercicio 2-}

posición_personaje :: Personaje -> Posición
posición_personaje (Personaje p n) = p
posición_personaje (Mueve p Norte) = siguiente_posición (posición_personaje  p) Norte
posición_personaje (Mueve p Sur) = siguiente_posición (posición_personaje  p) Sur
posición_personaje (Mueve p Este) = siguiente_posición (posición_personaje  p) Este
posición_personaje (Mueve p Oeste) = siguiente_posición (posición_personaje  p) Oeste
posición_personaje (Muere p) = posición_personaje  p

nombre_objeto :: Objeto -> String
nombre_objeto (Objeto pos name) = name
nombre_objeto (Tomado o p) = nombre_objeto o
nombre_objeto (EsDestruido o) = nombre_objeto o

{-Ejercicio 3-}

objetos_en :: Universo -> [Objeto]
objetos_en = foldr (\x r -> 
  case x of
    Right o -> o : r
    Left p -> r) []

personajes_en :: Universo -> [Personaje]
personajes_en = foldr (\x r -> if es_un_personaje x then personaje_de x : r else r) []

{-Ejercicio 4-}

-- creo que en_posesion_de esta mal, porque no termina de ejecutarse el test nunca
dar_objetos_de_personaje:: String -> Either Personaje Objeto -> [Objeto] -> [Objeto]
dar_objetos_de_personaje name (Right o) r = if en_posesión_de name o then o : r else r
dar_objetos_de_personaje name (Left p) r = r

objetos_en_posesión_de :: String -> Universo -> [Objeto]
objetos_en_posesión_de name = foldr (\x r -> case x of
  (Left p) -> r
  (Right o) -> if en_posesión_de name o then o : r else r
  ) []

{-Ejercicio 5-}

-- Asume que hay al menos un objeto
--objeto_libre_mas_cercano :: ?
--objeto_libre_mas_cercano = ?

{-Ejercicio 6-}

--tiene_thanos_todas_las_gemas :: ?
--tiene_thanos_todas_las_gemas = ?

{-Ejercicio 7-}

--podemos_ganarle_a_thanos :: ?
--podemos_ganarle_a_thanos = ?

{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  --"ejercicio4" ~: testsEj4,
  "foldObjeto" ~: testFoldObj,
  --"ejercicio5" ~: testsEj5,
  --"ejercicio6" ~: testsEj6,
  --"ejercicio7" ~: testsEj7
  "fue_destruido" ~: testFueDestruido,
  "en_posesión_de" ~: testEnPosesion
  ]

phil = Personaje (0,0) "Phil"
mjölnir = Objeto (2,2) "Mjölnir"
universo_sin_thanos = universo_con [phil] [mjölnir]

testsEj1 = test [ -- Casos de test para el ejercicio 1
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) phil             -- Caso de test 1 - expresión a testear
    ~=? 0                                                               -- Caso de test 1 - resultado esperado
  ,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Muere phil)     -- Caso de test 2 - expresión a testear
    ~=? 1 
  , 
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Mueve phil Norte)
    ~=? 1                                                       -- Caso de test 2 - resultado esperado
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  posición_personaje phil       -- Caso de test 1 - expresión a testear
    ~=? (0,0)                   -- Caso de test 1 - resultado esperado
  ,
  posición_personaje (Mueve phil Norte)
    ~=? (0,1)                   -- Caso de test 2 - resultado esperado
  ,
  posición_personaje (Mueve phil Sur)
    ~=? (0,-1)                  -- Caso de test 3 - resultado esperado
  ,
  posición_personaje (Mueve phil Este)
    ~=? (1,0)                   -- Caso de test 4 - resultado esperado
  ,
  posición_personaje (Mueve phil Oeste)
    ~=? (-1,0)                  -- Caso de test 5 - resultado esperado
  ,
  posición_personaje (Muere (Mueve phil Norte))
    ~=? (0,1)                   -- Caso de test 6 - resultado esperado            
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  objetos_en []       -- Caso de test 1 - expresión a testear
    ~=? []            -- Caso de test 1 - resultado esperado
  ,
  objetos_en [Right mjölnir]       -- Caso de test 2 - expresión a testear
    ~=? [mjölnir]                   -- Caso de test 2 - resultado esperados
  ]

--testsEj4 = test [ -- Casos de test para el ejercicio 4
  --objetos_en_posesión_de "Phil" []       -- Caso de test 1 - expresión a testear
 -- ~=? []                             -- Caso de test 1 - resultado esperado
 -- ,
  --objetos_en_posesión_de "Phil" [Right (Tomado mjölnir phil)]       -- Caso de test 2 - expresión a testear
 -- ~=? [mjölnir]                                         -- Caso de test 2 - resultado esperado
  --]


testFoldObj = test [
  foldObjeto (\o s -> 0) (\r p -> 1) (\r -> 1) mjölnir
  ~=? 0
  ,
  foldObjeto (\o s -> 0) (\r p -> 1) (\r -> 1) (Tomado mjölnir phil)
  ~=? 1
  ,
  foldObjeto (\o s -> 0) (\r p -> 1) (\r -> 1) (EsDestruido mjölnir)
  ~=? 1
  ]

testFueDestruido = test [
  fue_destruido mjölnir
  ~=? False
  ,
  fue_destruido (EsDestruido mjölnir)
  ~=? True
  ]

testEnPosesion = test [
  en_posesión_de "Phil" mjölnir
  ~=? False
  ,
  en_posesión_de "Phil" (Tomado mjölnir phil)
  ~=? True
  ]

--testsEj5 = test [ -- Casos de test para el ejercicio 5
--  objeto_libre_mas_cercano phil [Right mjölnir]       -- Caso de test 1 - expresión a testear
 --   ~=? mjölnir                                       -- Caso de test 1 - resultado esperado
 -- ]

--testsEj6 = test [ -- Casos de test para el ejercicio 6
 -- tiene_thanos_todas_las_gemas universo_sin_thanos       -- Caso de test 1 - expresión a testear
  --  ~=? False                                            -- Caso de test 1 - resultado esperado
 ---- ]

--testsEj7 = test [ -- Casos de test para el ejercicio 7
  --podemos_ganarle_a_thanos universo_sin_thanos         -- Caso de test 1 - expresión a testear
  --  ~=? False                                          -- Caso de test 1 - resultado esperado
 -- ]