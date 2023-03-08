--Practica 1-
-- :l practica1.hs     para ejecutar en la terminal
--------------------------------------------------------------
-- 1.1 Factorial (4 ejemplos)
--comprobacion: fact numero
--1
--Mediante plegado
fact7 :: Integer -> Integer
fact7 n = foldr (*) 1 [1..n]

--2
--Mediante patrones
fact3 :: Integer -> Integer
fact3 0 = 1
fact3 n = n * fact3 (n-1)

--3
--Con condicionales
fact1 ::Integer -> Integer
fact1 n = if n == 0 then 1
                    else n * fact1 (n-1)

--4
--Mediante guardas
fact2 :: Integer -> Integer
fact2 n
    | n == 0    = 1
    | otherwise = n * fact2 (n-1)


--------------------------------------------------------------
-- 1.3 Impar (3 ejemplos)
--comprobacion:  impar numero
--1
--Usando odd
impar1 :: Integer -> Bool
impar1 = odd

--2
--Usando not y even
impar2 :: Integer -> Bool
impar2 x = not (even x)

--3
--Usando not, . y even
impar3 :: Integer -> Bool
impar3 = not . even

--------------------------------------------------------------
-- 1.4 Cuadrado ( 4 ejemplos)
--comprobacion: cuadrado_  numero
--1
--Mediante *
cuadrado_1 :: Num a => a -> a
cuadrado_1  x = x * x

--2
--Mediante ^
cuadrado_2 :: Num a => a -> a
cuadrado_2 x = x ^ 2

--3
--Mediante secciones
cuadrado_3 :: Num a => a -> a
cuadrado_3 = (^2)

--4
--mediante Cuadrado
cuadrado  = cuadrado_1



--------------------------------------------------------------
-- 1.5 Suma de cuadrados (4 ejemplos)
--comprobacion: suma_cuadrados [n,n,n]
--1
--Con sum, map y cuadrado
suma_cuadrados1 :: [Integer] -> Integer
suma_cuadrados1  l = sum(map cuadrado l)

--2
--Con sum y listas intnsionales
suma_cuadrados2 :: [Integer] -> Integer
suma_cuadrados2 l = sum[x * x | x <- l]

--3
--Con sum, map y lambda
suma_cuadrados3 :: [Integer] -> Integer
suma_cuadrados3 l = sum (map (\x -> x*x) l)

--4
--Por recursion
suma_cuadrados4 :: [Integer] -> Integer
suma_cuadrados4 []      = 0
suma_cuadrados4 (x:xs)  = x*x + suma_cuadrados4 xs


--------------------------------------------------------------
-- 1.10 Funcion Anterior (1 ejemplo)
--comprobacion: anterior numero
--1
--Con guardas
anterior2 :: Integer -> Integer
anterior2 n | n > 0 = n-1


--------------------------------------------------------------
-- 2.2 Funcion Siguiente (4 ejemplos)
--comprobacion: siguiente numero
--1
--Mediante seccion
siguiente1 :: Integer -> Integer
siguiente1 = (+1)

--2
--Mediante instanciacion parcial
siguiente2 :: Integer -> Integer
siguiente2 = (+) 1

--3
--Sumando un 1 a la variable
siguiente3 :: Integer -> Integer
siguiente3 n = n + 1


--4
--Usando como siguiente la primera
siguiente = siguiente1


--------------------------------------------------------------
-- 2.9 Redefinir Funcion Filter (2 ejemplos)
--comprobacion: nfilter even [1,n,n,n,n,1]
            --  nfilter (>3) [1,n,n,n,n,1]
--1 
--Definicion por recursion
nfilter1 :: (a -> Bool) -> [a] -> [a]
nfilter1 p []              =[]
nfilter1 p (x:xs) | p x    = x : nfilter1 p xs
                   | otherwise = nfilter1 p xs

--2
--Definicion con listas intencionales
nfilter2 :: (a -> Bool) -> [a] -> [a]
nfilter2 p xs = [x | x <- xs, p x]



--------------------------------------------------------------
-- 2.10 Redefinir Funcion Sum (2 ejemplos)
--comprobacion: nsum [n,n,n]
--1
--Definicion recursiva
nsum1 :: Num a => [a] -> a
nsum1 []     = 0
nsum1 (x:xs) = x + nsum1 xs

--2
--Definicion de plegado
nsum2 :: Num a => [a] -> a
nsum2 = foldr (+) 0


--------------------------------------------------------------
-- 2.17 Definir Funcion Factoriales (4 ejemplos)
--comprobacion: factoriales numero
--1
--Definicion scan
factoriales1 :: Integer -> [Integer]
factoriales1 n = scanl(*) 1 [1..n]

--2
--Definicion con listas intencionales
factoriales3 :: Integer -> [Integer]
factoriales3 n = [fact1 x | x <- [0..n]]

--3
-- Definicion con map
factoriales4 :: Integer -> [Integer]
factoriales4 n = map fact1 [0..n]

--4
--Definicion por acululadores
factoriales5 :: Integer -> [Integer]
factoriales5 n =
    reverse(aux (n+1) 0 [1])
    where aux n m (x:xs) = if n==m then xs
                                   else aux n (m + 1) (((m+1)*x):x :xs)  



--------------------------------------------------------------
-- 2.21 Definir Funcion Divisible ( 1 ejemplo)
--comprobacion: divisible numero numero
--1
--Funcion divisible
divisible :: Int -> Int -> Bool
divisible x y = x `rem` y == 0


--------------------------------------------------------------
-- 2.22 Definir Funcion Divisores (4 ejemplos)
--Comprobacion: divisores 12
--1
divisores_1 :: Int -> [Int]
divisores_1 x = filter (divisible x) [1..x]

--2
divisores_2 :: Int -> [Int]
divisores_2 x = [y | y <- [1..x], divisible x y]

--3
prop_equivalencia_1_2 x = 
    divisores_1 x == divisores_2 x

--4
divisores = divisores_2

--------------------------------------------------------------
-- 2.23 Definir Funcion Primos (1 ejemplos)
--Comprobacion: primo numero
--1
primo :: Int -> Bool
primo x = divisores x == [1,x]

--------------------------------------------------------------
-- 2.24 Funcion primos (2 ejemplos)
--Comprobacion: primos numero
--1
----Mediante filtrado
primos_1 :: Int -> [Int]
primos_1 x = filter primo [1..x]

--2
--Mediante compresion
primos_2 :: Int -> [Int]
primos_2 x = [y | y <- [1..x], primo y] 



--------------------------------------------------------------
-- 2.25 Funcion dia ()
--Comprobacion:
--1
--Dia de la semana
dia :: Int -> Int -> Int -> String
dia d m a = diaSemana ((numeroDeDias d m a) `mod` 7)

--2
--Cuenta los dias desde el 1/ene/0 hasta la fecha proporcionada
numeroDeDias :: Int -> Int -> Int -> Int
numeroDeDias d m a = (a-1)*365
    + numeroDeBisiestos a
    + sum (take (m-1) (meses a))
    + d

--3
--Numero de años biciestos antes de año a
numeroDeBisiestos :: Int -> Int
numeroDeBisiestos a = length (filter bisiesto [1..a-1])

--4
--Verifica si el año es biciesto
bisiesto :: Int -> Bool
bisiesto a =
    divisible a 4 && (not(divisible a 100) || divisible a 400)


--5
--Nos muestra el utimo dia de todos los meses del año
meses :: Int -> [Int]
meses a = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    where feb | bisiesto a = 29
            | otherwise = 28

--6
--Nos muestra el dia de la semana equivalente
diasemana ::  Int -> String
diasemana 0 = "domingo"
diasemana 1 = "lunes"
diasemana 2 = "martes"
diasemana 3 = "miercoles"
diasemana 4 = "jueves"
diasemana 5 = "viernes"
diasemana 6 = "sábado"


-------------------------------------------------------
--ealiza un programa en Haskell que le pida al usuario su fecha de nacimiento y le devuelva en letras. ejemplo
--Entrada: 12/03/1989
--Salida: 12 de marzo de 1989
mmes :: Integer -> String
mmes m 
     | m == 01 = "enero"
     | m == 02 = "febrero"
     | m == 03 = "marzo"
     | m == 04 = "abril"
     | m == 05 = "mayo"
     | m == 06 = "junio"
     | m == 07 = "julio"
     | m == 08 = "agosto"
     | m == 09 = "septiembre"
     | m == 10 = "octubre"
     | m == 11 = "noviembre"
     | m == 12 = "diciembre"

fechan :: Integer -> Integer -> Integer -> String
fechan dia mes anio = show dia ++ " de " ++ mmes mes ++ " del " ++ show anio
