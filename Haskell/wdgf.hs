import Data.List
{-
   Programas de 5 puntos
-}

--1.- Generar lista de porcentajes de suma total de lista
listaPorcentajes :: [Double] -> [Double]
listaPorcentajes [] = []
listaPorcentajes xs = sacarPorcentaje xs (sacarTotal xs)

sacarPorcentaje :: [Double] -> Double -> [Double]
sacarPorcentaje [] _ = []
sacarPorcentaje (x:xs) n = [(x / n)*100] ++ sacarPorcentaje xs n

sacarTotal :: [Double] -> Double
sacarTotal [] = 0
sacarTotal (x:xs) = x + sacarTotal xs

--2.-Generar lista de restas entre 2 elementos
diferenciaConSig :: [Int] -> [Int]
diferenciaConSig [] = []
diferenciaConSig [x] = []
diferenciaConSig (x:y:xs) = [x - y] ++ diferenciaConSig (y:xs)

--7.-Repetir n veces un elemento de una lista
cuantos :: Int -> [a] ->[a]
cuantos _ [] = []
cuantos 0 xs = xs
cuantos n (x:xs) = cicloFor n x ++ cuantos n xs 

cicloFor :: Int -> a -> [a]
cicloFor 0 _ = []
cicloFor n a = [a] ++ cicloFor (n - 1) a

--9.-Funcion que retorne los ultimos n elementos de una lista
ultimos :: Int -> [a] -> [a]
ultimos n xs = eliminar (length xs - n) xs

eliminar::Int->[a]->[a]
eliminar _ [] = []
eliminar 0 xs = xs
eliminar n (_:xs) = eliminar (n-1) xs



{-
   Programas de 10 puntos
-}

--12.- Genera series
genera :: Int -> IO ()
genera 0 = putStrLn ("[(0,0)]")
genera n = do 
  genera (n-1)
  putStrLn (show (zip [0..n] (invertir [0..n])))

invertir :: [Int] -> [Int]
invertir [] = []
invertir (x:xs) = (invertir xs) ++ [x]

--18.-Elimina Valor Inversos Consecutivos
eliminaOpuestos :: [Int] -> [Int]
eliminaOpuestos lista
  | lista == x = x
  | otherwise = eliminaOpuestos x
  where x = eliminaIguales lista

eliminaIguales [] = []
eliminaIguales (x:[]) = [x]
eliminaIguales (x:y:xs)
  | abs x == abs y = eliminaIguales xs
  | otherwise = [x] ++ eliminaIguales (y:xs)

--10.- Recibir Lista de Listas y sustituir elementos impares con 0 y mostrar cual lista tuvo mas cambios
iniciarImpares xs = do 
  putStrLn (show x1)
  putStrLn (show (x1!!indice))
  where indice = (elemIndices (maximum listaConCeros) (listaConCeros))!!0
        listaConCeros = contarCerosListas (x1)
        x1 = imparesPorCero xs

imparesPorCero [] = []
imparesPorCero (x:xs) = [cambiaPorCero x] ++ imparesPorCero xs

cambiaPorCero [] = []
cambiaPorCero (x:xs)
  | odd x = [0] ++ cambiaPorCero xs
  | otherwise = [x] ++ cambiaPorCero xs

contarCerosListas [] = []
contarCerosListas (x:xs) = [contarCeros x] ++ contarCerosListas xs

contarCeros [] = 0
contarCeros (x:xs)
  | x == 0 = 1 + contarCeros xs
  | otherwise = contarCeros xs

--8.- Recibir 2 listar y decir si tienen los mismos elementos aunque sean repetidos
conjuntosEq xs ys = (sort (eliminaRepetidos xs)) == (sort (eliminaRepetidos ys))

eliminaRepetidos (x:xs)
  | (elem x xs) = eliminaRepetidos xs
  | (xs == []) = [x]
  | otherwise = [x] ++ eliminaRepetidos xs
 
 --7.- Verificar si un numero es reversible segun Euler 
euler :: Int -> Bool
euler numero 
  | (mod numero 10) == 0 = False
  | not par = True
  | otherwise = False
  where par = espar (show numeroSumado)
        numeroSumado = numero + numReverso
        numReverso = read (reverse (show numero))

espar :: [Char] -> Bool
espar [] = False
espar (x:xs)
  | odd (read [x]) = espar xs
  | otherwise = True

