{-import Data.List
{-
  12
  genera n
-}

--Genera una li
genera :: Int -> IO ()
genera 0 = putStrLn ("[(0,0)]")
genera n = do 
  genera (n-1)
  putStrLn (show (zip [0..n] (invertir [0..n])))

invertir :: [Int] -> [Int]
invertir [] = []
invertir (x:xs) = (invertir xs) ++ [x]

{-
  18
  eliminaopuestos [-2,1,-1,2,3,4,-3] == [3,4,-3]
-}

eliminaopuestos :: [Int] -> [Int]
eliminaopuestos lista
  | lista == x = x
  | otherwise = eliminaopuestos x
  where x = eliminaIguales lista

eliminaIguales [] = []
eliminaIguales (x:[]) = [x]
eliminaIguales (x:y:xs)
  | abs x == abs y = eliminaIguales xs
  | otherwise = [x] ++ eliminaIguales (y:xs)

{-
  10
  iniciarImpares 
-}

iniciarImpares::[[Int]]
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

{-
  8
  conjuntosEq  
-}

conjuntosEq :: [Int] -> [Int] -> Bool
conjuntosEq xs ys = (sort (eliminaRepetidos xs)) == (sort (eliminaRepetidos ys))

eliminaRepetidos (x:xs)
  | (elem x xs) = eliminaRepetidos xs
  | (xs == []) = [x]
  | otherwise = [x] ++ eliminaRepetidos xs


{-
  7
  euler 936
-}

euler :: Int -> Bool
euler numero 
  | (mod numero 10) == 0 = False
  | not par = True
  | otherwise = False
  where par = espar (show numeroSumado)
        numeroSumado = numero + x1
        x1 = read (reverse (show numero))

esPar :: [Char] -> Bool
esPar [] = False
esPar (x:xs)
  | odd (read [x]) = espar xs
  | otherwise = True
-}
--(5) 9.-Lista de los ultimos n elementos de una lista
--ultimos :: (Integral a) => a -> [Int] -> [Int]


ultimos :: Int -> [a] -> [a]
ultimos n xs = eliminar (length xs - n) xs

eliminar::Int->[a]->[a]
eliminar _ [] = []
eliminar 0 xs = xs
eliminar n (_:xs) = eliminar (n-1) xs