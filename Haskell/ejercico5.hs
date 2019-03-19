--Recursividad
fac::Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

--Recursividad Listas
--producto:: Num a -> [a] -> a
--producto [] = 1
--producto (n:ns) = n * producto ns

lenght:: [a] -> Int
lenght [] = 0
lenght (_:xs) = 1 + lenght xs

voltearLista::[a] -> [a]
voltearLista [] = []
voltearLista (x:xs) = voltearLista xs ++ [x]

zipp:: [a] -> [b] -> [(a,b)]
zipp [] _ = []
zipp _  [] = []
zipp (x:xs) (y:ys) = (x,y) : zip xs ys

--Eliminar el dato n de una lista
--drop:: Int -> [a] -> [a]
--drop 0 xs = xs
--drop _ [] = []
--drop n (n:xs) = drop (n-1) xs

--Funcion que concatena 2 listas
(+++)::[a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x : (xs +++ ys)

--QuickSort
qsort:: Ord a -> [a] -> [a]
qsort [] = []
qsort (x:xs) = 
    qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger = [ b | b <- xs, b > x]

--EJERCICIOS
--