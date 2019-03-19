--Regresar el ultimo elemento de una lista

ultimo:: [a] -> a
ultimo [] = error "La lista esta vacia, prro"
ultimo [x] = x
ultimo (_:xs) = ultimo xs

--Dice si 2 listas son iguales
--Eq sirve para declarar una lista generica
iguales:: Eq a => [a] -> [a] -> Bool
iguales [] [] = True
iguales (x:xs) (y:ys) = x==y && iguales xs ys
iguales _       _     = False

--Suma los elementos de una lista
suma:: Num a => [a] -> a
suma [] = 0
suma (x:xs) = x + suma xs

--Busca un elemento de una lista
busca::Eq a => a -> [a] -> Bool
busca x [] = False
busca y (x:xs)
    | x==y = True
    | otherwise = busca y xs

--Elimina N elementos al principio de una lista
eliminar:: Int -> [a] -> [a]
eliminar 0 xs = xs
eliminar n [] = []
eliminar n (_:xs) = eliminar (n-1) xs

--Retorna las iniciales de 2 listas
iniciales:: String -> String -> String
iniciales [] [] = "Y el nombre carnal?"
iniciales (n:om) (a:pe) = [n] ++ "." ++ [a] ++ "."

--Dice el tamaño de la lista menor a 3 elementos
--Se tiene que aplicar la propiedad en el prelurio y dar la instruccion para imprimir en pantalla un elemento
dice::Show a => [a] -> String
dice [] = "La lista esta vacia"
dice (x:[]) = "La lista solo tiene: " ++ show x
--dice (x:y:[]) = "La lista solo tiene: " ++ show x + "y" ++ show y
dice (x:y:_) = "La lista es muy larga"

--Unir 2 listas
unir:: [a] -> [a] -> [a]
unir [] [] = []
unir [] xs  = xs
unir xs []  = xs
unir (x:xs) ys = x:unir xs ys