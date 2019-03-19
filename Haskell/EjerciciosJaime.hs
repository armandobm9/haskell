--Calcular la suma de ls divisores propios de n, hasta que se genere un numero primo
sumaDivisores :: Int -> Int
sumaDivisores n = hacerCiclo n 1 0

hacerCiclo :: Int -> Int -> Int -> Int
hacerCiclo n contador sumatoria
 | esPrimo sumatoria = sumatoria
 | mod n contador == 0 = hacerCiclo n (contador+1) (contador+sumatoria)
 | otherwise = hacerCiclo n (contador+1) sumatoria

esPrimo :: Int -> Bool
esPrimo n = cuantosDivisores n n == 2

cuantosDivisores :: Int -> Int -> Int
cuantosDivisores _ 0 = 0
cuantosDivisores n div
  | mod n div == 0 = 1 + cuantosDivisores n (div-1)
  | otherwise = cuantosDivisores n (div-1)  

--Funcion que recibe m y n mayores de 0 y regresa la losta m^2 ignorano los primeros n
cuadrados :: Int -> Int -> [Int]