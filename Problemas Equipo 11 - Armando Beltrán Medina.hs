
--Problema 11 (Diferencias): Escribir en cada casilla los números del 1 al 8, todos y sin repetir, 
--                           con la condición de que la diferencia entre los números de dos casillas 
--                           vecinas no sea nunca menor que 4.
diferencias :: [Int] -> String
diferencias lista 
 | any (<4) lista2 = "Perdiste"
 | diferentes lista == False = "Perdiste"
 | otherwise = "Ganaste"
 where lista2 = zipWith (-) (tail lista) lista

diferentes :: [Int] -> Bool
diferentes [_] = True
diferentes (x:y) = if elem x y then False else diferentes y

intervalo :: [Int] -> Bool
intervalo [_] = True
intervalo (x:y) = if x>9 then False else intervalo y

longitud :: [Int] -> Bool
longitud lista = if length lista>10 then False else True
 --lista contiene los numeros del 1 al 8 según el orden del usuario

--Problema 19 (Cacahuates del mono): Un mono tiene una bolsa con muchos cacahuetes.
--                                   Cada mañana su dueño le añade 100 cacahuetes exactamente en la bolsa.
--                                   Luego, durante el día, el mono se come la mitad de los cacahuetes que encuentra en la bolsa y deja la otra mitad.
--                                   Una noche, después de varios años comportándose así, el dueño contó el número de cacahuetes que el mono había ahorrado en la bolsa.
--                                   ¿Cuántos había?.

monoCacahuates :: Int -> [Float]
monoCacahuates x = [((2^x-1)*100)/(2^x) | x <- [1..x]]



--x es el numero de días que han pasado, la función nace de n=(n1+100)/2

--Problema 14 (Potencia): Encontrar cuatro dígitos distintos de cero a,b,c,d (pueden repetirse), 
--                        de modo que (a^b)x(c^d)=abcd (a^b significa a elevado a b).
potencia :: Int -> Int -> Int -> Int -> String
potencia a b c d 
 | (a^b)*(c^d) == a*b*c*d = "Correcto"
 | a==0 || b==0 || c==0 || d==0 = "Introduce numeros validos"
 | otherwise = "Incorrecto"

--Problema 20 (MomentosHistoricos) Las cuatro primeras cifras indican hora y minutos. Las otras seis, 
--                                 una fecha (día, mes, año con dos dígitos). Todas juntas representan un momento histórico 
--                                 (un momento de la historia).
--                                 Obsérvese que está formado por los 10 dígitos del 0 al 9. Se utilizan todos y sin repetir.
momentosHistoricos :: [Int] -> String
momentosHistoricos fecha
 | diferentes fecha == False || intervalo fecha == False || longitud fecha==False = "Fecha incorrecta"
 | otherwise = "Fecha correcta"
--Se manda una lista con 10 digitos si hay repetidos te manda fecha incorrecta y si son diferentes es fecha correcta

numeroCapicuo :: [Int] -> Bool
numeroCapicuo n = n == (reverse n)
numeroCapicuo' []  = True
numeroCapicuo' [_] = True
numeroCapicuo' n  = (head n) == (last n) && (numeroCapicuo' $ init $ tail n)


-- Ladrillos Si un ladrillo se equilibra, en una balanza de dos platillos, 
-- con tres cuartos de ladrillo más una pesa de tres cuartos de kilo...
-- ¿Cuántos kilos pesa el ladrillo?
ladrillos :: Float -> String
ladrillos x
 | x==(3*x/4)+3/4="Correcto"
 | otherwise="Incorrecto"



