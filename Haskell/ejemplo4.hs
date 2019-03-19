--Listas comprimidas
--sintaxis
--[x^2| x <- [1..5]]
-- ^^^  ^^^^^^^^^^
-- Valores   Condiciones
-- de la     a cumplir,
-- lista     de donde tomar valores

--[(x,y)| x <- [1,2,3], y <- [4,5y)]]

--Generadores dependientes
--Los generadores pueden tener como base variables
--[(x,y)|x <- [1..3], y <- [x..3]]

--concatenar lista de listas
concat::[[a]] -> [a]
concat xss = [ x | xs <- xss, x <-xs]

--Guardias
--se puede usar guardias para restringir los valores producidos
--[x | x <- [1..10], odd x]

factors :: Int->[Int]
factors n = [ x | x <- [1..n], n `mod` x ==0]

factorsPrimo :: Int->Bool
factorsPrimo n = factors n == [1,n]

factorListaPrimo :: Int->[Int]
factorListaPrimo n = [ x | x <- [2..n], factorsPrimo x]