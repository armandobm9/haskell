import Data.List
--Examen

--Numero malvado
numMalvado :: Int -> Bool
numMalvado n = buscaBelzeboss(show (2 ^ n))

buscaBelzeboss :: [Char] -> Bool
buscaBelzeboss n = isInfixOf "666" n


--menorDeUnos
menorDeUnos :: Int -> Int
menorDeUnos n = contarUno (show n)

contarUno :: [Char] -> Int
contarUno [] = 0
contarUno (x:xs)
 | [x] == (show 1) = 1 +contarUno xs
 | otherwise = 0

