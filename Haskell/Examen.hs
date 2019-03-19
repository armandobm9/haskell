import Data.List
--Examen

--Numero malvado
numMalvado :: Int -> Bool
numMalvado n = buscaBelzeboss(show (2 ^ n))

buscaBelzeboss :: [Char] -> Bool
buscaBelzeboss (_:_:[]) = False
buscaBelzeboss (a:b:c:cuerpo)
	| (a == '6') && (b == '6') && (c == '6') = True
	| otherwise = buscaBelzeboss (b:c:cuerpo)




--Regresar un numero mayo o igual que n que se pueda dividir por todos sus digitos, ignorando los 0


