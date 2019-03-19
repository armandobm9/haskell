--suma = doble + triple
suma:: Int -> Int -> Int 
suma a b = res
 where
  doble = 2*a
  triple = 3*b
  res = doble + triple


--edad.escule Kinder,Primaria,Seundaria
escolaridad::Int -> String
escolaridad x
 | x<=4 = "Mosolbete suertudo"
 | x<=6 = "Jardin de ninos"
 | x<=12 = "Primaria"
 | x<=15 = "Secundaria"
 | otherwise = "A trabajar!"

predSue::Int -> (Int,Int)
predSue x = x-1 x+1

dataTemp = Calor | Frio | Templado derivering
(Eq,Ord,Enun,Read,Show,Bounded)