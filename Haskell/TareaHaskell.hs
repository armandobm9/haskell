-- 9. Sin usar las funciones drop y  take, elabore la función (ultimos n xs)
-- que retorne la lista formada por los últimos n elementos de xs.
-- Por ejemplo, ultimos 3 [2,5,47,9,5] == [47,9,5]

-- 7. cuantos n xs es la lista que resulta de repetir cada elemento de xs n veces. 
-- Por ejemplo, cuantos 5 "Holis" == "HHHHHooooollllliiiiisssss”

-- 11.Generar una lista con parejas de elementos consecutivos de xs con signos distintos, ignorando los ceros. 
-- Por ejemplo, paresDispares  [11,3,0,-2,11,-4,3] == [(3,-2),(-2,11),(11,-4), (-4,3)]

-- 1.	Dada una lista de números, la lista generada tendrá el porcentaje de cada elemento de la lista original en relación con la suma total de elementos.
-- Por ejemplo, la lista de porcentajes de [1,2,3,4] es [10.0,20.0,30.0,40.0], ya que 1 es el 10% de la suma (1+2+3+4 = 10), y así sucesivamente.
-- Definir la función por recursión y con generadores.

-- 2.(diferenciaConSig xs) es la lista de las diferencias entre cada el elemento de la lista y su consecutivo de xs.
-- Por ejemplo, diferenciaConSig [15,3,8,3] == [12,-5,5]