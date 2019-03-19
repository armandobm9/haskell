-- Definir una función que nos diga qué meses corresponden  a cada estación
-- ejemplo: Primavera == Marzo, Abril, Mayo

data Estacion = Primavera | Otono | Verano | Invierno
 deriving Show
meses :: Estacion -> String
meses Primavera = "Marzo, Abril, Mayo"
meses Verano = "Junio, Julio, Agosto"
meses Otono = "Septiembre, Octubre, Noviembre"
meses Invierno = "Diciembre, Enero, Febrero"

--Definir la función mcd, tal que (mcd a b) es el máximo común divisor de a y b calculado mediante el algoritmo de Euclides. Por ejemplo, mcd 30 45 == 15
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (mod a b)

-- Calcula el factorial de un numero usando: a) guardas y b) Patrones

factguardas :: Integer ->  Integer
factguardas n
 | n == 0 = 1
 | n > 0 = n*factguardas(n-1)

factpatrones :: Integer ->  Integer
factpatrones 0 = 1
factpatrones n = n*factpatrones (n-1)

-- Decir hacia donde miras si giras 90 grados
data Dirección = Arriba | Derecha | Abajo | Izquierda
 deriving Show
girar90 :: Dirección -> Dirección
girar90 Arriba = Derecha
girar90 Derecha = Abajo
girar90 Abajo = Izquierda
girar90 Izquierda = Derecha

-- Decir que color sigue en el semaforo a partir del color actual
data Color = Rojo | Verde | Amarillo
 deriving Show
semaforo :: Color -> Color
semaforo Rojo = Verde
semaforo Verde = Amarillo
semaforo Amarillo = Rojo

-- Dice si el parametro b es multiplo del parametro a
múltiploDe :: Integer -> Integer -> Bool 
múltiploDe a b = (mod a b) == 0

-- Retorna las raíces de una funcion en base a sus coeficientes
raíces :: Float -> Float -> Float -> (Float, Float)
raíces a b c = ((-b+num)/2.0*a,(-b-num)/2.0*a)
 where num = sqrt(b*b-4.0*a*c)


-- Funcion que reciba un numero y retorne cero
cero :: Integer -> Integer
cero _ = 0

-- Recibe la edad de una persona y le dice si puede votar
votar :: Int -> Bool
votar a = a>=18

-- Dice si se presenta examen, estan excentos solo los 100 
-- excenta :: (Eq a, Num a) => a -> [Char]
-- No entendí que es lo que quiere









