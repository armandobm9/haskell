import System.IO.Unsafe  -- be careful!                                         
import System.Random

colores :: String -> [String] -> (String,Int)
colores color [x] = (x,1)
colores color (x:y:z) 
 | m > n     = (x,m)
 | otherwise = (x2,n)
    where (x2,n) = colores color (y:z)
          m     = length (takeWhile (==x) (x:y:z))
 

sumaTuplas :: Int -> (Int, Int, Int)
sumaTuplas 0 = (0,0,0)
sumaTuplas n = (azar, azar2, n-(azar+azar2))
 where azar = unsafePerformIO (getStdRandom (randomR (0, n))) 
       azar2 = unsafePerformIO (getStdRandom (randomR (0, n-azar)))


vectores ::  [Int] -> [Int] -> Int
vectores vector1 vector2 = sum [x*y | (x,y) <- zip (vector1) (vector2)]

maymen ::  Int -> [Int] -> [[Int]] 
maymen n lista = [[x | x <- lista, x>n],[x | x <- lista, x<n]]

sumaListas ::  [Int] -> [Int] -> [Int]
sumaListas lista1 lista2 = [x+y | (x,y) <- zip lista1 lista2]

sumaListas2 ::  [Int] -> [Int] -> [Int]
sumaListas2 lista1 lista2 = zipWith (+) lista1 lista2

hazPares :: [Int] -> [Int]
hazPares [] = []
hazPares (x:xs)
 | x `mod` 2 == 0 = (x:hazPares xs)
 | otherwise = (x+1:hazPares xs)

