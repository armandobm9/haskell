--menorDeUnos
menorMultDeUnos :: Int -> Int
menorMultDeUnos 0 = 0
menorMultDeUnos n = buscaMultiplo n 1

buscaMultiplo :: Int-> Int -> Int
buscaMultiplo n m
 | isAllOne (show (m * n)) = m
 | otherwise = buscaMultiplo n (m+1)

isAllOne :: [Char] -> Bool
isAllOne (x:xs)
 | (x == '1') && (xs == []) = True
 | x == '1' = isAllOne xs
 | otherwise = False