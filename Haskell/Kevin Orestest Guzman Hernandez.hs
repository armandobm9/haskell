--cambiaSentido
cambiaSentido :: Ord a => [a] -> Maybe a
cambiaSentido (x:y:xs)
 | x > y = checaDecreciente (x:y:xs)
 | x < y = checaCreciente (x:y:xs)
 | otherwise = cambiaSentido (y:xs)

checaCreciente :: Ord a => [a] -> Maybe a
checaCreciente (x:y:xs)
 | x > y = Just y
 | xs == [] = Nothing
 | otherwise = checaCreciente (y:xs)

checaDecreciente :: Ord a => [a] -> Maybe a
checaDecreciente (x:y:xs)
 | x < y = Just y
 | xs == [] = Nothing
 | otherwise = checaDecreciente (y:xs)

--MostruoComeGalletas
comeGalletas :: [Int] -> IO ()
comeGalletas [] = putStrLn ("[]")
comeGalletas xs = do
   putStrLn (show (lista))
   comeGalletas lista
   where lista = empiezaComer xs

empiezaComer :: [Int] -> [Int]
--empiezaComer [] = []
empiezaComer (x:xs)
 | x == 1 = xs
 | otherwise = xs ++ [x `div` 2]