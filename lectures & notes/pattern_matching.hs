germanTranslationRobot :: Bool -> String
germanTranslationRobot False = "Falsch"
germanTranslationRobot True = "Richtig"

-------------------------------------------------------------------------

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum' xs

-------------------------------------------------------------------------

sum (first:rest) = first + sum rest

-------------------------------------------------------------------------

take :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (x:xs) = x : (take (n-1) xs)