data Maybe a = Nothing
             | Just a
             deriving Show

----------------------------------------------------------------

head :: [a] -> Maybe a
head [] = Nothing
head (x:xs) = Just x

----------------------------------------------------------------

tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (x:xs) = Just xs