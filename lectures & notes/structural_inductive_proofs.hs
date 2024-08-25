take n (map f xs) = map f (take n xs)

--------------------------------------------------

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : (map f xs)

--------------------------------------------------

take :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (x:xs) = x : (take (n-1) xs)

--------------------------------------------------

take n (map f [])
=== {- definition map -}
take n []
=== {- definition take -}
[]
=== {- definition map -}
map f []
=== {- definition take -}
map f (take n [])

--------------------------------------------------

take n (map f (x:xs))
=== {- definition map -}
take n (map f (x:xs))
=== {- definition take -}
(f x : take (n-1) (map f xs))
=== {- induction hypothesis -}
(f x : map f (take (n-1) xs))
=== {- definition map -}
map f (x : (take (n-1) xs))
=== {- definition take -}
map f (take n (x:xs))
