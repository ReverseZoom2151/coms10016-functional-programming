import Prelude hiding (sum, product, and, or, all, any, length, foldr, foldl,reverse, filter)

foldr f k [x1, x2, ..., xn ] = f x1 (f x2 (...(f xn k)...))
                             = x1 ‘f‘ (x2 ‘f‘ (...(xn ‘f‘ k)...))

----------------------------------------------------------------

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f k []      =  k
foldr f k (x:xs)  =  f x (foldr f k xs)

----------------------------------------------------------------

sum :: [Integer] → Integer
sum = foldr (+) 0

----------------------------------------------------------------

product :: [Integer] → Integer
product = foldr (×) 1

----------------------------------------------------------------

foldSum :: [Integer] -> Integer
foldSum = foldr (+) 0

----------------------------------------------------------------

product :: [Integer] -> Integer
product = foldr (*) 1

----------------------------------------------------------------

and :: [Bool] -> Bool
and = foldr (&&) True

----------------------------------------------------------------

or :: [Bool] -> Bool
or = foldr (||) False

----------------------------------------------------------------

all :: (a -> Bool) -> [a] -> Bool
all p = foldr ((&&) . p) True

----------------------------------------------------------------

any :: (a -> Bool) -> [a] -> Bool
any p = foldr ((||) . p) False

----------------------------------------------------------------

length :: [a] → Int
length = foldr (const (+1)) 0

----------------------------------------------------------------

foldLength :: [a] -> Int
foldLength = foldr (const (+1)) 0

----------------------------------------------------------------

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

----------------------------------------------------------------

reverse :: [a] -> [a]
reverse = foldr (flip snoc) []

----------------------------------------------------------------

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = foldr (consIf p) [] xs
  where
    consIf p x xs
      | p x        = x : xs
      | otherwise  = xs

----------------------------------------------------------------

group :: Eq a => [a] -> [[a]]
group = foldr f []
    where
        f x [] = [[x]]
        f x ((y:ys):yss)
            | x == y    =  ((x:y:ys):yss)
            |otherwise  =  [x]:((y:ys):yss)

----------------------------------------------------------------

transpose :: [[a]] -> [[a]]
transpose = foldr f []
    where
        f xs []   =  map (:[]) xs 
        f xs xss  =  zipWith (:) xs xss

----------------------------------------------------------------

transpose' :: [[a]] -> [[a]]
transpose' = foldr (zipWith (:)) (repeat [])

----------------------------------------------------------------

repeat :: a → [a]
repeat x = x : repeat x

----------------------------------------------------------------

permute :: [a] -> [[a]]
permute = foldr f k
    where
        f x yss  =  concat(map(sprinkle x) yss)
        k        =  [[]]

----------------------------------------------------------------

sprinkle :: a -> [a] -> [[a]]
sprinkle x xs = [ take n xs ++ [x] ++ drop n xs | n <- [0..length xs]]

----------------------------------------------------------------

sprinkle' :: a -> [a] -> [[a]]
sprinkle' x xs = zipWith f (inits xs) (tails xs)
  where
       f ys zs = ys ++ [x] ++ zs

----------------------------------------------------------------

sprinkle'' :: a -> [a] -> [[a]]
sprinkle'' x = foldr f k where
  k = [[x]]
  f x ((y:ys):yss) = (y:x:ys) : map (x:) ((y:ys):yss)

----------------------------------------------------------------

foldl f k [x1, x2, ..., xn ] = f (...(f (f k x1) x2)...) xn
                             = (...((k ‘f‘ x1) ‘f‘ x2)...) ‘f‘ xn

----------------------------------------------------------------

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f k []      =  k
foldl f k (x:xs)  =  foldl f (f k x) xs

----------------------------------------------------------------

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' g l xs = foldr f k xs l
  where
    k = id
    f x h = \y -> h (g y x)