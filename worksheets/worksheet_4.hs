length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

----------------------------------------------------------------

sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

----------------------------------------------------------------

fromJusts :: [Maybe a] -> [a]
fromJusts [] = []
fromJusts ((Just x):xs) = x:fromJusts xs
fromJusts (Nothing:xs) = fromJusts xs

----------------------------------------------------------------

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
  | x > y      =  y:insert x ys
  | otherwise  =  x:y:ys

----------------------------------------------------------------

isort :: [Int] -> [Int]
isort []     = []
isort (x:xs) = insert x (isort xs)

----------------------------------------------------------------

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y     =  x : merge xs (y:ys)
  | otherwise  =  y : merge (x:xs) ys

----------------------------------------------------------------

split :: [a] -> ([a], [a])
split []      = ([], [])
split (x:xs)  = (x:ys, zs)
  where
    (zs, ys) = split xs

----------------------------------------------------------------

data List a = Empty
            | Cons a (List a)
deriving Eq

toList :: [a] → List a
toList [ ] = Empty
toList (x : xs) = Cons x (toList xs)

----------------------------------------------------------------

fromList :: List a → [a]
fromList Empty = [ ]
fromList (Cons x xs) = x : (fromList xs)

----------------------------------------------------------------

prop_isoToFrom xs = fromList (toList xs) ≡ xs
prop_isoFromTo xs = toList (fromList xs) ≡ xs

----------------------------------------------------------------

repeat :: a → [a]
repeat x = x : repeat x

----------------------------------------------------------------

prop_repeat n x = n >= 0 ==> length (take n (repeat x)) == n

----------------------------------------------------------------

prop_append xs ys = length xs + length ys == length (xs ++ ys)
xs ++ [ ] = xs
(xs ++ ys) ++ zs = xs ++ (ys ++ zs)

----------------------------------------------------------------

(++) :: [a] → [a] → [a]
[ ] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

reverse :: [a] -> [a]
reverse []      =  []
reverse (x:xs)  =  reverse xs ++ [x]

----------------------------------------------------------------

prop_rev xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

----------------------------------------------------------------

reverse1 :: [a] -> [a]
reverse1 xs = reverse2 xs []
  where
    reverse2 (x:xs) acc  =  reverse2 xs (x:acc)
    reverse2 [] acc      =  acc