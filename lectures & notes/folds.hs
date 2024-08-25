function :: Num a => [a] -> a
function [] = 0
function (x:xs) = x + function xs

----------------------------------------------------

function' :: Num a => [a] -> a
function' [] = 1
function' (x:xs) = x * function' xs

----------------------------------------------------

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f k [] = k
foldr f k (x:xs) = x `f` foldr f k xs

----------------------------------------------------

[1,2,3,4] -> 1 : 2 : 3 : 4 : []

----------------------------------------------------

foldr (+) 0 [1,2,3,4] = 1 + 2 + 3 + 4 + 0 = 10

----------------------------------------------------

remove :: Eq a => [a] -> [a] -> [a]
remove a b = foldr (consif (not . flip elem a)) [] b

----------------------------------------------------

consif :: (a -> Bool) -> a -> [a] -> [a]
consif f x (xs)
| f x = x :(xs)
| otherwise = (xs)
remove "DON'T" "i DON'T like folds" = "i like folds"

----------------------------------------------------

remdups :: Eq a => [a] -> [a]
remdups = foldr f []
        where
            f x [] = [x]
            f x (y:ys)
                | x == y = y:ys
                | otherwise = x:y:ys
remdups "tyyypess likkeee thisssss" = "types like this"

----------------------------------------------------

data Tree a = Tip
            | Node (Tree a) a (Tree a)

----------------------------------------------------

data [] a = []
          | a : [a]

----------------------------------------------------

foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b

----------------------------------------------------

foldTree t n Tip = t
foldTree t n (Node l x r) = n (foldTree t n l) x (foldTree t n r)

----------------------------------------------------

sumTree :: Tree Int -> Int
sumTree = foldTree t n
        where
                t = 0
                n l x r = l + x + r

----------------------------------------------------

productTree :: Tree Int -> Int
productTree = foldTree t n
            where
                    t = 1
                    n l x r = l * x * r

----------------------------------------------------

nodes :: Tree a -> Int
nodes = foldTree tip node
    where
      tip = 0
      node l x r = l + r + 1

----------------------------------------------------

tips :: Tree a -> Int
tips = foldTree tip node
    where
        tip = 1
        node l x r = l+r

----------------------------------------------------

data TreeAlg a b = TreeAlg b (b -> a -> b -> b)

----------------------------------------------------

data TreeAlg a b = TreeAlg
                         b 
                       ( b -> a -> b -> b)

----------------------------------------------------

foldTree :: TreeAlg a b -> Tree a -> b

----------------------------------------------------

foldTree (TreeAlg tip node) Tip = tip
foldTree alg@(TreeAlg tip node) (Node l x r) = node (foldTree alg l) x (foldTree alg r)

----------------------------------------------------

noTip :: Num b => Tree a -> b
noTip = foldTree alg
            where
                alg = TreeAlg tip node
                tip = 1
                node l x r = l + r

----------------------------------------------------

data TreeAlgBetter a b = TreeAlgBetter { tip :: b
                                       , node :: ( b -> a -> b -> b)
                                       }

----------------------------------------------------

foldTree alg Tip = tip alg
foldTree alg (Node l x r) = (node alg) (foldTree alg l) x (foldTree alg r)

----------------------------------------------------

tip :: TreeAlgBetter a b -> b
node :: TreeAlgBetter a b -> b -> a -> b -> b
