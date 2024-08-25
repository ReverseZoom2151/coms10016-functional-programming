-- Functional Programming
-- Binary Trees
-- Alex Kavvos, TB1 2020

import Test.QuickCheck

data Tree a = Empty | Fork a (Tree a) (Tree a)
  deriving (Show, Eq)

-- Planting a balanced tree

split :: [a] -> ([a], [a])
split [] = ([], [])
split (x:xs) = (x:ys, zs)
  where (zs, ys) = split xs

plant :: [a] -> Tree a
plant [] = Empty
plant xs =  Fork z (plant zs) (plant ys)
  where (z:zs, ys) = split xs

-- Flattening a tree

flatten :: Tree a -> [a]
flatten Empty = []
flatten (Fork z lt rt) = flatten lt ++ [z] ++ flatten rt

-- Inserting an (orderable) value into a binary search tree.

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Fork x Empty Empty
insert x (Fork z lt rt)
  | x <= z    = Fork z (insert x lt) rt
  | otherwise = Fork z lt (insert x rt)

-- Planting a BST.

plantBST :: Ord a => [a] -> Tree a
plantBST = foldr insert Empty

-- Looking up a value in a BST.
-- NB we must be able to check the value for equality.

lookupBST :: (Eq a, Ord a) => a -> Tree a -> Bool
lookupBST x Empty = False
lookupBST x (Fork z lt rt)
  | x == z     = True
  | x < z      = lookupBST x lt
  | otherwise  = lookupBST x rt


sort :: Ord a => [a] -> [a]
sort = flatten . plantBST

-- 

example = plant (reverse ['z', 'f', 'k', 'b', 'a', 'd'])
example2 = Fork "six" (Fork "two" Empty Empty) (Fork "four" (Fork "three" Empty Empty) (Fork "five" Empty Empty))

-- Generating random trees for testing; here be dragons, move along.

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arbTree

arbTree :: Arbitrary a => Int -> Gen (Tree a)
arbTree 0 = do
    return Empty
arbTree n = do
    lt <- arbTree (n `div` 2)
    rt <- arbTree (n `div` 2)
    x <- arbitrary
    return (Fork x lt rt)