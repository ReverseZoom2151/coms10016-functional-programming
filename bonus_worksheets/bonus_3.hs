import qualified Data.Map as M
import Data.Maybe

mrX :: M.Map String Int
mrX = M.fromList [ ("Underground", 4)
                 , ("Secret", 1)
                 , ("Double", 2)
                 , ("Bus", 10)
                 , ("Taxi", 6)]

colours :: M.Map String String
colours = M.fromList [  ("Red", "Rot"),
                           (  "Green",  "Grün"),  (  "Brown", "Braun"), 
                           (  "Purple", "Lila"),  (  "Orange","Orange")]

lookup' :: Ord k => k -> M.Map k v -> Maybe v
lookup' k m
      |  (M.member k m)  =  Just (m M.! k)
      |  otherwise       =  Nothing

foldMap :: Ord k => (a -> b -> b) -> b -> M.Map k a -> b
foldMap f k m = foldr f k (map snd (M.toList m))

filterMap :: Ord k => (a -> Bool) -> M.Map k a -> M.Map k a
filterMap f m = (M.fromList . filter (\ (k,v) -> f v)) (M.toList m)

fibbies :: Int -> M.Map Int Integer
fibbies 1  =  (M.insert 1 1. M.insert 0 0 ) M.empty
fibbies n  =  M.insert n (x+y) oldMap where
                  oldMap  =  fibbies (n-1)
                  x       =  oldMap M.! (n-1) 
                  y       =  oldMap M.! (n-2) 

data Trie k v  =  Trie (Maybe v) (M.Map k (Trie k v))
               deriving Show

trieEmpty :: Trie k v
trieEmpty = Trie Nothing M.empty

trieRemove :: Ord k => [k] -> Trie k v -> Trie k v
trieRemove [] (Trie mv kvs)       =  Trie Nothing kvs
trieRemove (k:ks)  (Trie mv kvs)  =  Trie mv (M.insert k vs kvs)
                          where 
                              vs  = ( (trieRemove ks) . (fromMaybe trieEmpty) . M.lookup k) kvs

toTrie :: Ord a => M.Map [a] b -> Trie a b
toTrie m = toTrie' (M.toList m)
              where
                  toTrie' :: Ord a => [([a],b)] -> Trie a b
                  toTrie' []          =  trieEmpty
                  toTrie' ((x,y):ts)  =  trieInsert x y (toTrie' ts)

trieInsert :: Ord k => [k] -> v -> Trie k v -> Trie k v
trieInsert [] v (Trie mv kvs)      =  Trie (Just v)  kvs
trieInsert (k:ks) v (Trie mv kvs)  =  Trie mv kvs'
                  where 
                          kvs' = case M.lookup k kvs of
                               Nothing   ->  M.insert k (trieInsert ks v trieEmpty) kvs
                               (Just t)  ->  M.insert k (trieInsert ks v t) kvs

newtype Set a = Set [a] deriving Show

insertSetList :: Eq a => a -> Set a -> Set a
insertSetList x (Set xs)
               | x `elem` xs  =  Set xs
               | otherwise  =  Set (x:xs)

unionSetList :: Eq a => Set a -> Set a -> Set a
unionSetList (Set []) s      =  s
unionSetList (Set (x:xs)) s  =  insertSetList x (unionSetList (Set xs) s)
unionSetList' (Set xs) s = (foldr (\x s' -> insertSetList x s') s xs)

toSet :: Eq a => [a] -> Set a
toSet = foldr insertSetList (Set []) 
                                        intersectionSetList :: Eq a => Set a -> Set a -> Set a
intersectionSetList (Set xs) (Set ys) = toSet (filter (\z -> (z `elem` xs) && (z `elem` ys)) (xs ++ ys))

powerSet :: Eq a => Set a -> Set (Set a)
powerSet (Set xs) = Set (map Set (power xs)) 
              where
                  power = foldr (\x yss -> yss ++ map (x:) yss) [[]]

newtype Set' a = Set' (M.Map a ())
                   deriving Show

insertSetMap :: Ord a => a -> Set' a -> Set' a 
insertSetMap x (Set' m) = Set' (M.insert x () m)

unionSetMap :: Ord a => Set' a  -> Set' a  -> Set' a 
unionSetMap (Set' mx) (Set' my) = Set' (M.union mx my)

intersectionSetMap :: Ord a => Set' a  -> Set' a  -> Set' a 
intersectionSetMap (Set' mx) (Set' my) = Set' (M.intersection mx my)

data NotSoPerfectTree a  =  Tip a
                         |  Node (NotSoPerfectTree a) (NotSoPerfectTree a)
                          deriving Show

x :: NotSoPerfectTree Int
x = Node (Tip 9) (Node (Tip 8) (Tip 9))

copy :: a -> (a,a)
copy x = (x,x)

four n = copy (copy n)

data PTree a  =  Leaf a
              |  Fork (PTree (a,a))
               deriving Show

instance Functor PTree where
            fmap f (Leaf x)  =  Leaf (f x)
            fmap f (Fork t)  =  Fork (fmap (g f) t)
            
g :: (a -> b) -> (a,a) -> (b,b)
g f (x,y) = (f x, f y)