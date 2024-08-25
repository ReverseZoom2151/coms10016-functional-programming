class Show a where show :: a -> String

class Read a where read :: String -> a

class Eq a where
 (==), (/=) :: a -> a -> Bool

class Eq a => Ord a where
 (<), (<=), (>=), (>) :: a -> a -> Bool
 max, min :: a -> a -> a

class (Eq a, Show a) => Num a where
 (+), (-), (*) :: a -> a -> a
 negate :: a -> a
 abs, signum :: a -> a
 fromInteger :: Integer -> a

class (Num a, Ord a) => Real a where
 toRational :: a -> Rational

class (Real a, Enum a) => Integral a where
 quot, rem :: a -> a -> a
 div, mod :: a -> a -> a
 toInteger :: a -> Integer

class Num a => Fractional a where
 (/) :: a -> a -> a
 fromRational :: Rational -> a

class (Fractional a) => Floating a where
 exp, log, sqrt :: a -> a
 sin, cos, tan :: a -> a

class (Real a, Fractional a) => RealFrac a where
 truncate, round :: (Integral b) => a -> b
 ceiling, floor :: (Integral b) => a -> b

even, odd :: Integral a => a -> Bool
even n = n ‘rem‘ 2 == 0
odd = not . even

sequence :: Monad m => [m a] -> m [a]
sequence = foldr mcons (return [])
    where mcons p q = do x <- p
                         xs <- q
                         return (x:xs)

sequence_ :: Monad m => [m a] -> m ()
sequence_ xs = do sequence xs
                  return ()

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m1 = do x1 <- m1
                return (f x1)

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \ x -> f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

($) :: (a -> b) -> a -> b
f $ x = f x

data Bool = False | True

(&&), (||) :: Bool -> Bool -> Bool
True && x = x
False && _ = False
True || _ = True
False || x = x

not :: Bool -> Bool
not True = False
not False = True

data Maybe a = Nothing | Just a

isJust,isNothing :: Maybe a -> Bool
isJust (Just a) = True
isJust Nothing = False

isNothing = not . isJust

fromJust :: Maybe a -> a
fromJust (Just a) = a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:_) = Just a

catMaybes :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

fst :: (a,b) -> a
fst (x,y) = x

snd :: (a,b) -> b
snd (x,y) = y

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p = f (fst p) (snd p)

map :: (a -> b) -> [a] -> [b]
map f xs = [ f x | x <- xs ]

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [ x | x <- xs, p x ]

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

head, last :: [a] -> a
head (x:_) = x

last [x] = x
last (_:xs) = last xs

tail, init :: [a] -> [a]
tail (_:xs) = xs

init [x] = []
init (x:xs) = x : init xs

null :: [a] -> Bool
null [] = True
null (_:_) = False

length :: [a] -> Int
length = foldr (const (1+)) 0

(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n-1)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

repeat :: a -> [a]
repeat x = xs where xs = x:xs

replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

cycle :: [a] -> [a]
cycle [] = error "Prelude.cycle: empty list"
cycle xs = xs’ where xs’ = xs ++ xs’

tails :: [a] -> [[a]]
tails xs = xs : case xs of
                [] -> []
                _ : xs’ -> tails xs’

take, drop :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs

drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs

splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs = (take n xs, drop n xs)

takeWhile, dropWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs)
            | p x = x : takeWhile p xs
            | otherwise = []

dropWhile p [] = []
dropWhile p xs@(x:xs’)
            | p x = dropWhile p xs’
            | otherwise = xs

span :: (a -> Bool) -> [a] -> ([a], [a])
span p as = (takeWhile p as, dropWhile p as)

lines, words :: String -> [String]
-- lines "apa\nbepa\ncepa\n"
-- == ["apa","bepa","cepa"]
-- words "apa bepa\n cepa"
-- == ["apa","bepa","cepa"]

unlines, unwords :: [String] -> String
-- unlines ["apa","bepa","cepa"]
-- == "apa\nbepa\ncepa\n"
-- unwords ["apa","bepa","cepa"]
-- == "apa bepa cepa"

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

and, or :: [Bool] -> Bool
and = foldr (&&) True
or = foldr (||) False

any, all :: (a -> Bool) -> [a] -> Bool
any p = or . map p
all p = and . map p

elem, notElem :: (Eq a) => a -> [a] -> Bool
elem x = any (== x)
notElem x = all (/= x)

lookup :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup key [] = Nothing
lookup key ((x,y):xys)
    | key == x = Just y
    | otherwise = lookup key xys

sum, product :: (Num a) => [a] -> a
sum = foldl (+) 0
product = foldl (*) 1

maximum, minimum :: (Ord a) => [a] -> a
maximum [] = error "Prelude.maximum: empty list"
maximum (x:xs) = foldl max x xs

minimum [] = error "Prelude.minimum: empty list"
minimum (x:xs) = foldl min x xs

zip :: [a] -> [b] -> [(a,b)]
zip = zipWith (,)

zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs)
              = z a b : zipWith z as bs
zipWith _ _ _ = []

unzip :: [(a,b)] -> ([a],[b])
unzip =
    foldr (\(a,b) ˜(as,bs) -> (a:as,b:bs)) ([],[])

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) =
        x : nub [ y | y <- xs, x /= y ]

delete :: Eq a => a -> [a] -> [a]
delete y [] = []
delete y (x:xs) =
        if x == y then xs else x : delete y xs

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) = foldl (flip delete)

union :: Eq a => [a] -> [a] -> [a]
union xs ys = xs ++ (ys \\ xs)

intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys = [ x | x <- xs, x ‘elem‘ ys ]

intersperse :: a -> [a] -> [a]
-- intersperse 0 [1,2,3,4] == [1,0,2,0,3,0,4]

transpose :: [[a]] -> [[a]]
-- transpose [[1,2,3],[4,5,6]]
-- == [[1,4],[2,5],[3,6]]

partition :: (a -> Bool) -> [a] -> ([a],[a])
partition p xs =
    (filter p xs, filter (not . p) xs)

group :: Eq a => [a] -> [[a]]
group = groupBy (==)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy eq (x:xs) = (x:ys) : groupBy eq zs
                where (ys,zs) = span (eq x) xs

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y
                           && isPrefixOf xs ys

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf x y = reverse x
                    ‘isPrefixOf‘ reverse y

sort :: (Ord a) => [a] -> [a]
sort = foldr insert []

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:xs) =
    if x <= y then x:y:xs else y:insert x xs

type String = [Char]

toUpper, toLower :: Char -> Char
-- toUpper ’a’ == ’A’
-- toLower ’Z’ == ’z’

digitToInt :: Char -> Int
-- digitToInt ’8’ == 8

intToDigit :: Int -> Char
-- intToDigit 3 == ’3’

ord :: Char -> Int
chr :: Int -> Char

arbitrary :: Arbitrary a => Gen a
-- the generator for values of a type
-- in class Arbitrary, used by quickCheck

choose :: Random a => (a, a) -> Gen a
-- Generates a random element in the given
-- inclusive range.

oneof :: [Gen a] -> Gen a
-- Randomly uses one of the given generators

frequency :: [(Int, Gen a)] -> Gen a
-- Chooses from list of generators with
-- weighted random distribution.

elements :: [a] -> Gen a
-- Generates one of the given values.

listOf :: Gen a -> Gen [a]
-- Generates a list of random length.

vectorOf :: Int -> Gen a -> Gen [a]
-- Generates a list of the given length.

sized :: (Int -> Gen a) -> Gen a
-- construct generators that depend on
-- the size parameter.

putStr, putStrLn :: String -> IO ()
getLine :: IO String

type FilePath = String
readFile :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()

