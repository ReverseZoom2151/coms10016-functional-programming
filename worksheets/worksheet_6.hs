nats :: [Integer]
nats = [1 . .]

squares :: [Integer]
squares = [x × x | x ← nats]

map :: (a → b) → [a] → [b]
map f xs = [f x | x ← xs]

filter :: (a → Bool) → [a] → [a]
filter p xs = [x | x ← xs, p x]

cartesian :: [a] → [b] → [(a, b)]
cartesian xs ys = [(x, y) | x ← xs, y ← ys]

bitString :: Int → [String]
bitString 0 = [""]
bitString n = [x ++ y | x ← ["0", "1"], y ← bitString (n − 1)]

bitStrings :: Int → [String]
bitStrings n = concat (map bitString [0 . . n])

class Pretty a where
   pretty :: a -> String

data Reaction  =  Happy | Sad | Excited | Angry |  Indifferent

instance Pretty Reaction where
    pretty Happy = ":)"
    pretty Sad = ":("
    pretty Excited = ":D"
    pretty Angry = ">:("
    pretty Indifferent = ":/"

instance Eq a ⇒ Eq [a] where
    [ ] ≡ [ ] = True
    (x : xs) ≡ (y : ys) = (x ≡ y) ∧ (xs ≡ ys)
    xs ≡ ys = False

data Suit  =  Heart |  Diamond |  Club |  Spade deriving (Show, Eq)
data Face  =  Ace | Two | Three | Four | Five | Six | Seven
           |  Eight | Nine | Ten | Jack | Queen | King
  deriving (Show,Eq,Ord)
data Card  =  Joker |  Card (Face, Suit) deriving (Show, Eq)

instance Ord Card where
                ⩽ Joker = True
  Joker         ⩽       = False
  (Card (f, s)) ⩽ (Card (f′, s′))
    | s′ ≡ Spade ∧ s ̸≡ Spade = True
    | s′ ̸≡ Spade ∧ s ≡ Spade = False
    | s′ ≡ Heart ∧ s ̸≡ Heart = True
    | s′ ̸≡ Heart ∧ s ≡ Heart = False
    | otherwise = f ⩽ f

zipWith :: (a → b → c) → [a] → [b] → [c]
zipWith f (x : xs) (y : ys) = (f x y) : (zipWith f xs ys)
zipWith = [ ]

class Num a where
    (+) :: a → a → a
    (−) :: a → a → a
    (×) :: a → a → a
    abs :: a → a -- the absolute value of a number
    signum :: a → a -- the sign of a number, either −1, 0, or 1
    fromInteger :: Integer → a -- conversion from an integer into a number

instance Num a ⇒ Num [a] where
    (+) = zipWith (+)
    (−) = zipWith (−)
    (×) = zipWith (×)
    abs = map abs
    signum = map signum
    fromInteger x = repeat (fromInteger x)

class Num a ⇒ Fractional a where
    (/) :: a → a → a

instance Fractional a ⇒ Fractional [a] where
    (/) = zipWith (/)
    fromRational x = repeat (fromRational x)

nats :: [Double]
nats = [0..]

ns :: [Double]
ns = 2 × nats + 1

signs :: [Double]
signs = cycle [1, −1]

pis :: [Double]
pis = 4 / ns × signs

pi :: Int → Double
pi n = sum (take n pis)

data Tree' a  =  Leaf' a
              |  Fork (Tree' a) (Tree' a)
            deriving Show

data Bush a = Tip
            | Node (Bush a) a (Bush a)
          deriving Show

data Tush a = TLeaf a
            | TNode (Tush a) a (Tush a)
          deriving Show

data RoseTree a  =  RoseLeaf a
                 |  RoseFork [RoseTree a]
            deriving Show

data RoseBush a  =  RoseBush a [RoseBush a]
            deriving Show

data Tree a  =  Leaf a
             |  Split (Tree a) (Tree a)
      deriving Eq

collapse :: Tree Int -> [Int]
collapse (Leaf n)      = [n]
collapse (Split l r)   = collapse l ++ collapse r

mirror :: Tree a -> Tree a
mirror (Split l r)  = Split (mirror r) (mirror l)
mirror t            = t

foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree tleaf tsplit (Leaf x)      = tleaf x
foldTree tleaf tsplit (Split l r)   = tsplit (foldTree tleaf tsplit l) (foldTree tleaf tsplit r)

collapse' :: Tree Int -> [Int]
collapse' = foldTree (\x -> [x]) (++)

prop_mirrorMirror t = mirror (mirror t) == t

prop_mirrorCollapse t = collapse (mirror t) == reverse (collapse t)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized genTree
genTree :: Arbitrary a => Int -> Gen (Tree a)
genTree s = frequency
    [ (s, do l <- genTree s'
             r <- genTree s'
             return (Split l r))
    , (1, do a <- arbitrary
             return (Leaf a)
      )
    ]
  where
    s' = s `div` 2

orderedList :: Gen [Integer]
orderedList =
  do  xs  <- arbitrary
      return (sort xs)

listOfLength :: Integer → Gen a → Gen [a]

listOfLength :: Int -> Gen a -> Gen [a]
listOfLength n gen = sequence [ gen | i <- [1..n] ]