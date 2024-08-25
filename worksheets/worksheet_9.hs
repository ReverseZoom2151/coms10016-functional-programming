class Applicative m ⇒ Monad m where
    (>>=) :: m a → (a → m b) → m b

return :: Monad m ⇒ a → m a
return = pure

    pure a >>= k = k a 
    m >>= pure = m 
m >>= (λx → k x >>= h) = (m >>= k) >>= h 
    m >=> (k >=> h) = (m >=> k) >=> h

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g

data Maybe' a = Just' a | Nothing'
  deriving (Eq, Show)

instance Monad Maybe′ where
    Just′ x >>= f = f x
    Nothing′ >>= = Nothing′

instance Monad Maybe′ where
    Just′ x >>= f = Nothing′
    Nothing′ >>= = Nothing′

instance Monad Maybe′ where
    Just′ x >>= f = Just′ x >>= f
    Nothing′ >>= = Nothing′

data Either' a b = Left' a | Right' b
  deriving (Eq, Show)

instance Monad (Either′ e) where
    Left′ a >>= f = Left′ a
    Right′ b >>= f = f b

safeDiv :: Int -> Int -> Either' String Int
safeDiv x 0 = Left' ("Tried to divide " ++ show x ++ " by 0")
safeDiv x y = Right' (div x y)

safeDiv2 :: Int → Int → Int → Either′ String Int
safeDiv2 x y z = do
    u ← safeDiv x y
    safeDiv u z

safeDiv2 :: Int → Int → Int → Either′ String Int
safeDiv2 x y z
    = safeDiv x y >>= (λu →
    safeDiv u z)

safeDivAll :: Int → [Int] → Either′ String Int
safeDivAll x [ ] = Right′ x
safeDivAll x (y : ys) = do
    u ← safeDiv x y
    safeDivAll u ys

safeDivAll :: Int → [Int] → Either′ String Int
safeDivAll x [ ] = pure x
safeDivAll x (y : ys) = safeDiv x y >>= (λu → safeDivAll u ys)

import Data.Foldable (foldl′, foldlM)

safeDivAll :: Int → [Int] → Either′ String Int
safeDivAll x ys = foldl′ f (Right′ x) ys
    where
        f acc y = do
            u ← acc
            safeDiv u y

safeDivAll :: Int → [Int] → Either′ String Int
safeDivAll x ys = foldlM safeDiv x ys

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Eq, Show)

instance Monad Tree where
    (Leaf x) >>= f = f x
    (Branch l r) >>= f = Branch (l >>= f) (r >>= f)

instance Monad Tree where
    (Leaf x) >>= f = f x
    (Branch l r) >>= f = l >>= f

instance Monad Tree where
    (Leaf x) >>= f = Branch (f x) (f x)     
    (Branch l r) >>= f = Branch (l >>= f) (r >>= f)

instance Monad Tree where
    (Leaf x) >>= f = f x
    (Branch l r) >>= f = Branch (r >>= f) (l >>= f) 

map′ :: (a → b) → [a] → [b]
map′ f xs
    = [f x 
      | x ← xs]

map′ :: (a → b) → [a] → [b]
map′ f xs = do
    x ← xs
    pure (f x)

map′ :: (a → b) → [a] → [b]
map′ f xs
    = xs >>= (λx →
      pure (f x))


concat′ :: [[a]] → [a]
concat′ xss = do
    xs ← xss
    x ← xs
    return x

concat′ xss = do
    xs ← xss
    xs

concat′ xss = xss >>= id

cartesian :: [a] → [b] → [(a, b)]
cartesian xs ys =
    do x ← xs
       y ← ys
       return (x, y)

cartesian :: [a] → [b] → [(a, b)]
cartesian xs ys =
    do xs >>= (λx →
       ys >>= (λy →
       return (x, y)))

cartesian :: [a] → [b] → [(a, b)]
cartesian xs ys = liftA2 (,) xs ys

newtype Reader a b = MkReader (a -> b)

instance Monad (Reader d) where
    (>>=) :: Reader a b → (b → Reader a c) → Reader a c
    (MkReader fab) >>= fbrac
        = MkReader (λa → let b = fab a
            (MkReader fac) = fbrac b
        in fac a)

get :: Reader a a
get = MkReader id

runReader :: a → (Reader a b) → b
runReader a (MkReader fab) = fab a

mz :: Maybe Int
mz = pure 2 >>= (λx → Just (x × 10))

mz :: Maybe Int
mz = do
    x ← pure 2
    Just (x × 10)

mz :: Maybe Int
mz = Just (2 × 10)

class Monad Maybe where
    (>>=) :: Maybe a → (a → Maybe b) → Maybe b
    mx >>= f = Nothing