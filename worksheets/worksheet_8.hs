class Functor f where
    fmap :: (a → b) → f a → f b

instance Functor [ ] where
    fmap = map

data Maybe' a = Just' a | Nothing'

instance Functor Maybe′ where
    fmap f (Just′ x) = Just′ (f x)
    fmap f Nothing′ = Nothing′

data Either' a b = Left' a | Right' b

instance Functor (Either′ e) where
    fmap f (Right′ x) = Right′ (f x)
    fmap f (Left′ x) = Left′ x

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

newtype Reader a b = MkReader (a -> b)

instance Functor (Reader d) where
    fmap :: (b → c) → Reader a b → Reader a c 
    fmap f (MkReader g) = MkReader (f · g)

class Functor f ⇒ Applicative f where
    (⟨∗⟩) :: f (a → b) → f a → f b
    pure :: a → f a

(⟨∗⟩) :: f (a → b) → f a → f b

pure :: a → f a

data Maybe′ a = Just′ a | Nothing′

instance Applicative Maybe′ where
    (Just′ f) ⟨∗⟩ (Just′ x) = Just′ (f x)
    ⟨∗⟩ = Nothing′
    pure = Just′

data Either′ a b = Left′ a | Right′ b

instance Applicative (Either′ e) where
    Right′ f ⟨∗⟩ Right′ x = Right′ (f x)
    Left′ x ⟨∗⟩ = Left′ x
            ⟨∗⟩ Left′ y = Left′ y
    pure = Right′

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Applicative Tree where
    (Leaf f) ⟨∗⟩ oak = fmap f oak
    (Branch l r) ⟨∗⟩ oak = Branch (l ⟨∗⟩ oak) (r ⟨∗⟩ oak)
    pure = Leaf

newtype Reader a b = MkReader (a → b)

instance Applicative (Reader d) where
    (⟨∗⟩) :: Reader a (b → c) → Reader a b → Reader a c 
    (MkReader fabc) ⟨∗⟩ (MkReader fab) = MkReader (λx → fabc x (fab x))
    pure x = MkReader (\_ → x)
    pure x = MkReader (const x)

fmapAp :: Applicative f ⇒ (a → b) → f a → f b
fmapAp f mx = pure f ⟨∗⟩ mx
fmapAp = fmap

liftA2 :: Applicative f ⇒ (a → b → c) → f a → f b → f c
liftA2 :: Applicative f ⇒ (a → b → c) → (f a → f b → f c)
liftA2 (+) (Just 7) (Just 3) ≡ (Just 10)
liftA2 (,) ['a', 'b'] [1, 2] ≡ [('a', 1),('a', 2),('b', 1),('b', 2)]
liftA2 f mx my = fmap f mx ⟨∗⟩ my
liftA2 f mx my = f ⟨$⟩ mx ⟨∗⟩ my

doTwice :: Applicative f ⇒ f a → f (a, a)
doTwice (Just 10) ≡ Just (10, 10)
doTwice [1, 2] ≡ [(1, 1),(1, 2),(2, 1),(2, 2)]
doTwice mx = fmap (λx y → (x, y)) mx ⟨∗⟩ mx
doTwice mx = liftA2 (,) mx mx

sequenceAp :: Applicative f ⇒ [f a] → f [a]
sequenceAp [Just 1, Just 3] ≡ Just [1, 3]
sequenceAp [Just 1, Just 3,Nothing] ≡ Nothing
sequenceAp [print 1, print 5]

sequenceAp :: Applicative f ⇒ [f a] → f [a]
sequenceAp [ ] = pure [ ]
sequenceAp (mx : mxs) = liftA2 (:) mx (sequenceAp mxs)
sequenceAp = foldr (liftA2 (:)) (pure [ ])

mapAp :: Applicative f ⇒ (a → f b) → [a] → f [b]
mapAp f xs = sequenceAp (fmap f xs)

mapAp_ :: Applicative f ⇒ (a → f b) → [a] → f ()
mapAp_ f xs = fmap (const ()) (mapAp f xs)
