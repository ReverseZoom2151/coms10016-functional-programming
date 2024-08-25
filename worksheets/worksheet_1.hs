cloudCloud = 0.69
cloudCloud :: Double
cloudCloud :: Float

----------------------------------------------

square x = x * x
square :: Int → Int
square :: Int → Int
square :: (Num a) ⇒ a → a

----------------------------------------------

isPositive x = x > 0
isPositive :: (Ord a,Num a) ⇒ a → Bool

----------------------------------------------

data Cat = Persian | Siamese | Munchkin
data Dog = Labrador | Pug | Chihuahua
Either Cat Dog

----------------------------------------------

takeAway (x, y)  =  y - x
subtract x y  =  y - x
takeAway :: Num a ⇒ (a, a) → a 
subtract :: Num a ⇒ a → a → a
subtract = curry takeAway

----------------------------------------------

apply :: (a -> b) -> a -> b
apply f x = f x
apply (subtract 3) 1
apply (takeAway 3) 1
apply (λx → takeAway (3, x)) 1
apply (curry takeAway 3) 1
apply (takeAway · ((,) 3)) 1

----------------------------------------------

deception = Left (Right True)
deception :: Either (Either a Bool) b

----------------------------------------------

mysterious = ()
mysterious :: ()

----------------------------------------------

choose :: Bool → a → b → c
choose :: Bool -> a -> b -> Either a b
choose True x _   =  Left x
choose False _ y  =  Right y

----------------------------------------------

f :: (Int → Bool) → (Int → Bool)
