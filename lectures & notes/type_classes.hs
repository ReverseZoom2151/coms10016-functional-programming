class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
negate :: a -> a

add :: Num a => a -> a -> a
add x y = x + y

data Fresher = CompSci
             | Maths

class Show a where
    show :: a -> String

instance Show Fresher where
                show ComSci = "Hello I am a computer scientist!"
                show Maths = "Hi I study maths!"
