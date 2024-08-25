class Monoid m where
    mempty :: m 
    mappend :: m → m → m
    mconcat :: [m ] → m
    -- defining mconcat is optional, since it has the following default:
    mconcat = foldr mappend mempty

instance Monoid Integer where
    mempty = 0
    mappend = (+)

newtype Pint = Pint Integer deriving Show

instance Monoid Pint where
    mempty = Pint 1
    mappend (Pint x ) (Pint y) = Pint (x ∗ y)

fromPint :: Pint → Integer
fromPint (Pint x ) = x

magic = (fromPint ◦ mconcat ◦ (map Pint))
