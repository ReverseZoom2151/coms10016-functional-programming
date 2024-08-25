(++) :: [a] → [a] → [a]
[ ] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

    [] ++ ys
  === {- (++) -}
    ys

    [] ++ []
  === {- definition of |++| -}
    []

    (x:xs) ++ []
  === {- definition of |++| -}
    x:(xs++[])
  === {- induction hypothesis -}
    (x:xs)

class Semi s where
  binop :: s -> s -> s

newtype BooleOR = BooleOR Bool

instance Semigroup BooleOR where
  (<>) (BooleOR x) (BooleOR y) = BooleOR (x || y)

instance Monoid BooleOR where
  mempty  = BooleOR False
  mappend (BooleOR x) (BooleOR y) = BooleOR (x || y)

newtype BooleAND = BooleAND Bool

instance Semigroup BooleAND where
  (<>) (BooleAND x) (BooleAND y) = BooleAND (x || y)

instance Monoid BooleAND where
  mempty  = BooleAND True
  mappend (BooleAND x) (BooleAND y) = BooleAND (x && y)

newtype Function a = Function (a -> a)
instance Monoid (Function a) where
  mempty                             =  Function id
  mappend (Function x) (Function y)  =  Function (x . y)

        mappend mempty (Function x)
      === {- def. mempty -}
        mappend (Function id) (Function x)
      === {- def. mappend -}
        (Function (id . x))
      === {- $\langle$|a -> b, (.), id|$\rangle$ is a monoid -}
        Function x

        mappend (Function x) mempty
      === {- def. mempty -}
        mappend (Function x) (Function id)
      === {- def. mappend -}
        Function (x . id)
      === {- $\langle$|a -> b, (.), id|$\rangle$ is a monoid -}
        Function x

   (Function x `mappend` Function y) `mappend` Function z
 === {- def. mappend-} 
   Function (x . y) `mappend` Function z
 === {- def. mappend-} 
   Function ((x . y) . z)
 === {- $\langle$|a -> b, (.), id|$\rangle$ is a monoid -}
   Function (x . (y . z))
 === {- def. mappend-} 
   Function x `mappend` Function (y . z)
 === {- def. mappend-} 
   Function x `mappend` (Function y `mappend` Function z)

instance Monoid Integer where
    mempty   =  1
    mappend  =  (^)

class Group a where
          op        ::  a -> a -> a
          identity  ::  a
          inverse   ::  a -> a

class Monoid a => Group' a where
          inverse' :: a -> a
          
instance Group Integer where
          op a b     =  a + b
          identity   =  0
          inverse a  =  negate a