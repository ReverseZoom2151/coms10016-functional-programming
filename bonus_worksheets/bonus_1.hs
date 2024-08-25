data List a  =  Empty
             |  Cons a (List a)

toList :: [a] -> List a
toList []     =  Empty
toList (x:xs) =  Cons x (toList xs)

----------------------------------------------------------------

fromList :: List a -> [a]
fromList Empty       = []
fromList (Cons x xs) = x:(fromList xs)

----------------------------------------------------------------

          (toList . fromList) Empty
  ==== {- definition of |(.)| -}
          toList (fromList Empty)
  ==== {- definition of |fromList| -}
          toList []
  ==== {- definition of |toList| -}
          Empty
  ==== {- definition of |id| -}
          id Empty

----------------------------------------------------------------

          (toList . fromList) (Cons x xs)
  ==== {- definition of |(.)| -}
          toList (fromList (Cons x xs))
  ==== {- definition of |fromList| -}
          toList (x : (fromList xs))
  ==== {- definition of |toList| -}
          Cons x (toList (fromList xs))
  ==== {- definition of |(.)| -}
          Cons x ((toList . fromList) xs)
  ==== {- Induction Hypothesis -}
          Cons x (id xs)
  ==== {- definition of |id| -}
          Cons x xs
  ==== {- definition of |id| -}
          id (Cons x xs)

----------------------------------------------------------------

repeat :: a → [a]
repeat x = x : repeat x

----------------------------------------------------------------

   length (take 0 (repeat x))
 ==== {- def. |take| -}
   length []
 ==== {- def. |length| -}
   0

----------------------------------------------------------------

   length (take (n+1) (repeat x))
 ==== {- def. |repeat| -}
   length (take (n+1) (x : repeat x))
 ==== {- def. |take| -}
   length (x : take n (repeat x))
 ==== {- def. |length| -}
   1 + length (take n (repeat x))
 ==== {- induction hypothesis -}
   1 + n
 ==== {- commutativity -}
   n + 1

----------------------------------------------------------------

   length ([] ++ ys)
 ==== {- def. |(++)| -}
   length ys
 ==== {- zero operation on integers -}
   0 + length ys
 ==== {- def. |length| -}
   length [] + length ys

----------------------------------------------------------------

   length (x:xs) + length ys
 ==== {- def. |length| -}
   (1 + length xs) + length ys
 ==== {- associativity of |(+)| -}
   1 + length xs + length ys
 ==== {- induction hypothesis -}
   1 + length (xs ++ ys)
 ==== {- def. |length| -}
   length (x:(xs ++ ys))
 ==== {- def. |(++)| -}
   length ((x:xs) ++ ys)

----------------------------------------------------------------

reverse' :: [a] -> [a]
reverse' []      =  []
reverse' (x:xs)  =  reverse xs +++ [x]
 
(+++) ::  [a] -> [a] -> [a]
[] +++ ys      = ys
(x:xs) +++ ys  = x:(xs +++ ys)

----------------------------------------------------------------

            reverse ([] ++ ys)
          ==== {- definition of |(++)| -}
            reverse ys
          ==== {- |[]| is right unit of |(++)|, shown below -}
            reverse ys ++ []
          ==== {- definition of |reverse| -}
            reverse ys ++ reverse []

----------------------------------------------------------------

            reverse ((x:xs) ++ ys)
          ==== {- definition of |(++)| -}
            reverse (x:(xs++ys))
          ==== {- definition of |reverse| -}
            reverse (xs++ys) ++ [x]
          ====  {- induction hypothesis -}
            (reverse ys ++ reverse xs) ++ [x]
          ====  {- |(++)| is associative, shown below -}
            reverse ys ++ (reverse xs ++ [x])
          ==== {- definition of |reverse| -}
            reverse ys ++ reverse (x:xs)

----------------------------------------------------------------

        [] ++ []
      ====  {- definition of |(++)| -}
        []

----------------------------------------------------------------

        (x:xs) ++ []
      ====  {- definition of |++| -}
        (x:(xs ++ []))
      ====  {- induction hypothesis -}
        (x:xs)

----------------------------------------------------------------

        ([] ++ ys) ++ zs
      ====  {- definition of |++| -}
        ys ++ zs
      ====  {- definition of |++| -}
        [] ++ (ys ++ zs)

----------------------------------------------------------------

        ((x:xs) ++ ys) ++ zs
      ====  {- definition of |++| -}
        (x:(xs ++ ys)) ++ zs
      ====  {- definition of |++| -}
        (x:((xs ++ ys) ++ zs))
      ====  {- induction hypothesis -}
        (x:(xs ++ (ys ++ zs)))
      ====  {- definition of |++| -}
        (x:xs) ++ (ys ++ zs)

----------------------------------------------------------------

foldr (λx n → 1 + n) 0 xs = foldl (λn x → n + 1) 0 xs

foldr' :: (a -> b -> b) -> b -> [a] -> b

foldr' f k []      =  k
foldr' f k (x:xs)  =  f x (foldr' f k xs)

foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f k []      =  k
foldl'' f k (x:xs)  =  foldl'' f (f k x) xs

----------------------------------------------------------------

    foldr (\x n -> n + 1) 0 [] 
  ==== {- definition of |foldr| -} 
    0
  ==== {- definition of |foldl| -} 
    foldl (\n x -> 1 + n) 0 []

  let  f = \x n -> 1 + n
       g = \n x -> n + 1
  in
    foldr f 0 (x:xs)
  ==== {- definition of |foldr| -} 
    f x (foldr f 0 xs)
  ==== {- induction hypothesis -}
    f x (foldl g 0 xs)
  ==== {- definition of |f| -}
    1 + foldl g 0 xs
  ==== {- (+) is commutative -}
    foldl g 0 xs + 1
  ==== {- Proof below -}
    foldl g (0 + 1) xs
  ==== {- definition of |g| -} 
    foldl g (g 0 x) xs
  ==== {- definition of |foldl| -}
    foldl g 0 (x:xs)

----------------------------------------------------------------

    foldl f n [] + m
  ==== {- definition of |foldl| -}
    n + m
  ==== {- definition of |foldl| -}
    foldl f (n + m) []

----------------------------------------------------------------

    foldl f n (x:xs) + m
  ==== {- definition of |foldl| -} 
    foldl f (f n x) xs + m
  ==== {- definition of |f| -}
    foldl f (n + m) xs + m
  ====  {- induction hypothesis -}
    foldl f ((n + m) + m) xs
  ====  {- definition of |f| -}
    foldl f (f (n + m) x) xs
  ==== {- definition of |foldl| -}
    foldl f (n + m) (x:xs)

----------------------------------------------------------------

  (foldr g k . map f) []
 ==== {- def. |(.)| -}
  foldr g k (map f [])
 ==== {- def. |map| -}
  foldr g k []
 ==== {- def. |fold| -}
  k
 ==== {- def. |fold| -}
  foldr h k []

----------------------------------------------------------------

   (foldr g k . map f) (x:xs)
 ==== {- def. |(.)| -}
   foldr g k (map f (x:xs))
 ==== {- def. |map| -}
   foldr g k (f x : map f xs)
 ==== {- def. |fold| -}
   g (f x) (foldr g k (map f xs))
 ==== {- def. |h| -}
   h x (foldr g k (map f xs))
 ==== {- def. |(.)| -}
   h x ((foldr g k . map f) xs)
 ==== {- induction hypothesis -}
   h x (foldr h k xs)
 ==== {- def. |foldr| -}
   foldr h k (x:xs)