const' :: a -> b -> a
const' k x = k

------------------------------------------------------------

twice :: (a -> a) -> (a -> a)
twice f x = f (f x)

------------------------------------------------------------
 
infinity :: Integer
infinity = 1 + infinity

------------------------------------------------------------

    const 2 (square 3)
  === {- def. |const| -}
    2

------------------------------------------------------------

   const 2 (square 3)
  === {- def. |square| -}
   const 2 (3 * 3)
  === {- def. |(*)| -}
   const 2 9
  === {- def. |const| -}
   2

------------------------------------------------------------

         twice square 3
      === {- def. |twice| -}
         square (square 3)
      === {- def. |square| -}
         (square 3) * (square 3)
      === {- def. |square| -}
         (3 * 3) * (square 3)
      === {- def. |(*)| -}
         9 * (square 3)
      === {- def. |square| -}
         9 * (3 * 3)
      === {- def. |(*)| -}
         9 * 9
      === {- def. |(*)| -}
         81

------------------------------------------------------------

         twice square 3
      === {- def. |twice| -}
         square (square 3)
      === {- def. |square| -}
         square (3 * 3)
      === {- def. |(*)| -}
         square 9
      === {- def. |square| -}
         (9 * 9)
      === {- def. |(*)| -}
         81

------------------------------------------------------------

         square (const 3 infinity)
      === {- def. |square| -}
         (const 3 infinity) * (const 3 infinity)
      === {- def. |const| -}
         3 * (const 3 infinity)
      === {- def. |const| -}
         3 * 3
      === {- def. |(*)| -}
         9

------------------------------------------------------------

         square (const 3 infinity)
      === {- def. |infinity| -}
         square (const 3 (1 + infinity))
      === {- def. |infinity| -}
         square (const 3 (1 + (1 + infinity)))
      === {- def. |infinity| -}
         square (const 3 (1 + (1 + (1 + infinity))))
      === {- ... -}
         undefined

------------------------------------------------------------

length' :: [a] -> Int
length' []      =  0
length' (x:xs)  =  1 + length' xs

------------------------------------------------------------

factorial' :: Int -> Int
factorial' n
  | n < 0 = undefined
  | n == 0 = 1
  | otherwise = n * factorial' (n-1)

------------------------------------------------------------

polygon :: Int -> String
polygon sides
  | sides == 3 = "Triangle"
  | sides == 4 = "Square"
  | sides == 5 = "Pentagon"
  | otherwise  = show sides ++ "-gon"

------------------------------------------------------------

era :: Int -> String
era n
  | n <  0    = "Future"
  | n == 0    = "Present"
  | n <= 66   = "Cenozoic"
  | n <= 252  = "Mesozoic"
  | n <= 541  = "Paleozoic"
  | n <= 1000 = "Neoproterozoic"
  | n <= 1600 = "Mesoproterozoic"
  | n <= 2500 = "Paleoproterozoic"

------------------------------------------------------------

orientation :: Int -> Int -> String
orientation w h
  | w == h = "Square"
  | w >  h = "Landscape"
  | w <  h = "Portrait"