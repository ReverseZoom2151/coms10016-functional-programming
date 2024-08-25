h [ ] = ...
h (a : x) = ... a ... x ...

h [ ] = ...
h (a : x) = ... a ... x ... h x ...

h x | ... x ... = [ ]
    | otherwise = b : y
    where b = ... x ...
          y = ... x ...

h x | ... x ... = [ ]
    | otherwise = b : y
    where b = ... x ...
          y = h (...x...)

insertSort :: [Integer] → [Integer]

insertSort [ ]          = [ ]
insertSort [3, 2, 1]    = [1, 2, 3]
insertSort [1, 2, 3]    = [1, 2, 3]
insertSort [12, 20, −5] = [−5, 12, 20]
insertSort [1, 1, 2]    = [1, 1, 2]

insertSort [ ]     = ...
insertSort (a : x) = ... a ... x ... insertSort x ...
insertSort [ ]     = [ ]

insert :: Integer → [Integer] → [Integer]
insert b [ ]               = [b]
insert b (a : x) | b 6 a = b : a : x
                 | b > a = a : insert b x

quickSort :: [Integer] → [Integer]
quickSort x | null x = [ ]
            | otherwise = quickSort y ++ [head x] ++ quickSort z
    where y = [b | b ← tail x, b 6 head x]
          z = [b | b ← tail x, b > head x]

safeDiv :: Integer → Integer → Maybe Integer

safeDiv 7 2 = Just 3
safeDiv 7 0 = Nothing

safeDiv x y | ... x ... y ... = Nothing
            | otherwise = Just (... x ... y...)

safeDiv x y | y == 0 = Nothing
            | otherwise = Just (x ‘div‘ y)

data Date = Date {day :: Day, month :: Month, year :: Year}

readDate :: String → Date
readDate s = Date {day = d, month = m, year = y}
    where d = ... s...
          m = ... s...
          y = ... s...

zip :: [α] → [β] → [(α, β)]
zip [ ] y = [ ]
zip (a : x) [ ] = [ ]
zip (a : x) (b : y) = (a, b) :zip x y

zip :: [α] → [β] → [(α, β)]
zip x y | null x ∨ null y = [ ]
        | otherwise = (head x, head y) :zip (tail x) (tail y)

selectSort :: [Integer] → [Integer]
selectSort x | null x = [ ]
             | otherwise = let a = minimum x in a :selectSort (x \\ [a])

quickSort :: [Integer] → [Integer]
quickSort = flatten · build

data NTree = Empty | Node NTree Integer NTree
build :: [Integer] → NTree
build x | null x = Empty
        | otherwise = Node (build y) (head x) (build z)
    where y = [b | b ← tail x, b 6 head x]
          z = [b | b ← tail x, b > head x]

flatten :: NTree → [Integer]
flatten Empty = [ ]
flatten (Node t a u) = flatten t ++ [a] ++ flatten u

mergeSort :: [Integer] → [Integer]
mergeSort = mergeAll ·splitUp

splitUp :: [Integer] → NTree
splitUp x | null x = Empty
          | otherwise = let (y,z) = halve (tail x) in
                        Node (splitUp y) (head x) (splitUp z)

halve :: [α ] → ([α ], [α ])
halve [ ]         = ([ ], [ ])
halve [a]         = ([a], [ ])
halve (a : b : x) = let (y,z) = halve x in (a : y, b :z)

mergeAll :: NTree → [Integer]
mergeAll Empty = [ ]
mergeAll (Node t a u) = merge (mergeAll t) (merge [a] (mergeAll u))
merge :: [Integer] → [Integer] → [Integer]
merge [ ] y = y
merge x [ ] = x
merge (a : x) (b : y) = if a 6 b then a : merge x (b : y) else b : merge (a : x) y

data BTree = Tip Integer | Bin BTree BTree

mergeSort2 :: [Integer] → [Integer]
mergeSort2x = mergeAll2(splitUp2x)
splitUp2 :: [Integer] → Maybe BTree
splitUp2x | null x = Nothing
          | single x = Just (Tip (head x))
          | otherwise = let (y,z) = halve x 
                            (Just t, Just u) = (splitUp2y,splitUp2z)
                        in Just (Bin t u)

single :: [α ] → Bool
single [a] = True
single x = False

mergeAll2 :: Maybe BTree → [Integer]
mergeAll2 Nothing = [ ]
mergeAll2 (Just (Tip a)) = [a]
mergeAll2 (Just (Bin t u)) = merge (mergeAll2 (Just t)) (mergeAll2 (Just u))

moveUntilOut ball | outOfBounds ball = ball
                  | otherwise = moveUntilOut (moveBall ball)

bundle 3 "abcdefg" = ["abc", "def", "g"]

bundle :: Int → [α ] → [[α ]]
bundle n x | null x = [ ]
           | otherwise = take n x : bundle n (drop n x)

roman :: Integer → String
roman n = consume (labelled n)
    where consume [ ] = ""
          consume ((d, p) : x) = digit d p ++ consume x
          labelled n = ... 
          digit d p = ... 

roman n = concat (produce n letters)
    where produce n x | null x = [ ]
                      | n > v = r : produce (n − v) x
                      | otherwise = produce n (tail x)
            where (v,r) = head x
        letters = [(1000, "M"), (900, "CM"), (500, "D"), ...]

perms:: [α] → [[α]]
perms[ ] = [[ ]]
perms(a : x) = [y ++ [a] ++ z| x' ← perms x, (y,z) ← splits x']
    where splits[ ] = [([ ], [ ])]
        splits(a : x) = (a:x, [ ]) : [(y, a:z) | (y,z) ← splits x]

perms:: [α ] → [[α ]]
perms x | null x = [[ ]]
        | otherwise = [a :z| (a, y) ← pick x,z ← perms y]
    where pick [ ] = [ ]
          pick (a : x) = (a, x) : [(b, a : y) | (b, y) ← pick x]