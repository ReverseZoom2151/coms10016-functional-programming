square :: Int -> Int
square x = x * x

----------------------------------------------------------------

two :: Int -> Int
two x = 2

----------------------------------------------------------------

squareTwo :: Int -> Int
squareTwo x = square (two x)

-- Alternatively

squareTwo' :: Int -> Int
squareTwo' x = (square . two) x

----------------------------------------------------------------

twoSquare :: Int -> Int
twoSquare x = two (square x)

-- Alternatively

twoSquare' :: Int -> Int
twoSquare' x = (two . square) x