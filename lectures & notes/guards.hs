compare :: Eq a => a -> a -> String
compare x y
| x == y = "They are the same!"
| otherwise = "Different"