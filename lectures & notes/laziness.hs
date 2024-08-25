student :: Question -> Solution
lecturer :: Int -> [Question] -> [Solution]
lecturer n xs = take n (map student xs)