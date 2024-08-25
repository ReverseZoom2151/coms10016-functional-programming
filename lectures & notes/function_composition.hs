(.) :: (b -> c) -> (a -> b) -> (a -> c)

----------------------------------------

productionLine :: Int -> Int
productionLine = (+7) . (*2)