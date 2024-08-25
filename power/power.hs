power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

----------------------------------------------------------------

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = undefined

----------------------------------------------------------------

power1 :: Integer -> Integer -> Integer
power1 = undefined

----------------------------------------------------------------

power2 :: Integer -> Integer -> Integer
power2 = undefined

----------------------------------------------------------------

comparePower1 :: Integer -> Integer -> Bool
comparePower1 = undefined

----------------------------------------------------------------

comparePower2 :: Integer -> Integer -> Bool
comparePower2 = undefined

----------------------------------------------------------------

table :: Integer -> Integer -> IO ()
table = undefined