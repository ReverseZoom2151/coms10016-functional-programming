import Test.QuickCheck
import Text.Show.Functions
import Data.Char

-- (') is added to the function name to avoid name conflict with
-- the functions already defined in Prelude

sum' :: [Int] -> Int
sum' = foldr (+) 0

product' :: [Int] -> Int
product' = foldr (*) 1

and' :: [Bool] -> Bool
and' = foldr (&&) True

concat' :: [[a]] -> [a]
concat'  = foldr (++) []

maximum' :: Ord a => [a] -> a
maximum' (x:xs) = foldr max x xs

unlines' :: [String] -> String
unlines' = 
  foldr (\xs ys -> xs ++ "\n" ++ ys) []

takeLine :: String -> String
takeLine = takeWhile (/='\n')

prop_TW_DW :: (Int->Bool)->[Int]->Bool
prop_TW_DW p xs =
  takeWhile p xs ++ dropWhile p xs == xs

segments :: (a->Bool) -> [a] -> [[a]]
segments p [] = []
segments p xs =
   takeWhile p xs :
   segments p (drop 1 (dropWhile p xs))

lines' :: String -> [String]
lines' = segments (/='\n')

commaSep::String -> [String]
commaSep = segments (/=',')

words' :: String -> [String]
words' xs = 
  filter (/="")
     (segments (not.isSpace) xs)
