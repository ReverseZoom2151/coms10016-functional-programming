module Sudoku where
import System.Environment
import Data.List

type Cell = [Char]
data Sudoku = Sudoku [Cell]

bundle :: Int -> [a] -> [[a]]
bundle = undefined

stuck :: Sudoku -> Bool
stuck (Sudoku cells) = undefined

fixed :: Sudoku -> Bool
fixed (Sudoku cells) = undefined

rows :: Sudoku -> [[Cell]]
rows (Sudoku cells) = undefined

cols :: Sudoku -> [[Cell]]
cols (Sudoku cells) = undefined

boxs :: Sudoku -> [[Cell]]
boxs (Sudoku cells) = undefined

broken :: String
broken = (concat . concat . replicate 3)
  [ concat (replicate 3 ['1' .. '3'])
  , concat (replicate 3 ['4' .. '6'])
  , concat (replicate 3 ['7' .. '9']) ]

solved :: String
solved = concat
    [ simpleRow 1
    , simpleRow 4
    , simpleRow 7
    , simpleRow 2
    , simpleRow 5
    , simpleRow 8
    , simpleRow 3
    , simpleRow 6
    , simpleRow 9 ]
  where
    simpleRow n = take 9 (drop (n-1) (cycle ['1' .. '9']))

easy :: String
easy = " 4   2 19   351 8631  947   94     7         2     89   952  4142 169   16 8   7 "

hard:: String
hard = "8          36      7  9 2   5   7       457     1   3   1    68  85   1  9    4  "

lone :: [a] -> Bool
lone [x] = True
lone xs  = False

solve :: Sudoku -> [Sudoku]
solve puzzle
  | stuck puzzle       = []
  | not (valid puzzle) = []
  | fixed puzzle       = [puzzle]
  | otherwise          = [puzzle'' | puzzle'  <- expand puzzle
                                   , puzzle'' <- solve  puzzle']

unique :: Eq a => [a] -> Bool
unique xs = nub xs == xs

valid :: Sudoku -> Bool
valid puzzle = all (unique . filter lone) (rows puzzle)
            && all (unique . filter lone) (cols puzzle)
            && all (unique . filter lone) (boxs puzzle)

expand :: Sudoku -> [Sudoku]
expand (Sudoku cells) = [ Sudoku (ls ++ [choice] : rs) | choice <- choices ]
  where
    (ls, choices:rs) = break (not . lone) cells

main :: IO ()
main = do
  args <- getArgs
  puzzle <- case args of
                 [fileName] -> readFile fileName
                 []         -> return hard
  print (solve (sudoku puzzle))

sudoku :: String -> Sudoku
sudoku xs
  | length ys == 81 = Sudoku (map convert ys)
  | otherwise       = error ("Invalid sudoku puzzle: " ++ show xs)
  where convert x = if blank x then ['1'..'9'] else [x]
        newline c = c == '\n'  || c == '\r'
        blank c = c == ' ' || c == '.'
        ys = filter (not . newline) xs

instance Show Sudoku where
  show (Sudoku xs) = "\n" ++
      top ++
      intercalate mid
        [intercalate sep
          [row (xss !! (i + (j*3))) | i <- [0 .. 2] ] | j <- [0 .. 2]] ++
      bot
    where
     row :: [String] -> String
     top    = '╔' : intercalate "╦" (replicate 3 (intercalate "╤" (replicate 3 "═══"))) ++ "╗\n"
     row rs = '║' : intercalate "║" (map (intercalate "│") (bundle 3 rs)) ++ "║\n"
     mid    = '╠' : intercalate "╬" (replicate 3 (intercalate "╪" (replicate 3 "═══"))) ++ "╣\n"
     sep    = '╟' : intercalate "╫" (replicate 3 (intercalate "┼" (replicate 3 "───"))) ++ "╢\n"
     bot    = '╚' : intercalate "╩" (replicate 3 (intercalate "╧" (replicate 3 "═══"))) ++ "╝\n"
     xss = bundle 9 ys
     ys  = map summary xs
     summary [x] = " " ++ [x] ++ " "
     summary _   = "   "