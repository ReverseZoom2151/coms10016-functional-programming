echo :: IO ()
echo = do  line <- getLine
           putStrLn line

sayHello :: String -> String
sayHello name = "Hello, " ++ name ++ "!"

greet :: IO ()
greet = do  line <- getLine
            putStrLn (sayHello line)

greet' :: IO ()
greet' = do  putStr "What is your name? "
             greet

getInt :: IO Int
getInt = do  line <- getLine
             return (read line)

echoArgs :: IO ()
echoArgs = do  args <- getArgs
               print args

echoArgs' :: IO ()
echoArgs' = do  args <- getArgs
                putStrLn (unwords args)

addLineStart :: [String] -> [String]
addLineStart = map ("> " ++)

addLineStartToFile :: String -> String
addLineStartToFile = unlines . addLineStart . lines

hsToLhs :: String -> String -> IO ()
hsToLhs hs lhs = do  contents <- readFile hs
                     writeFile lhs (addLineStartToFile contents)

main :: IO ()
main = do  [hs, lhs] <- getArgs
           hsToLhs hs lhs

playRound :: Int -> IO ()
playRound number =
  do  putStr "Enter a guess: "
      guess <- getInt
      if       guess == number  then  putStrLn "You got it right!"
      else if  guess < number   then  do  putStrLn "Too low!";   playRound number
      else                            do  putStrLn "Too high!";  playRound number

dice :: Int -> IO Int
dice n =
  do  i <- randomIO
      return (mod i n)
      
game :: Int -> IO ()
game difficulty =
  do  number <- dice difficulty
      playRound number