module RunGame where

-- base
import Data.Char
import Data.Maybe (fromJust)

-- QuickCheck-2.14.2
import Test.QuickCheck

-- local
import Cards

-- | The interface to your implementation.
data Interface = Interface
  { iFullDeck :: Deck
  , iValue    :: Hand -> Int
  , iDisplay  :: Hand -> String
  , iGameOver :: Hand -> Bool
  , iWinner   :: Hand -> Hand -> Player
  , iDraw     :: Deck -> Hand -> (Deck, Hand)
  , iPlayBank :: Deck -> Hand
  , iShuffle  :: [Double] -> Deck -> Deck
  }
-- This is just a data type constructed using record syntax.
-- Record syntax allows us to give names to fields in a data type so that we can more easily
-- reference these fields later. See http://learnyouahaskell.com/making-our-own-types-and-typeclasses#record-syntax


-- | A type of players.
data Player = Guest | Bank
              deriving (Show, Eq)

-- | Runs a game given an implementation of the interface.
runGame :: Interface -> IO ()
runGame i = do
  putStrLn "Welcome to the game."
  -- Here, 'generate arbitrary' is generating the list of Doubles between 0 and 1, which is provided
  -- to your implementation of the shuffle function.
  Rand r <- generate arbitrary
  gameLoop i (iShuffle i r (iFullDeck i)) []

-- You may have noticed that the Interface itself is always given as an argument whenever we reference a
-- field by name, e.g. iShuffle.
-- This is because iShuffle is a actually a function iShuffle :: Interface -> [Double] -> Deck -> Deck.
-- When you provide it the Interface as input, it returns the value of the iShuffle field
-- (in this case, a function).

-- | Play until the guest player is bust or chooses to stop.
-- The return type of IO () means that this function produces some side effect
-- (here, printing to the console) which doesn't return anything.
gameLoop :: Interface -> Deck -> Hand -> IO ()
gameLoop i deck guest = do
  putStrLn $ "Your current score: " ++ displayHand i guest ++ "\n"
  if iGameOver i guest then finish i deck guest
   else do
    putStr (   "Draw "
            ++ (if null guest then "a " else "another ") -- null is a standard function that checks whether a list is empty.
            ++ "card? [y] ")
    yn <- getLine
    if null yn || (map toLower yn /= "n") then do
      let (deck', guest') = iDraw i deck guest
      gameLoop i deck' guest'
     else
      finish i deck guest

-- | Display the bank's final score and the winner.
finish :: Interface -> Deck -> Hand -> IO ()
finish i deck guest = do
  putStrLn $ "Your final score: " ++ displayHand i guest
  putStrLn $ "The bank's final score: " ++ displayHand i bank
  putStrLn $ "Winner: " ++ show (iWinner i guest bank)
  where
  bank = iPlayBank i deck

-- | A helper function for displaying a hand
displayHand :: Interface -> Hand -> String
displayHand i hand = show (iValue i hand) ++ if null hand then "" else " with cards: " ++ iDisplay i hand