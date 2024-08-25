aCard1 :: Card
aCard1 = Card (Numeric 7) Hearts

aCard2 :: Card
aCard2 = Card Ace Spades

aHand :: Hand
aHand = [aCard1, aCard2]

prop_aHand :: Bool
prop_aHand = elem aCard1 aHand && elem aCard2 aHand

size :: Num a => Hand -> a
size [] = 0
size (card : hand) = 1 + size hand

display :: Hand -> String
display hand = unlines $ map displayCard hand
  where
    displayCard (Card rank suit) = show rank ++ " of " ++ show suit

display :: Hand -> String
display hand = unlines (map displayCard hand)

displayCard :: Card -> String
displayCard (Card (Numeric n) suit) = show n ++ " of " ++ show suit
displayCard (Card Jack suit) = "Jack of " ++ show suit
displayCard (Card Queen suit) = "Queen of " ++ show suit
displayCard (Card King suit) = "King of " ++ show suit
displayCard (Card Ace suit) = "Ace of " ++ show suit

value :: Hand -> Int
value hand = max (sum $ map valueRank hand) (sum $ map valueRank hand - 10 * numberOfAces hand)
  where
    valueRank (Numeric n) = n
    valueRank _ = 10
    numberOfAces hand = length $ filter (\(Card rank _) -> rank == Ace) hand

gameOver :: Hand -> Bool
gameOver hand = value hand > 21

winner :: Hand -> Hand -> Player
winner guestHand bankHand
  | gameOver guestHand && gameOver bankHand = Bank
  | gameOver guestHand = Bank
  | gameOver bankHand = Guest
  | value guestHand > value bankHand = Guest
  | otherwise = Bank

fullDeck :: Deck
fullDeck = [Card rank suit | rank <- allRanks, suit <- allSuits]
  where
    allRanks = [Numeric n | n <- [1..10]] ++ [Jack, Queen, King, Ace]
    allSuits = [Hearts, Spades, Diamonds, Clubs]

draw :: Deck -> Hand -> (Deck, Hand)
draw [] _ = error "draw: The deck is empty."
draw (card:deck) hand = (deck, card:hand)

playBank :: Deck -> Hand
playBank deck = playBank' deck []
  where
    playBank' deck hand
      | value hand >= 16 = hand
      | otherwise = let (deck', hand') = draw deck hand in playBank' deck' hand'

prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
  card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck =
  size deck == size (shuffle randomlist deck)

shuffle :: [Double] -> Deck -> Deck
shuffle _ [] = []
shuffle (r:rs) deck = card : shuffle rs (removeCard card deck)
  where
    card = deck !! floor (r * fromIntegral (length deck))
    removeCard card deck = [c | c <- deck, c /= card]

implementation = Interface
  { iFullDeck = fullDeck
  , iValue = value
  , iDisplay = display
  , iGameOver = gameOver
  , iWinner = winner
  , iDraw = draw
  , iPlayBank = playBank
  , iShuffle = shuffle
  }

main :: IO ()
main = runGame implementation
