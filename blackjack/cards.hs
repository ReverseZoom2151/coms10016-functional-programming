-- These are language extensions. They enable non-standard features supported by the compiler.
-- For example, they can introduce new language constructs or make it easier to work with existing features.
-- You are not expected to understand them, but you can visit https://wiki.haskell.org/Language_extensions to
-- learn more if you are curious.
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cards where

-- QuickCheck-2.14.2
import Test.QuickCheck
        ( choose,
        elements,
        frequency,
        Arbitrary(arbitrary),
        InfiniteList(getInfiniteList) )

-- | A card has a rank and belongs to a suit
data Card = Card Rank Suit
            deriving (Eq, Show)

-- | Extract the rank of a card
rank :: Card -> Rank
rank (Card r _) = r

-- | Extract the suit of a card
suit :: Card -> Suit
suit (Card _ s) = s

-- | All the different suits.
data Suit = Hearts | Spades | Diamonds | Clubs
            deriving (Eq, Show)

-- | A rank is either a numeric card, a face card, or an ace. The
-- numeric cards range from two to ten.
data Rank = Numeric Int | Jack | Queen | King | Ace
            deriving (Eq, Show)

-- | A hand of cards is just a lists of Cards
type Hand = [Card]

-- | A deck of cards is just a lists of Cards
type Deck = [Card]

-- | The size of a hand.
-- Note that we could have used the function length.
size :: Num a => Hand -> a
size []           = 0
size (card:hand)  = 1 + size hand

---------------------------------------------------
-- The rest of this file is for QuickCheck.
-- You are not required to understand it (yet).

-- For generating an "arbitrary" (random suit)
-- e.g.
-- >>> generate (arbitrary) :: IO Suit
-- Spades
-- Don't worry about the IO type or use of generate, they are for mysterious Monad
-- reasons that we'll avoid talking about for now
instance Arbitrary Suit where
  arbitrary = elements [ Hearts, Spades, Diamonds, Clubs ]

-- Similar to the Suit Arbitrary instance, generates arbitrary cards
-- e.g.
-- >>> generate (arbitrary) :: IO Card
-- Card (Numeric 10) Hearts
instance Arbitrary Card where
  arbitrary = do
    suit <- arbitrary
    rank <- arbitrary
    return (Card rank suit)

instance Arbitrary Rank where
  arbitrary = frequency [ (4, elements [Jack,Queen,King,Ace])
                        , (9, do n <- choose (2, 10)
                                 return (Numeric n))
                        ]

-- A generator of infinite lists of numbers from 0.0 to 1.0
-- Consider type as:
-- data Rand = Rand [Double]
-- (Thanks to Nick Smallbone for the code.)
-- You will not be expected to understand this at any point...
--------------------

newtype ZeroOne = ZeroOne {getZeroOne :: Double}
instance Show ZeroOne where show = show . getZeroOne
instance Arbitrary ZeroOne
  where arbitrary = ZeroOne `fmap` choose (0.0,1.0)

newtype Rand = MkRand {getRand :: InfiniteList ZeroOne}
  deriving Arbitrary
instance Show Rand where show = show . getRand

pattern Rand xs <- (map getZeroOne . getInfiniteList . getRand -> xs)