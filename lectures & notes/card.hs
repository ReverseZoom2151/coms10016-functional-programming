import Test.QuickCheck

-------------------------------------------------------------------------

data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Eq, Show)

----------------------------------------------------------------

data Colour = Black | Red
  deriving (Eq, Show)

colour :: Suit -> Colour
colour Spades   = Black
colour Hearts   = Red
colour Diamonds = Red
colour Clubs    = Black

-------------------------------------------------------------------------

data Rank = Numeric Integer | Jack | Queen | King | Ace
  deriving (Eq, Show)

rankBeats :: Rank -> Rank -> Bool
rankBeats _           Ace         = False
rankBeats Ace         _           = True
rankBeats _           King        = False
rankBeats King        _           = True
rankBeats _           Queen       = False
rankBeats Queen       _           = True
rankBeats _           Jack        = False
rankBeats Jack        _           = True
rankBeats (Numeric m) (Numeric n) = m > n

prop_RankBeats a b =
   rankBeats a b || rankBeats b a

prop_SameRank a = not (rankBeats a a)

-------------------------------------------------------------------------

data Card = Card Rank Suit
  deriving (Eq, Show)

rank :: Card -> Rank
rank (Card r s) = r

suit :: Card -> Suit
suit (Card r s) = s

cardBeats :: Card -> Card -> Bool
cardBeats c c' = suit c == suit c' && rankBeats (rank c) (rank c')

-------------------------------------------------------------------------

data Hand = Empty | Add Card Hand
  deriving (Eq, Show)

handBeats :: Hand -> Card -> Bool
handBeats Empty     c' = False
handBeats (Add c h) c' = cardBeats c c' || handBeats h c'

chooseCard :: Card -> Hand -> Card
chooseCard c (Add c' Empty) = c'
chooseCard c (Add c' h)
   | cardBeats c' c = c'
   | otherwise      = chooseCard c h
   
haveCard :: Card -> Hand -> Bool
haveCard c Empty = False
haveCard c (Add c' h) = c == c' || haveCard c h

prop_chooseCardWinsIfPossible c h =
   h /= Empty ==> handBeats h c == 
                     cardBeats (chooseCard c h) c

-------------------------------------------------------------------------

instance Arbitrary Suit where
  arbitrary = suitGen

suitGen = oneof [return Spades,
                 return Hearts,
                 return Diamonds,
                 return Clubs]

instance Arbitrary Rank where
  arbitrary = rankGen
  shrink Ace = [King]
  shrink King = [Queen]
  shrink Queen = [Jack]
  shrink Jack = [Numeric 10]
  shrink (Numeric n) = [Numeric n' | n' <- shrink n, 2<=n']

rankGen = oneof [return Jack,
                 return Queen,
                 return King,
                 return Ace,
                 do r <- choose (2,10)
                    return (Numeric r)]

instance Arbitrary Card where
  arbitrary = cardGen
  shrink (Card r s) =
    [Card r' s | r' <- shrink r]

cardGen = 
    do r <- arbitrary
       s <- arbitrary
       return (Card r s)

instance Arbitrary Hand where
  arbitrary = handGen
  shrink (Add c h) =
    h : [Add c' h' | (c',h') <- shrink (c,h)]
  shrink Empty = []

handGen = oneof
         [return Empty,
          do c <- cardGen
             h <- handGen
             return (Add c h)]

prop_rank r = collect r (validRank r)

validRank :: Rank -> Bool
validRank (Numeric r) = 2<=r && r<=10
validRank         _         = True