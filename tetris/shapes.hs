-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where

-- base
import Data.List  (transpose)
import Data.Maybe (isNothing)

-- QuickCheck
import Test.QuickCheck

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

data Shape = S [Row] deriving (Eq)
type Row = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]

    showSquare Nothing = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey)  = 'g' -- can change to '▓'
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss)++r

-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of connected 4 blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes]
   where
      makeSquares = map (map colour)
      colour c    = lookup c [('I',Red),('J',Grey),('T',Blue),('O',Yellow),
                              ('Z',Cyan),('L',Green),('S',Purple)]
      shapes =
              [["I",
               "I",
               "I",
               "I"],
              [" J",
               " J",
               "JJ"],
              [" T",
               "TT",
               " T"],
              ["OO",
               "OO"],
              [" Z",
               "ZZ",
               "Z "],
              ["LL",
               " L",
               " L"],
              ["S ",
               "SS",
               " S"]]

-- * Some simple functions

-- ** TASK A01
emptyShape :: (Int,Int) -> Shape
emptyShape = error "A01 emptyShape undefined"

-- ** TASK A02

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int,Int)
shapeSize = error "A02 shapeSize undefined"

-- ** TASK A03

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount = error "A03 blockCount undefined"

-- * The Shape invariant

-- ** TASK A04
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape = error "A04 prop_Shape undefined"

-- * Test data generators

-- ** TASK A05
-- | A random generator for colours
rColour :: Gen Colour
rColour = error "A05 rColour undefined"

instance Arbitrary Colour where
  arbitrary = rColour

-- ** TASK A06
-- | A random generator for shapes
rShape :: Gen Shape
rShape = error "A06 rShape undefined"

instance Arbitrary Shape where
  arbitrary = rShape

-- * Transforming shapes

-- ** TASK A07
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape = error "A07 rotateShape undefined"

-- ** TASK A08
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape = error "A08 shiftShape undefined"

-- ** TASK A09
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int,Int) -> Shape -> Shape
padShape = error "A09 padShape undefined"

-- ** TASK A10
-- | pad a shape to a given size
padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo = error "A10 padShapeTo undefined"

-- * Comparing and combining shapes

-- ** TASK B01

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = error "B01 overlaps undefined"

-- ** TASK B02
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
zipShapeWith = error "B02 zipShapeWith undefined"

-- ** TASK B03
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = error "B03 zipShapeWith undefined"
