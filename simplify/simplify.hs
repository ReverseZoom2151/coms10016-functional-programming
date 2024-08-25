-- Code for the Simplify practical.
-- Write all your code in this file.
module Simplify where

-- QuickCheck-2.14.2
import           Test.QuickCheck

-- local
import           Poly

-- Use the following simple data type for binary operators.
data BinOp = AddOp | MulOp

--------------------------------------------------------------------------------
-- * Task 1
-- Define a data type Expr which represents three kinds of expression:
-- binary operators (use BinOp as a helper type) applied to two expressions,
-- numbers (use Int), and exponentiation x^n.
-- Note that since we consider expressions containing just a single variable, x,
-- your data type should not use String or Char anywhere.

data Expr = Expr -- change this!

--------------------------------------------------------------------------------
-- * Task 2
-- Define the data type invariant which checks that exponents are never negative.

prop_Expr :: Expr -> Bool
prop_Expr = undefined

--------------------------------------------------------------------------------
-- * Task 3
-- Make Expr an instance of Show.
-- You can use Haskell notation for powers, e.g. x^2.
-- You should show x^1 as just x.

instance Show Expr where
  show = undefined

--------------------------------------------------------------------------------
-- * Task 4
-- Make Expr an instance of Arbitrary.
-- Now you can check the data type invariant that you defined in Task 2 using
-- QuickCheck.

instance Arbitrary Expr where
  arbitrary = undefined

--------------------------------------------------------------------------------
-- * Task 5
-- Define the eval function that, given a value for x and an expression,
-- evaluates the expression.

eval :: Int -> Expr -> Int
eval = undefined

--------------------------------------------------------------------------------
-- * Task 6
-- Define prop_exprToPoly, which checks that evaluating the
-- polynomial you get from exprToPoly gives the same answer as evaluating
-- the expression

prop_exprToPoly :: Int -> Expr -> Bool
prop_exprToPoly = undefined

--------------------------------------------------------------------------------
-- * Task 7
-- Define exprToPoly, which converts an expression into a polynomial.

exprToPoly :: Expr -> Poly
exprToPoly = undefined

--------------------------------------------------------------------------------
-- * Task 8
-- Write a QuickCheck property for the polyToExpr function, similar to
-- Task 6.

prop_polyToExpr :: Int -> Poly -> Bool
prop_polyToExpr = undefined

--------------------------------------------------------------------------------
-- * Task 9
-- Define polyToExpr, which converts a polynomial into an expression.

polyToExpr :: Poly -> Expr
polyToExpr = undefined

--------------------------------------------------------------------------------
-- * Task 10
-- Define a function which simplifies an expression by converting it to a
-- polynomial and back again.

simplify :: Expr -> Expr
simplify = undefined

--------------------------------------------------------------------------------
-- * Task 11
-- Write a QuickCheck property which checks that a simplified expression does not
-- contain any "junk", where junk is defined to be: multiplication by one or zero,
-- addition of zero, addition or multiplication of numbers, or x to the
-- power zero.

prop_noJunk :: Expr -> Bool
prop_noJunk = undefined

--------------------------------------------------------------------------------