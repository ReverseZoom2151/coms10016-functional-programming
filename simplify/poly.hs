-- | Abstract type of polynomials (over Int).
module Poly ( Poly     -- The type of polymomials
                       -- Instances: Eq, Show, Arbitrary, Num
            , evalPoly -- :: Int -> Poly -> Int
                       -- Convert to and from lists of Int
            , toList   -- :: Poly -> [Int]
            , fromList -- :: [Int] -> Poly
            )
where

-- QuickCheck-2.14.2
import Test.QuickCheck

-- | A type representing polynomials.
data Poly = Poly [Int] -- List of polynomial's coefficients, least significant powers first
  deriving Eq

-- | Show polynomials with Unicode characters.
instance Show Poly where
  show (Poly ns) =
      showParts . reverse . filter (\(n,_) -> n /= 0) $ zip ns powers
    where showParts []           = show 0
          showParts ((n,p):rest) = showNum n p ++ concatMap showRest rest

          showNum n ""   = show n -- Show the constant term.
          showNum (-1) p = "-" ++ p -- If a coefficient is -1, do not display the 1; e.g. -x instead of -1x.
          showNum 1 p    = p -- If a coefficient is 1, do not display it.
          showNum n p    = show n ++ p

          showRest (n,p) | n < 0     = " - " ++ showNum (negate n) p
                         | otherwise = " + " ++ showNum n          p

          -- Map the powers of x to Unicode characters. Note that only powers up to 9 have corresponding
          -- Unicode characters.
          powers =
            prettyOnes ++ map (\n -> "x^" ++ show n ) [length prettyOnes ..]
              where
                prettyOnes = ["", "x"]
                          ++ ["x\178", "x\179"]
                          ++ map (\c -> 'x':c:[]) ['\8308'..'\8313']

-- | Allow QuickCheck to generate random polynomials.
instance Arbitrary Poly where
  arbitrary = do
            l <- choose (1,9) -- Stick the nice looking powers
            ns <- vectorOf l arbitrary
            return $ fromList ns

instance Num Poly where
  (+)    = addPoly
  (*)    = mulPoly
  negate = fromList . map negate . toList
  abs    = undefined
  signum = undefined
  fromInteger n = fromList [fromInteger n]

-- | Convert a polynomial to a list representation.
-- @x² + 2x + 3@ would be represented by @[1,2,3]@.
toList :: Poly -> [Int]
toList (Poly ns) = reverse ns

-- | Convert a list to a polynomial.
fromList :: [Int] -> Poly
fromList = poly . reverse
-- reverse is required because the Poly constructor needs the coefficients ordered least significant power first.

-- | Remove zeros from little-endian list (list of coefficients ordered least significant power first) if they occur at the end of
-- the list.
poly :: [Int] -> Poly
poly = Poly . reverse . strip . reverse

strip :: [Int] -> [Int]
strip = dropWhile (== 0)

-- | Evaluate a polynomial, given a value for x.
evalPoly :: Int -> Poly -> Int
evalPoly x (Poly cs) = sum $ zipWith (*) cs powersOfx
         where powersOfx = map (x^) [0..]

-- | Addition for polynomials.
addPoly :: Poly -> Poly -> Poly
addPoly (Poly p1) (Poly p2) =
   poly $ zipWith (+) (pad l1 p1) (pad l2 p2) -- Add the corresponding coefficients of the two polynomials.
     where pad lp p = p ++ replicate (maxlen - lp) 0 -- Make both lists the same length by right-padding the shorter one with zeroes.
           (l1, l2, maxlen) = (length p1, length p2, max l1 l2) -- Find out the highest power of x in the two polynomials.


-- | Multiplication for polynomials.
mulPoly :: Poly -> Poly -> Poly
mulPoly (Poly p1) (Poly p2) = poly $ mul p1 p2
  where
      -- Multiply each term of the first polynomial with the second polynomial, and add the results together.
      mul [] p         = []
      mul (n:ns) p     =  (n `times` p) `plus` timesX (ns `mul` p)
      timesX  p = 0:p
      times n p = map (n*) p
      plus p1 p2 = reverse . toList $ Poly p1 + Poly p2

-- Tests -----------------------------------------------------------------------
-- Check that eval is a homomorphism (and maintains the invariant).
-- You are not expected to understand this!
prop_PolyOps :: Poly -> Poly -> Int -> Bool
prop_PolyOps p1 p2 x = evalHom (*) mulPoly && evalHom (+) addPoly
   where evalHom f g = let p' = p1 `g` p2  in
                           evalPoly x p1 `f` evalPoly x p2 == evalPoly x p'
                           && prop_Poly p'

prop_Poly :: Poly -> Bool
prop_Poly p = fromList (toList p) == p

-- Examples of polynomials
ex1 :: Poly
ex1 = fromList [1,2,3] -- x^2 + 2x + 3

x :: Poly
x = fromList [1,0]

xPlus1 :: Poly
xPlus1 = fromList [1,1]
-----