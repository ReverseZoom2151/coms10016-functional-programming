{-# LANGUAGE InstanceSigs #-}
module Applicatives where

-- This is the final code we ended up with in the lecture.
-- If you have not yet seen the lecture, it is highly
-- recommended that you first watch it, adding
-- to your own blank file as you go, at least once 
-- *before* looking at this one.

import Prelude hiding (Functor(..), (<$>), Applicative(..))
import Functors -- This imports our local Functors.hs file
                -- Just for illustration. In practice, we would
                -- use the predefined classes in the Prelude

-- Goal: Apply functions within a context,
--       creating structure based on structure,
--       which will allow us to combine results

class Functor f => Applicative f where
--fmap  ::   (a -> b) -> f a -> f b
  (<*>) :: f (a -> b) -> f a -> f b
  pure  :: a -> f a

instance Applicative Maybe where
  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Just f  <*> Just x  = Just (f x)
  Nothing <*> Just x  = Nothing
  Just f  <*> Nothing = Nothing
  Nothing <*> Nothing = Nothing

  pure :: a -> Maybe a
  pure = Just

-- >>> Just (+1) <*> Just 2
-- Just 3

instance Applicative [] where
  (<*>) :: [a -> b] -> [a] -> [b]
  [] <*> xs = []
  fs <*> [] = []
  (f:fs) <*> xs = map f xs ++ (fs <*> xs) -- Cartesian product
  -- (f:fs) <*> (x:xs) = f x : (fs <*> xs)  -- Zip

  pure x = [x]

-- >>> [(+1), (*3)] <*> [-2,-1,0,1,2]
-- [-1,-3]
-- >>> [] <*> [-2,-1,0,1,2]
-- []

instance Applicative (Either e) where
  (<*>) :: Either e (a -> b) -> Either e a -> Either e b
  Right f <*> Right x = Right (f x)
  _       <*> Left y  = Left y
  Left x  <*> _       = Left x

  pure = Right

-- >>> pure (+1) <*> [1,2,3]
-- [2,3,4]

addOne :: Applicative f => f Int -> f Int
-- addOne mx = pure (+1) <*> mx
addOne mx = fmap (+1) mx

fmapAp :: Applicative f => (a -> b) -> f a -> f b
fmapAp f mx = pure f <*> mx

data User = MkUser String Int
  deriving Show

createUser :: Maybe String -> Maybe Int -> Maybe User
createUser maybeName maybeAge
  -- = pure MkUser <*> maybeName <*> maybeAge
  =  MkUser
 <$> maybeName
 <*> maybeAge

-- >>> createUser (Just "Haskell") (Just 31)
-- Just (MkUser "Haskell" 31)
-- >>> createUser (Just "Keano") Nothing
-- Nothing

-- Additional exercises to test your understanding

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f mx my = error "Implement me!"

-- What does this do for Maybe? What does it do for list?
doTwice :: Applicative f => f a -> f (a,a)
doTwice mx = error "Implement me!"

sequenceAp :: Applicative f => [f a] -> f [a]
sequenceAp = error "Implement me!"

-- >>> sequenceAp [Just 3, Just 2, Just 1]
-- Just [3,2,1]
-- >>> sequenceAp [Just 3, Nothing, Just 1]
-- Nothing

mapAp :: Applicative f => (a -> f b) -> [a] -> f [b]
mapAp f xs = error "Implement me!"

-- >>> mapAp justOdd [1,2,3]
-- Nothing
-- >>> mapAp justOdd [1,3]
-- Just [1,3]

justOdd :: Int -> Maybe Int
justOdd x = if odd x then Just x else Nothing