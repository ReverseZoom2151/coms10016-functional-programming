{-# LANGUAGE InstanceSigs #-}
module Functors where

-- This is the final code we ended up with in the lecture.
-- If you have not yet seen the lecture, it is highly
-- recommended that you first watch it, adding
-- to your own blank file as you go, at least once 
-- *before* looking at this one.

import Prelude hiding (Functor(..))

-- Goal: Apply functions into a context. 
--       More concretely, this often means applying a
--       function to every element of a container

mapList :: (a -> b) -> [a] -> [b]
mapList f []     = []
mapList f (x:xs) = (x':xs')
  where
    x' = f x
    xs' = mapList f xs

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing  = Nothing
mapMaybe f (Just x) = Just (f x)

data Tree a = Empty | Fork a (Tree a) (Tree a)
  deriving Show

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty = Empty
mapTree f (Fork x oak elm) = Fork (f x) (mapTree f elm) (mapTree f oak)

mapList'  :: (a -> b) -> []    a -> []    b
mapMaybe' :: (a -> b) -> Maybe a -> Maybe b
mapTree'  :: (a -> b) -> Tree  a -> Tree  b
mapList'  = mapList
mapMaybe' = mapMaybe
mapTree'  = mapTree

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap = mapMaybe

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap = mapTree

instance Functor [] where 
  fmap = map

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

-- Functor laws:
-- 1. Identity: fmap id == id
-- >>> fmap id [1,2,3] == id [1,2,3]
-- True

-- 2. Composition: fmap (f . g) == (fmap f . fmap g)
-- >>> fmap ((*2) . (+1)) [1,2,3] == (fmap (*2) . fmap (+1)) [1,2,3]
-- True

-- Additional exercises to test your understanding

-- 1. Test whether all of our Functor instances are lawful

-- 2. Write a functor instance for Either.
-- Be careful, there's a subtlety involved here

-- instance Functor Either where  -- Invalid. Why? Hint: Try expanding the fmap type
                                  -- when `f = Either` vs. `f = Either c`
instance Functor (Either c) where -- Valid. 
  fmap = error "Implement me to test your understanding"