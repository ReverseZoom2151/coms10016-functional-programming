{-# language InstanceSigs #-}
module Monads where

import Prelude hiding (Maybe(..), Either(..))
import Control.Applicative

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing  = Nothing

instance Applicative Maybe where
  pure = Just
  Just f <*> Just x = Just (f x)
  _      <*> _      = Nothing

instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (Just x) >>= f = f x
  Nothing  >>= f = Nothing

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe mx my
  -- = mx >>= (\x ->
  --   my >>= (\y ->
  --   pure (x + y)))
  = do
    x <- mx
    y <- my
    pure (x + y)
    
-- >>> addMaybe (Just 8) (Just 9)
-- Just 17

justOdd :: Int -> Maybe Int
justOdd x
  | odd x     = Just x
  | otherwise = Nothing

addMaybeIfOdd :: Maybe Int -> Maybe Int -> Maybe Int
addMaybeIfOdd mx my -- = do
  -- x <- mx
  -- y <- my
  -- justOdd (x + y)
  = mx >>= (\x ->
    my >>= (\y ->
    justOdd (x + y)))

data Either a b = Left a | Right b
  deriving Show

instance Functor (Either e) where
  fmap f (Right x) = Right (f x)
  fmap f (Left e)  = Left e

instance Applicative (Either e) where
  pure = Right
  Right f <*> Right x = Right (f x)
  Left e  <*> _       = Left e
  _       <*> Left e  = Left e

instance Monad (Either e) where
  (>>=) :: Either e a -> (a -> Either e b) -> Either e b
  Right x >>= f = f x
  Left e  >>= f = Left e

addEither :: Either String Int -> Either String Int -> Either String Int
addEither mx my
  = do
    x <- mx
    z <- Left "unknown error"
    y <- my
    pure (x + y)

-- >>> addEither (Right 9) (Right 7)
-- Right 16