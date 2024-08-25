{-# LANGUAGE ConstraintKinds #-}
{-# language TypeApplications, ScopedTypeVariables #-}
module MonadLawsChecker where

import Test.QuickCheck ( applyFun, quickCheck, Arbitrary, Fun )
import Test.QuickCheck.Poly ( A, B, C )
import Control.Monad ((>=>))

-- Need ConstraintKinds for this, purely for convenience instead of writing
-- all this stuff into the contraints for checkMonadLaws
type Checkable m a = (Arbitrary (m a), Eq (m a), Show (m a))
type PolyCheckable m = (Checkable m A, Checkable m B, Checkable m C)

-- `forall m a .` brings the type variables m and a into scope so they 
-- can be used in the body of the function. Enabled by ScopedTypeVariables.
checkMonadLaws :: forall m a. (Monad m, PolyCheckable m) => IO (m ())
checkMonadLaws = do
  putStrLn "Testing left identity (pure a >>= k  ==  k a):"
  quickCheck $ prop_left_identity @m   -- These are type applications. They're like normal applications of values,
  putStrLn "\nTesting right identity (m >>= pure  ==  m):"
  quickCheck $ prop_right_identity @m  -- except for types. These prop functions need to know which specific monad
  putStrLn "\nTesting associativity (m >=> (k >=> h)  ==  (m >=> k) >=> h):"
  quickCheck $ prop_associativity @m     -- they'll be used for, so we tell them to use the same 'm' that gets returned by this IO action
  pure (pure ()) -- This is here so we can specify what monad we want to test by giving a type to checkMonadLaws

-- Example uses (enter these into your terminal):
-- >>> checkMonadLaws :: IO (Maybe ())
-- >>> checkMonadLaws :: IO (Either Int ())
-- >>> checkMonadLaws :: IO ([()])
-- etc.
-- Or, using type applications
-- >>> checkMonadLaws @Maybe
-- >>> checkMonadLaws @(Either Int)
-- >>> checkMonadLaws @[]
-- etc.


prop_left_identity :: (Monad m, Eq (m B)) => A -> Fun A (m B) -> Bool
prop_left_identity a f = (pure a >>= applyFun f) == applyFun f a

prop_right_identity :: (Monad m, Eq (m A)) => m A -> Bool
prop_right_identity m = (m >>= pure) == m

prop_associativity :: (Monad m, Eq (m C))
           => Fun () (m A)
           -> Fun A  (m B)
           -> Fun B  (m C) -> Bool
prop_associativity f g h
    = ( applyFun f >=> (applyFun g  >=> applyFun h)) ()
   == ((applyFun f >=>  applyFun g) >=> applyFun h ) ()