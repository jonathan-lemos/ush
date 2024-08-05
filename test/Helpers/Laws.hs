{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Monad law, left identity" #-}
{-# HLINT ignore "Monad law, right identity" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use >=>" #-}

module Helpers.Laws where

import Control.Applicative (liftA2)
import Data.Proxy
import Test.Hspec
import Test.QuickCheck

instance Arbitrary (Proxy a) where
  arbitrary = return Proxy

data FunctionInputPair a b = FunctionInputPair a (a -> b)

instance (CoArbitrary a, Arbitrary a, Arbitrary b) => Arbitrary (FunctionInputPair a b) where
  arbitrary = liftA2 FunctionInputPair arbitrary arbitrary

instance (Show a, Show b) => Show (FunctionInputPair a b) where
  show (FunctionInputPair input f) = "f " <> show input <> " = " <> show (f input)

type CompareFn a = a -> a -> Expectation

lawProp :: (HasCallStack, Testable prop) => String -> prop -> Spec
lawProp title rawProp = it title $ (withMaxSuccess 500) rawProp

quickCheckFunctor :: forall f. (Functor f, Show (f Integer), Arbitrary (f Integer)) => Proxy (f Integer) -> CompareFn (f Integer) -> Spec
quickCheckFunctor _ cmp = do
  describe "satisfies functor laws" $ do
    lawProp
      "fmap id == id"
      ((\x -> fmap id x `cmp` x) :: (f Integer -> Expectation))

    lawProp
      "fmap (f . g) == fmap f . fmap g"
      ((\x -> (fmap (* 2) . fmap (+ 1)) x `cmp` fmap ((* 2) . (+ 1)) x) :: (f Integer -> Expectation))

quickCheckApplicative :: forall a. (Applicative a, Show (a Integer), Arbitrary (a Integer)) => Proxy (a Integer) -> CompareFn (a Integer) -> Spec
quickCheckApplicative proxy cmp = do
  quickCheckFunctor proxy cmp

  describe "satisfies applicative laws" $ do
    lawProp
      "pure id <*> v == v"
      ((\x -> pure id <*> x `cmp` x) :: (a Integer -> Expectation))

    lawProp
      "pure f <*> pure x == pure (f x)"
      ((\(x, _) -> ((pure (+ 1) :: a (Integer -> Integer)) <*> pure x) `cmp` (pure (x + 1))) :: ((Integer, Proxy (a Integer)) -> Expectation))

    lawProp
      "u <*> pure y == pure ($ y) <*> u"
      ((\(x, _) -> ((pure (+ 1) :: a (Integer -> Integer)) <*> pure x) `cmp` (pure ($ x) <*> pure (+ 1))) :: ((Integer, Proxy (a Integer)) -> Expectation))

    lawProp
      "pure (.) <*> u <*> v <*> w == u <*> (v <*> w)"
      ((\x -> (pure (.) <*> pure (+ 1) <*> pure (* 2) <*> x) `cmp` (pure (+ 1) <*> (pure (* 2) <*> x))) :: (a Integer -> Expectation))

quickCheckMonad :: forall m. (Monad m, Show (m Integer), Arbitrary (m Integer)) => Proxy (m Integer) -> CompareFn (m Integer) -> Spec
quickCheckMonad proxy cmp = do
  quickCheckApplicative proxy cmp

  describe "satisfies monad laws" $ do
    lawProp
      "return a >>= h == h a"
      ((\(FunctionInputPair input f) -> (return input >>= f) `cmp` f input) :: FunctionInputPair Integer (m Integer) -> Expectation)

    lawProp
      "m >>= return == m"
      ((\m -> (m >>= return) `cmp` m) :: m Integer -> Expectation)

    lawProp
      "(m >>= g) >>= h == m >>= (\\x -> g x >>= h)"
      ( ( \(input, FunctionInputPair _ g, FunctionInputPair _ h) ->
            ((input >>= g) >>= h) `cmp` (input >>= (\x -> g x >>= h))
        ) ::
          ((m Integer, FunctionInputPair Integer (m Integer), FunctionInputPair Integer (m Integer)) -> Expectation)
      )
