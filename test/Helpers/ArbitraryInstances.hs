{-# OPTIONS_GHC -Wno-orphans #-}

module Helpers.ArbitraryInstances where

import Control.Applicative (liftA2)
import Test.QuickCheck
import Ush.Parser.ParseResult
import Ush.Parser.Parser
import Ush.Util.Collector

instance (Arbitrary a, Arbitrary b) => Arbitrary (Collector a b) where
  arbitrary = liftA2 Collector (listOf arbitrary) arbitrary

instance (Arbitrary a) => Arbitrary (ParseResult a) where
  arbitrary =
    let arbitrarySuccess = liftA2 SuccessfulParse arbitrary arbitrary
        arbitraryFailure = liftA2 FailedParse arbitrary arbitrary
     in oneof [arbitrarySuccess, arbitraryFailure]

instance Show (Parser a) where
  show = const "Parser"

instance (Arbitrary a) => Arbitrary (Parser a) where
  arbitrary = Parser <$> arbitrary
