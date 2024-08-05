{-# OPTIONS_GHC -Wno-orphans #-}

module Ush.Parser.ParserSpec where

import Data.Proxy
import Helpers.ArbitraryInstances ()
import Helpers.Laws
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Ush.Parser.ParseResult
import Ush.Parser.Parser

parserShouldEq :: (Eq a) => Parser a -> Parser a -> Expectation
parserShouldEq a b = quickCheckWith (stdArgs {chatty = False, maxSuccess = 200}) $ \s -> parse a s == parse b s

constParser :: ParseResult a -> Parser a
constParser = Parser . const

spec :: Spec
spec = parallel $ do
  quickCheckMonad (Proxy :: Proxy (Parser Integer)) parserShouldEq

  describe "mapParseResult" $ do
    prop
      "mapParseResult id == id"
      ((\p -> mapParseResult id p `parserShouldEq` p) :: (Parser Integer -> Expectation))

    it "maps result" $
      mapParseResult
        ( \case
            SuccessfulParse a b -> SuccessfulParse (a + 1) (b <> b)
            x -> x
        )
        (constParser $ SuccessfulParse (1 :: Integer) "foo")
        `parserShouldEq` constParser (SuccessfulParse 2 "foofoo")

  describe "mapSuccessfulParse" $ do
    it "maps result" $
      mapSuccessfulParse
        (\a b -> SuccessfulParse (a + 1) (b <> b))
        (constParser $ SuccessfulParse (1 :: Integer) "foo")
        `parserShouldEq` constParser (SuccessfulParse 2 "foofoo")

    it "retains failed result" $
      mapSuccessfulParse
        (\a b -> SuccessfulParse (a + (1 :: Integer)) (b <> b))
        (constParser $ FailedParse "foo" "bar")
        `parserShouldEq` constParser (FailedParse "foo" "bar")

  describe "withFailedParseReason" $ do
    it "maps error" $
      ( constParser (FailedParse "reason" "input") `withFailedParseReason` \x ->
          x <> x
      )
        `parserShouldEq` (constParser (FailedParse "reasonreason" "input") :: Parser Integer)

    it "ignores success" $ do
      let p = constParser (SuccessfulParse (1 :: Integer) "input")
      (p `withFailedParseReason` const "dskljaflkasd") `parserShouldEq` p

  describe "functor" $ do
    it "maps the parse value" $ do
      let p = constParser (SuccessfulParse (1 :: Integer) "input")
      let q = constParser (SuccessfulParse (2 :: Integer) "input")
      ((+ 1) <$> p) `parserShouldEq` q

    it "preserves failure" $ do
      let p = constParser (FailedParse "foo" "input")
      ((+ (1 :: Integer)) <$> p) `parserShouldEq` p

  describe "applicative" $ do
    it "applicatives" $ do
      let p = constParser (SuccessfulParse (+ 1) "input")
      let q = constParser (SuccessfulParse (1 :: Integer) "input")
      let r = constParser (SuccessfulParse (2 :: Integer) "input")
      (p <*> q) `parserShouldEq` r

    it "preserves failure in left" $ do
      let p = constParser (FailedParse "foo" "bar") :: Parser (Integer -> Integer)
      let q = constParser (SuccessfulParse (1 :: Integer) "input")
      let r = constParser (FailedParse "foo" "bar") :: Parser Integer
      (p <*> q) `parserShouldEq` r

    it "preserves failure in right" $ do
      let p = constParser (SuccessfulParse (+ (1 :: Integer)) "input")
      let q = constParser (FailedParse "foo" "bar")
      (p <*> q) `parserShouldEq` q

  describe "monad" $ do
    it "monads" $ do
      let p = constParser (SuccessfulParse (1 :: Integer) "input")
      let f i = Parser $ \s -> SuccessfulParse (i + 1) ("foo" <> s)
      let q = constParser (SuccessfulParse 2 "fooinput")
      (p >>= f) `parserShouldEq` q

    it "preserves failure in left" $ do
      let p = constParser (FailedParse "foo" "bar")
      let f i = Parser $ \s -> SuccessfulParse (i + (1 :: Integer)) ("foo" <> s)
      (p >>= f) `parserShouldEq` p

    it "preserves failure in right" $ do
      let p = constParser (SuccessfulParse (1 :: Integer) "input") :: Parser Integer
      let f _ = constParser (FailedParse "foo" "bar") :: Parser Integer
      (p >>= f) `parserShouldEq` f (1 :: Integer)
