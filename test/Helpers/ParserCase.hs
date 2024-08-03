{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Helpers.ParserCase where

import Test.Hspec
import Ush.Parser.ParseResult
import Ush.Parser.Parser
import Ush.Util.Collector

data ParserMatcher a
  = ShouldParseTo (ParseResult a)
  | ShouldParseAndSatisfy (ParseResult a -> Bool)

parsesAndSatisfies :: (a -> String -> Bool) -> ParseResult a -> Bool
parsesAndSatisfies predicate = \case
  SuccessfulParse {value, remainingInput} -> predicate value remainingInput
  FailedParse {} -> False

data ParserCaseLine a = ParserCaseLine String (ParserMatcher a)

type ParserCase a b = Collector (ParserCaseLine a) b

shouldParseTo :: (Show a, Eq a) => String -> a -> String -> ParserCase a ()
shouldParseTo input val ri =
  asCollector $
    ParserCaseLine input (ShouldParseTo SuccessfulParse {value = val, remainingInput = ri})

infixl 1 `shouldParseTo`

shouldFailWithReason :: (Show a, Eq a) => String -> String -> String -> ParserCase a ()
shouldFailWithReason input rsn ri =
  asCollector $
    ParserCaseLine input (ShouldParseTo FailedParse {reason = rsn, failedInput = ri})

infixl 1 `shouldFailWithReason`

andRemainder :: (Show a, Eq a) => (String -> ParserCase a ()) -> String -> ParserCase a ()
andRemainder = ($)

infixl 1 `andRemainder`

shouldParseAndSatisfy :: (Show a, Eq a) => String -> (ParseResult a -> Bool) -> ParserCase a ()
shouldParseAndSatisfy input f = asCollector $ ParserCaseLine input (ShouldParseAndSatisfy f)

parserCaseLineToExpectation :: (Show a, Eq a) => Parser a -> ParserCaseLine a -> Expectation
parserCaseLineToExpectation parser (ParserCaseLine input matcher) =
  let parseResult = parse parser input
   in case matcher of
        ShouldParseTo expected -> parseResult `shouldBe` expected
        ShouldParseAndSatisfy predicate -> parseResult `shouldSatisfy` predicate

parserCase :: (Show a, Eq a) => String -> Parser a -> ParserCase a b -> SpecWith ()
parserCase title parser pc =
  it title $ mapM_ (parserCaseLineToExpectation parser) (collectedValues pc)
