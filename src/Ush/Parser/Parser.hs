module Ush.Parser.Parser where

import Test.QuickCheck (Arbitrary (arbitrary))
import Ush.Parser.ParseResult

type ParseFunction a = String -> ParseResult a

newtype Parser a = Parser {parse :: ParseFunction a}

mapParseResult :: (ParseResult a -> ParseResult b) -> Parser a -> Parser b
mapParseResult f (Parser {parse}) = Parser $ fmap f parse

mapSuccessfulParse :: (a -> String -> ParseResult b) -> Parser a -> Parser b
mapSuccessfulParse f =
  mapParseResult
    ( \case
        SuccessfulParse {value, remainingInput} -> f value remainingInput
        FailedParse {reason, failedInput} -> FailedParse reason failedInput
    )

withFailedParseReason :: Parser a -> (String -> String) -> Parser a
withFailedParseReason parser f =
  mapParseResult
    ( \case
        FailedParse {failedInput} -> FailedParse {reason = f failedInput, failedInput}
        success -> success
    )
    parser

infix 2 `withFailedParseReason`

instance Functor Parser where
  fmap f = mapParseResult (fmap f)

instance Applicative Parser where
  pure v = Parser $ SuccessfulParse v
  a <*> (Parser bParse) = (mapSuccessfulParse $ \aFunc remainder -> aFunc <$> bParse remainder) a

instance Monad Parser where
  a >>= f = (mapSuccessfulParse $ \aValue remainder -> (parse $ f aValue) remainder) a

instance MonadFail Parser where
  fail reason = Parser $ \s -> FailedParse {reason, failedInput = s}

instance (Arbitrary a) => Arbitrary (Parser a) where
  arbitrary = Parser <$> arbitrary
