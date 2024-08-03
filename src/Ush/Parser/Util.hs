module Ush.Parser.Util where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Ush.Parser.ParseResult
import Ush.Parser.Parser

char :: Parser Char
char = Parser $ \case
  [] -> FailedParse {reason = "Expected a character, but got EOF.", failedInput = []}
  (x : xs) -> SuccessfulParse {value = x, remainingInput = xs}

conditional :: (a -> Bool) -> Parser a -> Parser a
conditional predicate (Parser {parse}) = Parser $ \s -> case parse s of
  SuccessfulParse {value, remainingInput} ->
    if predicate value
      then SuccessfulParse {value, remainingInput}
      else FailedParse {reason = "Failed condition.", failedInput = s}
  failed -> failed

optional :: Parser a -> Parser (Maybe a)
optional (Parser {parse}) = Parser $ \s -> case parse s of
  FailedParse _ _ -> SuccessfulParse {value = Nothing, remainingInput = s}
  success -> Just <$> success

parseFoldr :: (a -> b -> b) -> b -> Parser a -> Parser b
parseFoldr folder seed parser = do
  maybeV <- optional parser
  case maybeV of
    Just v -> do
      sub <- parseFoldr folder seed parser
      return $ folder v sub
    Nothing -> return seed

parseFoldl :: (b -> a -> b) -> b -> Parser a -> Parser b
parseFoldl folder seed parser = do
  maybeV <- optional parser
  case maybeV of
    Just v ->
      let newSeed = folder seed v
       in parseFoldl folder newSeed parser
    Nothing -> return seed

star :: Parser a -> Parser [a]
star = parseFoldr (:) []

plus :: Parser a -> Parser (NonEmpty a)
plus parser = do
  v <- parser
  (v :|) <$> star parser

firstOf :: NonEmpty (Parser a) -> Parser a
firstOf (parser :| (nextParser : parsers)) = Parser $ \s ->
  case parse parser s of
    FailedParse {} -> parse (firstOf $ nextParser :| parsers) s
    success -> success
firstOf (parser :| []) = parser

literal :: String -> Parser String
literal "" = pure ""
literal (x : xs) =
  do
    c <-
      conditional (== x) char
        `withFailedParseReason` ( \s ->
                                    "Expected "
                                      <> show (x : xs)
                                      <> " but got "
                                      <> errorMessageToken s
                                )
    (c :) <$> literal xs

