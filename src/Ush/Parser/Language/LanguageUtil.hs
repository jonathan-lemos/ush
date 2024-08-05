module Ush.Parser.Language.LanguageUtil where

import Control.Applicative (liftA2)
import Data.Bits (shiftL)
import Data.Char
import Data.List (elemIndex)
import Data.List.NonEmpty (toList)
import Ush.Parser.ParseResult
import Ush.Parser.Parser
import Ush.Parser.Util

surrounded :: Parser a -> Parser b -> Parser c -> Parser b
surrounded a b c = do
  a
  v <- b
  c
  return v

whitespace :: Parser String
whitespace = star $ conditional isSpace char

parenthesized :: Parser a -> Parser a
parenthesized p =
  surrounded
    (literal "(")
    (whitespace >> p)
    (whitespace >> literal ")")

-- | Parses a single hex digit and returns its numeric value.
hexCharValue :: Parser Int
hexCharValue = do
  c <- char `withFailedParseReason` const "Expected a hexadecimal character, got EOF."
  let val = elemIndex (toUpper c) "0123456789ABCDEF"
  case val of
    Just v -> return v
    Nothing -> fail $ "Expected a hexadecimal character, got " <> show c <> "."

-- | Parses a sequence of 4 hex digits and returns the corresponding character.
unicodeEscapeSequence :: Parser Char
unicodeEscapeSequence = do
  a <- hexCharValue
  b <- hexCharValue
  c <- hexCharValue
  d <- hexCharValue
  let num = (a `shiftL` 12) + (b `shiftL` 8) + (c `shiftL` 4) + d
  return $ chr num

-- | Parses an escape character as seen in https://www.json.org/json-en.html
escapeChar :: Parser Char
escapeChar = do
  literal "\\"
  c <- char
  case c of
    '"' -> return '"'
    '\\' -> return '\\'
    '/' -> return '/'
    'b' -> return '\b'
    'f' -> return '\f'
    'n' -> return '\n'
    'r' -> return '\r'
    't' -> return '\t'
    'u' -> unicodeEscapeSequence
    _ -> fail $ "Invalid escape character " <> show c <> "."

isHeadIdentifierChar :: Char -> Bool
isHeadIdentifierChar = liftA2 (||) isAlpha (`elem` "-_")

isIdentifierChar :: Char -> Bool
isIdentifierChar = liftA2 (||) isHeadIdentifierChar isNumber

rawIdentifierString :: Parser String
rawIdentifierString = do
  h <-
    conditional isHeadIdentifierChar char
      `withFailedParseReason` \s ->
        "Expected [a-zA-Z\\-_], but got " <> errorMessageToken s
  t <- star (conditional isIdentifierChar char)
  return $ h : t

numberSequence :: Parser String
numberSequence = toList <$> plus (conditional isDigit char)

exponentialFactor :: Parser String
exponentialFactor = do
  expChar <- conditional (`elem` "eE") char
  sign <- optional $ conditional (== '-') char
  digits <- plus $ conditional isDigit char
  return $ [expChar] <> maybe "" (: []) sign <> toList digits
