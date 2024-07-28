module Ush.Parser.String (quotedString) where

import Control.Monad
import Data.Bits
import Data.Char
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Ush.Parser.Parser
import Ush.Parser.Util

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
  let value = (a `shiftL` 12) + (b `shiftL` 8) + (c `shiftL` 4) + d
  return $ chr value

-- | Parses an escape character as seen in https://www.json.org/json-en.html
escapeChar :: Parser Char
escapeChar = do
  _ <- literal "\\"
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

quotedString :: Parser String
quotedString = do
  _ <- literal "\""
  guts <-
    star $
      switchCase
        ( (void $ literal "\\", escapeChar)
            :| [(pure (), conditional (/= '"') char)]
        )
  _ <- literal "\""
  return guts
