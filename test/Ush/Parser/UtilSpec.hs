module Ush.Parser.UtilSpec where

import Data.Char (isDigit, ord)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Helpers.ParserCase
import Test.Hspec
import Ush.Parser.Parser
import Ush.Parser.Util

char3 :: Parser String
char3 = do
  a <- char
  b <- char
  c <- char
  return [a, b, c]

charThenBoom :: Parser String
charThenBoom = do
  char
  fail "boom"

abcChar :: Parser Char
abcChar =
  conditional (`elem` "abc") char

digitCharToInt :: Char -> Integer
digitCharToInt c = toInteger $ ord c - ord '0'

digit :: Parser Integer
digit = digitCharToInt <$> conditional isDigit char

int :: Parser Integer
int = parseFoldl (\a c -> a * 10 + c) 0 digit

digitExp :: Parser Integer
digitExp = parseFoldr (^) 1 digit

spec :: Spec
spec = do
  describe "char" $ do
    parserCase "parses a character" char $ do
      "abc" `shouldParseTo` 'a' `andRemainder` "bc"
      "x" `shouldParseTo` 'x' `andRemainder` ""

    parserCase "fails with EOF" char $ do
      "" `shouldFailWithReason` "Expected a character, but got EOF." `andRemainder` ""

  describe "conditional" $ do
    let char3abc = conditional (all (`elem` "abc")) char3
    parserCase "parses conditionally" char3abc $ do
      "abccba" `shouldParseTo` "abc" `andRemainder` "cba"
      "abc" `shouldParseTo` "abc" `andRemainder` ""
    parserCase "resets input on failure" char3abc $ do
      "fghxyz" `shouldFailWithReason` "Failed condition." `andRemainder` "fghxyz"
      "abhxyz" `shouldFailWithReason` "Failed condition." `andRemainder` "abhxyz"
    parserCase "passes through underlying error" char3abc $ do
      "ab" `shouldFailWithReason` "Expected a character, but got EOF." `andRemainder` ""
    parserCase "passes through underlying error 2" charThenBoom $ do
      "ab" `shouldFailWithReason` "boom" `andRemainder` "b"

  describe "optional" $ do
    parserCase "parses optionally" (optional $ literal "ab") $ do
      "abc" `shouldParseTo` Just "ab" `andRemainder` "c"
    parserCase "does not advance if Nothing" (optional $ literal "ab") $ do
      "acc" `shouldParseTo` Nothing `andRemainder` "acc"

  describe "parseFoldl" $ do
    parserCase "parses and folds" int $ do
      "123 456" `shouldParseTo` 123 `andRemainder` " 456"
      "123456" `shouldParseTo` 123456 `andRemainder` ""

    parserCase "returns seed on failure" int $ do
      "abcdef" `shouldParseTo` 0 `andRemainder` "abcdef"

  describe "parseFoldr" $ do
    parserCase "parses and folds" digitExp $ do
      "232 1929" `shouldParseTo` 512 `andRemainder` " 1929"
      "432" `shouldParseTo` 262144 `andRemainder` ""

    parserCase "returns seed on failure" int $ do
      "abcdef" `shouldParseTo` 0 `andRemainder` "abcdef"

  describe "star" $ do
    parserCase "parses zero or more" (star abcChar) $ do
      "abc" `shouldParseTo` "abc" `andRemainder` ""
      "abdd" `shouldParseTo` "ab" `andRemainder` "dd"
      "ddd" `shouldParseTo` "" `andRemainder` "ddd"

  describe "plus" $ do
    parserCase "parses one or more" (plus abcChar) $ do
      "abc" `shouldParseTo` 'a' :| "bc" `andRemainder` ""
      "abdd" `shouldParseTo` 'a' :| "b" `andRemainder` "dd"

    parserCase "passes through error parsing first element" (plus charThenBoom) $ do
      "ddd" `shouldFailWithReason` "boom" `andRemainder` "dd"
