module Ush.Parser.Language where

import Data.Char (isSpace)
import Ush.Parser.Parser
import Ush.Parser.Util

whitespace :: Parser String
whitespace = star $ conditional isSpace char

ignoringWhitespace :: Parser a -> Parser a
ignoringWhitespace parser = do
  _ <- whitespace
  v <- parser
  _ <- whitespace
  return v

parenthesized :: Parser a -> Parser a
parenthesized parser = do
  _ <- ignoringWhitespace $ literal "("
  v <- parser
  _ <- ignoringWhitespace $ literal ")"
  return v

data Factor = Literal String | Subexpression Expression | FactorList [Factor]

data Expression = FunctionCall
  { function :: String,
    args :: [Factor]
  }

functionCall :: Parser Expression
functionCall = undefined
