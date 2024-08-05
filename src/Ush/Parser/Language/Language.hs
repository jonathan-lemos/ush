{-# LANGUAGE OverloadedLists #-}

module Ush.Parser.Language.Language where

import Control.Applicative (Applicative (..), (<|>))
import Data.Char
import Data.Maybe
import Text.Read (readMaybe)
import Ush.Parser.Language.AST
import Ush.Parser.Language.LanguageUtil
import Ush.Parser.ParseResult (errorMessageToken)
import Ush.Parser.Parser
import Ush.Parser.SwitchCase
import Ush.Parser.Util

variableInterpolation :: Parser StringSegment
variableInterpolation = do
  let variableInterpolationExpression =
        Interpolation
          <$> surrounded
            (literal "(")
            factor
            (literal ")")

  literal "$"
  switch
    ( do
        literal "(" ->: variableInterpolationExpression
        defaultCase ->: Interpolation <$> literalValue
    )
    `withFailedParseReason` \s ->
      "Expected [a-zA-Z\\-_] or '(', but got " <> errorMessageToken s

stringSegment :: Parser StringSegment
stringSegment =
  switch
    ( do
        literal "\\" ->: LiteralChar <$> escapeChar
        literal "$" ->: variableInterpolation
        defaultCase ->: LiteralChar <$> conditional (not . (`elem` "\"\n")) char
    )

mapKeyValuePair :: Parser (String, Factor)
mapKeyValuePair = do
  key <- rawIdentifierString
  literal "="
  v <- factor
  return (key, v)

functionParameter :: Parser FunctionParameter
functionParameter =
  switch
    ( do
        literal "**" ->: KeywordVariadic <$> (literal "**" >> rawIdentifierString)
        literal "*" ->: PositionalVariadic <$> (literal "*" >> rawIdentifierString)
        defaultCase ->: NamedParameter <$> rawIdentifierString
    )
    `withFailedParseReason` \s ->
      "Expected *?*?[a-zA-Z\\-_]+, got " <> errorMessageToken s

functionArgument :: Parser FunctionArgument
functionArgument =
  let namedArgument = do
        key <- rawIdentifierString
        literal "="
        NamedArgument key <$> factor
   in switch
        ( do
            literal "**" ->: KeywordSpread <$> (literal "**" >> factor)
            literal "*" ->: PositionalSpread <$> (literal "*" >> factor)
            rawIdentifierString >> literal "=" ->: namedArgument
            defaultCase ->: FactorArgument <$> factor
        )
        `withFailedParseReason` \s ->
          "Expected **value, *value, keyword argument, or positional argument; got " <> errorMessageToken s

functionCallFactor :: Parser Factor
functionCallFactor = parenthesized $ do
  func <- factor
  args <- star $ whitespace >> functionArgument
  return $ FunctionCallFactor func args

literalValue :: Parser Factor
literalValue = LiteralValue <$> rawIdentifierString

stringValue :: Parser Factor
stringValue =
  StringValue
    <$> surrounded
      (literal "\"")
      (star stringSegment)
      (literal "\"")

listValue :: Parser Factor
listValue =
  ListValue
    <$> surrounded
      (literal "[")
      (star $ whitespace >> factor)
      (whitespace >> literal "]")

mapValue :: Parser Factor
mapValue =
  MapValue
    <$> surrounded
      (literal "{")
      (star $ whitespace >> mapKeyValuePair)
      (whitespace >> literal "}")

lambdaValue :: Parser Factor
lambdaValue = parenthesized $ do
  literal "lambda"
  whitespace
  literal "("
  params <- star $ whitespace >> functionParameter
  whitespace
  literal ")"
  whitespace
  literal "("
  whitespace
  body <- plus (parenthesized topLevelStatement)
  whitespace
  literal ")"
  return $ LambdaValue params body

numberValue :: Parser Factor
numberValue = do
  let decimalPart = liftA2 (<>) (literal ".") numberSequence
  sign <- optional $ conditional (== '-') char
  start <- numberSequence
  decimal <- optional decimalPart
  expFactor <- optional exponentialFactor

  let str = maybe "" (: []) sign <> start <> fromMaybe "" decimal <> fromMaybe "" expFactor

  return . fromJust $
    (NumberValue . IntNumber <$> (readMaybe str :: Maybe Integer))
      <|> (NumberValue . FloatNumber <$> (readMaybe str :: Maybe Double))

factor :: Parser Factor
factor =
  switch
    ( do
        conditional isHeadIdentifierChar char ->: literalValue
        literal "\"" ->: stringValue
        literal "[" ->: listValue
        literal "{" ->: mapValue
        literal "lambda" ->: lambdaValue
        conditional (liftA2 (||) (== '-') isDigit) char ->: numberValue
        literal "(" ->: functionCallFactor
    )
    `withFailedParseReason` \s ->
      "Expected a string, list, map, lambda expression, number, or function call, got " <> errorMessageToken s

functionCallStatement :: Parser TopLevelStatement
functionCallStatement = do
  func <- factor
  args <- star $ whitespace >> functionArgument
  return $ FunctionCallStatement func args

defineStatement :: Parser TopLevelStatement
defineStatement = do
  literal "define"
  whitespace
  ident <- rawIdentifierString
  whitespace
  DefineStatement ident <$> factor

topLevelStatement :: Parser TopLevelStatement
topLevelStatement =
  let body =
        switch
          ( do
              literal "define" ->: defineStatement
              defaultCase ->: functionCallStatement
          )
   in surrounded whitespace body whitespace `withFailedParseReason` \s ->
        "Expected **value, *value, keyword argument, a positional argument, or define; got " <> errorMessageToken s
