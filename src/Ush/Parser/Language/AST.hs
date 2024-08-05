module Ush.Parser.Language.AST where

import Data.List.NonEmpty (NonEmpty)

data StringSegment = LiteralChar Char | Interpolation Factor
  deriving (Show, Eq)

type ASTString = [StringSegment]

data Number = IntNumber Integer | FloatNumber Double
  deriving (Show, Eq)

data FunctionParameter
  = NamedParameter String (Maybe Factor)
  | PositionalVariadic String
  | KeywordVariadic String
  deriving (Show, Eq)

data FunctionArgument
  = FactorArgument Factor
  | NamedArgument String Factor
  | PositionalSpread Factor
  | KeywordSpread Factor
  deriving (Show, Eq)

data Factor
  = FunctionCallFactor Factor [FunctionArgument]
  | IfFactor Factor Factor Factor
  | LiteralValue String
  | StringValue ASTString
  | ListValue [Factor]
  | MapValue [(String, Factor)]
  | LambdaValue [FunctionParameter] (NonEmpty TopLevelStatement)
  | NumberValue Number
  deriving (Show, Eq)

data TopLevelStatement
  = FunctionCallStatement Factor [FunctionArgument]
  | DefineStatement String Factor
  deriving (Show, Eq)
