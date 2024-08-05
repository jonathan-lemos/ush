module Ush.Parser.ParseResult where

import Data.Char (isSpace)

data ParseResult a
  = SuccessfulParse
      { value :: a,
        remainingInput :: String
      }
  | FailedParse
      { reason :: String,
        failedInput :: String
      }
  deriving (Show, Eq, Functor)

isSuccess :: ParseResult a -> Bool
isSuccess (SuccessfulParse {}) = True
isSuccess _ = False

isFailure :: ParseResult a -> Bool
isFailure = not . isSuccess

errorMessageToken :: String -> String
errorMessageToken s =
  let go "" = ""
      go (c : cs) = if isSpace c then "" else c : go cs
   in case go s of
        "" -> "EOF"
        str -> show str
