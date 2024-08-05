module Ush.Parser.ParseResult where

import Control.Applicative (liftA2)
import Data.Char (isSpace)
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (oneof)

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

instance (Arbitrary a) => Arbitrary (ParseResult a) where
  arbitrary =
    let arbitrarySuccess = liftA2 SuccessfulParse arbitrary arbitrary
        arbitraryFailure = liftA2 FailedParse arbitrary arbitrary
     in oneof [arbitrarySuccess, arbitraryFailure]

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
