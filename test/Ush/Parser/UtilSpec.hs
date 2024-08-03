module Ush.Parser.UtilSpec where

import Test.Hspec
import Ush.Parser.ParseResult
import Ush.Parser.Parser
import Ush.Parser.Util

spec :: Spec
spec = do
  describe "star" $ do
    it "parses" $ do
      parse (star char) "abc" `shouldBe` SuccessfulParse {value = "abc", remainingInput = ""}
