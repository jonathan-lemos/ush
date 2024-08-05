module Ush.Util.CollectorSpec where

import Data.Proxy (Proxy (Proxy))
import Helpers.Laws
import Test.Hspec
import Ush.Util.Collector

spec :: Spec
spec = do
  quickCheckMonad (Proxy :: Proxy (Collector String Integer))

  it "collects in the correct order" $ do
    let coll = do
          asCollector (1 :: Integer)
          asCollector 2
          asCollector 3
          asCollector 4
    collectedValues coll `shouldBe` [1, 2, 3, 4]
