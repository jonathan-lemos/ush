module Ush.Util.CollectorSpec where

import Data.Proxy (Proxy (Proxy))
import Helpers.ArbitraryInstances ()
import Helpers.Laws
import Test.Hspec
import Ush.Util.Collector

spec :: Spec
spec = parallel $ do
  quickCheckMonad (Proxy :: Proxy (Collector String Integer)) shouldBe

  it "collects in the correct order" $ do
    let coll = do
          asCollector (1 :: Integer)
          asCollector 2
          asCollector 3
          asCollector 4
    collectedValues coll `shouldBe` [1, 2, 3, 4]

  it "fmaps the value" $ do
    let Collector a b = (+ 1) <$> Collector [1 :: Integer, 2] (3 :: Integer)
    a `shouldBe` [1, 2]
    b `shouldBe` 4

  it "applicative functors" $ do
    let Collector a b = Collector [1 :: Integer, 2] (+ 1) <*> Collector [3, 4] (5 :: Integer)
    a `shouldBe` [3, 4, 1, 2]
    b `shouldBe` 6

  it "monads" $ do
    let f i = Collector [3, 4] (i + 1)
    let Collector a b = Collector [1 :: Integer, 2] (5 :: Integer) >>= f
    a `shouldBe` [3, 4, 1, 2]
    b `shouldBe` 6
