module Main where

import Test.Hspec.Runner
import qualified Spec
import Test.Hspec

main :: IO ()
main = hspecWith defaultConfig (parallel Spec.spec)
