module Spec where

import Test.Hspec

main :: IO ()
main = hspec servantTSSpec

servantTSSpec :: Spec
servantTSSpec = do
  describe "base" $ do
    it "should test basic" $ do
      1 `shouldBe` 1
