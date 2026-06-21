module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "NpmSecureProxy" $ do
    it "placeholder" $
      True `shouldBe` True
