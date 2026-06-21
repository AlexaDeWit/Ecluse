module Main (main) where

import Test.Hspec

main :: IO ()
main =
    hspec $
        describe "NpmSecureProxy" $
            it "placeholder" $
                True `shouldBe` True
