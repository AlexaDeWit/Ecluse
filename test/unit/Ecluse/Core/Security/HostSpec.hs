{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Core.Security.HostSpec (spec) where

import Test.Hspec

import Ecluse.Core.Security.Host (isHex)

spec :: Spec
spec = do
    describe "isHex" $ do
        it "accepts valid lowercase hex strings" $ do
            isHex "0123456789abcdef" `shouldBe` True
            isHex "a" `shouldBe` True
            isHex "f" `shouldBe` True

        it "accepts valid uppercase hex strings" $ do
            isHex "0123456789ABCDEF" `shouldBe` True
            isHex "A" `shouldBe` True
            isHex "F" `shouldBe` True

        it "accepts mixed case hex strings" $ do
            isHex "aBcDeF" `shouldBe` True
            isHex "1a2B3c" `shouldBe` True

        it "rejects empty strings" $ do
            isHex "" `shouldBe` False

        it "rejects strings with non-hex characters" $ do
            isHex "g" `shouldBe` False
            isHex "G" `shouldBe` False
            isHex "0123456789abcdefg" `shouldBe` False
            isHex "abc def" `shouldBe` False
            isHex "abc-def" `shouldBe` False
            isHex "-1a" `shouldBe` False
            isHex "0x1a" `shouldBe` False
