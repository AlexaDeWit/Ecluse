module Ecluse.Version.TokenSpec (spec) where

import Test.Hspec

import Ecluse.Core.Version.Token (parseNumSeg)

spec :: Spec
spec = do
    describe "parseNumSeg" $ do
        it "parses a single digit" $
            parseNumSeg "0" `shouldBe` Just 0

        it "parses a multiple digit sequence" $
            parseNumSeg "123" `shouldBe` Just 123

        it "parses a sequence with leading zeros" $
            parseNumSeg "0123" `shouldBe` Just 123

        it "returns Nothing for empty text" $
            parseNumSeg "" `shouldBe` Nothing

        it "returns Nothing for non-digit text" $
            parseNumSeg "abc" `shouldBe` Nothing

        it "returns Nothing for mixed text (digits then letters)" $
            parseNumSeg "123a" `shouldBe` Nothing

        it "returns Nothing for mixed text (letters then digits)" $
            parseNumSeg "a123" `shouldBe` Nothing

        it "returns Nothing for negative numbers (minus is not a digit)" $
            parseNumSeg "-123" `shouldBe` Nothing

        it "returns Nothing for Unicode digits (only ASCII digits allowed by T.all isDigit when parsed by readMaybe)" $
            parseNumSeg "\x0660" `shouldBe` Nothing
