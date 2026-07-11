module Ecluse.Version.TokenSpec (spec) where

import Data.Char (isAlphaNum, isAscii)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Version.Token (isAsciiAlphaNum)

spec :: Spec
spec = do
    describe "isAsciiAlphaNum" $ do
        it "matches isAscii && isAlphaNum for all characters" $
            hedgehog $ do
                c <- forAll Gen.unicodeAll
                isAsciiAlphaNum c === (isAscii c && isAlphaNum c)

        it "returns True for ASCII letters and digits" $ do
            all isAsciiAlphaNum ['a' .. 'z'] `shouldBe` True
            all isAsciiAlphaNum ['A' .. 'Z'] `shouldBe` True
            all isAsciiAlphaNum ['0' .. '9'] `shouldBe` True

        it "returns False for ASCII punctuation" $ do
            isAsciiAlphaNum '-' `shouldBe` False
            isAsciiAlphaNum '.' `shouldBe` False
            isAsciiAlphaNum '_' `shouldBe` False
            isAsciiAlphaNum ' ' `shouldBe` False

        it "returns False for Unicode alphanumeric characters" $ do
            isAsciiAlphaNum 'é' `shouldBe` False
            isAsciiAlphaNum 'α' `shouldBe` False
            isAsciiAlphaNum '１' `shouldBe` False -- Fullwidth digit 1
            isAsciiAlphaNum '٤' `shouldBe` False -- Arabic-Indic digit 4
