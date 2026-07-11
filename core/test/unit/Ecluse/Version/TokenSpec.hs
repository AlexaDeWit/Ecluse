-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

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

        it "returns True for ASCII letters and digits" $
            hedgehog $ do
                c <- forAll Gen.ascii
                isAsciiAlphaNum c === isAlphaNum c

        it "returns False for non-ASCII characters" $
            hedgehog $ do
                c <- forAll (Gen.filter (not . isAscii) Gen.unicodeAll)
                isAsciiAlphaNum c === False
