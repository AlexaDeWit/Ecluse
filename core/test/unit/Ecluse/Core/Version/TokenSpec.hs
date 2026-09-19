-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Version.TokenSpec (spec) where

import Data.Char (isAlphaNum, isAscii)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Version.Token (isAsciiAlphaNum)

spec :: Spec
spec =
    describe "isAsciiAlphaNum" $
        it "is isAscii && isAlphaNum, over ASCII and over the whole code-point range" $
            hedgehog $ do
                -- Gen.unicodeAll draws an ASCII character about once in ten thousand, so the
                -- ASCII half of the law needs a generator of its own to be exercised at all.
                c <- forAll (Gen.choice [Gen.ascii, Gen.unicodeAll])
                isAsciiAlphaNum c === (isAscii c && isAlphaNum c)
