-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Package.Pep503Spec (spec) where

import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Package.Pep503 (normalisePyPI)

spec :: Spec
spec = do
    describe "normalisePyPI" $ do
        it "lower-cases and maps every separator to '-'" $
            normalisePyPI "Flask_Thing.X" `shouldBe` "flask-thing-x"

        it "collapses a run of separators to one '-'" $
            for_ ["zope.interface", "zope__interface", "zope-_.interface"] $ \spelling ->
                normalisePyPI spelling `shouldBe` "zope-interface"

        it "drops leading and trailing separators" $
            normalisePyPI "_acme." `shouldBe` "acme"

        it "yields the empty key for empty and separator-only input" $
            for_ ["", "._-"] $ \spelling ->
                normalisePyPI spelling `shouldBe` ""

        it "leaves a name already in canonical form alone" $
            normalisePyPI "acme-tools" `shouldBe` "acme-tools"

        it "is idempotent" $
            hedgehog $ do
                raw <- forAll (Gen.text (Range.linear 0 24) (Gen.element ("-_.aA0" :: String)))
                normalisePyPI (normalisePyPI raw) === normalisePyPI raw
