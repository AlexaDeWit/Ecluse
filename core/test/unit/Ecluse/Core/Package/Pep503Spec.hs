-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | PyPI normalisation contracts and comparison with the frozen base algorithm.
module Ecluse.Core.Package.Pep503Spec (spec) where

import Data.Text qualified as T
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Package.Pep503 (normalisePyPI)
import Ecluse.Core.Text (textStorageBytes)

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

        it "matches the base on canonical names and aliases" $
            for_ ["requests", "numpy", "boto3", "acme-tools", "zope-interface", "Flask", "ZOPE.Interface", "typing_extensions", "_Acme-_.Tools."] $ \spelling ->
                normalisePyPI spelling `shouldBe` frozenNormalisePyPI spelling

        it "retains the storage of lowercase ASCII canonical slices" $
            for_ ["requests", "acme-tools", "a\0! b"] $ \spelling -> do
                let backing = "prefix." <> spelling <> T.replicate 4096 "x"
                    nameSlice = T.take (T.length spelling) (T.drop 7 backing)
                normalisePyPI nameSlice `shouldBe` frozenNormalisePyPI nameSlice
                textStorageBytes (normalisePyPI nameSlice) `shouldBe` textStorageBytes backing

        it "matches the base on empty, punctuation, whitespace and Unicode edges" $
            for_ ["", "-", "._-", "--a--", "-a-b-", "a--b", "a-_.b", " \t\n", "!/@:+[]", "\0", "-\0-", "\x130", "\x212a", "\x3a3\x3a3", "\x10400", "\x1f600", "_\x130.\x212a-"] $ \spelling ->
                normalisePyPI spelling `shouldBe` frozenNormalisePyPI spelling

        it "matches the base for each ASCII character beside separators" $
            for_ ['\0' .. '\x7f'] $ \character -> do
                let part = T.singleton character
                for_ [part, "-" <> part, part <> "-", "a-" <> part <> "-b", "_." <> part <> "._"] $ \spelling ->
                    normalisePyPI spelling `shouldBe` frozenNormalisePyPI spelling

        it "matches the base on every short separator and case combination" $
            for_ [0 .. 4] $ \combinationLength ->
                for_ (replicateM combinationLength ("-_.aA" :: String)) $ \characters -> do
                    let spelling = T.pack characters
                    normalisePyPI spelling `shouldBe` frozenNormalisePyPI spelling

        it "matches the base on long canonical, alias and Unicode inputs" $
            for_ [T.replicate 100000 "a", T.replicate 20000 "acme-" <> "tools", T.replicate 20000 "Acme-_." <> "Tools", T.replicate 100000 "._-", T.replicate 20000 "\x130-\x212a._", T.replicate 20000 "a\0! "] $ \spelling ->
                normalisePyPI spelling `shouldBe` frozenNormalisePyPI spelling

        modifyMaxSuccess (const 1000) $
            it "matches complete base results on arbitrary Unicode and mixed separators" $
                hedgehog $ do
                    spelling <- forAll (Gen.text (Range.linear 0 512) (Gen.choice [Gen.ascii, Gen.unicodeAll, Gen.element ("-_.aA\0" :: String)]))
                    normalisePyPI spelling === frozenNormalisePyPI spelling

-- Frozen from b795016660d171db729bc773a9a8c495e38d3294, independent of candidate helpers.
frozenNormalisePyPI :: Text -> Text
frozenNormalisePyPI t =
    T.intercalate "-"
        . filter (not . T.null)
        . T.splitOn "-"
        $ T.map (\c -> if c == '_' || c == '.' then '-' else c) (T.toLower t)
