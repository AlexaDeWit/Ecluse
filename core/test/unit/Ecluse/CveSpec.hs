module Ecluse.CveSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Ecluse.Core.Cve (AdvisoryRange (..), insideAffectedRange)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))

-- A builder exposing only the axes under test: the interval bounds.
range :: Maybe Text -> Maybe Text -> AdvisoryRange
range intro fixed =
    AdvisoryRange
        { arCveId = "GHSA-test"
        , arSeverity = Nothing
        , arIntroduced = intro
        , arFixed = fixed
        }

inside :: Text -> AdvisoryRange -> Bool
inside = insideAffectedRange Npm

spec :: Spec
spec = describe "insideAffectedRange" $ do
    describe "the half-open interval [introduced, fixed)" $ do
        it "contains a version strictly between the bounds" $
            inside "1.5.0" (range (Just "1.0.0") (Just "2.0.0")) `shouldBe` True

        it "contains the introduced bound itself" $
            inside "1.0.0" (range (Just "1.0.0") (Just "2.0.0")) `shouldBe` True

        it "excludes a version below the introduced bound" $
            inside "0.9.0" (range (Just "1.0.0") (Just "2.0.0")) `shouldBe` False

        it "excludes the fixed bound itself (the fix is not affected)" $
            inside "2.0.0" (range (Just "1.0.0") (Just "2.0.0")) `shouldBe` False

        it "excludes a version above the fixed bound" $
            inside "2.1.0" (range (Just "1.0.0") (Just "2.0.0")) `shouldBe` False

    describe "open ends" $ do
        it "a missing introduced bound starts the range at the beginning" $
            inside "0.0.1" (range Nothing (Just "2.0.0")) `shouldBe` True

        it "a missing fixed bound never ends the range" $
            inside "99.0.0" (range (Just "1.0.0") Nothing) `shouldBe` True

    describe "fail-closed on unprovable comparisons" $ do
        it "an unparseable introduced bound counts as inside" $
            inside "0.0.1" (range (Just "not-a-version") (Just "2.0.0")) `shouldBe` True

        it "an unparseable fixed bound counts as inside" $
            inside "99.0.0" (range (Just "1.0.0") (Just "not-a-version")) `shouldBe` True

        it "an unparseable subject version counts as inside" $
            inside "definitely not semver" (range (Just "1.0.0") (Just "2.0.0")) `shouldBe` True
