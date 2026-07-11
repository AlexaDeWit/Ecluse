module Ecluse.CveSpec (spec) where

import Database.SQLite.Simple (close, open)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Ecluse.Core.Cve (AdvisoryRange (..), CveDbRejected (CveDbMetaUnreadable), insideAffectedRange)
import Ecluse.Core.Cve.Internal (provenanceQuery)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))

-- A builder for a fixed-bounded (half-open) interval, exposing only its bounds.
range :: Maybe Text -> Maybe Text -> AdvisoryRange
range intro fixed =
    AdvisoryRange
        { arCveId = "GHSA-test"
        , arSeverity = Nothing
        , arIntroduced = intro
        , arFixed = fixed
        , arLastAffected = Nothing
        }

-- A builder for an interval closed by an inclusive @last_affected@ bound.
through :: Maybe Text -> Maybe Text -> AdvisoryRange
through intro lastAffected = (range intro Nothing){arLastAffected = lastAffected}

-- A builder for an exact affected point (introduced == last_affected).
point :: Text -> AdvisoryRange
point v = through (Just v) (Just v)

inside :: Text -> AdvisoryRange -> Bool
inside = insideAffectedRange Npm

spec :: Spec
spec = do
    describe "provenanceQuery" $ do
        it "returns Left CveDbMetaUnreadable when the connection is closed" $ do
            conn <- open ":memory:"
            close conn
            res <- provenanceQuery conn
            case res of
                Left (CveDbMetaUnreadable _) -> pass
                other -> fail ("expected Left CveDbMetaUnreadable, got " <> show other)

    describe "AdvisoryRange Show instance" $ do
        it "exercises the constructor" $ do
            let (isNotNull :: [Char] -> Bool) = not . null
            show (AdvisoryRange "CVE-1" (Just 5.0) (Just "0") (Just "1") Nothing) `shouldSatisfy` isNotNull

    describe "insideAffectedRange" $ do
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

    describe "the inclusive last_affected bound [introduced, last_affected]" $ do
        it "contains the last_affected bound itself (unlike a fix)" $
            inside "3.8.8" (through (Just "0") (Just "3.8.8")) `shouldBe` True

        it "excludes a version above the last_affected bound" $
            inside "3.9.0" (through (Just "0") (Just "3.8.8")) `shouldBe` False

    describe "an exact affected point (introduced == last_affected)" $ do
        it "is affected only at that exact version" $
            inside "1.0.0" (point "1.0.0") `shouldBe` True

        it "excludes any other version, above or below" $ do
            inside "1.0.1" (point "1.0.0") `shouldBe` False
            inside "0.9.9" (point "1.0.0") `shouldBe` False

    describe "fail-closed on unprovable comparisons" $ do
        it "an unparseable introduced bound counts as inside" $
            inside "0.0.1" (range (Just "not-a-version") (Just "2.0.0")) `shouldBe` True

        it "an unparseable fixed bound counts as inside" $
            inside "99.0.0" (range (Just "1.0.0") (Just "not-a-version")) `shouldBe` True

        it "an unparseable subject version counts as inside" $
            inside "definitely not semver" (range (Just "1.0.0") (Just "2.0.0")) `shouldBe` True
