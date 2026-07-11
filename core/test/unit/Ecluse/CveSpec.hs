module Ecluse.CveSpec (spec) where

import Database.SQLite.Simple (close, execute_, open)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Ecluse.Core.Cve (AdvisoryRange (..), CveDbRejected (..), insideAffectedRange, severityAtLeast)
import Ecluse.Core.Cve.Internal (checkEpochStamp, checkIntegrity, checkMetaEcosystem, checkRangesTable, openHardenedConnection, provenanceQuery)
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
    describe "openHardenedConnection" $ do
        it "rejects a directory path as a value" $
            withSystemTempDirectory "ecluse-cve-robust" $ \dir ->
                openHardenedConnection Npm dir >>= \case
                    Left (CveDbIntegrityFailed _) -> pass
                    res -> fail ("expected Left CveDbIntegrityFailed, got " <> show (void res))

    describe "acceptance components (robustness under closed connection)" $ do
        it "checkEpochStamp returns Left CveDbIntegrityFailed" $ do
            conn <- open ":memory:"
            close conn
            checkEpochStamp conn >>= \case
                Left (CveDbIntegrityFailed _) -> pass
                other -> fail ("expected Left CveDbIntegrityFailed, got " <> show other)

        it "checkIntegrity returns Left CveDbIntegrityFailed" $ do
            conn <- open ":memory:"
            close conn
            checkIntegrity conn >>= \case
                Left (CveDbIntegrityFailed _) -> pass
                other -> fail ("expected Left CveDbIntegrityFailed, got " <> show other)

        it "checkRangesTable returns Left CveDbIntegrityFailed" $ do
            conn <- open ":memory:"
            close conn
            checkRangesTable conn >>= \case
                Left (CveDbIntegrityFailed _) -> pass
                other -> fail ("expected Left CveDbIntegrityFailed, got " <> show other)

        it "checkMetaEcosystem returns Left CveDbEcosystemMismatch Nothing" $ do
            conn <- open ":memory:"
            close conn
            checkMetaEcosystem Npm conn >>= \case
                Left (CveDbEcosystemMismatch Nothing) -> pass
                other -> fail ("expected Left CveDbEcosystemMismatch Nothing, got " <> show other)

        it "provenanceQuery returns Left CveDbMetaUnreadable" $ do
            conn <- open ":memory:"
            close conn
            provenanceQuery conn >>= \case
                Left (CveDbMetaUnreadable _) -> pass
                other -> fail ("expected Left CveDbMetaUnreadable, got " <> show other)

        it "provenanceQuery decodes multiple rows and sorts by key" $ do
            conn <- open ":memory:"
            execute_ conn "CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT)"
            execute_ conn "INSERT INTO meta VALUES ('b', '2'), ('a', '1')"
            provenanceQuery conn >>= \case
                Right [("a", "1"), ("b", "2")] -> pass
                other -> fail ("expected Right meta, got " <> show other)
            close conn

    describe "Show and Eq instances" $ do
        let (isNotNull :: [Char] -> Bool) = not . null
        it "AdvisoryRange exercises constructor, show and eq" $ do
            let ar1 = AdvisoryRange "CVE-1" (Just 5.0) (Just "0") (Just "1") Nothing
                ar2 = ar1{arFixed = Just "2"}
            show ar1 `shouldSatisfy` isNotNull
            ar1 `shouldBe` ar1
            ar1 `shouldSatisfy` (/= ar2)

        it "CveDbRejected exercises all constructors, show and eq" $ do
            let rejs =
                    [ CveDbWrongEpoch 1
                    , CveDbIntegrityFailed ["p1"]
                    , CveDbRangesNotATable
                    , CveDbEcosystemMismatch (Just "bad")
                    , CveDbEcosystemMismatch Nothing
                    , CveDbMetaUnreadable ["err"]
                    ]
            forM_ rejs $ \rej -> do
                show rej `shouldSatisfy` isNotNull
                rej `shouldBe` rej
            (CveDbWrongEpoch 1 == CveDbWrongEpoch 2) `shouldBe` False

    describe "severityAtLeast" $ do
        it "returns True when severity is Nothing (fail-closed)" $
            severityAtLeast 5.0 Nothing `shouldBe` True

        it "returns True when severity is equal to threshold" $
            severityAtLeast 5.0 (Just 5.0) `shouldBe` True

        it "returns True when severity is above threshold" $
            severityAtLeast 5.0 (Just 7.5) `shouldBe` True

        it "returns False when severity is below threshold" $
            severityAtLeast 5.0 (Just 2.5) `shouldBe` False

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
