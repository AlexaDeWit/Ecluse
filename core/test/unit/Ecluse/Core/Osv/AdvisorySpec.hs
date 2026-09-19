-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

-- | Decoding one OSV record, and the rows and bounds it extracts to.
module Ecluse.Core.Osv.AdvisorySpec (spec) where

import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.ByteString qualified as BS
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Osv.Advisory
import Ecluse.Core.Osv.Epss (EpssScores, mkEpssScores)
import Ecluse.Core.Osv.Types (UpperBound (..))
import Ecluse.Test.Osv.Withdrawal (withdrawalBytes)

advisory :: [OsvSeverityEntry] -> Maybe Text -> OsvAdvisory
advisory entries label =
    OsvAdvisory
        { osvId = "GHSA-test-severity"
        , osvAliases = Nothing
        , osvAffected = Nothing
        , osvSeverity = if null entries then Nothing else Just entries
        , osvDatabaseSpecific = OsvDatabaseSpecific . Just <$> label
        , osvWithdrawn = Nothing
        , osvModified = Nothing
        }

noScores :: EpssScores
noScores = mkEpssScores []

decodeWithdrawal :: Maybe Value -> IO OsvAdvisory
decodeWithdrawal withdrawn = withdrawalBytes withdrawn >>= either fail pure . eitherDecodeStrict

spec :: Spec
spec = describe "one OSV advisory record" $ do
    describe "osvExportUrl" $ do
        it "derives the per-ecosystem export under the base URL" $
            osvExportUrl "https://osv-vulnerabilities.storage.googleapis.com" "npm"
                `shouldBe` "https://osv-vulnerabilities.storage.googleapis.com/npm/all.zip"

        it "tolerates a trailing slash on the base URL" $
            osvExportUrl "https://mirror.example.com/osv/" "npm"
                `shouldBe` "https://mirror.example.com/osv/npm/all.zip"

    it "decodes a sample OSV advisory and extracts remediation boundaries" $ do
        fileBytes <- BS.readFile "test/unit/fixtures/osv/sample.json"
        let res = eitherDecodeStrict fileBytes :: Either String OsvAdvisory
        case res of
            Left err -> fail ("Failed to decode: " <> err)
            Right adv -> do
                osvId adv `shouldBe` "GHSA-2234-fmw7-43wr"
                let extracted = extractFromAdvisory noScores adv
                extracted
                    `shouldBe` [ ExtractedOsv
                                    { extPackage = "hono"
                                    , extEcosystem = "npm"
                                    , extCveId = "GHSA-2234-fmw7-43wr"
                                    , extIntroduced = Nothing
                                    , extUpperBound = FixedBefore "4.6.5"
                                    , -- The fixture carries both a CVSS 3.1 vector and the
                                      -- "MODERATE" label. The computed base score wins.
                                      extSeverity = Just 5.9
                                    , -- sample.json aliases CVE-2024-48913, which the empty
                                      -- fixture table does not score.
                                      extEpss = Nothing
                                    }
                               ]

    describe "advisorySeverity" $ do
        it "computes the base score from a CVSS vector and prefers it over the label" $
            advisorySeverity
                (advisory [OsvSeverityEntry "CVSS_V3" "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H"] (Just "HIGH"))
                `shouldBe` Just 9.8

        it "takes the highest score when several vectors parse" $
            advisorySeverity
                ( advisory
                    [ OsvSeverityEntry "CVSS_V3" "CVSS:3.1/AV:N/AC:H/PR:N/UI:R/S:U/C:L/I:H/A:N" -- 5.9
                    , OsvSeverityEntry "CVSS_V3" "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H" -- 9.8
                    ]
                    Nothing
                )
                `shouldBe` Just 9.8

        it "parses a CVSS v4 vector (needs cvss >= 0.3) rather than dropping it" $
            -- A critical v4 vector, no label: it can only score above 8 if the v4
            -- parser is present. On cvss 0.2 the vector is unscored (Nothing).
            advisorySeverity
                (advisory [OsvSeverityEntry "CVSS_V4" "CVSS:4.0/AV:N/AC:L/AT:N/PR:N/UI:N/VC:H/VI:H/VA:H/SC:N/SI:N/SA:N"] Nothing)
                `shouldSatisfy` maybe False (>= 8.0)

        it "falls back to the qualitative label when no vector parses" $
            advisorySeverity
                (advisory [OsvSeverityEntry "CVSS_V3" "not-a-real-vector"] (Just "CRITICAL"))
                `shouldBe` Just 10.0

        it "yields Nothing for an advisory with no severity evidence at all" $
            advisorySeverity (advisory [] Nothing) `shouldBe` Nothing

    describe "withdrawal" $ do
        for_ [Nothing, Just Null] $ \withdrawn ->
            it ("retains active ranges with withdrawal " <> show withdrawn) $ do
                adv <- decodeWithdrawal withdrawn
                osvWithdrawn adv `shouldBe` Nothing
                length (extractFromAdvisory noScores adv) `shouldBe` 4

        for_ ["2024-05-14T20:15:44Z", "2099-01-01T00:00:00Z"] $ \timestamp ->
            it ("omits every affected package and exact version after withdrawal " <> toString timestamp) $ do
                adv <- decodeWithdrawal (Just (String timestamp))
                osvWithdrawn adv `shouldSatisfy` isJust
                extractFromAdvisory noScores adv `shouldBe` []

        it "decodes the withdrawal timestamp" $ do
            adv <- decodeWithdrawal (Just (String "2024-05-14T20:15:44Z"))
            osvWithdrawn adv `shouldBe` Just (UTCTime (fromGregorian 2024 5 14) 72944)

        for_ [String "", String "not-a-timestamp", Number 1, Bool True, Object mempty] $ \invalid ->
            it ("rejects malformed withdrawal " <> show invalid) $ do
                bytes <- withdrawalBytes (Just invalid)
                (eitherDecodeStrict bytes :: Either String OsvAdvisory) `shouldSatisfy` isLeft

    describe "extractFromAdvisory (the EPSS join)" $ do
        let aliased ids =
                OsvAdvisory
                    "GHSA-aliased"
                    (Just ids)
                    (Just [OsvAffected (OsvPackage "aliased-pkg" "npm") Nothing (Just ["1.0.0"])])
                    Nothing
                    Nothing
                    Nothing
                    Nothing

        it "scores a GHSA-keyed advisory through its CVE alias" $
            map extEpss (extractFromAdvisory (mkEpssScores [("CVE-2026-77777", 0.5)]) (aliased ["CVE-2026-77777"]))
                `shouldBe` [Just 0.5]

        it "takes the highest score when several aliases are scored" $
            map
                extEpss
                ( extractFromAdvisory
                    (mkEpssScores [("CVE-2026-77777", 0.5), ("CVE-2026-88888", 0.75)])
                    (aliased ["CVE-2026-77777", "CVE-2026-88888"])
                )
                `shouldBe` [Just 0.75]

        it "leaves the score absent when the feed scores none of the identifiers" $
            map extEpss (extractFromAdvisory (mkEpssScores [("CVE-2026-99999", 0.5)]) (aliased ["CVE-2026-77777"]))
                `shouldBe` [Nothing]

    describe "extractFromAdvisory (package identity)" $ do
        for_ [("PyPI", "Flask_Thing", "flask-thing"), ("PyPI", "FLASK..__Thing", "flask-thing"), ("PyPI", "flask-thing", "flask-thing"), ("npm", "@Acme/Flask_Thing", "@Acme/Flask_Thing"), ("RubyGems", "Flask_Thing", "Flask_Thing"), ("other", "Flask_Thing", "Flask_Thing")] $ \(eco, raw, expected) ->
            it (toString ("keys " <> eco <> " package " <> raw <> " as " <> expected)) $ do
                let adv = OsvAdvisory "GHSA-name" Nothing (Just [OsvAffected (OsvPackage raw eco) Nothing (Just ["1.0", "2.0"])]) Nothing Nothing Nothing Nothing
                extractFromAdvisory noScores adv
                    `shouldBe` [ExtractedOsv expected eco "GHSA-name" (Just version) (LastAffected version) Nothing Nothing | version <- ["1.0", "2.0"]]

    describe "extractFromAdvisory (affected-set shapes)" $ do
        it "records an exact enumerated version as a point segment (no ranges)" $ do
            let adv = OsvAdvisory "MAL-test" Nothing (Just [OsvAffected (OsvPackage "bad-pkg" "npm") Nothing (Just ["1.0.0"])]) Nothing Nothing Nothing Nothing
            extractFromAdvisory noScores adv
                `shouldBe` [ExtractedOsv "bad-pkg" "npm" "MAL-test" (Just "1.0.0") (LastAffected "1.0.0") Nothing Nothing]

        it "carries an inclusive last_affected bound distinct from a fix" $ do
            let events = [OsvEvent (Just "0") Nothing Nothing, OsvEvent Nothing Nothing (Just "3.8.8")]
                adv = OsvAdvisory "GHSA-la" Nothing (Just [OsvAffected (OsvPackage "electerm" "npm") (Just [OsvRange "SEMVER" events]) Nothing]) Nothing Nothing Nothing Nothing
            extractFromAdvisory noScores adv
                `shouldBe` [ExtractedOsv "electerm" "npm" "GHSA-la" Nothing (LastAffected "3.8.8") Nothing Nothing]

        it "ignores a GIT range whose commit-SHA bounds are not versions" $ do
            -- A commit interpreted as an unorderable version bound would deny every release.
            let events = [OsvEvent (Just "0") Nothing Nothing, OsvEvent Nothing (Just "a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0") Nothing]
                adv = OsvAdvisory "GHSA-git" Nothing (Just [OsvAffected (OsvPackage "healthy-pkg" "npm") (Just [OsvRange "GIT" events]) Nothing]) Nothing Nothing Nothing Nothing
            extractFromAdvisory noScores adv `shouldBe` []

        it "leaves a segment unbounded above when no event closes it" $ do
            let events = [OsvEvent (Just "0") Nothing Nothing, OsvEvent (Just "2.0.0") Nothing Nothing]
                adv = OsvAdvisory "GHSA-open" Nothing (Just [OsvAffected (OsvPackage "open-pkg" "npm") (Just [OsvRange "SEMVER" events]) Nothing]) Nothing Nothing Nothing Nothing
            extractFromAdvisory noScores adv
                `shouldBe` [ ExtractedOsv "open-pkg" "npm" "GHSA-open" Nothing Unbounded Nothing Nothing
                           , ExtractedOsv "open-pkg" "npm" "GHSA-open" (Just "2.0.0") Unbounded Nothing Nothing
                           ]

        it "extracts the version range and drops a co-published GIT range" $ do
            let semverEvents = [OsvEvent (Just "0") Nothing Nothing, OsvEvent Nothing (Just "2.0.0") Nothing]
                gitEvents = [OsvEvent (Just "0") Nothing Nothing, OsvEvent Nothing (Just "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef") Nothing]
                adv =
                    OsvAdvisory
                        "GHSA-both"
                        Nothing
                        (Just [OsvAffected (OsvPackage "mixed-pkg" "npm") (Just [OsvRange "GIT" gitEvents, OsvRange "ECOSYSTEM" semverEvents]) Nothing])
                        Nothing
                        Nothing
                        Nothing
                        Nothing
            extractFromAdvisory noScores adv
                `shouldBe` [ExtractedOsv "mixed-pkg" "npm" "GHSA-both" Nothing (FixedBefore "2.0.0") Nothing Nothing]

        it "decodes OSV's \"0\" lower bound to no lower bound at all" $ do
            -- The beginning sentinel must not become an unorderable version bound.
            let events = [OsvEvent (Just "0") Nothing Nothing, OsvEvent Nothing (Just "1.2.3") Nothing]
                adv = OsvAdvisory "MAL-zero" Nothing (Just [OsvAffected (OsvPackage "mal-pkg" "npm") (Just [OsvRange "SEMVER" events]) Nothing]) Nothing Nothing Nothing Nothing
            extractFromAdvisory noScores adv
                `shouldBe` [ExtractedOsv "mal-pkg" "npm" "MAL-zero" Nothing (FixedBefore "1.2.3") Nothing Nothing]

        it "keeps an exactly enumerated \"0\" as the version it names" $ do
            -- The sentinel reading belongs to a range's lower bound. In versions[] the same
            -- text names a version a package may really carry.
            let adv = OsvAdvisory "MAL-v0" Nothing (Just [OsvAffected (OsvPackage "zero-pkg" "npm") Nothing (Just ["0"])]) Nothing Nothing Nothing Nothing
            extractFromAdvisory noScores adv
                `shouldBe` [ExtractedOsv "zero-pkg" "npm" "MAL-v0" (Just "0") (LastAffected "0") Nothing Nothing]

    it "extracts multiple packages and ranges from a complex OSV advisory" $ do
        fileBytes <- BS.readFile "test/unit/fixtures/osv/complex.json"
        let res = eitherDecodeStrict fileBytes :: Either String OsvAdvisory
        case res of
            Left err -> fail ("Failed to decode: " <> err)
            Right adv -> do
                osvId adv `shouldBe` "GHSA-multi"
                let extracted = extractFromAdvisory noScores adv
                -- The complex fixture has "database_specific": null, so every
                -- extracted range carries no severity label.
                extracted
                    `shouldBe` [ ExtractedOsv
                                    { extPackage = "multi-pkg"
                                    , extEcosystem = "npm"
                                    , extCveId = "GHSA-multi"
                                    , extIntroduced = Nothing
                                    , extUpperBound = FixedBefore "1.0.0"
                                    , extSeverity = Nothing
                                    , extEpss = Nothing
                                    }
                               , ExtractedOsv
                                    { extPackage = "multi-pkg"
                                    , extEcosystem = "npm"
                                    , extCveId = "GHSA-multi"
                                    , extIntroduced = Just "1.1.0"
                                    , extUpperBound = FixedBefore "1.2.0"
                                    , extSeverity = Nothing
                                    , extEpss = Nothing
                                    }
                               , ExtractedOsv
                                    { extPackage = "multi-pkg"
                                    , extEcosystem = "npm"
                                    , extCveId = "GHSA-multi"
                                    , extIntroduced = Just "2.0.0"
                                    , extUpperBound = FixedBefore "2.1.0"
                                    , extSeverity = Nothing
                                    , extEpss = Nothing
                                    }
                               , ExtractedOsv
                                    { extPackage = "other-pkg"
                                    , extEcosystem = "npm"
                                    , extCveId = "GHSA-multi"
                                    , extIntroduced = Nothing
                                    , extUpperBound = FixedBefore "3.0.0"
                                    , extSeverity = Nothing
                                    , extEpss = Nothing
                                    }
                               ]

    describe "orderableBounds" $ do
        let row intro upper = ExtractedOsv "pkg" "npm" "GHSA-bounds" intro upper Nothing Nothing

        it "admits a row whose bounds the ecosystem's grammar parses" $
            orderableBounds Npm (row (Just "1.0.0") (FixedBefore "2.0.0")) `shouldBe` True

        it "does not order a row whose upper bound is no version of the ecosystem" $
            -- Date-stamped and two-component bounds both ride the real npm feed. Unordered,
            -- the segment matches every version of the package.
            orderableBounds Npm (row (Just "1.0.0") (FixedBefore "2026.05.1")) `shouldBe` False

        it "does not order a point segment naming a version the grammar rejects" $
            orderableBounds PyPI (row (Just "0.1-bulbasaur") (LastAffected "0.1-bulbasaur")) `shouldBe` False

        it "admits a segment carrying no bound to order" $
            orderableBounds Npm (row Nothing Unbounded) `shouldBe` True

        it "judges each ecosystem by its own grammar" $ do
            -- "1.2" is not semver, and is a legal PEP 440 release.
            orderableBounds Npm (row (Just "1.2") Unbounded) `shouldBe` False
            orderableBounds PyPI (row (Just "1.2") Unbounded) `shouldBe` True
