-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

-- | Decoding one OSV record, and the rows and bounds it extracts to.
module Ecluse.Core.Osv.AdvisorySpec (spec) where

import Prelude hiding (universe)

import Codec.Compression.GZip qualified as GZip
import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.JsonStream.Parser qualified as J
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Data.Universe.Class (Universe (universe))
import Database.SQLite.Simple (Only (..), execute, executeMany, execute_, query, query_, withConnection, withTransaction)
import Hedgehog (Gen, evalIO, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory (copyFile, listDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Cve (AdvisoryRange (..), CveDb (cveDbLookup), CveLookup (cveAdvisoriesFor, cveCoveredNames), packageAdvisories)
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems), ecosystemName)
import Ecluse.Core.Osv.Advisory
import Ecluse.Core.Osv.Compile (osvToRow)
import Ecluse.Core.Osv.Ecosystem (osvEcosystemFor, osvExportDirectory)
import Ecluse.Core.Osv.Epss (EpssScores, epssForIds, mkEpssScores, parseEpssLine)
import Ecluse.Core.Osv.Types (UpperBound (..))
import Ecluse.Core.Package (canonicalise, mkPackageName)
import Ecluse.Core.Registry.PyPI.Project (fcVersionKey, fileCoordinate)
import Ecluse.Core.Rules (AdvisoryRows, VerdictSource (..), readAdvisories, verdictSource)
import Ecluse.Core.Rules.Types (DenyIfCveParams (..), DenyIfEpssParams (..), EvalContext (..), FailureAlignment (FailDeny), Rule (..), RuleVerdict, completeEvidence)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage), captureTexts, corpusPackages, cpName, pypiCorpusPackages)
import Ecluse.Test.Corpus.Advisories (AdvisoryInputs (..), compileAdvisoryInputs, corpusAdvisories)
import Ecluse.Test.Osv (noScores, osvZipOf)
import Ecluse.Test.Osv.Withdrawal (withdrawalBytes)
import Ecluse.Test.OsvDb (metaOf, withServedArtifact)
import Ecluse.Test.Package (sampleDetails)
import Ecluse.Test.Rules (servingRuleDeps)
import Ecluse.Test.Version (genGem, genNpm, genPyPI)

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
                                    , extSeverity = Just 5.9
                                    , extEpss = Nothing
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
            advisorySeverity
                (advisory [OsvSeverityEntry "CVSS_V4" "CVSS:4.0/AV:N/AC:L/AT:N/PR:N/UI:N/VC:H/VI:H/VA:H/SC:N/SI:N/SA:N"] Nothing)
                `shouldBe` Just 9.3

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
            let events = [OsvEvent (Just "0") Nothing Nothing, OsvEvent Nothing (Just "1.2.3") Nothing]
                adv = OsvAdvisory "MAL-zero" Nothing (Just [OsvAffected (OsvPackage "mal-pkg" "npm") (Just [OsvRange "SEMVER" events]) Nothing]) Nothing Nothing Nothing Nothing
            extractFromAdvisory noScores adv
                `shouldBe` [ExtractedOsv "mal-pkg" "npm" "MAL-zero" Nothing (FixedBefore "1.2.3") Nothing Nothing]

        it "keeps an exactly enumerated \"0\" as the version it names" $ do
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

    describe "covered enumerated versions" $ do
        for_ coverCases $ \(label, eco, introduced, upper, versions, kept) ->
            it label $ do
                let adv = rangedAdvisory eco introduced upper versions
                    points = drop 1 (extractFromAdvisory noScores adv)
                map extIntroduced points `shouldBe` map Just kept
                verdicts eco "pkg" (extractFromAdvisory noScores adv) versions
                    `shouldBe` verdicts eco "pkg" (withoutDrop noScores adv) versions

        it "uses ranges in another entry of the same advisory and canonical package" $ do
            let adv = rangedAdvisory PyPI Nothing (FixedBefore "2") ["1"]
                entries = fromMaybe [] (osvAffected adv)
                separated = adv{osvAffected = Just [part{affectedPackage = if isNothing (affectedRanges part) then OsvPackage "PKG" "PyPI" else affectedPackage part} | part <- concatMap splitAffected entries]}
            length (extractFromAdvisory noScores separated) `shouldBe` 1
            let other = [aff{affectedPackage = OsvPackage "other" "PyPI"} | aff <- entries]
            length (extractFromAdvisory noScores adv{osvAffected = Just (other <> [OsvAffected (OsvPackage "pkg" "PyPI") Nothing (Just ["1"])])}) `shouldBe` 2

        it "keeps every point for an unknown ecosystem" $ do
            let adv = rangedAdvisory PyPI Nothing Unbounded ["1", "bogus"]
                unknown aff = aff{affectedPackage = OsvPackage "pkg" "unknown"}
            length (extractFromAdvisory noScores adv{osvAffected = map unknown <$> osvAffected adv}) `shouldBe` 3

        it "preserves the real reader's ordered verdicts for the review probe" $ do
            let records = reviewProbe
                scoreRows = [("CVE-A", 0.75), ("CVE-B", 0.75)]
                before = concatMap (withoutDrop (mkEpssScores scoreRows)) records
            inputs <- generatedInputs records scoreRows
            withDifferentialArtifacts PyPI before inputs $ \beforePath afterPath -> do
                for_ [beforePath, afterPath] $ \path ->
                    readerPlan path "pkg" >>= \plan -> putStrLn (path <> " reader plan: " <> show plan)
                compareArtifacts PyPI beforePath afterPath [("pkg", ["1", "2", "3", "bogus"])]

        it "preserves duplicate ids with mixed GIT, invalid and valid ranges through artifacts" $ do
            let scoreRows = [("CVE-A", 0.75), ("CVE-B", 0.75)]
                duplicate = mixedAdvisory
                queries = ordNub ("1" : "bogus" : concatMap advisoryVersions (reviewProbe <> [duplicate]))
            for_ [reviewProbe <> [duplicate], duplicate : reverse reviewProbe] $ \records -> do
                inputs <- generatedInputs records scoreRows
                withDifferentialArtifacts PyPI (concatMap (withoutDrop (mkEpssScores scoreRows)) records) inputs $ \beforePath afterPath ->
                    compareArtifacts PyPI beforePath afterPath [("pkg", queries)]

        for_ [(Npm, genNpm), (PyPI, genPyPI), (RubyGems, genGem)] $ \(eco, genVersion) ->
            it ("preserves generated rule verdicts and ordered advisory ids for " <> show eco) $
                hedgehog $ do
                    advs <- forAll (Gen.list (Range.linear 1 5) (generatedAdvisory eco genVersion))
                    query <- forAll genVersion
                    let identified = zipWith (\n adv -> adv{osvId = "CVE-generated-" <> show ((n :: Int) `mod` 3)}) [0 ..] advs
                        scoreRows = [(osvId adv, fromIntegral n / 4) | (n, adv) <- zip [0 :: Int ..] identified]
                        scores = mkEpssScores scoreRows
                        before = concatMap (withoutDrop scores) identified
                        after = concatMap (extractFromAdvisory scores) identified
                        queries = ordNub (query : "bogus" : concatMap advisoryVersions identified)
                    verdicts eco "pkg" after queries === verdicts eco "pkg" before queries
                    evalIO $ do
                        inputs <- generatedInputs identified scoreRows
                        withDifferentialArtifacts eco before inputs $ \beforePath afterPath ->
                            compareArtifacts eco beforePath afterPath [("pkg", queries)]

        for_ [Npm, PyPI] $ \eco ->
            it ("preserves all captured rule verdicts and ordered advisory ids for " <> show eco) $ do
                advs <- corpusRecords eco
                feed <- BS.readFile "bench/corpus/advisories/epss.csv"
                let scores = mkEpssScores (mapMaybe parseEpssLine (BS.split 10 feed))
                    before = concatMap (withoutDrop scores) advs
                    after = concatMap (extractFromAdvisory scores) advs
                    packages = if eco == Npm then corpusPackages else pypiCorpusPackages
                inputs <- corpusAdvisories eco
                queries <- forM packages $ \package -> do
                    served <- captureVersions eco package
                    served `shouldSatisfy` (not . null)
                    let name = cpName package
                        rows = filter ((== name) . extPackage)
                        versions = ordNub (served <> concatMap advisoryVersions advs <> ["bogus"])
                    verdicts eco name (rows after) versions `shouldBe` verdicts eco name (rows before) versions
                    pure (name, versions)
                withDifferentialArtifacts eco before inputs $ \beforePath afterPath -> do
                    compareArtifacts eco beforePath afterPath queries
                    when (eco == Npm) (unchangedNpmArtifact beforePath afterPath)
                let compiledRows = filter ((== osvExportDirectory (osvEcosystemFor eco)) . extEcosystem)
                when (eco == Npm) (compiledRows after `shouldBe` compiledRows before)
                compareForeignRecords eco advs scores inputs

    describe "orderableBounds" $ do
        let row intro upper = ExtractedOsv "pkg" "npm" "GHSA-bounds" intro upper Nothing Nothing

        it "admits a row whose bounds the ecosystem's grammar parses" $
            orderableBounds Npm (row (Just "1.0.0") (FixedBefore "2.0.0")) `shouldBe` True

        it "does not order a row whose upper bound is no version of the ecosystem" $
            orderableBounds Npm (row (Just "1.0.0") (FixedBefore "2026.05.1")) `shouldBe` False

        it "does not order a point segment naming a version the grammar rejects" $
            orderableBounds PyPI (row (Just "0.1-bulbasaur") (LastAffected "0.1-bulbasaur")) `shouldBe` False

        it "admits a segment carrying no bound to order" $
            orderableBounds Npm (row Nothing Unbounded) `shouldBe` True

        it "judges each ecosystem by its own grammar" $ do
            -- "1.2" is not semver, and is a legal PEP 440 release.
            orderableBounds Npm (row (Just "1.2") Unbounded) `shouldBe` False
            orderableBounds PyPI (row (Just "1.2") Unbounded) `shouldBe` True

coverCases :: [(String, Ecosystem, Maybe Text, UpperBound, [Text], [Text])]
coverCases =
    [ ("drops covered PEP 440 spellings", PyPI, Just "1.0", FixedBefore "2.0", ["0.9", "1", "1.0.0", "1.5", "2.0"], ["0.9", "2.0"])
    , ("keeps an exclusive fix", Npm, Nothing, FixedBefore "2.0.0", ["1.0.0", "2.0.0"], ["2.0.0"])
    , ("drops an inclusive last affected version", Npm, Just "1.0.0", LastAffected "2.0.0", ["0.9.0", "1.0.0", "2.0.0", "2.1.0"], ["0.9.0", "2.1.0"])
    , ("keeps an unorderable lower bound", PyPI, Just "bad", FixedBefore "2", ["1"], ["1"])
    , ("keeps an unorderable upper bound", PyPI, Nothing, LastAffected "bad", ["1"], ["1"])
    , ("keeps an unorderable exact version", PyPI, Nothing, Unbounded, ["bad", "1"], ["bad"])
    , ("keeps an unorderable npm zero point", Npm, Nothing, Unbounded, ["0", "1.0.0"], ["0"])
    , ("keeps a gap outside an unbounded range", PyPI, Just "2", Unbounded, ["1", "2", "3"], ["1"])
    , ("keeps every point beside a reversed range", PyPI, Just "3", FixedBefore "1", ["1", "2", "3"], ["1", "2", "3"])
    , ("orders semver prereleases and ignores build metadata", Npm, Just "1.0.0-rc.1", FixedBefore "1.0.0", ["1.0.0-beta.1", "1.0.0-rc.1+build", "1.0.0"], ["1.0.0-beta.1", "1.0.0"])
    , ("orders PEP 440 epochs and local versions", PyPI, Just "1!1.0rc1", FixedBefore "1!2", ["1.9", "1!1.0rc1", "1!1+local", "1!2"], ["1.9", "1!2"])
    , ("orders RubyGems zero padding", RubyGems, Just "1", LastAffected "2", ["0.9", "1.0", "2.0.0", "2.1"], ["0.9", "2.1"])
    ]

rangedAdvisory :: Ecosystem -> Maybe Text -> UpperBound -> [Text] -> OsvAdvisory
rangedAdvisory eco introduced upper versions =
    OsvAdvisory "CVE-test" Nothing (Just [OsvAffected (OsvPackage "pkg" (osvExportDirectory (osvEcosystemFor eco))) (Just [OsvRange "ECOSYSTEM" events]) (Just versions)]) Nothing Nothing Nothing Nothing
  where
    events =
        OsvEvent (Just (fromMaybe "0" introduced)) Nothing Nothing : case upper of
            FixedBefore fixed -> [OsvEvent Nothing (Just fixed) Nothing]
            LastAffected lastAffected -> [OsvEvent Nothing Nothing (Just lastAffected)]
            Unbounded -> []

generatedAdvisory :: Ecosystem -> Gen Text -> Gen OsvAdvisory
generatedAdvisory eco genVersion = do
    let bound = Gen.choice [genVersion, Gen.element ["0", "bad"]]
    lower <- Gen.maybe bound
    upper <- Gen.choice [FixedBefore <$> bound, LastAffected <$> bound, pure Unbounded]
    versions <- Gen.list (Range.linear 1 8) bound
    extra <- genVersion
    let adv = rangedAdvisory eco lower upper (extra : versions)
        pointRange = rangedAdvisory eco (Just extra) (LastAffected extra) []
    severity <- Gen.element [Nothing, Just (OsvDatabaseSpecific (Just "LOW")), Just (OsvDatabaseSpecific (Just "CRITICAL"))]
    pure adv{osvAffected = Just (fromMaybe [] (osvAffected pointRange) <> fromMaybe [] (osvAffected adv)), osvDatabaseSpecific = severity}

splitAffected :: OsvAffected -> [OsvAffected]
splitAffected aff = [aff{affectedVersions = Nothing}, aff{affectedRanges = Nothing}]

-- Frozen from 1a53a01b: extraction, point formation, range selection and event folding.
withoutDrop :: EpssScores -> OsvAdvisory -> [ExtractedOsv]
withoutDrop scores adv = do
    guard (isNothing (osvWithdrawn adv))
    aff <- fromMaybe [] (osvAffected adv)
    let pkg = affectedPackage aff
        eco = find ((== packageEcosystem pkg) . osvExportDirectory . osvEcosystemFor) universe
        name = maybe id canonicalise eco (packageName pkg)
    BaseSegment intro upper <- baseAffectedSegments aff
    pure $
        ExtractedOsv
            { extPackage = name
            , extEcosystem = packageEcosystem pkg
            , extCveId = osvId adv
            , extIntroduced = intro
            , extUpperBound = upper
            , extSeverity = severity
            , extEpss = epss
            }
  where
    severity = advisorySeverity adv
    epss = epssForIds scores (osvId adv : fromMaybe [] (osvAliases adv))

data BaseSegment = BaseSegment (Maybe Text) UpperBound

baseRangeSegment :: Maybe Text -> UpperBound -> BaseSegment
baseRangeSegment introduced = BaseSegment (introduced >>= beyondTheBeginning)
  where
    beyondTheBeginning i = if i == "0" then Nothing else Just i

baseAffectedSegments :: OsvAffected -> [BaseSegment]
baseAffectedSegments aff =
    maybe [] (concatMap (baseExtractRange . rangeEvents) . filter versionTyped) (affectedRanges aff)
        <> maybe [] (map exactVersion) (affectedVersions aff)
  where
    exactVersion v = BaseSegment (Just v) (LastAffected v)

    versionTyped :: OsvRange -> Bool
    versionTyped r = T.toUpper (T.strip (rangeType r)) `elem` ["SEMVER", "ECOSYSTEM"]

baseExtractRange :: [OsvEvent] -> [BaseSegment]
baseExtractRange = go Nothing
  where
    go Nothing [] = []
    go (Just i) [] = [baseRangeSegment (Just i) Unbounded]
    go current (e : es)
        | Just i <- eventIntroduced e =
            case current of
                Just prev -> baseRangeSegment (Just prev) Unbounded : go (Just i) es
                Nothing -> go (Just i) es
        | Just f <- eventFixed e = baseRangeSegment current (FixedBefore f) : go Nothing es
        | Just la <- eventLastAffected e = baseRangeSegment current (LastAffected la) : go Nothing es
        | otherwise = go current es

advisoryVersions :: OsvAdvisory -> [Text]
advisoryVersions adv = maybe [] (concatMap versions) (osvAffected adv)
  where
    versions aff = fromMaybe [] (affectedVersions aff) <> maybe [] (concatMap (concatMap bounds . rangeEvents)) (affectedRanges aff)
    bounds event = catMaybes [eventIntroduced event, eventFixed event, eventLastAffected event]

-- Complete verdict equality includes the ids and their order in both deny and remediation reasons.
verdicts :: Ecosystem -> Text -> [ExtractedOsv] -> [Text] -> [[RuleVerdict]]
verdicts eco name rows = verdictsFor eco name (Just (DbEtag "differential", advisories))
  where
    advisories = packageAdvisories eco [AdvisoryRange (extCveId row) (extSeverity row) (extIntroduced row) (extUpperBound row) (extEpss row) | row <- rows]

verdictsFor :: Ecosystem -> Text -> AdvisoryRows -> [Text] -> [[RuleVerdict]]
verdictsFor eco name advisories versions = map decide rules
  where
    evidence = [completeEvidence (sampleDetails (mkPackageName eco Nothing name) (mkVersion eco version)) | version <- versions]
    context = EvalContext (UTCTime (fromGregorian 2026 1 1) 0) Nothing
    rules = AllowIfRemediatesCve : [DenyIfCve (DenyIfCveParams threshold FailDeny) | threshold <- [0, 8, 10]] <> [DenyIfEpss (DenyIfEpssParams threshold FailDeny) | threshold <- [0, 0.5, 1]]
    decide rule = case verdictSource rule of
        FromAdvisories _ verdict -> map (verdict advisories) evidence
        FromEvidence verdict -> map (verdict context) evidence

corpusRecords :: Ecosystem -> IO [OsvAdvisory]
corpusRecords eco = do
    let dir = "bench/corpus/advisories" </> toString (ecosystemName eco)
    files <- sort . filter ((== ".json") . takeExtension) <$> listDirectory dir
    traverse (\file -> BS.readFile (dir </> file) >>= either fail pure . eitherDecodeStrict) files

captureVersions :: Ecosystem -> CorpusPackage -> IO [Text]
captureVersions eco package = case eco of
    PyPI -> do
        files <- concat <$> captureTexts 1 ("files" J..: J.arrayOf (many ("filename" J..: J.string))) package
        pure (ordNub (mapMaybe (fmap fcVersionKey . fileCoordinate (cpPackage package)) files))
    _ -> ordNub . concat <$> captureTexts 1 ("versions" J..: J.objectValues (many ("version" J..: J.string))) package

reviewProbe :: [OsvAdvisory]
reviewProbe =
    [ (rangedAdvisory PyPI Nothing (FixedBefore "3") ["1"]){osvId = "CVE-A", osvDatabaseSpecific = Just (OsvDatabaseSpecific (Just "CRITICAL"))}
    , (rangedAdvisory PyPI Nothing (FixedBefore "2") []){osvId = "CVE-B", osvDatabaseSpecific = Just (OsvDatabaseSpecific (Just "CRITICAL"))}
    ]

mixedAdvisory :: OsvAdvisory
mixedAdvisory =
    (rangedAdvisory PyPI (Just "1") (LastAffected "2") ["1", "2", "3", "bogus"])
        { osvId = "CVE-A"
        , osvDatabaseSpecific = Just (OsvDatabaseSpecific (Just "LOW"))
        , osvAffected = Just [OsvAffected (OsvPackage "pkg" "PyPI") (Just ranges) (Just ["1", "2", "3", "bogus"])]
        }
  where
    ranges =
        [ OsvRange "GIT" [OsvEvent (Just "0") Nothing Nothing, OsvEvent Nothing (Just "deadbeef") Nothing]
        , OsvRange "ECOSYSTEM" [OsvEvent (Just "bad") Nothing Nothing, OsvEvent Nothing (Just "4") Nothing]
        , OsvRange "ECOSYSTEM" [OsvEvent (Just "1") Nothing Nothing, OsvEvent Nothing Nothing (Just "2")]
        ]

generatedInputs :: [OsvAdvisory] -> [(Text, Double)] -> IO AdvisoryInputs
generatedInputs records scores = do
    for_ records $ \adv ->
        eitherDecodeStrict (LBS.toStrict (encode (advisoryValue adv))) `shouldBe` Right adv
    archive <- osvZipOf [("record-" <> show (n :: Int) <> ".json", encode (advisoryValue adv)) | (n, adv) <- zip [0 ..] records]
    let feed = "#model_version:synthetic,score_date:2026-01-01T00:00:00+0000\ncve,epss,percentile\n" <> foldMap (\(cve, score) -> cve <> "," <> show score <> ",0.5\n") scores
    pure (AdvisoryInputs archive (GZip.compress (LBS.fromStrict (encodeUtf8 feed))))

advisoryValue :: OsvAdvisory -> Value
advisoryValue adv =
    object
        [ "id" .= osvId adv
        , "aliases" .= osvAliases adv
        , "withdrawn" .= osvWithdrawn adv
        , "modified" .= osvModified adv
        , "database_specific" .= fmap (\specific -> object ["severity" .= dbsSeverity specific]) (osvDatabaseSpecific adv)
        , "severity" .= fmap (map (\entry -> object ["type" .= sevType entry, "score" .= sevScore entry])) (osvSeverity adv)
        , "affected" .= fmap (map affectedValue) (osvAffected adv)
        ]
  where
    affectedValue aff =
        object
            [ "package" .= object ["name" .= packageName (affectedPackage aff), "ecosystem" .= packageEcosystem (affectedPackage aff)]
            , "versions" .= affectedVersions aff
            , "ranges" .= fmap (map rangeValue) (affectedRanges aff)
            ]
    rangeValue range = object ["type" .= rangeType range, "events" .= map eventValue (rangeEvents range)]
    eventValue event = object ["introduced" .= eventIntroduced event, "fixed" .= eventFixed event, "last_affected" .= eventLastAffected event]

-- Copy the compiler's schema, indexes and provenance. Only the frozen rows and their count differ.
withDifferentialArtifacts :: Ecosystem -> [ExtractedOsv] -> AdvisoryInputs -> (FilePath -> FilePath -> IO a) -> IO a
withDifferentialArtifacts eco beforeRows inputs use =
    withSystemTempDirectory "ecluse-advisory-differential" $ \dir -> do
        afterPath <- compileAdvisoryInputs eco dir inputs
        let beforePath = dir </> "before.db"
            rows = filter ((== osvExportDirectory (osvEcosystemFor eco)) . extEcosystem) beforeRows
        copyFile afterPath beforePath
        withConnection beforePath $ \conn -> withTransaction conn $ do
            execute_ conn "DELETE FROM package_vulnerability_ranges"
            executeMany
                conn
                "INSERT OR IGNORE INTO package_vulnerability_ranges (package_name, cve_id, introduced_version, fixed_version, last_affected_version, severity, epss_score) VALUES (?, ?, ?, ?, ?, ?, ?)"
                (map osvToRow rows)
            counts <- query_ conn "SELECT COUNT(*) FROM package_vulnerability_ranges" :: IO [Only Int]
            for_ counts $ \(Only count) -> execute conn "UPDATE meta SET value = ? WHERE key = 'row_count'" (Only (show count :: Text))
        beforeMeta <- Map.delete "row_count" <$> metaOf beforePath
        afterMeta <- Map.delete "row_count" <$> metaOf afterPath
        beforeMeta `shouldBe` afterMeta
        use beforePath afterPath

compareArtifacts :: Ecosystem -> FilePath -> FilePath -> [(Text, [Text])] -> IO ()
compareArtifacts eco beforePath afterPath queries =
    withServedArtifact eco beforePath $ \_ beforeDb ->
        withServedArtifact eco afterPath $ \_ afterDb ->
            for_ queries $ \(name, versions) -> do
                let beforeLookup = cveDbLookup beforeDb
                    afterLookup = cveDbLookup afterDb
                    package = mkPackageName eco Nothing name
                    deps = servingRuleDeps (DbEtag "differential")
                before <- readAdvisories (deps beforeLookup) package
                after <- readAdvisories (deps afterLookup) package
                verdictsFor eco name after versions `shouldBe` verdictsFor eco name before versions
                beforeRows <- cveAdvisoriesFor beforeLookup name
                afterRows <- cveAdvisoriesFor afterLookup name
                putStrLn (toString name <> " persisted advisory rows: " <> show (length beforeRows) <> " -> " <> show (length afterRows))

readerPlan :: FilePath -> Text -> IO [(Int, Int, Int, Text)]
readerPlan path name = withConnection path $ \conn ->
    query conn "EXPLAIN QUERY PLAN SELECT cve_id, introduced_version, fixed_version, last_affected_version, severity, epss_score FROM package_vulnerability_ranges WHERE package_name = ?" (Only name)

compareForeignRecords :: Ecosystem -> [OsvAdvisory] -> EpssScores -> AdvisoryInputs -> IO ()
compareForeignRecords compiledEco records scores inputs =
    for_ [Npm, PyPI, RubyGems] $ \eco -> when (eco /= compiledEco) $ do
        let rows = filter ((== osvExportDirectory (osvEcosystemFor eco)) . extEcosystem)
            before = rows (concatMap (withoutDrop scores) records)
            after = rows (concatMap (extractFromAdvisory scores) records)
            names = ordNub (map extPackage before)
            versions = ordNub ("bogus" : concatMap advisoryVersions records)
        for_ names $ \name ->
            verdicts eco name (filter ((== name) . extPackage) after) versions
                `shouldBe` verdicts eco name (filter ((== name) . extPackage) before) versions
        unless (null names) $
            withDifferentialArtifacts eco before inputs $ \beforePath afterPath ->
                compareArtifacts eco beforePath afterPath [(name, versions) | name <- names]

unchangedNpmArtifact :: FilePath -> FilePath -> IO ()
unchangedNpmArtifact beforePath afterPath =
    withServedArtifact Npm beforePath $ \_ beforeDb ->
        withServedArtifact Npm afterPath $ \_ afterDb -> do
            let before = cveDbLookup beforeDb
                after = cveDbLookup afterDb
            names <- cveCoveredNames before
            cveCoveredNames after >>= (`shouldBe` names)
            for_ names $ \name -> do
                beforeRows <- cveAdvisoriesFor before name
                afterRows <- cveAdvisoriesFor after name
                afterRows `shouldBe` beforeRows
