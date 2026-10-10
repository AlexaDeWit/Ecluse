-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

-- | Decoding one OSV record, and the rows and bounds it extracts to.
module Ecluse.Core.Osv.AdvisorySpec (spec) where

import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Cve (AdvisoryRange (..), packageAdvisories)
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems), ecosystemName)
import Ecluse.Core.Osv.Advisory
import Ecluse.Core.Osv.Ecosystem (osvEcosystemFor, osvExportDirectory)
import Ecluse.Core.Osv.Epss (EpssScores, mkEpssScores, parseEpssLine)
import Ecluse.Core.Osv.Types (UpperBound (..))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Registry.PyPI.Project (fcVersionKey, fileCoordinate)
import Ecluse.Core.Rules (VerdictSource (..), verdictSource)
import Ecluse.Core.Rules.Types (DenyIfCveParams (..), DenyIfEpssParams (..), EvalContext (..), FailureAlignment (FailDeny), Rule (..), RuleVerdict, completeEvidence)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage), captureTexts, corpusPackages, cpName, pypiCorpusPackages)
import Ecluse.Test.Osv (noScores)
import Ecluse.Test.Osv.Withdrawal (withdrawalBytes)
import Ecluse.Test.Package (sampleDetails)
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

        for_ [(Npm, genNpm), (PyPI, genPyPI), (RubyGems, genGem)] $ \(eco, genVersion) ->
            it ("preserves generated rule verdicts and ordered advisory ids for " <> show eco) $
                hedgehog $ do
                    advs <- forAll (Gen.list (Range.linear 1 5) (generatedAdvisory eco genVersion))
                    query <- forAll genVersion
                    let identified = zipWith (\n adv -> adv{osvId = "CVE-generated-" <> show (n :: Int)}) [0 ..] advs
                        scores = mkEpssScores [(osvId adv, fromIntegral n / 4) | (n, adv) <- zip [0 :: Int ..] identified]
                        before = concatMap (withoutDrop scores) identified
                        after = concatMap (extractFromAdvisory scores) identified
                        queries = ordNub (query : "bogus" : concatMap advisoryVersions identified)
                    verdicts eco "pkg" after queries === verdicts eco "pkg" before queries

        for_ [Npm, PyPI] $ \eco ->
            it ("preserves all captured rule verdicts and ordered advisory ids for " <> show eco) $ do
                advs <- corpusRecords eco
                feed <- BS.readFile "bench/corpus/advisories/epss.csv"
                let scores = mkEpssScores (mapMaybe parseEpssLine (BS.split 10 feed))
                    before = concatMap (withoutDrop scores) advs
                    after = concatMap (extractFromAdvisory scores) advs
                    packages = if eco == Npm then corpusPackages else pypiCorpusPackages
                for_ packages $ \package -> do
                    served <- captureVersions eco package
                    served `shouldSatisfy` (not . null)
                    let name = cpName package
                        rows = filter ((== name) . extPackage)
                        versions = ordNub (served <> concatMap advisoryVersions advs <> ["bogus"])
                    verdicts eco name (rows after) versions `shouldBe` verdicts eco name (rows before) versions
                    putStrLn (toString name <> " advisory rows: " <> show (length (rows before)) <> " -> " <> show (length (rows after)))
                when (eco == Npm) (after `shouldBe` before)

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

-- Each extraction sees either ranges or points, so this reference cannot invoke the cover test.
withoutDrop :: EpssScores -> OsvAdvisory -> [ExtractedOsv]
withoutDrop scores adv = concatMap (\aff -> extractFromAdvisory scores adv{osvAffected = Just [aff]}) (maybe [] (concatMap splitAffected) (osvAffected adv))

advisoryVersions :: OsvAdvisory -> [Text]
advisoryVersions adv = maybe [] (concatMap versions) (osvAffected adv)
  where
    versions aff = fromMaybe [] (affectedVersions aff) <> maybe [] (concatMap (concatMap bounds . rangeEvents)) (affectedRanges aff)
    bounds event = catMaybes [eventIntroduced event, eventFixed event, eventLastAffected event]

-- Complete verdict equality includes the ids and their order in both deny and remediation reasons.
verdicts :: Ecosystem -> Text -> [ExtractedOsv] -> [Text] -> [[RuleVerdict]]
verdicts eco name rows versions = map decide rules
  where
    advisories = packageAdvisories eco [AdvisoryRange (extCveId row) (extSeverity row) (extIntroduced row) (extUpperBound row) (extEpss row) | row <- rows]
    evidence = [completeEvidence (sampleDetails (mkPackageName eco Nothing name) (mkVersion eco version)) | version <- versions]
    context = EvalContext (UTCTime (fromGregorian 2026 1 1) 0) Nothing
    rules = AllowIfRemediatesCve : [DenyIfCve (DenyIfCveParams threshold FailDeny) | threshold <- [0, 8, 10]] <> [DenyIfEpss (DenyIfEpssParams threshold FailDeny) | threshold <- [0, 0.5, 1]]
    decide rule = case verdictSource rule of
        FromAdvisories _ verdict -> map (verdict (Just (DbEtag "differential", advisories))) evidence
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
