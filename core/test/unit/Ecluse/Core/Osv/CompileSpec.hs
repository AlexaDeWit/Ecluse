-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

{- | Compiler regressions over committed feeds
and local HTTP stubs.
-}
module Ecluse.Core.Osv.CompileSpec (spec) where

import Codec.Compression.GZip qualified as GZip
import Conduit (runResourceT)
import Data.Aeson (decodeStrict, encode, object, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text (unpack)
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Data.Version (showVersion)
import Database.SQLite.Simple
import Katip (LogEnv, closeScribes, runKatipContextT)
import OpenTelemetry.Attributes (fromAttribute, lookupAttribute)
import OpenTelemetry.Exporter.InMemory.Span (inMemoryListExporter)
import OpenTelemetry.Trace (createTracerProvider, emptyTracerProviderOptions, forceFlushTracerProvider)
import OpenTelemetry.Trace.Core (ImmutableSpan (spanHot), SpanHot (hotAttributes, hotName, hotStatus), SpanStatus (Error, Unset))
import Paths_ecluse (version)
import System.Directory (doesFileExist, getModificationTime, listDirectory, removeFile, setModificationTime)
import System.FilePath (takeFileName, (</>))
import System.IO.Error (catchIOError)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)
import UnliftIO.Exception (finally)

import Ecluse.Core.Cve (CveDb (..), CveLookup (..), openCveDb)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Osv.Advisory (ExtractedOsv (..))
import Ecluse.Core.Osv.Compile (CompileSources (..), compileOsvToSqlite, osvToRow)
import Ecluse.Core.Osv.Ecosystem (osvEcosystemFor)
import Ecluse.Core.Osv.Provenance (QuietTime (..))
import Ecluse.Core.Osv.Schema (EpssRequirement (..), osvDbFileName, osvSchemaEpoch)
import Ecluse.Core.Osv.Stream (PilotIngestAborted (..))
import Ecluse.Core.Osv.Types (UpperBound (..))
import Ecluse.Core.Security.Authority (authorityLabel)
import Ecluse.Core.Telemetry.Metrics (
    AdvisoryCompileResult (CompileAborted, CompileCompleted),
    AdvisoryDropCause (DropMalformed, DropOversize),
 )
import Ecluse.Test.Log (captureStdout, jsonLogEnv, newTestLogEnv)
import Ecluse.Test.Osv (CorpusVersion (CorpusV1), osvCorpusZip, osvZipOf, runOsvTestM, runOsvTestMWith)
import Ecluse.Test.OsvDb (epssFixtureFile)
import Ecluse.Test.Port (RecordedCompile (RecordedCompile), recordingAdvisoryCompileMetricsPort)
import Ecluse.Test.Stub (Stub, stubBaseUrl, withStub, withStubHeaders)
import Network.HTTP.Client (applyBasicAuth, defaultRequest, requestHeaders)
import Network.HTTP.Types.Header (hLastModified)
import Network.HTTP.Types.Status (status200, status404)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)

spec :: Spec
spec = describe "SQLite OSV Compilation" $ do
    it "fetches an OSV zip and compiles it into a named, stamped SQLite artifact" $ do
        zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
        epssData <- LBS.readFile epssFixtureFile
        (metrics, readRecorded) <- recordingAdvisoryCompileMetricsPort
        (dbFile, sources) <- withStub status200 zipData $ \stub ->
            withStub status200 epssData $ \epssStub -> do
                let sources = sourcesOf stub epssStub "/sample.zip"
                path <- runOsvTestM (compileOsvToSqlite metrics Nothing "/tmp" (osvEcosystemFor Npm) sources testQuietTime)
                pure (path, sources)
        let sourceHost = authorityLabel (toText (csOsvExportUrl sources))
            epssHost = authorityLabel (toText (csEpssFeedUrl sources))

        conn <- open dbFile
        rows <- query_ conn "SELECT package_name, cve_id, fixed_version, severity, epss_score FROM package_vulnerability_ranges" :: IO [(Text, Text, Maybe Text, Maybe Double, Maybe Double)]
        stamped <- query_ conn "PRAGMA user_version" :: IO [Only Int]
        metaRows <- query_ conn "SELECT key, value FROM meta" :: IO [(Text, Text)]
        indexes <- query_ conn "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = 'package_vulnerability_ranges' AND name LIKE 'idx_%' ORDER BY name" :: IO [Only Text]
        strictTables <- query_ conn "SELECT name FROM pragma_table_list WHERE name IN ('package_vulnerability_ranges', 'meta') AND strict = 1 ORDER BY name" :: IO [Only Text]
        dedupIndexes <- query_ conn "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = 'package_vulnerability_ranges' AND name LIKE 'uq_%'" :: IO [Only Text]
        close conn
        catchIOError (removeFile dbFile) (const $ pure ())

        -- The file-name literal and the meta keys below pin the artifact's wire contract, the forms
        -- a reader depends on, not the constants that produced them.
        takeFileName dbFile `shouldBe` "npm-osv-schema4.db"
        -- EPSS joins through CVE-2024-48913, while the row retains the GHSA id.
        rows `shouldBe` [("hono", "GHSA-2234-fmw7-43wr", Just "4.6.5", Just 5.9, Just 0.75)]
        map fromOnly stamped `shouldBe` [osvSchemaEpoch]
        map fromOnly indexes `shouldBe` ["idx_package_fixed", "idx_package_name"]
        map fromOnly strictTables `shouldBe` ["meta", "package_vulnerability_ranges"]
        map fromOnly dedupIndexes `shouldBe` ["uq_ranges_segment"]

        let meta = Map.fromList metaRows
        -- The stubs answer with no Last-Modified, so those two keys write no row at all
        -- rather than an invented date.
        Map.keys meta
            `shouldBe` [ "built_at"
                       , "ecosystem"
                       , "epss_model_version"
                       , "epss_score_date"
                       , "epss_source"
                       , "epss_source_url"
                       , "epss_status"
                       , "osv_newest_modified"
                       , "osv_source"
                       , "pilot_version"
                       , "row_count"
                       , "source_url"
                       ]
        Map.lookup "ecosystem" meta `shouldBe` Just "npm"
        Map.lookup "row_count" meta `shouldBe` Just "1"
        Map.lookup "pilot_version" meta `shouldBe` Just (toText (showVersion version))
        Map.lookup "source_url" meta `shouldBe` Just sourceHost
        Map.lookup "epss_source_url" meta `shouldBe` Just epssHost
        -- These stub URLs carry no credential, so the recorded identity is the URL as written.
        Map.lookup "osv_source" meta `shouldBe` Just (toText (csOsvExportUrl sources))
        Map.lookup "epss_source" meta `shouldBe` Just (toText (csEpssFeedUrl sources))
        Map.lookup "osv_newest_modified" meta `shouldBe` Just "2026-03-23T17:41:30.891186Z"
        Map.lookup "epss_score_date" meta `shouldBe` Just "2026-08-29T00:00:00Z"
        Map.lookup "epss_model_version" meta `shouldBe` Just "v2026.08.01"
        Map.lookup "epss_status" meta `shouldBe` Just "available"
        Map.lookup "built_at" meta `shouldSatisfy` maybe False (not . T.null)

        recorded <- readRecorded
        recorded `shouldBe` RecordedCompile [1] [(DropOversize, 0), (DropMalformed, 0)] [CompileCompleted]

    for_ [("matching", "CVE-2024-48913", Just 0.75), ("unmatched", "CVE-2026-10001", Nothing)] $ \(label, cveId, expectedScore) ->
        it ("records available enrichment without feed dates for " <> label <> " scores") $
            withSystemTempDirectory "epss-status" $ \outDir -> do
                zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
                let epssData = GZip.compress ("cve,epss,percentile\n" <> cveId <> ",0.75,0.9\n")
                (metrics, _) <- recordingAdvisoryCompileMetricsPort
                dbFile <- withStub status200 zipData $ \stub ->
                    withStub status200 epssData $ \epssStub ->
                        runOsvTestM (compileOsvToSqlite metrics Nothing outDir (osvEcosystemFor Npm) (sourcesOf stub epssStub "/sample.zip") testQuietTime)
                meta <- metaOf dbFile
                Map.lookup "epss_status" meta `shouldBe` Just "available"
                for_ ["epss_last_modified", "epss_score_date", "epss_model_version"] $ \key ->
                    Map.lookup key meta `shouldBe` Nothing
                withConnection dbFile $ \conn -> do
                    scores <- query_ conn "SELECT epss_score FROM package_vulnerability_ranges" :: IO [Only (Maybe Double)]
                    map fromOnly scores `shouldBe` [expectedScore]
                openCveDb Npm EpssRequired dbFile >>= \case
                    Left rejection -> fail ("EPSS-stamped artifact rejected: " <> show rejection)
                    Right db ->
                        flip finally (cveDbClose db) $
                            cveCoveredNames (cveDbLookup db) `shouldReturn` ["hono"]

    it "fetches both credential-bearing overrides without persisting or logging their credentials" $ do
        zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
        epssData <- LBS.readFile epssFixtureFile
        (metrics, _) <- recordingAdvisoryCompileMetricsPort
        (dbFile, logged) <- captureStdout' $ \logEnv ->
            withCredentialSource "OSV" zipData $ \source ->
                withCredentialSource "EPSS" epssData $ \epssSource -> do
                    path <- runOsvTestMWith logEnv (compileOsvToSqlite metrics Nothing "/tmp" (osvEcosystemFor Npm) (CompileSources source epssSource) testQuietTime)
                    withConnection path $ \conn -> do
                        meta <- Map.fromList <$> (query_ conn "SELECT key, value FROM meta" :: IO [(Text, Text)])
                        Map.lookup "source_url" meta `shouldBe` Just (authorityLabel (toText source))
                        Map.lookup "epss_source_url" meta `shouldBe` Just (authorityLabel (toText epssSource))
                        -- The recorded identity keeps the path that names the source, and none
                        -- of the credential material the fetch had to send.
                        for_ [("osv_source", "OSV"), ("epss_source", "EPSS")] $ \(key, tag) -> do
                            let stored = fromMaybe "" (Map.lookup key meta)
                            stored `shouldSatisfy` T.isSuffixOf "/feed"
                            for_ ["user-", "password-", "query-", "fragment-"] $ \prefix ->
                                stored `shouldSatisfy` (not . T.isInfixOf (prefix <> tag))
                        Map.lookup "built_at" meta `shouldSatisfy` maybe False (not . T.null)
                        Map.lookup "row_count" meta `shouldBe` Just "1"
                    pure path
        bytes <- readFileBS dbFile
        for_ ["OSV", "EPSS"] $ \tag ->
            for_ ["user-", "password-", "query-", "fragment-"] $ \prefix -> do
                let credential = prefix <> tag
                bytes `shouldSatisfy` (not . BS.isInfixOf (encodeUtf8 credential))
                logged `shouldSatisfy` (not . T.isInfixOf credential)
        removeFile dbFile

    it "aborts the compile without publishing when the drop rate is systemic" $ do
        -- 20 malformed entries to one good one trips the systemic-drop breaker. The breaker must
        -- abandon the run rather than finalise an artifact that silently omits most advisories.
        zipData <- systemicDropZip
        epssData <- LBS.readFile epssFixtureFile
        (metrics, readRecorded) <- recordingAdvisoryCompileMetricsPort
        let action =
                withStub status200 zipData $ \stub ->
                    withStub status200 epssData $ \epssStub ->
                        runOsvTestM (compileOsvToSqlite metrics Nothing "/tmp" (osvEcosystemFor Npm) (sourcesOf stub epssStub "/all.zip") testQuietTime)
        action `shouldThrow` (\(PilotIngestAborted _) -> True)

        recorded <- readRecorded
        recorded `shouldBe` RecordedCompile [1] [(DropOversize, 0), (DropMalformed, 20)] [CompileAborted]

    it "accepts a rebuilt PyPI artifact with canonical names and raw fix versions" $ do
        zipData <-
            osvZipOf
                [("pypi-advisory.json", "{\"id\":\"GHSA-pypi\",\"affected\":[{\"package\":{\"name\":\"Flask_Thing\",\"ecosystem\":\"PyPI\"},\"ranges\":[{\"type\":\"ECOSYSTEM\",\"events\":[{\"introduced\":\"0\"},{\"fixed\":\"1.0.0\"}]}]}]}")]
        epssData <- LBS.readFile epssFixtureFile
        (metrics, _) <- recordingAdvisoryCompileMetricsPort
        dbFile <- withStub status200 zipData $ \stub ->
            withStub status200 epssData $ \epssStub ->
                runOsvTestM (compileOsvToSqlite metrics Nothing "/tmp" (osvEcosystemFor PyPI) (sourcesOf stub epssStub "/all.zip") testQuietTime)

        conn <- open dbFile
        rows <- query_ conn "SELECT package_name FROM package_vulnerability_ranges" :: IO [Only Text]
        metaRows <- query_ conn "SELECT key, value FROM meta" :: IO [(Text, Text)]
        close conn

        map fromOnly rows `shouldBe` ["flask-thing"]
        takeFileName dbFile `shouldBe` "pypi-osv-schema4.db"
        Map.lookup "ecosystem" (Map.fromList metaRows) `shouldBe` Just "pypi"
        Map.lookup "epss_status" (Map.fromList metaRows) `shouldBe` Just "available"
        openCveDb PyPI EpssRequired dbFile >>= \case
            Left rejection -> fail ("rebuilt PyPI artifact rejected: " <> show rejection)
            Right db -> flip finally (cveDbClose db) $ do
                let cve = cveDbLookup db
                cveCoveredNames cve `shouldReturn` ["flask-thing"]
                cveRemediationProbe cve "flask-thing" "1.0.0" `shouldReturn` True
                cveRemediationProbe cve "flask-thing" "1.0" `shouldReturn` False
                cveAdvisoriesFor cve "flask-thing" >>= (`shouldSatisfy` (not . null))
        removeFile dbFile

    it "writes an unorderable bound into the artifact and decodes the \"0\" lower bound" $ do
        -- Malware feeds can name versions outside semver. Dropping them would admit affected versions.
        zipData <-
            osvZipOf
                [ ("point.json", "{\"id\":\"MAL-point\",\"affected\":[{\"package\":{\"name\":\"pointy\",\"ecosystem\":\"npm\"},\"versions\":[\"1.0.0\",\"2026.05.1\"]}]}")
                , ("range.json", "{\"id\":\"MAL-range\",\"affected\":[{\"package\":{\"name\":\"ranged\",\"ecosystem\":\"npm\"},\"ranges\":[{\"type\":\"SEMVER\",\"events\":[{\"introduced\":\"0\"},{\"fixed\":\"1.2.3\"}]}]}]}")
                ]
        epssData <- LBS.readFile epssFixtureFile
        (metrics, _) <- recordingAdvisoryCompileMetricsPort
        (dbFile, logged) <- captureStdout' $ \logEnv ->
            withStub status200 zipData $ \stub ->
                withStub status200 epssData $ \epssStub ->
                    runOsvTestMWith logEnv (compileOsvToSqlite metrics Nothing "/tmp" (osvEcosystemFor Npm) (sourcesOf stub epssStub "/all.zip") testQuietTime)

        logged `shouldSatisfy` T.isInfixOf "for example pointy 2026.05.1"
        logged `shouldSatisfy` T.isInfixOf "kept 1 unorderable"
        logged `shouldSatisfy` (not . T.isInfixOf "\"sev\":\"Error\"")

        conn <- open dbFile
        rows <- query_ conn "SELECT package_name, introduced_version, fixed_version, last_affected_version FROM package_vulnerability_ranges ORDER BY package_name, introduced_version" :: IO [(Text, Maybe Text, Maybe Text, Maybe Text)]
        close conn
        catchIOError (removeFile dbFile) (const $ pure ())

        rows
            `shouldBe` [ ("pointy", Just "1.0.0", Nothing, Just "1.0.0")
                       , ("pointy", Just "2026.05.1", Nothing, Just "2026.05.1")
                       , ("ranged", Nothing, Just "1.2.3", Nothing)
                       ]

    it "fails the pass when the EPSS feed answers non-2xx, so nothing reaches the export" $ do
        -- A 404 is permanent, so the fetch gives up at once rather than spending the backoff
        -- budget. The compile throws before it writes meta, and the caller's upload never runs.
        zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
        (metrics, _) <- recordingAdvisoryCompileMetricsPort
        let action =
                withStub status200 zipData $ \stub ->
                    withStub status404 LBS.empty $ \epssStub ->
                        runOsvTestM (compileOsvToSqlite metrics Nothing "/tmp" (osvEcosystemFor Npm) (sourcesOf stub epssStub "/all.zip") testQuietTime)
        action `shouldThrow` anyException

    for_ [("empty", osvZipOf []), ("wrong-ecosystem", LBS.readFile "test/unit/fixtures/osv/sample.zip")] $ \(label, rejectedZip) ->
        for_ [False, True] $ \hasPrevious ->
            it ("refuses " <> label <> " output with ERROR and preserves publication state, previous=" <> show hasPrevious) $
                withSystemTempDirectory "ecluse-zero-output" $ \outDir -> do
                    epssData <- LBS.readFile epssFixtureFile
                    goodZip <- osvCorpusZip CorpusV1
                    badZip <- rejectedZip
                    (metrics, readRecorded) <- recordingAdvisoryCompileMetricsPort
                    let dbFile = outDir </> osvDbFileName "pypi"
                        previousModified = UTCTime (fromGregorian 2020 1 1) 0
                        compile logEnv zipData = withStub status200 zipData $ \stub ->
                            withStub status200 epssData $ \epssStub ->
                                runOsvTestMWith logEnv (compileOsvToSqlite metrics Nothing outDir (osvEcosystemFor PyPI) (sourcesOf stub epssStub "/all.zip") testQuietTime)
                    previous <-
                        if hasPrevious
                            then do
                                (path, _) <- captureStdout' (`compile` goodZip)
                                setModificationTime path previousModified
                                getModificationTime path >>= (`shouldBe` previousModified)
                                Just <$> readFileBS path
                            else pure Nothing
                    (_, logged) <- captureStdout' $ \logEnv ->
                        compile logEnv badZip `shouldThrow` (\(PilotIngestAborted _) -> True)
                    logged `shouldSatisfy` T.isInfixOf "zero relevant advisory rows"
                    logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Error\""
                    recorded <- readRecorded
                    case recorded of
                        RecordedCompile _ _ verdicts -> verdicts `shouldBe` ([CompileCompleted | hasPrevious] <> [CompileAborted])
                    case previous of
                        Nothing -> do
                            doesFileExist dbFile >>= (`shouldBe` False)
                            listDirectory outDir >>= (`shouldBe` [])
                        Just bytes -> do
                            readFileBS dbFile >>= (`shouldBe` bytes)
                            getModificationTime dbFile >>= (`shouldBe` previousModified)
                            listDirectory outDir >>= (`shouldBe` [takeFileName dbFile])

    describe "compile traces"
        $ for_
            [ ("accepted", Npm, LBS.readFile "test/unit/fixtures/osv/sample.zip", Nothing, 1, 0)
            , ("empty", Npm, osvZipOf [], Just "zero relevant advisory rows, compile abandoned", 0, 0)
            , ("wrong ecosystem", PyPI, LBS.readFile "test/unit/fixtures/osv/sample.zip", Just "zero relevant advisory rows, compile abandoned", 1, 0)
            , ("systemic drops", Npm, systemicDropZip, Just "systemic advisory drop rate, compile abandoned", 1, 20)
            ]
        $ \(label, ecosystem, zipSource, refusal, accepted, malformed) ->
            it ("records the " <> label <> " verdict without source credentials") $
                withSystemTempDirectory "ecluse-compile-trace" $ \outDir -> do
                    zipData <- zipSource
                    epssData <- LBS.readFile epssFixtureFile
                    (metrics, _) <- recordingAdvisoryCompileMetricsPort
                    (processor, spansRef) <- inMemoryListExporter
                    tracerProvider <- createTracerProvider [processor] emptyTracerProviderOptions
                    withCredentialSource "OSV" zipData $ \source ->
                        withCredentialSource "EPSS" epssData $ \epssSource -> do
                            let compile = compileOsvToSqlite metrics (Just tracerProvider) outDir (osvEcosystemFor ecosystem) (CompileSources source epssSource) testQuietTime
                                runCompile logEnv = runKatipContextT logEnv () mempty (runResourceT compile)
                            (_, logged) <- captureStdout' $ \logEnv -> case refusal of
                                Nothing -> void (runCompile logEnv)
                                Just _ -> runCompile logEnv `shouldThrow` (\(PilotIngestAborted _) -> True)
                            let prefix = if isNothing refusal then "Compiled " else "Aborting OSV compile "
                                -- jsonLogEnv uses Katip's "msg" field.
                                summaries = filter (maybe False (T.isPrefixOf prefix) . parseMaybe (.: "msg")) (mapMaybe (decodeStrict . encodeUtf8) (T.lines logged))
                                ecosystemText = if ecosystem == Npm then "npm" else "pypi" :: Text
                                fields =
                                    [ "ecosystem" .= ecosystemText
                                    , "accepted" .= (accepted :: Int)
                                    , "dropped_oversize" .= (0 :: Int)
                                    , "dropped_malformed" .= (malformed :: Int)
                                    , "unorderable" .= (0 :: Int)
                                    ]
                                        <> ["row_count" .= (1 :: Int) | isNothing refusal]
                            length summaries `shouldBe` 1
                            for_ summaries $ \loggedObject -> do
                                parseMaybe (.: "data") loggedObject `shouldBe` Just (object fields)
                                parseMaybe (.: "sev") loggedObject `shouldBe` Just (if isNothing refusal then "Info" else "Error" :: Text)
                            _ <- forceFlushTracerProvider tracerProvider Nothing
                            spans <- readIORef spansRef >>= traverse (readIORef . spanHot)
                            let compiled = filter ((== "ecluse.pilot.osv.compile") . hotName) spans
                            map hotStatus compiled `shouldBe` [maybe Unset Error refusal]
                            for_ compiled $ \compiledSpan -> do
                                for_ ["user-OSV", "password-OSV", "query-OSV", "fragment-OSV"] $ \credential ->
                                    show (hotAttributes compiledSpan) `shouldSatisfy` (not . T.isInfixOf credential)
                                for_
                                    [ ("ecluse.osv.ecosystem", Just (if ecosystem == Npm then "npm" else "pypi"))
                                    , ("ecluse.osv.source_host", Just (authorityLabel (toText source)))
                                    , ("ecluse.osv.accepted", Just (show accepted))
                                    , ("ecluse.osv.dropped_oversize", Just "0")
                                    , ("ecluse.osv.dropped_malformed", Just (show malformed))
                                    , ("ecluse.osv.unorderable", Just "0")
                                    , ("ecluse.osv.row_count", if isNothing refusal then Just "1" else Nothing)
                                    ]
                                    $ \(key, expected) ->
                                        (lookupAttribute (hotAttributes compiledSpan) key >>= fromAttribute) `shouldBe` (expected :: Maybe Text)

    describe "source provenance" $ do
        it "records the export's Last-Modified from the response that carried the rows" $ do
            zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
            epssData <- LBS.readFile epssFixtureFile
            (metrics, _) <- recordingAdvisoryCompileMetricsPort
            dbFile <- withStubHeaders status200 [(hLastModified, "Sat, 29 Aug 2026 06:30:00 GMT")] zipData $ \stub ->
                withStub status200 epssData $ \epssStub ->
                    runOsvTestM (compileOsvToSqlite metrics Nothing "/tmp" (osvEcosystemFor Npm) (sourcesOf stub epssStub "/all.zip") testQuietTime)
            meta <- metaOf dbFile
            Map.lookup "osv_last_modified" meta `shouldBe` Just "2026-08-29T06:30:00Z"
            removeFile dbFile

        it "keeps the newest record date and skips a record that carries none" $ do
            zipData <-
                osvZipOf
                    [ ("older.json", datedAdvisory "GHSA-older" "older-pkg" (Just "2026-01-01T00:00:00Z"))
                    , ("newer.json", datedAdvisory "GHSA-newer" "newer-pkg" (Just "2026-02-01T09:00:00Z"))
                    , ("undated.json", datedAdvisory "GHSA-undated" "undated-pkg" Nothing)
                    ]
            dbFile <- compileZip zipData testQuietTime
            meta <- metaOf dbFile
            Map.lookup "osv_newest_modified" meta `shouldBe` Just "2026-02-01T09:00:00Z"
            removeFile dbFile

        it "keeps a record dated after the run's clock and ignores only its date" $ do
            -- A date the source cannot yet know is not evidence about the ranges beside it,
            -- so the rows stay and the age reading takes the newest date the run can trust.
            zipData <-
                osvZipOf
                    [ ("ok.json", datedAdvisory "GHSA-ok" "ok-pkg" (Just "2026-01-01T00:00:00Z"))
                    , ("ahead.json", datedAdvisory "GHSA-ahead" "ahead-pkg" (Just "2099-01-01T00:00:00Z"))
                    ]
            (dbFile, logged) <- captureStdout' $ \logEnv -> compileZipWith logEnv zipData testQuietTime
            meta <- metaOf dbFile
            Map.lookup "osv_newest_modified" meta `shouldBe` Just "2026-01-01T00:00:00Z"
            packagesOf dbFile `shouldReturn` ["ahead-pkg", "ok-pkg"]
            logged `shouldSatisfy` T.isInfixOf "Ignoring the modified date of 1 npm advisory record(s), unreadable or dated after this run's clock"
            logged `shouldSatisfy` (not . T.isInfixOf "\"sev\":\"Error\"")
            removeFile dbFile

        it "keeps a record whose modified date no grammar reads, and ignores only that date" $ do
            zipData <-
                osvZipOf
                    [ ("ok.json", datedAdvisory "GHSA-ok" "ok-pkg" (Just "2026-01-01T00:00:00Z"))
                    , ("unreadable.json", datedAdvisory "GHSA-unreadable" "unreadable-pkg" (Just "the day before yesterday"))
                    ]
            (dbFile, logged) <- captureStdout' $ \logEnv -> compileZipWith logEnv zipData testQuietTime
            meta <- metaOf dbFile
            Map.lookup "osv_newest_modified" meta `shouldBe` Just "2026-01-01T00:00:00Z"
            packagesOf dbFile `shouldReturn` ["ok-pkg", "unreadable-pkg"]
            logged `shouldSatisfy` T.isInfixOf "Ignoring the modified date of 1 npm advisory record(s), unreadable or dated after this run's clock"
            logged `shouldSatisfy` (not . T.isInfixOf "\"sev\":\"Error\"")
            removeFile dbFile

        it "logs the quiet-time alarm at ERROR, naming the source, the age, and the threshold" $ do
            zipData <- osvZipOf [("old.json", datedAdvisory "GHSA-old" "old-pkg" (Just "2026-01-01T00:00:00Z"))]
            (dbFile, logged) <- captureStdout' $ \logEnv -> compileZipWith logEnv zipData tightQuietTime
            logged `shouldSatisfy` T.isInfixOf "OSV export http://127.0.0.1:"
            logged `shouldSatisfy` T.isInfixOf "quiet-time threshold 1s"
            logged `shouldSatisfy` T.isInfixOf "so the source has gone quiet"
            logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Error\""
            removeFile dbFile

        it "logs the ages at INFO and raises nothing while every source is inside its threshold" $ do
            zipData <- osvZipOf [("old.json", datedAdvisory "GHSA-old" "old-pkg" (Just "2026-01-01T00:00:00Z"))]
            (dbFile, logged) <- captureStdout' $ \logEnv -> compileZipWith logEnv zipData testQuietTime
            logged `shouldSatisfy` T.isInfixOf "last changed "
            logged `shouldSatisfy` (not . T.isInfixOf "so the source has gone quiet")
            logged `shouldSatisfy` (not . T.isInfixOf "\"sev\":\"Error\"")
            removeFile dbFile

    describe "osvToRow" $ do
        let rowFor upper = osvToRow (ExtractedOsv "pkg" "npm" "GHSA-row" (Just "1.0.0") upper (Just 5.9) (Just 0.25))

        it "writes an exclusive bound to fixed_version and leaves last_affected_version null" $
            rowFor (FixedBefore "2.0.0") `shouldBe` ("pkg", "GHSA-row", Just "1.0.0", Just "2.0.0", Nothing, Just 5.9, Just 0.25)

        it "writes an inclusive bound to last_affected_version and leaves fixed_version null" $
            rowFor (LastAffected "2.0.0") `shouldBe` ("pkg", "GHSA-row", Just "1.0.0", Nothing, Just "2.0.0", Just 5.9, Just 0.25)

        it "leaves both bound columns null for a segment with no upper bound" $
            rowFor Unbounded `shouldBe` ("pkg", "GHSA-row", Just "1.0.0", Nothing, Nothing, Just 5.9, Just 0.25)

        it "carries an unscored segment's null epss_score through" $
            osvToRow (ExtractedOsv "pkg" "npm" "GHSA-row" Nothing Unbounded Nothing Nothing)
                `shouldBe` ("pkg", "GHSA-row", Nothing, Nothing, Nothing, Nothing, Nothing)

systemicDropZip :: IO LByteString
systemicDropZip =
    osvZipOf
        ( [("mal-" <> show i <> ".json", "this is not valid json") | i <- [1 .. 20 :: Int]]
            <> [("good.json", "{\"id\":\"GHSA-ok\",\"affected\":[{\"package\":{\"name\":\"ok\",\"ecosystem\":\"npm\"},\"versions\":[\"1.0.0\"]}]}")]
        )

captureStdout' :: (LogEnv -> IO a) -> IO (a, Text)
captureStdout' body = do
    resultRef <- newIORef Nothing
    logged <- captureStdout $ do
        logEnv <- jsonLogEnv
        body logEnv >>= writeIORef resultRef . Just
        void (closeScribes logEnv)
    result <- readIORef resultRef
    maybe (fail "the compile under capture produced no result") (pure . (,logged)) result

sourcesOf :: Stub -> Stub -> String -> CompileSources
sourcesOf osvStub epssStub osvPath =
    CompileSources
        { csOsvExportUrl = unpack (stubBaseUrl osvStub) <> osvPath
        , csEpssFeedUrl = unpack (stubBaseUrl epssStub) <> "/epss.csv.gz"
        }

-- The shared stub omits query strings, so this fixture checks authentication before serving bytes.
withCredentialSource :: Text -> LByteString -> (String -> IO a) -> IO a
withCredentialSource tag bytes use =
    testWithApplication (pure app) $ \port ->
        use (toString ("http://user-" <> tag <> ":password-" <> tag <> "@127.0.0.1:" <> show port <> "/feed?unfamiliar=query-" <> tag <> "#fragment-" <> tag))
  where
    expectedAuth = lookup "Authorization" (requestHeaders (applyBasicAuth (encodeUtf8 ("user-" <> tag)) (encodeUtf8 ("password-" <> tag)) defaultRequest))
    app request respond = do
        let authorised =
                lookup "Authorization" (Wai.requestHeaders request) == expectedAuth
                    && Wai.rawQueryString request == encodeUtf8 ("?unfamiliar=query-" <> tag)
                    && Wai.rawPathInfo request == "/feed"
        respond (Wai.responseLBS (if authorised then status200 else status404) [] (if authorised then bytes else "credentials missing"))

-- A century, so a committed fixture's own date never ages into the quiet-time alarm in a test
-- that is about something else.
testQuietTime :: QuietTime
testQuietTime = QuietTime{qtOsv = century, qtEpss = century}
  where
    century = 100 * 365 * 86400

tightQuietTime :: QuietTime
tightQuietTime = QuietTime{qtOsv = 1, qtEpss = 1}

datedAdvisory :: Text -> Text -> Maybe Text -> LByteString
datedAdvisory advisoryId pkg mModified =
    encode
        ( object
            ( [ "id" .= advisoryId
              , "affected" .= [object ["package" .= object ["name" .= pkg, "ecosystem" .= ("npm" :: Text)], "versions" .= ["1.0.0" :: Text]]]
              ]
                <> maybe [] (\modified -> ["modified" .= modified]) mModified
            )
        )

compileZip :: LByteString -> QuietTime -> IO FilePath
compileZip zipData quietTime = newTestLogEnv >>= \logEnv -> compileZipWith logEnv zipData quietTime

compileZipWith :: LogEnv -> LByteString -> QuietTime -> IO FilePath
compileZipWith logEnv zipData quietTime = do
    epssData <- LBS.readFile epssFixtureFile
    (metrics, _) <- recordingAdvisoryCompileMetricsPort
    withStub status200 zipData $ \stub ->
        withStub status200 epssData $ \epssStub ->
            runOsvTestMWith logEnv (compileOsvToSqlite metrics Nothing "/tmp" (osvEcosystemFor Npm) (sourcesOf stub epssStub "/all.zip") quietTime)

metaOf :: FilePath -> IO (Map Text Text)
metaOf dbFile = withConnection dbFile $ \conn ->
    Map.fromList <$> (query_ conn "SELECT key, value FROM meta" :: IO [(Text, Text)])

packagesOf :: FilePath -> IO [Text]
packagesOf dbFile = withConnection dbFile $ \conn ->
    map fromOnly <$> (query_ conn "SELECT package_name FROM package_vulnerability_ranges ORDER BY package_name" :: IO [Only Text])
