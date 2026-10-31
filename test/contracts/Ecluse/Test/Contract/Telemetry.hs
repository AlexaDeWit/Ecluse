-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Correlated telemetry and continued service when an exporter fails.
module Ecluse.Test.Contract.Telemetry (
    TrafficDriver (..),
    ReadTelemetryDriver (..),
    TelemetryMode (..),
    TelemetrySession (..),
    readTelemetryContract,
    LogDriver (..),
    CollectorDriver (..),
    MirrorTelemetryDriver (..),
    MirrorTelemetryFixture (..),
    PrivateFetchKind (..),
    mirrorTelemetryContract,
    TelemetryIdentity (..),
    exportsSignals,
    exportsDomainSpans,
    exportsPrivateFetch,
    logsWithoutCollector,
    servesAfterExportFailure,
    exportsIdentity,
    hasPopulatedTraceId,
) where

import Data.Aeson (Value (Object, String), decodeStrict)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isDigit)
import Data.Text qualified as T
import Numeric (readHex)
import Test.Hspec (Expectation, SpecWith, it, shouldBe, shouldReturn, shouldSatisfy)

import Ecluse.Test.Contract.Catalogue (caseLabel)

import Ecluse.Test.Collector
import Ecluse.Test.Contract.Install (ClientResult, shouldSucceed)

-- | Exporter configurations required for read-only service conformance.
data TelemetryMode = EnabledTelemetry | DisabledTelemetry | FailedExporter | IdentityTelemetry
    deriving stock (Eq, Show)

-- | Operations scoped to one configured running service.
data TelemetrySession = TelemetrySession
    { sessionTraffic :: TrafficDriver
    , sessionLogs :: LogDriver
    , sessionCollector :: CollectorDriver
    }

-- | Acquire a service per configuration, keeping native clients and fixtures in the driver.
data ReadTelemetryDriver = ReadTelemetryDriver
    { withReadTelemetry :: TelemetryMode -> (TelemetrySession -> IO ()) -> IO ()
    , readTelemetryPackage :: Text
    , readTelemetrySecondPackage :: Text
    , readTelemetryIdentityPackage :: Text
    , readTelemetryIdentity :: TelemetryIdentity
    }

-- | All ecosystems must export signals and retain service without a working exporter.
readTelemetryContract :: Text -> SpecWith ReadTelemetryDriver
readTelemetryContract prefix = do
    it (label "signals" "exports metrics and a span") $ \driver ->
        withReadTelemetry driver EnabledTelemetry $ \session ->
            exportsSignals (readTelemetryPackage driver) (sessionTraffic session) (sessionCollector session)
    it (label "disabled" "serves and logs without a collector") $ \driver ->
        withReadTelemetry driver DisabledTelemetry $ \session ->
            logsWithoutCollector (readTelemetryPackage driver) (sessionTraffic session) (sessionLogs session)
    it (label "export-failure" "warns and keeps serving after export failure") $ \driver ->
        withReadTelemetry driver FailedExporter $ \session ->
            servesAfterExportFailure (readTelemetryPackage driver) (readTelemetrySecondPackage driver) (sessionTraffic session) (sessionLogs session)
    it (label "datadog" "exports the configured identity and log correlation") $ \driver ->
        withReadTelemetry driver IdentityTelemetry $ \session ->
            exportsIdentity (readTelemetryIdentityPackage driver) (readTelemetryIdentity driver) (sessionTraffic session) (sessionLogs session) (sessionCollector session)
  where
    label name = caseLabel (prefix <> "/" <> name)

-- | A fresh real-client request and the process readiness endpoint.
data TrafficDriver = TrafficDriver
    { telemetryInstall :: Text -> IO ClientResult
    , telemetryReadiness :: IO Int
    }

-- | Bounded observation of process logs, returning the last evidence even on timeout.
newtype LogDriver = LogDriver
    { telemetryLogs :: (Text -> Bool) -> Int -> IO Text
    }

-- | Bounded observation of actual collector output.
newtype CollectorDriver = CollectorDriver
    { telemetryCollected :: (Text -> Bool) -> Int -> IO Text
    }

-- | Exact inventory reads and a bounded wait for worker completion.
data MirrorTelemetryDriver = MirrorTelemetryDriver
    { telemetryVersions :: Text -> IO [Text]
    , telemetryAwaitVersion :: Text -> Text -> IO [Text]
    }

-- | A native private miss occurs at an archive or at the index needed to locate it.
data PrivateFetchKind = ConventionalArtifact | IndexedLookup

-- | Independent coordinates for worker spans and private miss-to-hit observations.
data MirrorTelemetryFixture = MirrorTelemetryFixture
    { mirrorTelemetryTraffic :: TrafficDriver
    , mirrorTelemetryStore :: MirrorTelemetryDriver
    , mirrorTelemetryCollector :: CollectorDriver
    , mirrorTelemetryPackage :: Text
    , mirrorTelemetryVersion :: Text
    , mirrorTelemetryPrivatePackage :: Text
    , mirrorTelemetryPrivateVersion :: Text
    , mirrorTelemetryHost :: Text
    , mirrorTelemetryTarget :: Text
    , mirrorTelemetryPrivateFetch :: PrivateFetchKind
    }

-- | Write-capable ecosystems must correlate worker activity and subsequent private fetches.
mirrorTelemetryContract :: Text -> SpecWith MirrorTelemetryFixture
mirrorTelemetryContract prefix = do
    it (caseLabel (prefix <> "/domain-spans") "correlates rule, enqueue and worker spans") $ \fixture ->
        exportsDomainSpans
            (mirrorTelemetryPackage fixture)
            (mirrorTelemetryVersion fixture)
            (mirrorTelemetryTraffic fixture)
            (mirrorTelemetryStore fixture)
            (mirrorTelemetryCollector fixture)
    it (caseLabel (prefix <> "/private-fetch") "records the private miss and subsequent success") $ \fixture ->
        exportsPrivateFetch
            (mirrorTelemetryPrivateFetch fixture)
            (mirrorTelemetryPrivatePackage fixture)
            (mirrorTelemetryPrivateVersion fixture)
            (mirrorTelemetryHost fixture)
            (mirrorTelemetryTarget fixture)
            (mirrorTelemetryTraffic fixture)
            (mirrorTelemetryStore fixture)
            (mirrorTelemetryCollector fixture)

-- | Expected resource and log identity from the configured telemetry provider.
data TelemetryIdentity = TelemetryIdentity
    { telemetryService :: Text
    , telemetryEnvironment :: Text
    , telemetryVersion :: Text
    }

-- | Both metric and trace signals must reach the configured collector.
exportsSignals :: Text -> TrafficDriver -> CollectorDriver -> Expectation
exportsSignals package traffic collector = do
    void (telemetryInstall traffic package >>= shouldSucceed)
    logs <- telemetryCollected collector signals 80
    logs `shouldSatisfy` signals
  where
    signals logs = "ecluse.serve.decision" `T.isInfixOf` logs && "Span #" `T.isInfixOf` logs

{- | All domain spans must identify this scenario's own coordinate. The rule evaluation and the
enqueue share one trace, and the worker's job span links back to that trace.
-}
exportsDomainSpans :: Text -> Text -> TrafficDriver -> MirrorTelemetryDriver -> CollectorDriver -> Expectation
exportsDomainSpans package version traffic mirror collector = do
    void (telemetryInstall traffic package >>= shouldSucceed)
    versions <- telemetryAwaitVersion mirror package version
    (version `elem` versions) `shouldBe` True
    logs <- telemetryCollected collector emitted 120
    logs `shouldSatisfy` emitted
  where
    emitted logs =
        let spans = printedSpans logs
            named name = filter (spanFor package version name . psSpan) spans
         in not $
                null
                    [ traceKey
                    | enqueue <- named "ecluse.mirror.enqueue"
                    , Just traceKey <- [psTraceId enqueue]
                    , any ((== Just traceKey) . psTraceId) (named "ecluse.rule.eval")
                    , any (elem traceKey . psLinkTraceIds) (named "ecluse.mirror.job")
                    ]

-- | A fresh second install records a successful private fetch after the initial miss.
exportsPrivateFetch :: PrivateFetchKind -> Text -> Text -> Text -> Text -> TrafficDriver -> MirrorTelemetryDriver -> CollectorDriver -> Expectation
exportsPrivateFetch kind package version host target traffic mirror collector = do
    versions <- telemetryVersions mirror package
    (version `elem` versions) `shouldBe` False
    void (telemetryInstall traffic package >>= shouldSucceed)
    stored <- telemetryAwaitVersion mirror package version
    (version `elem` stored) `shouldBe` True
    missed <- telemetryCollected collector (elem (IntValue 404) . answers) 80
    ordNub (answers missed) `shouldBe` [IntValue 404]
    void (telemetryInstall traffic package >>= shouldSucceed)
    answered <- telemetryCollected collector (elem (IntValue 200) . answers) 80
    ordNub (answers answered) `shouldBe` [IntValue 404, IntValue 200]
  where
    answers = case kind of
        ConventionalArtifact -> privateArtifactAnswers host target . exportedSpans
        IndexedLookup -> privateLookupAnswers host target

-- | Logging and client service remain available without a collector.
logsWithoutCollector :: Text -> TrafficDriver -> LogDriver -> Expectation
logsWithoutCollector package traffic logs = do
    void (telemetryInstall traffic package >>= shouldSucceed)
    observed <- telemetryLogs logs (T.isInfixOf "\"message\":") 80
    observed `shouldSatisfy` T.isInfixOf "\"message\":"

-- | Export failure produces a warning without losing readiness or fresh-client service.
servesAfterExportFailure :: Text -> Text -> TrafficDriver -> LogDriver -> Expectation
servesAfterExportFailure firstPackage secondPackage traffic logs = do
    logsWithoutCollector firstPackage traffic logs
    warned <- telemetryLogs logs (T.isInfixOf "telemetry export error") 80
    warned `shouldSatisfy` T.isInfixOf "telemetry export error"
    telemetryReadiness traffic `shouldReturn` 200
    void (telemetryInstall traffic secondPackage >>= shouldSucceed)

{- | Configured resource identity reaches the collector, and a log line under that identity
carries the Datadog form of an exported span's trace and span IDs.
-}
exportsIdentity :: Text -> TelemetryIdentity -> TrafficDriver -> LogDriver -> CollectorDriver -> Expectation
exportsIdentity package expectedIdentity traffic logs collector = do
    void (telemetryInstall traffic package >>= shouldSucceed)
    logged <- loggedSpans <$> telemetryLogs logs (not . null . loggedSpans) 80
    logged `shouldSatisfy` (not . null)
    collected <- telemetryCollected collector (correlated logged) 80
    collected `shouldSatisfy` correlated logged
  where
    correlated logged output = carriesIdentity output && any (`elem` logged) (mapMaybe datadogIds (printedSpans output))
    carriesIdentity output =
        all
            (`T.isInfixOf` output)
            [ "-> service.name: Str(" <> telemetryService expectedIdentity <> ")"
            , "-> deployment.environment.name: Str(" <> telemetryEnvironment expectedIdentity <> ")"
            , "-> service.version: Str(" <> telemetryVersion expectedIdentity <> ")"
            ]
    loggedSpans output =
        [ (traceKey, spanKey)
        | line <- lines output
        , Just (Object fields) <- [decodeStrict (encodeUtf8 (T.dropWhile (/= '{') line))]
        , all (\(key, expected) -> KeyMap.lookup key fields == Just (String expected)) identityFields
        , Just (Object dd) <- [KeyMap.lookup "dd" fields]
        , Just (String traceKey) <- [KeyMap.lookup "trace_id" dd]
        , Just (String spanKey) <- [KeyMap.lookup "span_id" dd]
        ]
    identityFields =
        [ ("service", telemetryService expectedIdentity)
        , ("env", telemetryEnvironment expectedIdentity)
        , ("version", telemetryVersion expectedIdentity)
        ]

{- | A printed span's IDs as Datadog logs them: the low 64 bits of the trace ID and the span ID,
each as an unsigned decimal.
-}
datadogIds :: PrintedSpan -> Maybe (Text, Text)
datadogIds printed = (,) <$> (decimal . T.takeEnd 16 =<< psTraceId printed) <*> (decimal =<< psSpanId printed)
  where
    decimal hex = case readHex (toString hex) of
        [(value, "")] -> Just (show (value :: Word64))
        _ -> Nothing

-- | A trace identifier must contain a leading digit rather than an absent or empty value.
hasPopulatedTraceId :: Text -> Bool
hasPopulatedTraceId logs = any leadsWithDigit (drop 1 (T.splitOn "\"trace_id\":\"" logs))
  where
    leadsWithDigit segment = maybe False (isDigit . fst) (T.uncons segment)
