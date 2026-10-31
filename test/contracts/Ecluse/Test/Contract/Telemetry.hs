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
    logsWithoutCollector,
    hasPopulatedTraceId,
) where

import Data.Char (isDigit)
import Data.Text qualified as T
import Test.Hspec (Expectation, SpecWith, it, shouldBe, shouldNotSatisfy, shouldReturn, shouldSatisfy)

import Ecluse.Test.Contract.Catalogue (caseLabel)

import Ecluse.Test.Collector
import Ecluse.Test.Contract.Install (ClientResult, shouldSucceed)
import Ecluse.Test.Log (lineField, lineMessage)

{- | Exporter configurations required for read-only service conformance. 'DisabledTelemetry' turns
telemetry off beside a reachable collector, so an ignored switch shows as exported signals.
-}
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
            exportsNothingWhenDisabled (readTelemetryPackage driver) (sessionTraffic session) (sessionLogs session) (sessionCollector session)
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

-- Both metric and trace signals must reach the configured collector.
exportsSignals :: Text -> TrafficDriver -> CollectorDriver -> Expectation
exportsSignals package traffic collector = do
    void (telemetryInstall traffic package >>= shouldSucceed)
    logs <- telemetryCollected collector signals 80
    logs `shouldSatisfy` signals
  where
    signals logs = "ecluse.serve.decision" `T.isInfixOf` logs && "Span #" `T.isInfixOf` logs

{- The coordinate's rule evaluation and enqueue share the serving request's trace, and its mirror
job links back to that enqueue span.
-}
exportsDomainSpans :: Text -> Text -> TrafficDriver -> MirrorTelemetryDriver -> CollectorDriver -> Expectation
exportsDomainSpans package version traffic mirror collector = do
    void (telemetryInstall traffic package >>= shouldSucceed)
    versions <- telemetryAwaitVersion mirror package version
    (version `elem` versions) `shouldBe` True
    logs <- telemetryCollected collector (not . null . correlated) 120
    (correlated logs, [esName exported | exported <- exportedSpans logs, any (\name -> spanFor package version name exported) domainSpans])
        `shouldSatisfy` (not . null . fst)
  where
    correlated = linkedMirrorTraces package version
    domainSpans = ["ecluse.rule.eval", "ecluse.mirror.enqueue", "ecluse.mirror.job"]

-- A fresh second install records a successful private fetch after the initial miss.
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

{- With telemetry off, a fresh client is served, its request is logged, and nothing reaches the
collector the exporter could have reached, over several export intervals.
-}
exportsNothingWhenDisabled :: Text -> TrafficDriver -> LogDriver -> CollectorDriver -> Expectation
exportsNothingWhenDisabled package traffic logs collector = do
    void (telemetryInstall traffic package >>= shouldSucceed)
    logged <- telemetryLogs logs namesPackage 80
    logged `shouldSatisfy` namesPackage
    exported <- telemetryCollected collector anySignal 40
    exported `shouldNotSatisfy` anySignal
    warned <- telemetryLogs logs exportError 1
    warned `shouldNotSatisfy` exportError
  where
    namesPackage = any (maybe False (T.isInfixOf package) . lineMessage) . lines
    anySignal output = "Span #" `T.isInfixOf` output || "Metric #" `T.isInfixOf` output
    exportError = T.isInfixOf "telemetry export error"

-- | Logging and client service remain available without a collector.
logsWithoutCollector :: Text -> TrafficDriver -> LogDriver -> Expectation
logsWithoutCollector package traffic logs = do
    void (telemetryInstall traffic package >>= shouldSucceed)
    observed <- telemetryLogs logs (T.isInfixOf "\"message\":") 80
    observed `shouldSatisfy` T.isInfixOf "\"message\":"

-- Export failure produces a warning without losing readiness or fresh-client service.
servesAfterExportFailure :: Text -> Text -> TrafficDriver -> LogDriver -> Expectation
servesAfterExportFailure firstPackage secondPackage traffic logs = do
    logsWithoutCollector firstPackage traffic logs
    warned <- telemetryLogs logs (T.isInfixOf "telemetry export error") 80
    warned `shouldSatisfy` T.isInfixOf "telemetry export error"
    telemetryReadiness traffic `shouldReturn` 200
    void (telemetryInstall traffic secondPackage >>= shouldSucceed)

{- Configured resource identity reaches the collector, and a log line under that identity
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
        [ ids
        | line <- map (T.dropWhile (/= '{')) (lines output)
        , all (\(key, expected) -> lineField [key] line == Just expected) identityFields
        , Just ids <- [loggedDatadogIds [] line]
        ]
    identityFields =
        [ ("service", telemetryService expectedIdentity)
        , ("env", telemetryEnvironment expectedIdentity)
        , ("version", telemetryVersion expectedIdentity)
        ]

-- | A trace identifier must contain a leading digit rather than an absent or empty value.
hasPopulatedTraceId :: Text -> Bool
hasPopulatedTraceId logs = any leadsWithDigit (drop 1 (T.splitOn "\"trace_id\":\"" logs))
  where
    leadsWithDigit segment = maybe False (isDigit . fst) (T.uncons segment)
