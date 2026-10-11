-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Correlated telemetry and continued service when an exporter fails.
module Ecluse.Test.Contract.Telemetry (
    TrafficDriver (..),
    LogDriver (..),
    CollectorDriver (..),
    MirrorTelemetryDriver (..),
    TelemetryIdentity (..),
    exportsSignals,
    exportsDomainSpans,
    exportsPrivateFetch,
    logsWithoutCollector,
    servesAfterExportFailure,
    exportsIdentity,
    hasPopulatedTraceId,
) where

import Data.Char (isDigit)
import Data.Text qualified as T
import Test.Hspec (Expectation, shouldBe, shouldReturn, shouldSatisfy)

import Ecluse.Test.Collector
import Ecluse.Test.Contract.Install (ClientResult, shouldSucceed)

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

-- | All domain spans must identify this scenario's own coordinate.
exportsDomainSpans :: Text -> Text -> TrafficDriver -> MirrorTelemetryDriver -> CollectorDriver -> Expectation
exportsDomainSpans package version traffic mirror collector = do
    void (telemetryInstall traffic package >>= shouldSucceed)
    versions <- telemetryAwaitVersion mirror package version
    (version `elem` versions) `shouldBe` True
    logs <- telemetryCollected collector emitted 120
    logs `shouldSatisfy` emitted
  where
    emitted logs =
        all
            (\name -> any (spanFor package version name) (exportedSpans logs))
            ["ecluse.rule.eval", "ecluse.mirror.enqueue", "ecluse.mirror.job"]

-- | A fresh second install records a successful private fetch after the initial miss.
exportsPrivateFetch :: Text -> Text -> Text -> Text -> TrafficDriver -> MirrorTelemetryDriver -> CollectorDriver -> Expectation
exportsPrivateFetch package version host target traffic mirror collector = do
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
    answers = privateArtifactAnswers host target . exportedSpans

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

-- | Configured resource identity and populated log correlation reach both outputs.
exportsIdentity :: Text -> TelemetryIdentity -> TrafficDriver -> LogDriver -> CollectorDriver -> Expectation
exportsIdentity package expectedIdentity traffic logs collector = do
    void (telemetryInstall traffic package >>= shouldSucceed)
    collected <- telemetryCollected collector carriesIdentity 80
    collected `shouldSatisfy` carriesIdentity
    logged <- telemetryLogs logs correlated 80
    logged `shouldSatisfy` correlated
  where
    carriesIdentity output =
        all
            (`T.isInfixOf` output)
            [ "service.name"
            , telemetryService expectedIdentity
            , "deployment.environment"
            , telemetryEnvironment expectedIdentity
            , "service.version"
            , telemetryVersion expectedIdentity
            ]
    correlated output =
        hasPopulatedTraceId output
            && all
                (`T.isInfixOf` output)
                [ "\"service\":\"" <> telemetryService expectedIdentity <> "\""
                , "\"env\":\"" <> telemetryEnvironment expectedIdentity <> "\""
                , "\"version\":\"" <> telemetryVersion expectedIdentity <> "\""
                ]

-- | A trace identifier must contain a leading digit rather than an absent or empty value.
hasPopulatedTraceId :: Text -> Bool
hasPopulatedTraceId logs = any leadsWithDigit (drop 1 (T.splitOn "\"trace_id\":\"" logs))
  where
    leadsWithDigit segment = maybe False (isDigit . fst) (T.uncons segment)
