-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Service telemetry laws reject incomplete export and readiness observations.
module Ecluse.Test.Contract.TelemetrySpec (spec) where

import Data.Text qualified as T
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec

import Ecluse.Test.Contract.Install (ClientResult (..))
import Ecluse.Test.Contract.Telemetry

spec :: Spec
spec = do
    readTelemetrySpec
    privateFetchSpec

readTelemetrySpec :: Spec
readTelemetrySpec = describe "read telemetry evidence" $ do
    it "requires both a service metric and an exported span" $
        exportsSignals "package" traffic (collector "ecluse.serve.decision Span #1")
    it "rejects a metric-only collector" $
        exportsSignals "package" traffic (collector "ecluse.serve.decision") `shouldThrow` anyException
    it "rejects a trace-only collector" $
        exportsSignals "package" traffic (collector "Span #1") `shouldThrow` anyException
    it "rejects missing logs when telemetry is disabled" $
        logsWithoutCollector "package" traffic (logs "") `shouldThrow` anyException
    it "requires readiness after exporter failure" $
        servesAfterExportFailure
            "first"
            "second"
            traffic{telemetryReadiness = pure 503}
            (logs "\"message\":\"telemetry export error\"")
            `shouldThrow` anyException
    it "requires another fresh client after exporter failure" $ do
        requests <- newIORef []
        let observed = traffic{telemetryInstall = \package -> modifyIORef' requests (<> [package]) >> pure success}
        servesAfterExportFailure "first" "second" observed (logs "\"message\":\"telemetry export error\"")
        readIORef requests `shouldReturn` ["first", "second"]
  where
    success = ClientResult "client" ExitSuccess "" ""
    traffic = TrafficDriver (const (pure success)) (pure 200)
    collector output = CollectorDriver (\_ _ -> pure output)
    logs output = LogDriver (\_ _ -> pure output)

privateFetchSpec :: Spec
privateFetchSpec = describe "native private fetch evidence" $ do
    it "keeps conventional archive checks and requires the second fresh client" $
        fetchEvidence ConventionalArtifact (archiveSpan "404") (archiveSpan "404" <> archiveSpan "200")
            `shouldReturn` ["pkg", "pkg"]
    it "requires a serving lookup miss and a fresh serving success despite worker verification" $
        fetchEvidence IndexedLookup indexedMiss (indexedMiss <> servingLookup "200")
            `shouldReturn` ["pkg", "pkg"]
    it "rejects a worker lookup success as evidence of the second serving fetch" $
        fetchEvidence IndexedLookup indexedMiss indexedMiss `shouldThrow` anyException
    it "rejects indexed evidence without the initial serving miss" $
        fetchEvidence IndexedLookup (servingLookup "200") (servingLookup "200") `shouldThrow` anyException
    it "rejects extra status categories rather than accepting any miss and success" $
        fetchEvidence IndexedLookup indexedMiss (indexedMiss <> servingLookup "500" <> servingLookup "200") `shouldThrow` anyException

fetchEvidence :: PrivateFetchKind -> Text -> Text -> IO [Text]
fetchEvidence kind initial subsequent = do
    requests <- newIORef []
    observations <- newIORef [initial, subsequent]
    let success = ClientResult "native client" ExitSuccess "" ""
        traffic = TrafficDriver (\package -> modifyIORef' requests (<> [package]) $> success) (pure 200)
        mirror = MirrorTelemetryDriver (const (pure [])) (\_ version -> pure [version])
        collector = CollectorDriver $ \_ _ -> atomicModifyIORef' observations $ \remaining -> case remaining of
            firstOutput : rest -> (rest, firstOutput)
            [] -> ([], subsequent)
        target = case kind of
            ConventionalArtifact -> "/pkg/-/pkg-1.0.0.tgz"
            IndexedLookup -> "/ec/lu/ecluse-e2e-macro"
    exportsPrivateFetch kind "pkg" "1.0.0" "mirror" target traffic mirror collector
    readIORef requests

archiveSpan :: Text -> Text
archiveSpan = httpSpan "/pkg/-/pkg-1.0.0.tgz" "" ""

servingLookup :: Text -> Text
servingLookup status = rootSpan "Server" servingTrace <> httpSpan "/ec/lu/ecluse-e2e-macro" servingTrace "Client" status

indexedMiss :: Text
indexedMiss = servingLookup "404" <> rootSpan "Consumer" workerTrace <> httpSpan "/ec/lu/ecluse-e2e-macro" workerTrace "Client" "200"

servingTrace, workerTrace :: Text
servingTrace = T.replicate 32 "a"
workerTrace = T.replicate 32 "b"

rootSpan :: Text -> Text -> Text
rootSpan kind identity = "Span #0\n     Name: request\n     Kind: " <> kind <> "\n     Trace ID: " <> identity <> "\n     Parent ID: \n"

httpSpan :: Text -> Text -> Text -> Text -> Text
httpSpan target identity kind status =
    "Span #0\n     Name: GET\n     Kind: "
        <> kind
        <> "\n     Trace ID: "
        <> identity
        <> "\n     Parent ID: 1111111111111111\nAttributes:\n"
        <> "     -> http.host: Str(mirror)\n     -> http.method: Str(GET)\n     -> http.target: Str("
        <> target
        <> ")\n     -> http.status_code: Int("
        <> status
        <> ")\n"
