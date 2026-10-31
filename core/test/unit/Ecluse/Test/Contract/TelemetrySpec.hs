-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Service telemetry laws reject incomplete export and readiness observations.
module Ecluse.Test.Contract.TelemetrySpec (spec) where

import Data.Text qualified as T
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec

import Ecluse.Test.Collector (detailedSpan)
import Ecluse.Test.Contract.Install (ClientResult (..))
import Ecluse.Test.Contract.Telemetry

spec :: Spec
spec = do
    readTelemetrySpec
    domainSpansSpec
    identitySpec
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
        exportsNothingWhenDisabled "package" traffic (logs "") (collector "") `shouldThrow` anyException
    it "requires a request log and a quiet collector when telemetry is disabled" $
        exportsNothingWhenDisabled "package" traffic (logs requestLog) (collector "")
    it "rejects a disabled-telemetry log that never names the requested package" $
        exportsNothingWhenDisabled "package" traffic (logs "{\"message\":\"listening on port 8080\"}") (collector "") `shouldThrow` anyException
    for_ [("span", "Span #0"), ("metric", "Metric #0")] $ \(signal, printed) ->
        it ("rejects an exported " <> signal <> " when telemetry is disabled") $
            exportsNothingWhenDisabled "package" traffic (logs requestLog) (collector printed) `shouldThrow` anyException
    it "rejects an export attempt when telemetry is disabled" $
        exportsNothingWhenDisabled "package" traffic (logs (requestLog <> "\n{\"message\":\"telemetry export error\"}")) (collector "") `shouldThrow` anyException
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
    requestLog = "{\"message\":\"fetching public origin for package\"}"

domainSpansSpec :: Spec
domainSpansSpec = describe "domain span correlation" $ do
    it "accepts a mirror job linked to the enqueue that shares the rule evaluation's trace" $
        domainSpans servingTrace [(servingTrace, enqueueSpan)]
    it "rejects a mirror job without a link to its enqueue" $
        domainSpans servingTrace [] `shouldThrow` anyException
    it "rejects a mirror job linked to another trace" $
        domainSpans servingTrace [(workerTrace, enqueueSpan)] `shouldThrow` anyException
    it "rejects a mirror job linked to another span of the enqueue's trace" $
        domainSpans servingTrace [(servingTrace, ruleSpan)] `shouldThrow` anyException
    it "rejects a rule evaluation outside the enqueueing trace" $
        domainSpans workerTrace [(servingTrace, enqueueSpan)] `shouldThrow` anyException
  where
    enqueueSpan = "1111111111111111"
    ruleSpan = "2222222222222222"
    domainSpans ruleTrace links =
        exportsDomainSpans "pkg" "1.0.0" (TrafficDriver (const (pure installed)) (pure 200)) (MirrorTelemetryDriver (const (pure [])) (\_ version -> pure [version])) . CollectorDriver $ \_ _ ->
            pure $
                coordinateSpan "ecluse.rule.eval" ruleTrace ruleSpan []
                    <> coordinateSpan "ecluse.mirror.enqueue" servingTrace enqueueSpan []
                    <> coordinateSpan "ecluse.mirror.job" workerTrace "3333333333333333" links

identitySpec :: Spec
identitySpec = describe "Datadog identity and log correlation" $ do
    it "accepts a log line carrying an exported span's Datadog trace and span IDs" $
        correlates resourceLines (logLine datadogTraceId datadogSpanId)
    it "rejects a log line whose trace ID is the span ID" $
        correlates resourceLines (logLine datadogSpanId datadogSpanId) `shouldThrow` anyException
    it "rejects a log line for a span the collector never exported" $
        correlates resourceLines (logLine datadogTraceId "1") `shouldThrow` anyException
    it "reads a log line after a container log prefix" $
        correlates resourceLines ("proxy-1  | " <> logLine datadogTraceId datadogSpanId)
    it "rejects a log line under another service" $
        correlates resourceLines (T.replace "\"service\":\"ecluse-test\"" "\"service\":\"other\"" (logLine datadogTraceId datadogSpanId)) `shouldThrow` anyException
    it "rejects an identity value under another attribute" $
        correlates (resourceAttributes [("service.name", "ecluse-test"), ("deployment.environment.name", "9.9.9"), ("service.version", "staging")]) (logLine datadogTraceId datadogSpanId)
            `shouldThrow` anyException
  where
    expected = TelemetryIdentity "ecluse-test" "staging" "9.9.9"
    correlates resource logged =
        exportsIdentity "pkg" expected (TrafficDriver (const (pure installed)) (pure 200)) (LogDriver (\_ _ -> pure logged)) (CollectorDriver (\_ _ -> pure (resource <> coordinateSpan "request" servingTrace datadogSpanHex [])))
    resourceLines = resourceAttributes [("service.name", "ecluse-test"), ("deployment.environment.name", "staging"), ("service.version", "9.9.9")]
    resourceAttributes pairs = "Resource attributes:\n" <> T.concat ["     -> " <> key <> ": Str(" <> value <> ")\n" | (key, value) <- pairs]
    logLine traceKey spanKey = "{\"service\":\"ecluse-test\",\"env\":\"staging\",\"version\":\"9.9.9\",\"dd\":{\"trace_id\":\"" <> traceKey <> "\",\"span_id\":\"" <> spanKey <> "\"}}\n"
    -- The low 64 bits of 'servingTrace' and the whole of 'datadogSpanHex', each in decimal.
    datadogTraceId = "12297829382473034410"
    datadogSpanId = "67667974448284343"

datadogSpanHex :: Text
datadogSpanHex = "00f067aa0ba902b7"

installed :: ClientResult
installed = ClientResult "client" ExitSuccess "" ""

-- A span carrying the scenario coordinate, with its trace and span IDs and its links.
coordinateSpan :: Text -> Text -> Text -> [(Text, Text)] -> Text
coordinateSpan name traceKey spanKey = detailedSpan name "Internal" traceKey spanKey [("ecluse.package", "Str(pkg)"), ("ecluse.version", "Str(1.0.0)")]

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
        collector = CollectorDriver $ \_ _ -> atomicModifyIORef' observations $ \case
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
rootSpan kind traceKey = "Span #0\n     Name: request\n     Kind: " <> kind <> "\n     Trace ID: " <> traceKey <> "\n     Parent ID: \n"

httpSpan :: Text -> Text -> Text -> Text -> Text
httpSpan target traceKey kind status =
    "Span #0\n     Name: GET\n     Kind: "
        <> kind
        <> "\n     Trace ID: "
        <> traceKey
        <> "\n     Parent ID: 1111111111111111\nAttributes:\n"
        <> "     -> http.host: Str(mirror)\n     -> http.method: Str(GET)\n     -> http.target: Str("
        <> target
        <> ")\n     -> http.status_code: Int("
        <> status
        <> ")\n"
