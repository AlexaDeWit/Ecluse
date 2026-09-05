-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.Telemetry.TracingSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Network.HTTP.Client (
    Request,
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
    requestHeaders,
 )
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (HeaderName, hAuthorization, hUserAgent)
import Network.Wai (Application, responseLBS)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import OpenTelemetry.Attributes (Attributes, fromAttribute, lookupAttribute)
import OpenTelemetry.Exporter.InMemory.Span (inMemoryListExporter)
import OpenTelemetry.Instrumentation.HttpClient (instrumentManagerSettings)
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware')
import OpenTelemetry.Metric (noopMeterProvider)
import OpenTelemetry.Metric.Core (getMeter)
import OpenTelemetry.Propagator (setGlobalTextMapPropagator)
import OpenTelemetry.Propagator.W3CTraceContext (w3cTraceContextPropagator)
import OpenTelemetry.Trace (
    createTracerProvider,
    emptyTracerProviderOptions,
    forceFlushTracerProvider,
    setGlobalTracerProvider,
 )
import OpenTelemetry.Trace.Core (
    ImmutableSpan (spanContext, spanHot),
    Link (frozenLinkContext),
    SpanHot (hotAttributes, hotLinks, hotName, hotStatus),
    SpanStatus (Error, Unset),
 )
import OpenTelemetry.Trace.Core qualified as TraceCore
import OpenTelemetry.Util (appendOnlyBoundedCollectionValues)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Queue (RemoteSpanContext (RemoteSpanContext))
import Ecluse.Core.Server.Response (
    RejectReason (BelowIntegrityFloor, ByPolicy, MissingIntegrity, Unavailable, UpstreamInvalid),
    Rejection (Rejection),
    RuleName (RuleName),
    ServeDecision (Admit, Reject),
    Transience (WontResolve),
 )
import Ecluse.Core.Telemetry.Metrics (AdvisorySyncResult (AdvisoryRefused))
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Runtime.Telemetry (
    Telemetry (TelemetryEnabled),
    TelemetryProviders (TelemetryProviders),
    telemetryDisabled,
 )
import Ecluse.Runtime.Telemetry.Tracing (
    JobSpanOutcome (JobSpanOutcome),
    dataPlaneInstrumentationConfig,
    ruleVerdictFields,
    withAdvisorySyncSpan,
    withMirrorEnqueueSpan,
    withMirrorJobSpan,
    withRuleEvalSpan,
 )

{- | Tests the request-lifecycle tracing layer: the verdict attribute mapping is exact, a
domain span is inert when telemetry is off, and the forwarded client token and the
@Authorization@ header never reach a captured span attribute.
-}
spec :: Spec
spec = do
    verdictMappingSpec
    gatingSpec
    scrubSpec
    advisorySyncSpanSpec
    crossAsyncLinkSpec
    enqueueStatusSpec
    enqueueAuthoritySpec
    traceparentInjectionSpec

-- A distinctive secret that must never surface on a span. The scrub assertions search
-- the captured spans for it.
secretToken :: Text
secretToken = "s3cr3t-bearer-tok3n-do-not-leak"

verdictMappingSpec :: Spec
verdictMappingSpec = describe "ruleVerdictFields" $ do
    it "maps an admit to the decision field alone" $
        ruleVerdictFields Admit `shouldBe` [("ecluse.rule.decision", "admit")]

    it "maps a policy denial to the rule name, reason class, and message" $
        ruleVerdictFields (Reject (Rejection (ByPolicy (RuleName "DenyInstallTimeExecution")) "denied: runs install scripts"))
            `shouldBe` [ ("ecluse.rule.decision", "deny")
                       , ("ecluse.rule.reason_class", "by_policy")
                       , ("ecluse.rule.message", "denied: runs install scripts")
                       , ("ecluse.rule.name", "DenyInstallTimeExecution")
                       ]

    it "maps a missing-integrity refusal to its reason class, with no rule name" $
        ruleVerdictFields (Reject (Rejection MissingIntegrity "no integrity digest"))
            `shouldBe` [ ("ecluse.rule.decision", "deny")
                       , ("ecluse.rule.reason_class", "missing_integrity")
                       , ("ecluse.rule.message", "no integrity digest")
                       ]

    it "maps an unavailability to its reason class, with no rule name" $
        ruleVerdictFields (Reject (Rejection (Unavailable WontResolve) "could not decide"))
            `shouldBe` [ ("ecluse.rule.decision", "deny")
                       , ("ecluse.rule.reason_class", "unavailable")
                       , ("ecluse.rule.message", "could not decide")
                       ]

    it "maps a below-integrity-floor refusal to its reason class, with no rule name" $
        ruleVerdictFields (Reject (Rejection BelowIntegrityFloor "weaker than the integrity floor"))
            `shouldBe` [ ("ecluse.rule.decision", "deny")
                       , ("ecluse.rule.reason_class", "below_integrity_floor")
                       , ("ecluse.rule.message", "weaker than the integrity floor")
                       ]

    it "maps an upstream-invalid refusal to its reason class, with no rule name" $
        ruleVerdictFields (Reject (Rejection UpstreamInvalid "upstream returned a different package"))
            `shouldBe` [ ("ecluse.rule.decision", "deny")
                       , ("ecluse.rule.reason_class", "upstream_invalid")
                       , ("ecluse.rule.message", "upstream returned a different package")
                       ]

gatingSpec :: Spec
gatingSpec = describe "domain-span brackets (telemetry disabled)" $ do
    it "runs the rule-eval body and returns its result, opening no span" $ do
        -- With the disabled handle there is no tracer to reach for. The helper must
        -- run the body and thread its result through, never demanding a provider.
        result <-
            withRuleEvalSpan telemetryDisabled (mkPackageName Npm Nothing "left-pad") (mkVersion Npm "1.0.0") $
                pure (42 :: Int, Admit)
        result `shouldBe` 42

    it "runs the advisory-sync attempt and returns its result, opening no span" $ do
        -- The sync loop brackets unconditionally, so the disabled bracket must never
        -- reach for a provider and never change what the attempt concluded.
        result <- withAdvisorySyncSpan telemetryDisabled Npm (const AdvisoryRefused) (pure (7 :: Int))
        result `shouldBe` 7

scrubSpec :: Spec
scrubSpec = describe "secret scrubbing" $ do
    it "keeps a forwarded Bearer token off the http-client client span" $ do
        (processor, ref) <- inMemoryListExporter
        tracerProvider <- createTracerProvider [processor] emptyTracerProviderOptions
        -- The http-client manager instrumentation reads the process-global tracer
        -- provider, so the in-memory one must be installed there.
        setGlobalTracerProvider tracerProvider
        settings <- instrumentManagerSettings dataPlaneInstrumentationConfig defaultManagerSettings
        manager <- newManager settings
        Warp.testWithApplication (pure okApp) $ \port -> do
            baseReq <- parseRequest ("http://127.0.0.1:" <> show port <> "/some/package")
            _ <- httpLbs (withBearer baseReq) manager
            pass
        _ <- forceFlushTracerProvider tracerProvider Nothing
        spans <- readIORef ref
        length spans `shouldSatisfy` (>= 1)
        dump <- attributeDump ref
        (secretToken `T.isInfixOf` dump) `shouldBe` False

    it "keeps the request Authorization header off the WAI server span" $ do
        (processor, ref) <- inMemoryListExporter
        tracerProvider <- createTracerProvider [processor] emptyTracerProviderOptions
        meter <- getMeter noopMeterProvider "ecluse-test"
        middleware <- newOpenTelemetryWaiMiddleware' tracerProvider meter
        let tracedApp = middleware okApp
        Warp.testWithApplication (pure tracedApp) $ \port -> do
            manager <- newManager defaultManagerSettings
            baseReq <- parseRequest ("http://127.0.0.1:" <> show port <> "/some/route")
            _ <- httpLbs (withBearer baseReq) manager
            pass
        _ <- forceFlushTracerProvider tracerProvider Nothing
        spans <- readIORef ref
        length spans `shouldSatisfy` (>= 1)
        dump <- attributeDump ref
        (secretToken `T.isInfixOf` dump) `shouldBe` False

-- A trivial @200@ application: the target the instrumented requests hit.
okApp :: Application
okApp _ respond = respond (responseLBS status200 [] "ok")

-- Stamp a request with the secret Bearer credential (and a benign User-Agent, which
-- the WAI instrumentation /does/ record, to show only the token is scrubbed).
withBearer :: Request -> Request
withBearer req =
    req
        { requestHeaders =
            [ (hAuthorization, encodeUtf8 ("Bearer " <> secretToken))
            , (hUserAgent, "npm/10")
            ]
        }

-- The captured spans rendered to text: every span's name and full attribute set. A
-- substring search then proves the secret is present nowhere on any span.
attributeDump :: IORef [ImmutableSpan] -> IO Text
attributeDump ref = do
    spans <- readIORef ref
    parts <- forM spans $ \theSpan -> do
        hot <- readIORef (spanHot theSpan)
        pure (hotName hot <> " " <> show (hotAttributes hot))
    pure (T.intercalate "\n" parts)

-- The package/version coordinates the domain spans carry. Fixed, since these tests
-- assert on the trace structure (links, status), not the coordinate attributes.
samplePackage :: PackageName
samplePackage = mkPackageName Npm Nothing "left-pad"

sampleVersion :: Version
sampleVersion = mkVersion Npm "1.3.0"

{- One attempt yields exactly one @ecluse.advisory.sync.attempt@ span whose two @ecluse.@
attributes are the ones the metric labels join on. A third added later fails here. -}
advisorySyncSpanSpec :: Spec
advisorySyncSpanSpec = describe "advisory sync span" $
    it "opens one span per attempt whose attributes are exactly the ecosystem and the result" $ do
        (processor, ref) <- inMemoryListExporter
        tracerProvider <- createTracerProvider [processor] emptyTracerProviderOptions
        let telemetry = TelemetryEnabled (TelemetryProviders tracerProvider noopMeterProvider)
        withAdvisorySyncSpan telemetry Npm (const AdvisoryRefused) pass
        _ <- forceFlushTracerProvider tracerProvider Nothing
        spans <- readIORef ref
        names <- traverse (fmap hotName . readIORef . spanHot) spans
        names `shouldBe` ["ecluse.advisory.sync.attempt"]
        syncSpan <- findSpan ref "ecluse.advisory.sync.attempt"
        attributes <- hotAttributes <$> readIORef (spanHot syncSpan)
        -- The SDK stamps its own code.* and thread.* attributes on every span, so the closed-set
        -- guard covers the ecluse. namespace alone.
        ecluseAttributeCount attributes `shouldBe` 2
        textAttribute attributes "ecluse.ecosystem" `shouldBe` Just "npm"
        textAttribute attributes "ecluse.advisory.sync.result" `shouldBe` Just "refused"

-- Read one span attribute back as text, 'Nothing' when the key is absent or holds
-- another type.
textAttribute :: Attributes -> Text -> Maybe Text
textAttribute attributes key = lookupAttribute attributes key >>= fromAttribute

-- How many @ecluse.@-namespaced attributes a span carries. Only a key follows a pair's
-- opening parenthesis, so a value can never be miscounted as a key.
ecluseAttributeCount :: Attributes -> Int
ecluseAttributeCount = T.count "(\"ecluse." . show

{- The @ecluse.mirror.job@ span must link back to the @ecluse.mirror.enqueue@ span's trace.
That proves the job is linked to the enqueueing request, not merely correlated by
package\/version. -}
crossAsyncLinkSpec :: Spec
crossAsyncLinkSpec = describe "cross-async span link (enqueue → worker job)" $ do
    it "links the worker-job span back to the enqueueing span's trace" $ do
        (processor, ref) <- inMemoryListExporter
        tracerProvider <- createTracerProvider [processor] emptyTracerProviderOptions
        let telemetry = TelemetryEnabled (TelemetryProviders tracerProvider noopMeterProvider)
        -- The serve path captures the enqueue span's context. Carry it across the hop.
        carrier <-
            withMirrorEnqueueSpan telemetry samplePackage sampleVersion "https://artifact" (const Nothing) pure
        -- The worker re-establishes it as a link on its per-job span.
        withMirrorJobSpan telemetry samplePackage sampleVersion carrier (const (JobSpanOutcome "succeeded" Nothing)) pass
        _ <- forceFlushTracerProvider tracerProvider Nothing

        enqueueSpan <- findSpan ref "ecluse.mirror.enqueue"
        jobSpan <- findSpan ref "ecluse.mirror.job"
        jobHot <- readIORef (spanHot jobSpan)
        let jobLinks = toList (appendOnlyBoundedCollectionValues (hotLinks jobHot))
        -- Exactly one link, pointing at the enqueue span's trace.
        map (TraceCore.traceId . frozenLinkContext) jobLinks
            `shouldBe` [TraceCore.traceId (spanContext enqueueSpan)]

    it "carries no link when the job carried no trace context" $ do
        (processor, ref) <- inMemoryListExporter
        tracerProvider <- createTracerProvider [processor] emptyTracerProviderOptions
        let telemetry = TelemetryEnabled (TelemetryProviders tracerProvider noopMeterProvider)
        -- A job enqueued with no context (tracing was off at enqueue) yields an unlinked
        -- worker span: still emitted, just not linked.
        withMirrorJobSpan telemetry samplePackage sampleVersion Nothing (const (JobSpanOutcome "succeeded" Nothing)) pass
        _ <- forceFlushTracerProvider tracerProvider Nothing
        jobSpan <- findSpan ref "ecluse.mirror.job"
        jobHot <- readIORef (spanHot jobSpan)
        toList (appendOnlyBoundedCollectionValues (hotLinks jobHot)) `shouldSatisfy` null

    it "carries no link, and does not crash, when the carried context is not a valid W3C traceparent" $ do
        (processor, ref) <- inMemoryListExporter
        tracerProvider <- createTracerProvider [processor] emptyTracerProviderOptions
        let telemetry = TelemetryEnabled (TelemetryProviders tracerProvider noopMeterProvider)
        -- The carrier is untrusted transport: a present-but-unparseable traceparent must
        -- decode to no link and never fail the job (the worker mirrors regardless of trace).
        let garbled = RemoteSpanContext "not-a-w3c-traceparent" ""
        withMirrorJobSpan telemetry samplePackage sampleVersion (Just garbled) (const (JobSpanOutcome "succeeded" Nothing)) pass
        _ <- forceFlushTracerProvider tracerProvider Nothing
        jobSpan <- findSpan ref "ecluse.mirror.job"
        jobHot <- readIORef (spanHot jobSpan)
        toList (appendOnlyBoundedCollectionValues (hotLinks jobHot)) `shouldSatisfy` null

{- A swallowed best-effort enqueue failure sets the @ecluse.mirror.enqueue@ span status to
'Error' with the detail, so a trace explains it. A success leaves the status 'Unset'. -}
enqueueStatusSpec :: Spec
enqueueStatusSpec = describe "enqueue span status on a swallowed failure" $ do
    it "marks the enqueue span errored with the failure detail" $ do
        (processor, ref) <- inMemoryListExporter
        tracerProvider <- createTracerProvider [processor] emptyTracerProviderOptions
        let telemetry = TelemetryEnabled (TelemetryProviders tracerProvider noopMeterProvider)
        -- The body's result projects to a failure detail, as the swallowed-failure path does.
        withMirrorEnqueueSpan telemetry samplePackage sampleVersion "https://artifact" (const (Just "mirror enqueue failed: queue unreachable")) (const pass)
        _ <- forceFlushTracerProvider tracerProvider Nothing
        enqueueSpan <- findSpan ref "ecluse.mirror.enqueue"
        hot <- readIORef (spanHot enqueueSpan)
        hotStatus hot `shouldBe` Error "mirror enqueue failed: queue unreachable"

    it "leaves the enqueue span status unset on a successful enqueue" $ do
        (processor, ref) <- inMemoryListExporter
        tracerProvider <- createTracerProvider [processor] emptyTracerProviderOptions
        let telemetry = TelemetryEnabled (TelemetryProviders tracerProvider noopMeterProvider)
        withMirrorEnqueueSpan telemetry samplePackage sampleVersion "https://artifact" (const Nothing) (const pass)
        _ <- forceFlushTracerProvider tracerProvider Nothing
        enqueueSpan <- findSpan ref "ecluse.mirror.enqueue"
        hot <- readIORef (spanHot enqueueSpan)
        hotStatus hot `shouldBe` Unset

{- The enqueue span names the artifact's authority, never its URL. The upstream location can
carry a credential in its userinfo or query, and a span attribute leaves the node. -}
enqueueAuthoritySpec :: Spec
enqueueAuthoritySpec = describe "enqueue span artifact authority" $
    it "records the artifact host and port, dropping userinfo, path, and query" $ do
        (processor, ref) <- inMemoryListExporter
        tracerProvider <- createTracerProvider [processor] emptyTracerProviderOptions
        let telemetry = TelemetryEnabled (TelemetryProviders tracerProvider noopMeterProvider)
        withMirrorEnqueueSpan
            telemetry
            samplePackage
            sampleVersion
            "https://deploy:hunter2@registry.npmjs.org/left-pad/-/left-pad-1.3.0.tgz?sig=abc"
            (const Nothing)
            (const pass)
        _ <- forceFlushTracerProvider tracerProvider Nothing
        dump <- attributeDump ref
        dump `shouldSatisfy` T.isInfixOf "ecluse.mirror.artifact_host"
        dump `shouldSatisfy` T.isInfixOf "registry.npmjs.org:443"
        dump `shouldSatisfy` (not . T.isInfixOf "hunter2")
        dump `shouldSatisfy` (not . T.isInfixOf "sig=abc")
        dump `shouldSatisfy` (not . T.isInfixOf "left-pad-1.3.0.tgz")

{- The data-plane instrumentation must inject a W3C @traceparent@ on each outbound request,
so a downstream service continues the trace. -}
traceparentInjectionSpec :: Spec
traceparentInjectionSpec = describe "W3C traceparent injection on the data plane" $
    it "injects a traceparent header on an outbound data-plane request" $ do
        (processor, _ref) <- inMemoryListExporter
        tracerProvider <- createTracerProvider [processor] emptyTracerProviderOptions
        setGlobalTracerProvider tracerProvider
        -- The production posture: the SDK installs the W3C propagator globally, which is
        -- what the http-client instrumentation injects through.
        setGlobalTextMapPropagator w3cTraceContextPropagator
        settings <- instrumentManagerSettings dataPlaneInstrumentationConfig defaultManagerSettings
        manager <- newManager settings
        headersRef <- newIORef []
        Warp.testWithApplication (pure (captureHeadersApp headersRef)) $ \port -> do
            req <- parseRequest ("http://127.0.0.1:" <> show port <> "/some/package")
            _ <- httpLbs req manager
            pass
        _ <- forceFlushTracerProvider tracerProvider Nothing
        received <- readIORef headersRef
        find ((== "traceparent") . fst) received `shouldSatisfy` isJust

-- A WAI application that records the headers of the request it received, then answers
-- @200@: the downstream stub the traceparent-injection assertion inspects.
captureHeadersApp :: IORef [(HeaderName, ByteString)] -> Application
captureHeadersApp ref req respond = do
    writeIORef ref (Wai.requestHeaders req)
    respond (responseLBS status200 [] "ok")

-- Read the captured span with the given name. A missing span fails the test loudly, so
-- a missing emission is a clear failure rather than a pattern-match crash.
findSpan :: IORef [ImmutableSpan] -> Text -> IO ImmutableSpan
findSpan ref name = do
    spans <- readIORef ref
    named <- filterM (fmap ((== name) . hotName) . readIORef . spanHot) spans
    case named of
        (s : _) -> pure s
        [] -> fail ("no captured span named " <> toString name)
