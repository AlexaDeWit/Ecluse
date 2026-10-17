-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.Telemetry.TracingSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Network.HTTP.Client (
    Manager,
    ManagerSettings (managerTlsConnection),
    Request (secure),
    defaultManagerSettings,
    httpLbs,
    httpNoBody,
    newManager,
    parseRequest,
    requestHeaders,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (HeaderName, hAuthorization, hUserAgent)
import Network.TLS qualified as TLS
import Network.Wai (Application, responseLBS)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import OpenTelemetry.Attributes (Attributes, fromAttribute, lookupAttribute)
import OpenTelemetry.Exporter.InMemory.Span (inMemoryListExporter)
import OpenTelemetry.Instrumentation.HttpClient (instrumentManagerSettings)
import OpenTelemetry.Metric (noopMeterProvider)
import OpenTelemetry.Propagator (setGlobalTextMapPropagator)
import OpenTelemetry.Propagator.W3CTraceContext (w3cTraceContextPropagator)
import OpenTelemetry.Trace (
    TracerProvider,
    createTracerProvider,
    emptyTracerProviderOptions,
    forceFlushTracerProvider,
    setGlobalTracerProvider,
 )
import OpenTelemetry.Trace.Core (
    Event (eventAttributes, eventName),
    ImmutableSpan (spanContext, spanHot),
    Link (frozenLinkContext),
    SpanHot (hotAttributes, hotEvents, hotLinks, hotName, hotStatus),
    SpanStatus (Error, Unset),
 )
import OpenTelemetry.Trace.Core qualified as TraceCore
import OpenTelemetry.Trace.Id (TraceId)
import OpenTelemetry.Util (appendOnlyBoundedCollectionValues)

import Ecluse.Core.Cve.Types (OsvDbFetchFault (OsvDbTransport))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportTimeout), TransportFault, transportFault)
import Ecluse.Core.Fault.Http (classifyTransport, displayFaultT)
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Queue (RemoteSpanContext (RemoteSpanContext))
import Ecluse.Core.Rules.Types (AdvisoryScore (Cvss), Reason (AffectedBy, RunsOnInstall), mkAdvisoryIds)
import Ecluse.Core.Server.Response (
    RejectReason (BelowIntegrityFloor, ByPolicy, MissingIntegrity, Unavailable, UpstreamInvalid),
    Rejection (Rejection),
    RuleName (RuleName),
    ServeDecision (Admit, Reject),
    Transience (WontResolve),
 )
import Ecluse.Core.Telemetry.Metrics (AdvisorySyncResult (AdvisoryFetchFailed, AdvisoryRefused))
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Runtime.Telemetry.Escape (withEscapeHandler)
import Ecluse.Runtime.Telemetry.Internal (
    Telemetry (TelemetryEnabled),
    TelemetryProviders (TelemetryProviders),
    telemetryDisabled,
 )
import Ecluse.Runtime.Telemetry.Tracing.Internal (
    JobSpanOutcome (JobSpanOutcome),
    dataPlaneInstrumentationConfig,
    instrumentDataPlaneManagerSettings,
    ruleVerdictFields,
    telemetryWaiMiddleware,
    withAdvisorySyncSpan,
    withMirrorEnqueueSpan,
    withMirrorJobSpan,
    withRuleEvalSpan,
 )
import Ecluse.Test.Stub (refusedFetch, requestSecrets, signedRequest)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))
import UnliftIO.Exception (throwIO, try, tryAny)

-- | The verdict mapping is exact, a domain span is inert without telemetry, and no token or @Authorization@ header reaches a span.
spec :: Spec
spec = do
    verdictMappingSpec
    gatingSpec
    scrubSpec
    advisorySyncSpanSpec
    escapedExceptionSpec
    dataPlaneFaultSpec
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
        ruleVerdictFields (Reject (Rejection (ByPolicy (RuleName "DenyInstallTimeExecution") [RunsOnInstall "install scripts"]) "denied: runs install scripts"))
            `shouldBe` [ ("ecluse.rule.decision", "deny")
                       , ("ecluse.rule.reason_class", "by_policy")
                       , ("ecluse.rule.message", "denied: runs install scripts")
                       , ("ecluse.rule.name", "DenyInstallTimeExecution")
                       ]

    it "adds no span field for the advisories a policy denial names" $
        ruleVerdictFields (Reject (Rejection (ByPolicy (RuleName "DenyIfCve") [AffectedBy Cvss 7.0 (mkAdvisoryIds ("GHSA-span-0001" :| []))]) "denied: affected"))
            `shouldBe` [ ("ecluse.rule.decision", "deny")
                       , ("ecluse.rule.reason_class", "by_policy")
                       , ("ecluse.rule.message", "denied: affected")
                       , ("ecluse.rule.name", "DenyIfCve")
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
        result <- withAdvisorySyncSpan telemetryDisabled Npm (const (AdvisoryRefused, Nothing)) (pure (7 :: Int))
        result `shouldBe` 7

scrubSpec :: Spec
scrubSpec = describe "secret scrubbing" $ do
    it "keeps a forwarded Bearer token off the http-client client span" $
        withSpanCapture
            ( \tracerProvider -> do
                -- The http-client manager instrumentation reads the process-global tracer
                -- provider, so the in-memory one must be installed there.
                setGlobalTracerProvider tracerProvider
                settings <- instrumentManagerSettings dataPlaneInstrumentationConfig defaultManagerSettings
                newManager settings >>= callWithBearer okApp
            )
            scrubbedButRecording

    it "keeps the request Authorization header off the WAI server span" $
        withTelemetrySpans
            ( \telemetry -> do
                middleware <- telemetryWaiMiddleware telemetry
                newManager defaultManagerSettings >>= callWithBearer (middleware okApp)
            )
            scrubbedButRecording

{- The request carried two headers. The benign one must land on the span and the credential
must not, so a run that recorded no header at all fails rather than reads as scrubbed. -}
scrubbedButRecording :: IORef [ImmutableSpan] -> Expectation
scrubbedButRecording ref = do
    dump <- attributeDump ref
    dump `shouldSatisfy` T.isInfixOf "npm/10"
    dump `shouldSatisfy` (not . T.isInfixOf secretToken)

-- A trivial @200@ application: the target the instrumented requests hit.
okApp :: Application
okApp _ respond = respond (responseLBS status200 [] "ok")

-- Send one request carrying the secret Bearer credential and a benign User-Agent.
callWithBearer :: Application -> Manager -> IO ()
callWithBearer app manager =
    Warp.testWithApplication (pure app) $ \port -> do
        request <- parseRequest ("http://127.0.0.1:" <> show port <> "/some/package")
        void (httpLbs (withBearer request) manager)

withBearer :: Request -> Request
withBearer req =
    req
        { requestHeaders =
            [ (hAuthorization, encodeUtf8 ("Bearer " <> secretToken))
            , (hUserAgent, "npm/10")
            ]
        }

{- Capture every span the body opens. The in-memory processor is the only one installed, so a
span the body never opened reads as missing rather than filtered. -}
withSpanCapture :: (TracerProvider -> IO ()) -> (IORef [ImmutableSpan] -> IO a) -> IO a
withSpanCapture emit inspect = do
    (processor, ref) <- inMemoryListExporter
    tracerProvider <- createTracerProvider [processor] (withEscapeHandler emptyTracerProviderOptions)
    emit tracerProvider
    _ <- forceFlushTracerProvider tracerProvider Nothing
    inspect ref

-- 'withSpanCapture' over an enabled handle, which is how a domain span reaches a tracer.
withTelemetrySpans :: (Telemetry -> IO ()) -> (IORef [ImmutableSpan] -> IO a) -> IO a
withTelemetrySpans emit =
    withSpanCapture (\tracerProvider -> emit (TelemetryEnabled (TelemetryProviders tracerProvider noopMeterProvider)))

-- Every captured span's name, status, attributes and event attributes as text, so a substring
-- search proves a secret is present nowhere on any span.
attributeDump :: IORef [ImmutableSpan] -> IO Text
attributeDump ref = do
    spans <- readIORef ref
    parts <- forM spans $ \theSpan -> do
        hot <- readIORef (spanHot theSpan)
        pure (T.unwords [hotName hot, show (hotStatus hot), show (hotAttributes hot), show (map eventAttributes (exceptionEvents hot))])
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
    it "opens one span per attempt whose attributes are exactly the ecosystem and the result" $
        withTelemetrySpans (\telemetry -> withAdvisorySyncSpan telemetry Npm (const (AdvisoryRefused, Nothing)) pass) $ \ref -> do
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

{- An exception escaping a span is recorded by its typed cause when its own rendering would print a
request. Every capture here runs the production 'withEscapeHandler', as 'withSpanCapture' does. -}
escapedExceptionSpec :: Spec
escapedExceptionSpec = describe "an exception escaping a span" $ do
    it "records a request-carrying HTTP exception on a domain span by its typed cause, never its request" $ do
        refused <- refusedFetch =<< signedRequest
        withTelemetrySpans (escapeFrom (\telemetry -> withAdvisorySyncSpan telemetry Npm (const (AdvisoryRefused, Nothing)) (throwIO refused))) $ \ref -> do
            syncSpan <- findSpan ref "ecluse.advisory.sync.attempt"
            hot <- readIORef (spanHot syncSpan)
            hotStatus hot `shouldBe` Error (displayFaultT (toException refused))
            map eventName (exceptionEvents hot) `shouldBe` ["exception"]
            attributeDump ref >>= \dump -> for_ requestSecrets $ \secret -> dump `shouldSatisfy` (not . T.isInfixOf secret)

    it "records one escaping the application on the WAI server span by its typed cause, never its request" $ do
        refused <- refusedFetch =<< signedRequest
        withTelemetrySpans (throwingBehindMiddleware (toException refused)) $ \ref -> do
            statuses <- traverse (fmap hotStatus . readIORef . spanHot) =<< readIORef ref
            statuses `shouldSatisfy` elem (Error (displayFaultT (toException refused)))
            attributeDump ref >>= \dump -> for_ requestSecrets $ \secret -> dump `shouldSatisfy` (not . T.isInfixOf secret)

    it "leaves any other exception to the library, which records it as before" $
        withTelemetrySpans (escapeFrom (\telemetry -> withAdvisorySyncSpan telemetry Npm (const (AdvisoryRefused, Nothing)) (throwIO (TestContractEscape "contract broken")))) $ \ref -> do
            hot <- readIORef . spanHot =<< findSpan ref "ecluse.advisory.sync.attempt"
            hotStatus hot `shouldSatisfy` \case
                Error message -> T.isInfixOf "contract broken" message
                _ -> False
            map eventName (exceptionEvents hot) `shouldBe` ["exception"]

    it "marks an attempt span errored with the fetch fault its result projects" $
        withTelemetrySpans (\telemetry -> withAdvisorySyncSpan telemetry Npm (const (AdvisoryFetchFailed, Just timedOutFetch)) pass) $ \ref -> do
            hot <- readIORef . spanHot =<< findSpan ref "ecluse.advisory.sync.attempt"
            hotStatus hot `shouldBe` Error "127.0.0.1:1: ResponseTimeout (the peer did not answer in time)"

-- A sync fetch that timed out at the closed port.
timedOutFetch :: OsvDbFetchFault
timedOutFetch = OsvDbTransport (transportFault TransportTimeout "127.0.0.1:1: ResponseTimeout")

{- The TLS manager tags a connection fault with its request, and the client instrumentation records
what the manager raises. The client span gets the client's reason, and the caller the request. -}
dataPlaneFaultSpec :: Spec
dataPlaneFaultSpec = describe "a data-plane fetch that cannot connect" $ do
    it "records the client's reason on the client span, never the request it was sent" $ do
        request <- tlsSignedRequest
        withSpanCapture (\tracerProvider -> void (fetchFault request =<< dataPlaneManager tracerProvider tlsManagerSettings)) $ \ref -> do
            hots <- traverse (readIORef . spanHot) =<< readIORef ref
            let reasons = [reason | Error reason <- map hotStatus hots]
                errorTypes = mapMaybe ((`textAttribute` "error.type") . hotAttributes) hots
            reasons `shouldSatisfy` any (T.isInfixOf "HostCannotConnect")
            for_ (reasons <> errorTypes) $ \recorded -> for_ requestSecrets $ \secret -> recorded `shouldSatisfy` (not . T.isInfixOf secret)
            -- The span records the URL it fetched, never a header.
            attributeDump ref >>= (`shouldSatisfy` (not . T.isInfixOf "token-secret"))

    it "hands the caller the transport fault the TLS manager raises alone" $ do
        request <- tlsSignedRequest
        let refusing = tlsManagerSettings{managerTlsConnection = pure (\_ _ _ -> throwIO (TLS.HandshakeFailed (TLS.Error_Misc "refused")))}
        alone <- fetchFault request =<< newManager refusing
        observed <- newIORef Nothing
        withSpanCapture (\tracerProvider -> writeIORef observed . Just =<< fetchFault request =<< dataPlaneManager tracerProvider refusing) (const pass)
        readIORef observed `shouldReturn` Just alone

-- 'signedRequest' over TLS, so the fetch takes the TLS manager's connection path.
tlsSignedRequest :: IO Request
tlsSignedRequest = (\request -> request{secure = True}) <$> signedRequest

-- The data plane's manager over @base@, as production builds it. The instrumentation reads the global tracer provider.
dataPlaneManager :: TracerProvider -> ManagerSettings -> IO Manager
dataPlaneManager tracerProvider base = do
    setGlobalTracerProvider tracerProvider
    newManager =<< instrumentDataPlaneManagerSettings (TelemetryEnabled (TelemetryProviders tracerProvider noopMeterProvider)) base

-- The transport fault a fetch hands its caller. A fetch that answers fails the case.
fetchFault :: Request -> Manager -> IO TransportFault
fetchFault request manager =
    try (httpNoBody request manager) >>= \case
        Left err -> pure (classifyTransport err)
        Right _ -> fail "a fetch of the closed port answered"

-- Serve an application that throws @err@ behind the telemetry WAI middleware, and send it one request.
throwingBehindMiddleware :: SomeException -> Telemetry -> IO ()
throwingBehindMiddleware err telemetry = do
    middleware <- telemetryWaiMiddleware telemetry
    manager <- newManager defaultManagerSettings
    void (tryAny (callWithBearer (middleware (\_ _ -> throwIO err)) manager))

-- Run a span body that throws, and swallow the throw, so the capture reads what the span recorded.
escapeFrom :: (Telemetry -> IO ()) -> Telemetry -> IO ()
escapeFrom emit telemetry = void (tryAny (emit telemetry))

-- The events a span holds, the library's own @exception@ event included.
exceptionEvents :: SpanHot -> [Event]
exceptionEvents = toList . appendOnlyBoundedCollectionValues . hotEvents

-- Read one span attribute back as text, 'Nothing' when the key is absent or holds
-- another type.
textAttribute :: Attributes -> Text -> Maybe Text
textAttribute attributes key = lookupAttribute attributes key >>= fromAttribute

-- How many @ecluse.@-namespaced attributes a span carries. Only a key follows a pair's
-- opening parenthesis, so a value can never be miscounted as a key.
ecluseAttributeCount :: Attributes -> Int
ecluseAttributeCount = T.count "(\"ecluse." . show

-- The @ecluse.mirror.job@ span links back to the @ecluse.mirror.enqueue@ span's trace, so the job is
-- linked to the enqueueing request, not merely correlated by package and version.
crossAsyncLinkSpec :: Spec
crossAsyncLinkSpec = describe "cross-async span link (enqueue to worker job)" $ do
    it "links the worker-job span back to the enqueueing span's trace"
        $ withTelemetrySpans
            ( \telemetry -> do
                -- The serve path captures the enqueue span's context. Carry it across the hop,
                -- and the worker re-establishes it as a link on its per-job span.
                carrier <- withMirrorEnqueueSpan telemetry samplePackage sampleVersion "https://artifact" (const Nothing) pure
                mirrorJobSpan telemetry carrier
            )
        $ \ref -> do
            enqueueSpan <- findSpan ref "ecluse.mirror.enqueue"
            links <- jobSpanLinks ref
            -- Exactly one link, pointing at the enqueue span's trace.
            links `shouldBe` [TraceCore.traceId (spanContext enqueueSpan)]

    it "carries no link when the job carried no trace context" $
        -- A job enqueued with no context (tracing was off at enqueue) yields an unlinked
        -- worker span: still emitted, just not linked.
        withTelemetrySpans (`mirrorJobSpan` Nothing) $ \ref ->
            jobSpanLinks ref `shouldReturn` []

    it "carries no link, and does not crash, when the carried context is not a valid W3C traceparent" $
        -- The carrier is untrusted transport: a present-but-unparseable traceparent must
        -- decode to no link and never fail the job (the worker mirrors regardless of trace).
        withTelemetrySpans (`mirrorJobSpan` Just (RemoteSpanContext "not-a-w3c-traceparent" "")) $ \ref ->
            jobSpanLinks ref `shouldReturn` []

-- One succeeding worker-job span over the given carrier.
mirrorJobSpan :: Telemetry -> Maybe RemoteSpanContext -> IO ()
mirrorJobSpan telemetry carrier =
    withMirrorJobSpan telemetry samplePackage sampleVersion carrier (const (JobSpanOutcome "succeeded" Nothing)) pass

-- The traces the captured worker-job span links back to.
jobSpanLinks :: IORef [ImmutableSpan] -> IO [TraceId]
jobSpanLinks ref = do
    jobHot <- readIORef . spanHot =<< findSpan ref "ecluse.mirror.job"
    pure (map (TraceCore.traceId . frozenLinkContext) (toList (appendOnlyBoundedCollectionValues (hotLinks jobHot))))

{- A swallowed best-effort enqueue failure sets the @ecluse.mirror.enqueue@ span status to
'Error' with the detail, so a trace explains it. A success leaves the status 'Unset'. -}
enqueueStatusSpec :: Spec
enqueueStatusSpec = describe "enqueue span status on a swallowed failure" $ do
    it "marks the enqueue span errored with the failure detail" $
        -- The body's result projects to a failure detail, as the swallowed-failure path does.
        withTelemetrySpans (enqueueSpanOver (Just "mirror enqueue failed: queue unreachable")) $ \ref ->
            enqueueSpanStatus ref `shouldReturn` Error "mirror enqueue failed: queue unreachable"

    it "leaves the enqueue span status unset on a successful enqueue" $
        withTelemetrySpans (enqueueSpanOver Nothing) $ \ref ->
            enqueueSpanStatus ref `shouldReturn` Unset

-- One enqueue span over the fixed artifact, whose body projects to the given failure detail.
enqueueSpanOver :: Maybe Text -> Telemetry -> IO ()
enqueueSpanOver detail telemetry =
    withMirrorEnqueueSpan telemetry samplePackage sampleVersion "https://artifact" (const detail) (const pass)

enqueueSpanStatus :: IORef [ImmutableSpan] -> IO SpanStatus
enqueueSpanStatus ref = hotStatus <$> (readIORef . spanHot =<< findSpan ref "ecluse.mirror.enqueue")

{- The enqueue span names the artifact's authority, never its URL. The upstream location can
carry a credential in its userinfo or query, and a span attribute leaves the node. -}
enqueueAuthoritySpec :: Spec
enqueueAuthoritySpec = describe "enqueue span artifact authority"
    $ it "records the artifact host and port, dropping userinfo, path, and query"
    $ withTelemetrySpans
        ( \telemetry ->
            withMirrorEnqueueSpan
                telemetry
                samplePackage
                sampleVersion
                "https://deploy:hunter2@registry.npmjs.org/left-pad/-/left-pad-1.3.0.tgz?sig=abc"
                (const Nothing)
                (const pass)
        )
    $ \ref -> do
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
        headersRef <- newIORef []
        withSpanCapture
            ( \tracerProvider -> do
                setGlobalTracerProvider tracerProvider
                -- The production posture: the SDK installs the W3C propagator globally, which is
                -- what the http-client instrumentation injects through.
                setGlobalTextMapPropagator w3cTraceContextPropagator
                settings <- instrumentManagerSettings dataPlaneInstrumentationConfig defaultManagerSettings
                manager <- newManager settings
                Warp.testWithApplication (pure (captureHeadersApp headersRef)) $ \port -> do
                    request <- parseRequest ("http://127.0.0.1:" <> show port <> "/some/package")
                    void (httpLbs request manager)
            )
            (const pass)
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
