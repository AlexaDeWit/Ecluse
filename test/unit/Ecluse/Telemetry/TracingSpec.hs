module Ecluse.Telemetry.TracingSpec (spec) where

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
import Network.HTTP.Types.Header (hAuthorization, hUserAgent)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp
import OpenTelemetry.Exporter.InMemory.Span (inMemoryListExporter)
import OpenTelemetry.Instrumentation.HttpClient (instrumentManagerSettings)
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware')
import OpenTelemetry.Metric (noopMeterProvider)
import OpenTelemetry.Metric.Core (getMeter)
import OpenTelemetry.Trace (
    createTracerProvider,
    emptyTracerProviderOptions,
    forceFlushTracerProvider,
    setGlobalTracerProvider,
 )
import OpenTelemetry.Trace.Core (
    ImmutableSpan (spanHot),
    SpanHot (hotAttributes, hotName),
 )

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Server.Response (
    RejectReason (BelowIntegrityFloor, ByPolicy, MissingIntegrity, Unavailable, UpstreamInvalid),
    Rejection (Rejection),
    RuleName (RuleName),
    ServeDecision (Admit, Reject),
    Transience (WontResolve),
 )
import Ecluse.Core.Version (mkVersion)
import Ecluse.Telemetry (telemetryDisabled)
import Ecluse.Telemetry.Tracing (
    dataPlaneInstrumentationConfig,
    ruleVerdictFields,
    withRuleEvalSpan,
 )

{- | Tests for the request-lifecycle tracing layer. They prove the three promises
this slice carries that can be proven at the pure\/offline tier: the verdict
attribute mapping is exact (so a denial is explainable from the trace), a domain-span
bracket is genuinely inert when telemetry is disabled, and — the load-bearing one —
the forwarded client token and the @Authorization@ header are scrubbed from anything
the http-client and WAI instrumentation capture.

The scrub tests are not vacuous: each drives a real request carrying a
@Bearer@ token through the very instrumentation the proxy installs (the http-client
manager hooks under 'dataPlaneInstrumentationConfig' and the WAI server-span
middleware), captures the resulting spans through an in-memory exporter, and asserts
both that a span was produced /and/ that the token appears in no captured attribute.
-}
spec :: Spec
spec = do
    verdictMappingSpec
    gatingSpec
    scrubSpec

-- A distinctive secret that must never surface on a span; the scrub assertions search
-- the captured spans for it.
secretToken :: Text
secretToken = "s3cr3t-bearer-tok3n-do-not-leak"

-- ── verdict attribute mapping ──────────────────────────────────────────────────

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

-- ── gating (inert when disabled) ───────────────────────────────────────────────

gatingSpec :: Spec
gatingSpec = describe "withRuleEvalSpan (telemetry disabled)" $
    it "runs the body and returns its result, opening no span" $ do
        -- With the disabled handle there is no tracer to reach for, so the helper must
        -- simply run the body and thread its result through, never demanding a provider.
        result <-
            withRuleEvalSpan telemetryDisabled (mkPackageName Npm Nothing "left-pad") (mkVersion Npm "1.0.0") $
                pure (42 :: Int, Admit)
        result `shouldBe` 42

-- ── secret scrubbing ───────────────────────────────────────────────────────────

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

-- A trivial @200@ application: the target the instrumented requests are driven at.
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

-- The captured spans rendered to text — every span's name and full attribute set —
-- so a substring search proves the secret is present nowhere on any span.
attributeDump :: IORef [ImmutableSpan] -> IO Text
attributeDump ref = do
    spans <- readIORef ref
    parts <- forM spans $ \theSpan -> do
        hot <- readIORef (spanHot theSpan)
        pure (hotName hot <> " " <> show (hotAttributes hot))
    pure (T.intercalate "\n" parts)
