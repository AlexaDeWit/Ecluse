module Ecluse.Telemetry.ResolveSpec (spec) where

import Data.IP (IP)
import Data.List (lookup)
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian)
import Test.Hspec

import Ecluse.Telemetry.Resolve (
    EgressDecision (..),
    EndpointEgress (..),
    EndpointSource (..),
    ResolvedTelemetry (..),
    TelemetryEndpoint (..),
    ThrottleEmit (..),
    ThrottleState (..),
    classifyResolved,
    egressDecision,
    initialThrottle,
    otelEnvironmentOverrides,
    readAllowPublicEgress,
    resolveTelemetry,
    throttleStep,
 )

{- | Tests for the telemetry config resolver, egress classifier, and export-failure
throttle. They exercise the promises a downstream operator and the @dd@ log object
depend on: the four-field precedence is __Datadog-value-wins → vanilla OpenTelemetry
→ default__; the resolved identity projects to the canonical @OTEL_*@ the SDK reads
while preserving operator-set resource attributes; a public endpoint is gated and a
loopback one is free; and SDK export errors are coalesced rather than flooded. Pure
and offline — no environment is mutated and no DNS is performed (the endpoint
classifier is exercised over constructed addresses).
-}
spec :: Spec
spec = do
    resolveSpec
    overridesSpec
    classifySpec
    decisionSpec
    allowPublicSpec
    throttleSpec

-- ── the precedence resolver ──────────────────────────────────────────────────

resolveSpec :: Spec
resolveSpec = describe "resolveTelemetry" $ do
    it "prefers DD_SERVICE over OTEL_SERVICE_NAME and the resource attribute" $
        rtServiceName
            ( resolveTelemetry
                [ ("DD_SERVICE", "from-dd")
                , ("OTEL_SERVICE_NAME", "from-otel")
                , ("OTEL_RESOURCE_ATTRIBUTES", "service.name=from-attr")
                ]
            )
            `shouldBe` "from-dd"

    it "falls back to OTEL_SERVICE_NAME, then the resource attribute, then the default" $ do
        rtServiceName (resolveTelemetry [("OTEL_SERVICE_NAME", "from-otel")]) `shouldBe` "from-otel"
        rtServiceName (resolveTelemetry [("OTEL_RESOURCE_ATTRIBUTES", "service.name=from-attr")])
            `shouldBe` "from-attr"
        rtServiceName (resolveTelemetry []) `shouldBe` "ecluse"

    it "resolves env and version DD-first, then the resource attribute, else unset" $ do
        let dd = resolveTelemetry [("DD_ENV", "prod"), ("DD_VERSION", "1.2.3")]
        rtEnvironment dd `shouldBe` Just "prod"
        rtVersion dd `shouldBe` Just "1.2.3"

        let attrs = resolveTelemetry [("OTEL_RESOURCE_ATTRIBUTES", "deployment.environment=stg,service.version=9")]
        rtEnvironment attrs `shouldBe` Just "stg"
        rtVersion attrs `shouldBe` Just "9"

        let none = resolveTelemetry []
        rtEnvironment none `shouldBe` Nothing
        rtVersion none `shouldBe` Nothing

    it "treats a present-but-blank value as unset" $
        rtEnvironment
            ( resolveTelemetry
                [("DD_ENV", "   "), ("OTEL_RESOURCE_ATTRIBUTES", "deployment.environment=stg")]
            )
            `shouldBe` Just "stg"

    it "resolves the endpoint DD_AGENT_HOST → OTEL endpoint → localhost default, tagging the source" $ do
        rtEndpoint (resolveTelemetry [("DD_AGENT_HOST", "10.1.2.3")])
            `shouldBe` TelemetryEndpoint "http://10.1.2.3:4318" FromDdAgentHost
        rtEndpoint (resolveTelemetry [("OTEL_EXPORTER_OTLP_ENDPOINT", "http://collector:4318")])
            `shouldBe` TelemetryEndpoint "http://collector:4318" FromOtelEndpoint
        rtEndpoint (resolveTelemetry [])
            `shouldBe` TelemetryEndpoint "http://localhost:4318" DefaultedEndpoint

-- ── the canonical OTEL_* projection ──────────────────────────────────────────

overridesSpec :: Spec
overridesSpec = describe "otelEnvironmentOverrides" $ do
    it "projects the resolved identity to the canonical OTEL_* the SDK reads" $ do
        let overrides = otelEnvironmentOverrides [("DD_SERVICE", "api"), ("DD_AGENT_HOST", "10.0.0.9")]
        lookup "OTEL_SERVICE_NAME" overrides `shouldBe` Just "api"
        lookup "OTEL_EXPORTER_OTLP_ENDPOINT" overrides `shouldBe` Just "http://10.0.0.9:4318"
        lookup "OTEL_EXPORTER_OTLP_PROTOCOL" overrides `shouldBe` Just "http/protobuf"

    it "overlays the resolved attributes onto operator-set resource attributes, preserving extras" $
        lookup
            "OTEL_RESOURCE_ATTRIBUTES"
            ( otelEnvironmentOverrides
                [ ("DD_SERVICE", "api")
                , ("DD_ENV", "prod")
                , ("DD_VERSION", "1.2.3")
                , ("OTEL_RESOURCE_ATTRIBUTES", "team=core")
                ]
            )
            `shouldBe` Just "deployment.environment=prod,service.name=api,service.version=1.2.3,team=core"

-- ── the egress classifier ────────────────────────────────────────────────────

classifySpec :: Spec
classifySpec = describe "classifyResolved" $ do
    it "is internal when every resolved address is in an internal range" $
        classifyResolved (Just (["127.0.0.1", "10.4.5.6", "::1"] :: [IP]))
            `shouldBe` EgressInternal

    it "is public when any resolved address is public" $
        classifyResolved (Just (["10.0.0.1", "8.8.8.8"] :: [IP]))
            `shouldBe` EgressPublic

    it "is unverified when the host resolves to nothing" $ do
        classifyResolved Nothing `shouldBe` EgressUnverified
        classifyResolved (Just []) `shouldBe` EgressUnverified

-- ── the boot decision ────────────────────────────────────────────────────────

decisionSpec :: Spec
decisionSpec = describe "egressDecision" $ do
    it "allows an internal endpoint silently" $
        egressDecision False "http://localhost:4318" EgressInternal `shouldBe` EgressAllow

    it "allows an unverified endpoint with a warning (never blocks boot)" $
        case egressDecision False "http://agent:4318" EgressUnverified of
            EgressAllowWithWarning _ -> pure ()
            other -> expectationFailure ("expected allow-with-warning, got " <> show other)

    it "fails boot for a public endpoint without the opt-in" $
        case egressDecision False "http://1.2.3.4:4318" EgressPublic of
            EgressFailBoot _ -> pure ()
            other -> expectationFailure ("expected fail-boot, got " <> show other)

    it "allows a public endpoint with a warning once opted in" $
        case egressDecision True "http://1.2.3.4:4318" EgressPublic of
            EgressAllowWithWarning _ -> pure ()
            other -> expectationFailure ("expected allow-with-warning, got " <> show other)

-- ── the public-egress opt-in ─────────────────────────────────────────────────

allowPublicSpec :: Spec
allowPublicSpec = describe "readAllowPublicEgress" $ do
    it "defaults to False when absent or blank" $ do
        readAllowPublicEgress [] `shouldBe` Right False
        readAllowPublicEgress [("PROXY_TELEMETRY_ALLOW_PUBLIC_EGRESS", "  ")] `shouldBe` Right False

    it "accepts the conventional boolean spellings" $ do
        readAllowPublicEgress [("PROXY_TELEMETRY_ALLOW_PUBLIC_EGRESS", "true")] `shouldBe` Right True
        readAllowPublicEgress [("PROXY_TELEMETRY_ALLOW_PUBLIC_EGRESS", "YES")] `shouldBe` Right True
        readAllowPublicEgress [("PROXY_TELEMETRY_ALLOW_PUBLIC_EGRESS", "0")] `shouldBe` Right False

    it "rejects a malformed value loudly" $
        readAllowPublicEgress [("PROXY_TELEMETRY_ALLOW_PUBLIC_EGRESS", "maybe")]
            `shouldSatisfy` isLeft

-- ── the export-failure throttle ──────────────────────────────────────────────

throttleSpec :: Spec
throttleSpec = describe "throttleStep" $ do
    let t0 = UTCTime (fromGregorian 2026 1 1) 0
        interval = 60

    it "surfaces the first error and records when it was logged" $ do
        let (state', emit) = throttleStep interval t0 initialThrottle
        emit `shouldBe` EmitFirst
        tsLastLogged state' `shouldBe` Just t0
        tsSuppressed state' `shouldBe` 0

    it "suppresses and counts errors within the window" $ do
        let (state', _) = throttleStep interval t0 initialThrottle
            (state'', emit) = throttleStep interval (addUTCTime 1 t0) state'
        emit `shouldBe` EmitSuppress
        tsSuppressed state'' `shouldBe` 1

    it "surfaces a heartbeat once the window elapses, carrying the suppressed count and resetting" $ do
        let (s1, _) = throttleStep interval t0 initialThrottle
            (s2, _) = throttleStep interval (addUTCTime 1 t0) s1
            (s3, emit) = throttleStep interval (addUTCTime 61 t0) s2
        emit `shouldBe` EmitHeartbeat 2
        tsSuppressed s3 `shouldBe` 0
        tsLastLogged s3 `shouldBe` Just (addUTCTime 61 t0)
