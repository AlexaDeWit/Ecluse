module Ecluse.Telemetry.ResolveSpec (spec) where

import Data.List (lookup)
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian)
import System.Environment (unsetEnv)
import Test.Hspec
import UnliftIO (bracket_)

import Katip (Environment (Environment), LogEnv, Namespace (Namespace), initLogEnv)

import Ecluse.Core.Security (hostAddress)
import Ecluse.Runtime.Telemetry.Resolve (
    EndpointSource (..),
    ResolvedTelemetry (..),
    TelemetryEndpoint (..),
    ThrottleEmit (..),
    ThrottleState (..),
    initialThrottle,
    otelEnvironmentOverrides,
    prepareTelemetry,
    resolveTelemetry,
    throttleStep,
 )

{- | Tests for the telemetry config resolver and the export-failure throttle. They
exercise the promises a downstream operator and the @dd@ log object depend on: the
four-field precedence is __Datadog-value-wins → vanilla OpenTelemetry → default__; the
resolved identity projects to the canonical @OTEL_*@ the SDK reads while preserving
operator-set resource attributes; and SDK export errors are coalesced rather than
flooded. The 'prepareTelemetry' cases drive the boot normalisation and restore the
environment they set; everything else is pure and offline.
-}
spec :: Spec
spec = do
    resolveSpec
    overridesSpec
    prepareSpec
    throttleSpec

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

    it "brackets a literal IPv6 DD_AGENT_HOST and leaves an already-qualified value alone" $ do
        teUrl (rtEndpoint (resolveTelemetry [("DD_AGENT_HOST", "fd00::1")]))
            `shouldBe` "http://[fd00::1]:4318"
        -- A value already carrying a scheme is used verbatim (no double scheme).
        teUrl (rtEndpoint (resolveTelemetry [("DD_AGENT_HOST", "https://agent.internal:4318")]))
            `shouldBe` "https://agent.internal:4318"
        -- A host already carrying a port is not given a second.
        teUrl (rtEndpoint (resolveTelemetry [("DD_AGENT_HOST", "10.0.0.9:4317")]))
            `shouldBe` "http://10.0.0.9:4317"

    it "keeps a bracketed IPv6 endpoint host extractable (a well-formed authority)" $
        -- Regression: an unbracketed IPv6 authority truncates under 'hostAddress'
        -- ("fd00::1:4318" → "fd00") and would hand the SDK exporter a malformed URL.
        hostAddress (teUrl (rtEndpoint (resolveTelemetry [("DD_AGENT_HOST", "2606:4700:4700::1111")])))
            `shouldBe` "2606:4700:4700::1111"

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

    it "lets a resolved attribute win over a same-key inherited OTEL_RESOURCE_ATTRIBUTES value" $
        -- The merge is a left-biased union with the resolved map on the left, so a
        -- stale operator-set value of the same key never overrides the resolution.
        lookup
            "OTEL_RESOURCE_ATTRIBUTES"
            ( otelEnvironmentOverrides
                [ ("DD_SERVICE", "api")
                , ("OTEL_RESOURCE_ATTRIBUTES", "service.name=stale,team=core")
                ]
            )
            `shouldBe` Just "service.name=api,team=core"

prepareSpec :: Spec
prepareSpec = describe "prepareTelemetry" $ do
    it "normalises the canonical OTEL_* environment the SDK reads from the resolved identity" $ do
        logEnv <- quietLogEnv
        endpoint <- withCleanOtelEnv $ do
            prepareTelemetry logEnv [("DD_SERVICE", "api"), ("DD_AGENT_HOST", "10.0.0.9")]
            lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
        endpoint `shouldBe` Just "http://10.0.0.9:4318"

    it "warns and defaults to localhost when no endpoint is configured" $ do
        logEnv <- quietLogEnv
        endpoint <- withCleanOtelEnv $ do
            prepareTelemetry logEnv []
            lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
        endpoint `shouldBe` Just "http://localhost:4318"
  where
    -- A scribe-less katip environment: log calls are accepted and dropped, so the
    -- boot warning under test produces no stdout.
    quietLogEnv :: IO LogEnv
    quietLogEnv = initLogEnv (Namespace ["test"]) (Environment "test")

    -- Run an action, then clear the OTEL_* variables prepareTelemetry writes, so a
    -- mutated process environment never leaks into another spec.
    withCleanOtelEnv :: IO a -> IO a
    withCleanOtelEnv = bracket_ (pure ()) (mapM_ unsetEnv otelVars)

    otelVars :: [String]
    otelVars =
        [ "OTEL_SERVICE_NAME"
        , "OTEL_EXPORTER_OTLP_ENDPOINT"
        , "OTEL_EXPORTER_OTLP_PROTOCOL"
        , "OTEL_RESOURCE_ATTRIBUTES"
        ]

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
