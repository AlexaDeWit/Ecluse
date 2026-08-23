-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Telemetry.ResolveSpec (spec) where

import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian)
import Data.Version (showVersion)
import Paths_ecluse (version)
import System.Environment (unsetEnv)
import Test.Hspec
import UnliftIO (bracket_)

import OpenTelemetry.Attributes (Attribute, fromAttribute)
import OpenTelemetry.Resource.Detect (detectResourceAttributes)

import Ecluse.Test.Log (newTestLogEnv)

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
    telemetryWarnings,
    throttleStep,
 )

{- | Tests the telemetry config resolver and the export-failure throttle. Precedence is the
Datadog value, then vanilla OpenTelemetry, then the default. One W3C baggage grammar reads
@OTEL_RESOURCE_ATTRIBUTES@ for both the log identity and the span resource. Export errors
coalesce.
-}
spec :: Spec
spec = do
    resolveSpec
    resourceAttributeSpec
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

    it "resolves env and version DD-first, then the resource attribute" $ do
        let dd = resolveTelemetry [("DD_ENV", "prod"), ("DD_VERSION", "1.2.3")]
        rtEnvironment dd `shouldBe` Just "prod"
        rtVersion dd `shouldBe` Just "1.2.3"

        let attrs = resolveTelemetry [("OTEL_RESOURCE_ATTRIBUTES", "deployment.environment=stg,service.version=9")]
        rtEnvironment attrs `shouldBe` Just "stg"
        rtVersion attrs `shouldBe` Just "9"

    it "leaves the environment unset but falls the version back to the build version" $ do
        -- The process cannot know its deployment environment, so that field stays optional. The
        -- version is a fact about the running binary, so every trace and log line carries one.
        let none = resolveTelemetry []
        rtEnvironment none `shouldBe` Nothing
        rtVersion none `shouldBe` Just buildVersion

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

resourceAttributeSpec :: Spec
resourceAttributeSpec = describe "OTEL_RESOURCE_ATTRIBUTES" $ do
    it "percent-decodes a value, so the log identity reads what the SDK reads" $
        rtServiceName (resolveTelemetry [("OTEL_RESOURCE_ATTRIBUTES", "service.name=a%20b")])
            `shouldBe` "a b"

    it "tolerates a trailing comma and stray spacing" $
        -- This is operator-authored configuration, so blank members are dropped before the
        -- baggage grammar sees the value.
        rtEnvironment
            (resolveTelemetry [("OTEL_RESOURCE_ATTRIBUTES", " deployment.environment=stg , ")])
            `shouldBe` Just "stg"

    it "resolves as unset and warns when the baggage grammar rejects the value" $ do
        -- A non-token key drops the operator's attributes from both halves. The projection
        -- exports the resolved identity alone, so the log object and the span resource agree.
        let environment = [("OTEL_RESOURCE_ATTRIBUTES", "bad key=1,service.name=api")]
        rtServiceName (resolveTelemetry environment) `shouldBe` "ecluse"
        rtEnvironment (resolveTelemetry environment) `shouldBe` Nothing
        telemetryWarnings environment
            `shouldSatisfy` any (T.isInfixOf "OTEL_RESOURCE_ATTRIBUTES is not valid W3C baggage")
        detectedResourceAttributes environment
            `shouldReturn` [("service.name", "ecluse"), ("service.version", buildVersion)]

    it "raises no warning for a value the grammar accepts" $
        telemetryWarnings
            [ ("OTEL_EXPORTER_OTLP_ENDPOINT", "http://collector:4318")
            , ("OTEL_RESOURCE_ATTRIBUTES", "service.name=api,")
            ]
            `shouldBe` []

    it "lands a percent-encoded value identically on the log identity and the span resource" $ do
        let environment = [("OTEL_RESOURCE_ATTRIBUTES", "service.name=a%20b")]
        detected <- detectedResourceAttributes environment
        lookup "service.name" detected `shouldBe` Just (rtServiceName (resolveTelemetry environment))
        lookup "service.name" detected `shouldBe` Just "a b"

    it "round-trips a value carrying an encoded comma through the SDK's own codec" $
        detectedResourceAttributes [("OTEL_RESOURCE_ATTRIBUTES", "team=core%2Cplatform")]
            `shouldReturn` [ ("service.name", "ecluse")
                           , ("service.version", buildVersion)
                           , ("team", "core,platform")
                           ]

overridesSpec :: Spec
overridesSpec = describe "otelEnvironmentOverrides" $ do
    it "projects the resolved identity to the canonical OTEL_* the SDK reads" $ do
        let overrides = otelEnvironmentOverrides [("DD_SERVICE", "api"), ("DD_AGENT_HOST", "10.0.0.9")]
        lookup "OTEL_SERVICE_NAME" overrides `shouldBe` Just "api"
        lookup "OTEL_EXPORTER_OTLP_ENDPOINT" overrides `shouldBe` Just "http://10.0.0.9:4318"
        lookup "OTEL_EXPORTER_OTLP_PROTOCOL" overrides `shouldBe` Just "http/protobuf"

    it "overlays the resolved attributes onto operator-set resource attributes, preserving extras" $
        detectedResourceAttributes
            [ ("DD_SERVICE", "api")
            , ("DD_ENV", "prod")
            , ("DD_VERSION", "1.2.3")
            , ("OTEL_RESOURCE_ATTRIBUTES", "team=core")
            ]
            `shouldReturn` [ ("deployment.environment", "prod")
                           , ("service.name", "api")
                           , ("service.version", "1.2.3")
                           , ("team", "core")
                           ]

    it "lets a resolved attribute win over a same-key inherited OTEL_RESOURCE_ATTRIBUTES value" $
        -- The resolved attributes are inserted over the inherited baggage, so a stale
        -- operator-set value of the same key never overrides the resolution.
        detectedResourceAttributes
            [("DD_SERVICE", "api"), ("OTEL_RESOURCE_ATTRIBUTES", "service.name=stale,team=core")]
            `shouldReturn` [ ("service.name", "api")
                           , ("service.version", buildVersion)
                           , ("team", "core")
                           ]

prepareSpec :: Spec
prepareSpec = describe "prepareTelemetry" $ do
    it "normalises the canonical OTEL_* environment the SDK reads from the resolved identity" $ do
        logEnv <- newTestLogEnv
        endpoint <- withCleanOtelEnv $ do
            prepareTelemetry logEnv [("DD_SERVICE", "api"), ("DD_AGENT_HOST", "10.0.0.9")]
            lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
        endpoint `shouldBe` Just "http://10.0.0.9:4318"

    it "warns and defaults to localhost when no endpoint is configured" $ do
        logEnv <- newTestLogEnv
        endpoint <- withCleanOtelEnv $ do
            prepareTelemetry logEnv []
            lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
        endpoint `shouldBe` Just "http://localhost:4318"

{- The resource attributes the OpenTelemetry SDK detects from the environment the projection
writes, sorted by key. This is the span-resource half of the identity, read through the SDK's
own detector. The encoder emits members in hash order, so only the decoded set is stable. -}
detectedResourceAttributes :: [(String, String)] -> IO [(Text, Text)]
detectedResourceAttributes environment = do
    logEnv <- newTestLogEnv
    detected <- withCleanOtelEnv $ do
        prepareTelemetry logEnv environment
        detectResourceAttributes
    pure (sortOn fst (mapMaybe textAttribute detected))
  where
    textAttribute :: (Text, Attribute) -> Maybe (Text, Text)
    textAttribute (key, attribute) = (key,) <$> fromAttribute attribute

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

-- The build version the resolver falls back to, read from the same generated module
-- the resolver reads. A version bump therefore does not red these expectations.
buildVersion :: Text
buildVersion = toText (showVersion version)

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
