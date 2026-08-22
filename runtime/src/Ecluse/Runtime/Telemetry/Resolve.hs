-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Telemetry configuration resolution and export-failure routing: the boot-time
substrate that sits between the operator's environment and the OpenTelemetry SDK.

Écluse's maintainer runs Datadog, but the project is vendor-neutral, so an operator
may describe the same telemetry identity in either dialect. A Datadog shop sets the
@DD_*@ variables. A plain OpenTelemetry shop sets the @OTEL_*@ ones. This module is
the __self-aligning resolver__ that collapses both into one answer, so logs and
traces share a single identity whichever dialect was provided.

== The resolver

'resolveTelemetry' is a bounded precedence table over exactly four fields:
@service.name@, @deployment.environment@, @service.version@, and the OTLP export
endpoint. Each resolves __Datadog-value-wins → vanilla OpenTelemetry → default__. It
is deliberately /not/ a general per-variable merge: only these four cross between the
dialects, and only their fixed precedence is encoded. The @DD_API_KEY@ \/ @DD_SITE@
agentless-SaaS credentials are __never read__. The exporter targets an
__operator-declared__, node-local collector\/Agent, never a vendor's cloud directly,
so no key in the environment can turn into off-cluster egress. The endpoint itself is
a declared destination (like the mirror queue), not an attack surface. This module
normalises it and uses it as given, never classified or gated.

The resolved 'ResolvedTelemetry' is the __single source of truth__ for both halves of
the telemetry stack. 'otelEnvironmentOverrides' projects it back to the canonical
@OTEL_*@ variables the env-driven SDK reads, so a @DD_*@-only deployment still
configures the exporter. The same record feeds the @dd@ log object that stitches a log
line to its trace.

== Export-failure routing

Telemetry failures must stay off the request path and out of raw stderr. The SDK's
batch exporter runs asynchronously, so an unreachable collector never touches a served
request. This module owns the __shared throttle__ those failures coalesce through. An
'ExportFailureSink' carries one throttle plus a @katip@ target. 'routeExportFailure'
surfaces the first failure plainly, then a periodic heartbeat carrying the suppressed
count. A persistently unreachable endpoint is then one visible warning and a heartbeat,
not a per-flush flood. The exporter wrappers ("Ecluse.Runtime.Telemetry") feed the sink
through 'observeExportResult'. 'installExportErrorHandler' routes the SDK's own
diagnostic stream through the same sink.

@docs\/architecture\/observability.md@ describes the configuration model and the
export-failure mechanism.
-}
module Ecluse.Runtime.Telemetry.Resolve (
    -- * The resolved telemetry identity
    ResolvedTelemetry (..),
    TelemetryEndpoint (..),
    EndpointSource (..),
    resolveTelemetry,

    -- * Canonical @OTEL_*@ projection
    otelEnvironmentOverrides,

    -- * Export-failure throttle (pure core)
    ThrottleState (..),
    ThrottleEmit (..),
    initialThrottle,
    throttleInterval,
    throttleStep,

    -- * Export-failure routing
    ExportFailureSink,
    newExportFailureSink,
    exportFailureSink,
    routeExportFailure,
    observeExportResult,
    installExportErrorHandler,

    -- * Boot wiring
    prepareTelemetry,
) where

import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Version (showVersion)
import Paths_ecluse (version)
import System.Environment (setEnv)

import Katip (LogEnv, Severity (WarningS), logFM, ls)
import Katip.Monadic (runKatipContextT)
import OpenTelemetry.Exporter.Span (ExportResult (..))
import OpenTelemetry.Internal.Logging (setGlobalErrorHandler)

import Ecluse.Core.Text (nonBlank)
import Ecluse.Runtime.Log (moduleField)

{- | Where a resolved OTLP endpoint came from, so the boot path can distinguish a
deliberately-configured target from the silent default and warn on the latter.
-}
data EndpointSource
    = -- | Derived from @DD_AGENT_HOST@ (as @http:\/\/{host}:4318@).
      FromDdAgentHost
    | -- | Taken verbatim from @OTEL_EXPORTER_OTLP_ENDPOINT@.
      FromOtelEndpoint
    | -- | No endpoint was configured, so the @http:\/\/localhost:4318@ default applies.
      DefaultedEndpoint
    deriving stock (Eq, Show)

-- | A resolved OTLP export endpoint and the source it was resolved from.
data TelemetryEndpoint = TelemetryEndpoint
    { teUrl :: Text
    -- ^ The endpoint URL the exporter targets (always @http\/protobuf@).
    , teSource :: EndpointSource
    -- ^ How the URL was resolved.
    }
    deriving stock (Eq, Show)

{- | The telemetry identity resolved from the environment: the single source of
truth for both the SDK configuration and the @dd@ log object. 'rtEnvironment' is
'Nothing' when the operator named neither dialect's form. The process cannot know its
own deployment environment, so that field stays a genuinely optional resource attribute
and never carries a placeholder.
-}
data ResolvedTelemetry = ResolvedTelemetry
    { rtServiceName :: Text
    -- ^ @service.name@ \/ @dd.service@ (defaults to @ecluse@).
    , rtEnvironment :: Maybe Text
    -- ^ @deployment.environment@ \/ @dd.env@, when configured.
    , rtVersion :: Maybe Text
    -- ^ @service.version@ \/ @dd.version@ (defaults to the build version).
    , rtEndpoint :: TelemetryEndpoint
    -- ^ The resolved OTLP export endpoint.
    }
    deriving stock (Eq, Show)

{- | Resolve the telemetry identity from an environment list, each field
__Datadog-value-wins → vanilla OpenTelemetry → default__. The @service.name@ field
falls @DD_SERVICE@ → @OTEL_SERVICE_NAME@ → @service.name@ in
@OTEL_RESOURCE_ATTRIBUTES@ → @ecluse@. The @deployment.environment@ field falls
@DD_ENV@ → the matching @OTEL_RESOURCE_ATTRIBUTES@ key → unset. The @service.version@
field falls @DD_VERSION@ → the matching @OTEL_RESOURCE_ATTRIBUTES@ key → the running
binary's own build version. The endpoint falls @DD_AGENT_HOST@ (as
@http:\/\/{host}:4318@) → @OTEL_EXPORTER_OTLP_ENDPOINT@ → @http:\/\/localhost:4318@.

The resolver treats a present but blank value as unset, so an empty @DD_ENV=@ does not
stamp an empty environment onto every signal. It never consults @DD_API_KEY@ or
@DD_SITE@.

>>> rtServiceName (resolveTelemetry [("DD_SERVICE", "api"), ("OTEL_SERVICE_NAME", "ignored")])
"api"

>>> teUrl (rtEndpoint (resolveTelemetry []))
"http://localhost:4318"
-}
resolveTelemetry :: [(String, String)] -> ResolvedTelemetry
resolveTelemetry environment =
    ResolvedTelemetry
        { rtServiceName = fromMaybe defaultServiceName serviceName
        , rtEnvironment = lk "DD_ENV" <|> attr "deployment.environment"
        , rtVersion = lk "DD_VERSION" <|> attr "service.version" <|> Just buildVersion
        , rtEndpoint = endpoint
        }
  where
    lk :: String -> Maybe Text
    lk name = nonBlank . toText =<< lookup name environment

    attrs :: Map Text Text
    attrs = maybe Map.empty parseResourceAttributes (lk "OTEL_RESOURCE_ATTRIBUTES")

    attr :: Text -> Maybe Text
    attr key = nonBlank =<< Map.lookup key attrs

    serviceName :: Maybe Text
    serviceName = lk "DD_SERVICE" <|> lk "OTEL_SERVICE_NAME" <|> attr "service.name"

    endpoint :: TelemetryEndpoint
    endpoint = case lk "DD_AGENT_HOST" of
        Just host -> TelemetryEndpoint (agentHostUrl host) FromDdAgentHost
        Nothing -> case lk "OTEL_EXPORTER_OTLP_ENDPOINT" of
            Just url -> TelemetryEndpoint url FromOtelEndpoint
            Nothing -> TelemetryEndpoint defaultEndpointUrl DefaultedEndpoint

defaultServiceName :: Text
defaultServiceName = "ecluse"

{- The running binary's own version, the fallback @service.version@. An operator who
names no version still gets one identity on every trace and every log line. The version
is a fact about the process rather than a placeholder. -}
buildVersion :: Text
buildVersion = toText (showVersion version)

defaultEndpointUrl :: Text
defaultEndpointUrl = "http://localhost:4318"

{- Build the OTLP HTTP\/protobuf endpoint URL for a Datadog Agent host: the Agent's
OTLP receiver listens on 4318 for HTTP\/protobuf, the only transport we build. A
literal IPv6 host is bracketed so the authority is well-formed: @http:\/\/[fd00::1]:4318@,
not the invalid @http:\/\/fd00::1:4318@ the SDK exporter would fail to parse. A host
that already carries a scheme goes through verbatim, and one that already carries a
port gains no second port. A deliberately-qualified @DD_AGENT_HOST@ is never mangled. Colon
count disambiguates: a bare IPv6 literal has two or more colons, a @host:port@ exactly
one, and a bare host or IPv4 none. -}
agentHostUrl :: Text -> Text
agentHostUrl raw
    | "://" `T.isInfixOf` host = host
    | otherwise = "http://" <> authority
  where
    host = T.strip raw
    authority
        | "[" `T.isPrefixOf` host = if "]:" `T.isInfixOf` host then host else host <> ":4318"
        | T.count ":" host >= 2 = "[" <> host <> "]:4318"
        | T.count ":" host == 1 = host
        | otherwise = host <> ":4318"

{- | Project the resolved identity back to the canonical @OTEL_*@ variables the
env-driven SDK reads, so a @DD_*@-only deployment still configures the exporter. The
overrides set @OTEL_SERVICE_NAME@, the OTLP endpoint, the @http\/protobuf@ protocol,
and an @OTEL_RESOURCE_ATTRIBUTES@ value. That protocol is the only transport built,
since gRPC is behind a disabled cabal flag. The resolution overlays the
@service.name@\/@deployment.environment@\/@service.version@ keys there and preserves
every other operator-set attribute.

Applied with 'System.Environment.setEnv' before the SDK initialises (see
'prepareTelemetry'). Idempotent for a vanilla deployment that already set the same
@OTEL_*@ values.
-}
otelEnvironmentOverrides :: [(String, String)] -> [(String, String)]
otelEnvironmentOverrides environment =
    [ ("OTEL_SERVICE_NAME", toString (rtServiceName resolved))
    , ("OTEL_EXPORTER_OTLP_ENDPOINT", toString (teUrl (rtEndpoint resolved)))
    , ("OTEL_EXPORTER_OTLP_PROTOCOL", "http/protobuf")
    , ("OTEL_RESOURCE_ATTRIBUTES", toString (renderResourceAttributes (mergedResourceAttributes resolved environment)))
    ]
  where
    resolved :: ResolvedTelemetry
    resolved = resolveTelemetry environment

mergedResourceAttributes :: ResolvedTelemetry -> [(String, String)] -> Map Text Text
mergedResourceAttributes resolved environment =
    -- Left-biased union: a resolved attribute must win over an inherited
    -- OTEL_RESOURCE_ATTRIBUTES value of the same key. The resolved map therefore sits
    -- on the LEFT of (<>), since 'Map.union' is left-biased. Reversing the operands
    -- would let a stale operator-set value silently override the resolution.
    resolvedAttrs <> existing
  where
    existing :: Map Text Text
    existing =
        maybe
            Map.empty
            parseResourceAttributes
            (nonBlank . toText =<< lookup "OTEL_RESOURCE_ATTRIBUTES" environment)

    resolvedAttrs :: Map Text Text
    resolvedAttrs =
        Map.fromList
            [ (key, value)
            | (key, Just value) <-
                [ ("service.name", Just (rtServiceName resolved))
                , ("deployment.environment", rtEnvironment resolved)
                , ("service.version", rtVersion resolved)
                ]
            ]

-- Parse the @key1=value1,key2=value2@ resource-attribute string into a map,
-- trimming surrounding whitespace and dropping any entry that carries no @=@ or an
-- empty key. Lenient by design, because this is operator-authored configuration, not
-- a wire format, so a stray trailing comma or spacing is tolerated.
parseResourceAttributes :: Text -> Map Text Text
parseResourceAttributes raw =
    Map.fromList
        [ (key, T.strip (T.drop 1 value))
        | pair <- T.splitOn "," raw
        , let (before, value) = T.breakOn "=" pair
        , let key = T.strip before
        , not (T.null key)
        , not (T.null value)
        ]

-- Render a resource-attribute map back to the @key1=value1,key2=value2@ form, in
-- key order so the projection is deterministic.
renderResourceAttributes :: Map Text Text -> Text
renderResourceAttributes =
    T.intercalate "," . map (\(key, value) -> key <> "=" <> value) . Map.toList

defaultedEndpointMessage :: Text -> Text
defaultedEndpointMessage url =
    "no telemetry export endpoint configured (DD_AGENT_HOST / OTEL_EXPORTER_OTLP_ENDPOINT unset); defaulting to "
        <> url
        <> "."

{- | The throttle state for SDK export-error routing: when an error was last logged,
and how many it suppressed since. Exposed so a test asserts the throttle decision
without wall-clock timing.
-}
data ThrottleState = ThrottleState
    { tsLastLogged :: Maybe UTCTime
    -- ^ When an error was last surfaced ('Nothing' before the first).
    , tsSuppressed :: Int
    -- ^ Errors suppressed since the last surfaced one.
    }
    deriving stock (Eq, Show)

-- | What 'throttleStep' decided to do with an export error.
data ThrottleEmit
    = -- | The first error: surface it plainly.
      EmitFirst
    | {- | The throttle window elapsed: surface a heartbeat carrying the count of
      errors since the last surfaced one (this one included).
      -}
      EmitHeartbeat Int
    | -- | Within the window: suppress and count.
      EmitSuppress
    deriving stock (Eq, Show)

-- | The initial throttle state: nothing logged, nothing suppressed.
initialThrottle :: ThrottleState
initialThrottle = ThrottleState Nothing 0

-- | How long export errors are coalesced between surfaced heartbeats.
throttleInterval :: NominalDiffTime
throttleInterval = 60

{- | Advance the throttle for one export error at @now@. The first error surfaces
plainly. A heartbeat surfaces once the 'throttleInterval' has elapsed since the last
surfaced one, which resets the suppressed count. Otherwise the step suppresses the
error and counts it. Pure, so a test asserts a sequence of @(time, decision)@ steps
directly.
-}
throttleStep :: NominalDiffTime -> UTCTime -> ThrottleState -> (ThrottleState, ThrottleEmit)
throttleStep interval now st = case tsLastLogged st of
    Nothing -> (ThrottleState (Just now) 0, EmitFirst)
    Just lastLogged
        | diffUTCTime now lastLogged >= interval ->
            (ThrottleState (Just now) 0, EmitHeartbeat (tsSuppressed st + 1))
        | otherwise ->
            (st{tsSuppressed = tsSuppressed st + 1}, EmitSuppress)

{- | Prepare the telemetry substrate at boot, before the SDK initialises. Resolve the
identity and normalise the canonical @OTEL_*@ environment the env-driven SDK reads, so a
@DD_*@-only deployment still configures the exporter. The export-failure observation
is wired when the substrate stands up ("Ecluse.Runtime.Telemetry.withTelemetry"). That
step builds the shared sink and installs the exporter wrappers and the SDK error
handler.

A defaulted endpoint, with neither @DD_AGENT_HOST@ nor @OTEL_EXPORTER_OTLP_ENDPOINT@
set, surfaces through @katip@ as one boot warning and falls back to
@http:\/\/localhost:4318@. It is never a failure. The OTLP endpoint is an
__operator-declared destination__ (like the mirror queue), so this module normalises it
and uses it as given, never classified or gated.
-}
prepareTelemetry :: LogEnv -> [(String, String)] -> IO ()
prepareTelemetry logEnv environment = do
    let resolved = resolveTelemetry environment
    when (teSource (rtEndpoint resolved) == DefaultedEndpoint) $
        logResolve logEnv WarningS (defaultedEndpointMessage (teUrl (rtEndpoint resolved)))
    mapM_ (uncurry setEnv) (otelEnvironmentOverrides environment)

{- | The shared export-failure sink: a single throttle plus the @katip@ target that
every export failure feeds. The failures come from the span exporter, the metric
exporter, and the SDK's own diagnostic stream. A persistently unreachable collector is
then one coalesced stream (the first failure plainly, then a periodic heartbeat) rather
than several independent floods.

The clock and the surfacing action are injected, so a test asserts the throttle
decision without wall-clock timing or a live @katip@ scribe. This mirrors the pure
'throttleStep' tests. 'exportFailureSink' wires the production clock and @katip@ target.
-}
data ExportFailureSink = ExportFailureSink
    { sinkNow :: IO UTCTime
    , sinkState :: IORef ThrottleState
    , sinkSurface :: Severity -> Text -> IO ()
    }

-- | Build an export-failure sink over an injected clock and surfacing action.
newExportFailureSink :: IO UTCTime -> (Severity -> Text -> IO ()) -> IO ExportFailureSink
newExportFailureSink now surface = do
    throttleRef <- newIORef initialThrottle
    pure ExportFailureSink{sinkNow = now, sinkState = throttleRef, sinkSurface = surface}

{- | The production sink: the wall clock and the composition-root 'LogEnv' as the
@katip@ target, tagged with this module. This is the plain-'IO' @katip@ path the boot
phase uses.
-}
exportFailureSink :: LogEnv -> IO ExportFailureSink
exportFailureSink logEnv = newExportFailureSink getCurrentTime (logResolve logEnv)

{- | Route one export-failure diagnostic through the shared throttle into @katip@. The
first surfaces plainly. A heartbeat carrying the suppressed count surfaces once
'throttleInterval' has elapsed since the last surfaced one. Otherwise the throttle
suppresses the diagnostic and counts it.
-}
routeExportFailure :: ExportFailureSink -> Text -> IO ()
routeExportFailure sink diagnostic = do
    now <- sinkNow sink
    emit <- atomicModifyIORef' (sinkState sink) (throttleStep throttleInterval now)
    case emit of
        EmitFirst -> sinkSurface sink WarningS (firstErrorMessage diagnostic)
        EmitHeartbeat suppressed -> sinkSurface sink WarningS (heartbeatMessage suppressed diagnostic)
        EmitSuppress -> pass

{- | Observe one exporter's 'ExportResult', routing a 'Failure' through the sink and
ignoring a 'Success'. This only /observes/ the failure. The inner result is the
caller's to return unchanged, so export semantics are untouched: a failed export stays
off the request path. The @signal@ argument names the failing exporter
(@span@ \/ @metric@).
-}
observeExportResult :: ExportFailureSink -> Text -> ExportResult -> IO ()
observeExportResult sink signal = \case
    Success -> pass
    Failure mErr -> routeExportFailure sink (signal <> " export failed" <> maybe "" ((": " <>) . show) mErr)

{- | Install a process-global handler for the SDK's own diagnostic stream, routed through
the shared sink so it coalesces with the exporter-failure feed. In @hs-opentelemetry
1.0.0.0@ the only caller of this handler is the SDK's internal logging, which drops a
failed OTLP export rather than routing it here. The export-failure feed therefore comes
from the exporter wrappers ('observeExportResult'). This handler stays for the
SDK-internal diagnostics it still serves.

The forwarded diagnostic 'String' is the SDK's own text and is trusted not to carry
secrets. This module never reads the credential-bearing telemetry inputs
(@OTEL_EXPORTER_OTLP_HEADERS@, @DD_API_KEY@, @DD_SITE@). The only residual channel is
whatever the SDK itself chooses to log, and the upstream exporter keeps that to
endpoint/status diagnostics.
-}
installExportErrorHandler :: ExportFailureSink -> IO ()
installExportErrorHandler sink = setGlobalErrorHandler (routeExportFailure sink . toText)

firstErrorMessage :: Text -> Text
firstErrorMessage diagnostic =
    "telemetry export error (subsequent identical errors are throttled): " <> diagnostic

heartbeatMessage :: Int -> Text -> Text
heartbeatMessage suppressed diagnostic =
    "telemetry export still failing: "
        <> show suppressed
        <> " export errors since the last report. Latest: "
        <> diagnostic

-- Log one line through the composition-root 'LogEnv', tagged with this module: the
-- plain-'IO' katip path the boot phase uses, since it holds no 'Handler' reader.
logResolve :: LogEnv -> Severity -> Text -> IO ()
logResolve logEnv severity message =
    runKatipContextT logEnv (moduleField "Ecluse.Runtime.Telemetry.Resolve") mempty $
        logFM severity (ls message)
