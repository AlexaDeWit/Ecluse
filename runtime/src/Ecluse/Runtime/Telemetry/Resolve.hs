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
so no key in the environment can turn into off-cluster egress. This module normalises
the endpoint and uses it as given, never classified or gated.

@OTEL_RESOURCE_ATTRIBUTES@ is read with the __W3C baggage grammar the SDK itself
uses__. One grammar reads the variable, so a percent-encoded value decodes the same
way for the @dd@ log object and for the span resource. Blank members are dropped
first, because operator-authored configuration carries a stray comma often enough.
A value the grammar still rejects warns at boot and contributes nothing. The
projection then overwrites the variable with the resolved identity alone. Both the
@dd@ log object and the span resource carry that identity, and neither carries the
operator's attributes.

The exported header carries the operator's own attributes plus @deployment.environment@
and @service.version@. It never carries @service.name@, because @OTEL_SERVICE_NAME@ in
the same projection already does and every SDK signal path prefers that variable. The
W3C baggage limits cap the header, and the SDK's encoder sheds whatever overflows them in
hash order. 'resourceAttributes' therefore makes the choice first: the resolved identity
is admitted ahead of the operator's own keys, and a key the limits exclude warns once at
boot, by name.

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
    ResourceAttributes (..),
    resourceAttributes,

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
    telemetryWarnings,
    prepareTelemetry,
) where

import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Version (showVersion)
import GHC.Exts qualified as Exts
import Paths_ecluse (version)
import System.Environment (setEnv)

import Katip (LogEnv, Severity (WarningS))
import OpenTelemetry.Baggage (Baggage, Element, Token)
import OpenTelemetry.Baggage qualified as Baggage
import OpenTelemetry.Exporter.Span (ExportResult (..))
import OpenTelemetry.Internal.Logging (setGlobalErrorHandler)

import Ecluse.Core.Text (nonBlank)
import Ecluse.Runtime.Log (moduleLog)

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

{- | The telemetry identity resolved from the environment: the single source of truth for the
SDK configuration and the @dd@ log object. The process cannot know its own deployment
environment, so 'rtEnvironment' stays optional and never carries a placeholder.
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
__Datadog value wins → vanilla OpenTelemetry → default__.

@service.name@ falls @DD_SERVICE@ → @OTEL_SERVICE_NAME@ → the @OTEL_RESOURCE_ATTRIBUTES@ key →
@ecluse@. @deployment.environment@ and @service.version@ fall @DD_ENV@ \/ @DD_VERSION@ → the
matching attribute key → unset and the build version. The endpoint falls @DD_AGENT_HOST@ (as
@http:\/\/{host}:4318@) → @OTEL_EXPORTER_OTLP_ENDPOINT@ → @http:\/\/localhost:4318@.

A present but blank value counts as unset, so an empty @DD_ENV=@ stamps no environment onto a
signal. The resolver never reads @DD_API_KEY@ or @DD_SITE@.

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

    attributes :: Baggage
    attributes = fromRight Baggage.empty (decodeResourceAttributes environment)

    attr :: Text -> Maybe Text
    attr key = do
        name <- Baggage.mkToken key
        nonBlank =<< Baggage.getValue name attributes

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

buildVersion :: Text
buildVersion = toText (showVersion version)

defaultEndpointUrl :: Text
defaultEndpointUrl = "http://localhost:4318"

{- The Datadog Agent's OTLP receiver listens on 4318 for HTTP\/protobuf, the only transport we
build. A literal IPv6 host is bracketed so the authority is well-formed: @http:\/\/[fd00::1]:4318@,
not the invalid @http:\/\/fd00::1:4318@. A host that already carries a scheme or a port goes
through unchanged, so a deliberately-qualified @DD_AGENT_HOST@ is never mangled. -}
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

{- | Project the resolved identity back to the canonical @OTEL_*@ variables the env-driven SDK
reads, so a @DD_*@-only deployment still configures the exporter. The protocol is pinned to
@http\/protobuf@ because gRPC sits behind a disabled cabal flag. 'prepareTelemetry' sets these
before the SDK initialises.
-}
otelEnvironmentOverrides :: [(String, String)] -> [(String, String)]
otelEnvironmentOverrides environment =
    [ ("OTEL_SERVICE_NAME", toString (rtServiceName resolved))
    , ("OTEL_EXPORTER_OTLP_ENDPOINT", toString (teUrl (rtEndpoint resolved)))
    , ("OTEL_EXPORTER_OTLP_PROTOCOL", "http/protobuf")
    , ("OTEL_RESOURCE_ATTRIBUTES", renderResourceAttributes (raCarried (resourceAttributes environment)))
    ]
  where
    resolved :: ResolvedTelemetry
    resolved = resolveTelemetry environment

-- Overlay the resolved identity onto the operator's own attributes. An inserted member replaces
-- an inherited one of the same name, so a stale operator value never overrides the resolution.
mergedResourceAttributes :: ResolvedTelemetry -> [(String, String)] -> Baggage
mergedResourceAttributes resolved environment =
    foldr insertAttribute withoutServiceName (resolvedAttributes resolved)
  where
    inherited :: Baggage
    inherited = fromRight Baggage.empty (decodeResourceAttributes environment)

    -- OTEL_SERVICE_NAME carries the service name, and every SDK signal path prefers that
    -- variable, so an inherited copy here spends header budget to fight it and lose.
    withoutServiceName :: Baggage
    withoutServiceName = maybe inherited (`Baggage.delete` inherited) (Baggage.mkToken "service.name")

resolvedAttributes :: ResolvedTelemetry -> [(Text, Text)]
resolvedAttributes resolved =
    [ (key, value)
    | (key, Just value) <-
        [ ("deployment.environment", rtEnvironment resolved)
        , ("service.version", rtVersion resolved)
        ]
    ]

-- A key the W3C token grammar cannot express is dropped, because the SDK's decoder rejects a
-- whole header over one such member.
insertAttribute :: (Text, Text) -> Baggage -> Baggage
insertAttribute (key, value) bag =
    maybe bag (\name -> Baggage.insert name (Baggage.element value) bag) (Baggage.mkToken key)

-- | The members the exported header carries, and the keys the W3C baggage limits left out.
data ResourceAttributes = ResourceAttributes
    { raCarried :: Baggage
    -- ^ What @OTEL_RESOURCE_ATTRIBUTES@ exports.
    , raDropped :: [Text]
    -- ^ The keys the limits excluded, in admission order.
    }
    deriving stock (Eq, Show)

{- | Decide what the exported header carries. The SDK's encoder sheds whatever overflows the W3C
limits in hash order, so the choice is made here: the carried set is the same on every restart, and
'telemetryWarnings' names what did not fit.
-}
resourceAttributes :: [(String, String)] -> ResourceAttributes
resourceAttributes environment = carry (admitMembers 0 0 (admissionOrder resolved merged))
  where
    resolved :: ResolvedTelemetry
    resolved = resolveTelemetry environment

    merged :: Baggage
    merged = mergedResourceAttributes resolved environment

    carry :: ([(Token, Element)], [Text]) -> ResourceAttributes
    carry (kept, dropped) = ResourceAttributes (foldr (uncurry Baggage.insert) Baggage.empty kept) dropped

-- The resolved identity is offered first, so the limits shed the operator's extras rather than
-- the keys a dashboard joins on. Everything else follows in key order.
admissionOrder :: ResolvedTelemetry -> Baggage -> [(Token, Element)]
admissionOrder resolved bag = sortOn (rank . memberKey . fst) (Exts.toList (Baggage.values bag))
  where
    identityKeys :: [Text]
    identityKeys = map fst (resolvedAttributes resolved)

    rank :: Text -> (Int, Text)
    rank key = (if key `elem` identityKeys then 0 else 1, key)

{- Take members while the W3C limits allow and name the rest. An excluded member is skipped rather
than ending the scan, so a small attribute still lands after a large one is left out. -}
admitMembers :: Int -> Int -> [(Token, Element)] -> ([(Token, Element)], [Text])
admitMembers _ _ [] = ([], [])
admitMembers usedBytes usedMembers ((tok, el) : rest)
    | admissible = first ((tok, el) :) (admitMembers (usedBytes + separator + size) (usedMembers + 1) rest)
    | otherwise = second (memberKey tok :) (admitMembers usedBytes usedMembers rest)
  where
    size :: Int
    size = encodedMemberBytes tok el

    separator :: Int
    separator = if usedMembers == 0 then 0 else 1

    admissible :: Bool
    admissible =
        size <= Baggage.maxMemberBytes
            && usedMembers < Baggage.maxMembers
            && usedBytes + separator + size <= Baggage.maxBaggageBytes

-- One member's encoded size, measured with the SDK's own encoder. That encoder emits nothing for a
-- member over its per-member limit, so an empty encoding reports as one byte past the limit.
encodedMemberBytes :: Token -> Element -> Int
encodedMemberBytes tok el =
    case BS.length (Baggage.encodeBaggageHeader (Baggage.insert tok el Baggage.empty)) of
        0 -> Baggage.maxMemberBytes + 1
        n -> n

memberKey :: Token -> Text
memberKey = decodeUtf8 . Baggage.tokenValue

{- Decode @OTEL_RESOURCE_ATTRIBUTES@ with the SDK's own W3C baggage parser, which percent-decodes
every value. Blank members are dropped first, so a trailing comma or stray spacing still parses
where the grammar alone would reject the whole value. -}
decodeResourceAttributes :: [(String, String)] -> Either Text Baggage
decodeResourceAttributes environment = case members of
    [] -> Right Baggage.empty
    _ -> first toText (Baggage.decodeBaggageHeader (encodeUtf8 (T.intercalate "," members)))
  where
    members :: [Text]
    members = filter (not . T.null) (map T.strip (T.splitOn "," raw))

    raw :: Text
    raw = maybe "" toText (lookup "OTEL_RESOURCE_ATTRIBUTES" environment)

-- Render with the SDK's own encoder, so the value the SDK decodes is the one this module resolved.
-- 'resourceAttributes' has already brought the bag within the limits, so nothing is shed here.
renderResourceAttributes :: Baggage -> String
renderResourceAttributes = decodeUtf8 . Baggage.encodeBaggageHeader

{- | The boot warnings the environment raises, in the order 'prepareTelemetry' surfaces them.
Exposed as values so a test pins each message without a @katip@ scribe.
-}
telemetryWarnings :: [(String, String)] -> [Text]
telemetryWarnings environment = endpointWarning <> attributeWarning <> droppedWarning
  where
    endpoint :: TelemetryEndpoint
    endpoint = rtEndpoint (resolveTelemetry environment)

    endpointWarning :: [Text]
    endpointWarning =
        [defaultedEndpointMessage (teUrl endpoint) | teSource endpoint == DefaultedEndpoint]

    attributeWarning :: [Text]
    attributeWarning =
        either
            (\reason -> [malformedAttributesMessage reason])
            (const [])
            (decodeResourceAttributes environment)

    droppedWarning :: [Text]
    droppedWarning = case raDropped (resourceAttributes environment) of
        [] -> []
        dropped -> [droppedAttributesMessage dropped]

defaultedEndpointMessage :: Text -> Text
defaultedEndpointMessage url =
    "no telemetry export endpoint configured (DD_AGENT_HOST / OTEL_EXPORTER_OTLP_ENDPOINT unset); defaulting to "
        <> url
        <> "."

malformedAttributesMessage :: Text -> Text
malformedAttributesMessage reason =
    "OTEL_RESOURCE_ATTRIBUTES is not valid W3C baggage ("
        <> reason
        <> "). Dropping its attributes and exporting the resolved service identity alone."

droppedAttributesMessage :: [Text] -> Text
droppedAttributesMessage dropped =
    "OTEL_RESOURCE_ATTRIBUTES is over the W3C baggage limits ("
        <> show Baggage.maxBaggageBytes
        <> " bytes total, "
        <> show Baggage.maxMemberBytes
        <> " bytes per member, "
        <> show Baggage.maxMembers
        <> " members). Dropping "
        <> T.intercalate ", " dropped
        <> " from the exported resource attributes."

{- | The throttle state for SDK export-error routing. Exposed so a test asserts the throttle
decision without wall-clock timing.
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

{- | Advance the throttle for one export error at @now@: the first error surfaces, then a
heartbeat once @interval@ has elapsed since the last surfaced error, and anything between is
suppressed and counted.
-}
throttleStep :: NominalDiffTime -> UTCTime -> ThrottleState -> (ThrottleState, ThrottleEmit)
throttleStep interval now st = case tsLastLogged st of
    Nothing -> (ThrottleState (Just now) 0, EmitFirst)
    Just lastLogged
        | diffUTCTime now lastLogged >= interval ->
            (ThrottleState (Just now) 0, EmitHeartbeat (tsSuppressed st + 1))
        | otherwise ->
            (st{tsSuppressed = tsSuppressed st + 1}, EmitSuppress)

{- | Prepare the telemetry substrate at boot, before the SDK initialises: resolve the identity
and normalise the canonical @OTEL_*@ environment the SDK reads. "Ecluse.Runtime.Telemetry" wires
the export-failure observation later, when the substrate stands up.

Every 'telemetryWarnings' message goes through @katip@ first. A defaulted endpoint, with neither
@DD_AGENT_HOST@ nor @OTEL_EXPORTER_OTLP_ENDPOINT@ set, falls back to @http:\/\/localhost:4318@.
That is never a failure, since the OTLP endpoint is an operator-declared destination this module
never classifies or gates.
-}
prepareTelemetry :: LogEnv -> [(String, String)] -> IO ()
prepareTelemetry logEnv environment = do
    mapM_ (moduleLog logEnv resolveModule WarningS) (telemetryWarnings environment)
    mapM_ (uncurry setEnv) (otelEnvironmentOverrides environment)

{- | The shared export-failure sink: one throttle and one @katip@ target for the span exporter,
the metric exporter, and the SDK's own diagnostics, so an unreachable collector produces one
coalesced stream instead of several independent floods. The clock and the surfacing action are
injected, so a test asserts the throttle decision without wall-clock timing.
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

-- | The production sink: the wall clock and the composition-root 'LogEnv' as the @katip@ target.
exportFailureSink :: LogEnv -> IO ExportFailureSink
exportFailureSink logEnv = newExportFailureSink getCurrentTime (moduleLog logEnv resolveModule)

-- The module name every line this module raises is tagged with.
resolveModule :: Text
resolveModule = "Ecluse.Runtime.Telemetry.Resolve"

{- | Route one export-failure diagnostic through the shared throttle into @katip@. The first
error surfaces plainly and later ones fold into a heartbeat carrying the suppressed count.
-}
routeExportFailure :: ExportFailureSink -> Text -> IO ()
routeExportFailure sink diagnostic = do
    now <- sinkNow sink
    emit <- atomicModifyIORef' (sinkState sink) (throttleStep throttleInterval now)
    case emit of
        EmitFirst -> sinkSurface sink WarningS (firstErrorMessage diagnostic)
        EmitHeartbeat suppressed -> sinkSurface sink WarningS (heartbeatMessage suppressed diagnostic)
        EmitSuppress -> pass

{- | Observe one exporter's 'ExportResult', routing a 'Failure' through the sink. It only
observes, so export semantics stay untouched and a failed export never reaches the request
path. @signal@ names the failing exporter (@span@ \/ @metric@).
-}
observeExportResult :: ExportFailureSink -> Text -> ExportResult -> IO ()
observeExportResult sink signal = \case
    Success -> pass
    Failure mErr -> routeExportFailure sink (signal <> " export failed" <> maybe "" ((": " <>) . show) mErr)

{- | Install a process-global handler for the SDK's own diagnostic stream, routed through the
shared sink. In @hs-opentelemetry 1.0.0.0@ the SDK drops a failed OTLP export instead of routing
it here, so 'observeExportResult' carries the export-failure feed and this handler serves only
SDK-internal diagnostics.

The forwarded diagnostic is the SDK's own text. This module never reads the credential-bearing
inputs (@OTEL_EXPORTER_OTLP_HEADERS@, @DD_API_KEY@, @DD_SITE@), so the only residual leak channel
is whatever the SDK itself logs.
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
