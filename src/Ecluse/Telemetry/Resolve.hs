{- | Telemetry configuration resolution, egress safety, and export-failure routing
— the boot-time substrate that sits between the operator's environment and the
OpenTelemetry SDK.

Écluse's maintainer runs Datadog, but the project is vendor-neutral, so an operator
may describe the same telemetry identity in either dialect: a Datadog shop sets the
@DD_*@ variables, a plain OpenTelemetry shop sets the @OTEL_*@ ones. This module is
the __self-aligning resolver__ that collapses both into one answer, so logs and
traces share a single identity whichever dialect was provided.

== The resolver

'resolveTelemetry' is a bounded precedence table over exactly four fields —
@service.name@, @deployment.environment@, @service.version@, and the OTLP export
endpoint — each resolved __Datadog-value-wins → vanilla OpenTelemetry → default__.
It is deliberately /not/ a general per-variable merge: only these four cross between
the dialects, and only their fixed precedence is encoded. The @DD_API_KEY@ \/
@DD_SITE@ agentless-SaaS credentials are __never read__ — Écluse exports to a
node-local collector\/Agent, never directly to a vendor's cloud, so there is no path
by which a key in the environment turns into off-cluster egress.

The resolved 'ResolvedTelemetry' is the __single source of truth__ for both halves
of the telemetry stack: 'otelEnvironmentOverrides' projects it back to the canonical
@OTEL_*@ variables the env-driven SDK reads (so a @DD_*@-only deployment still
configures the exporter), and the same record feeds the @dd@ log object that stitches
a log line to its trace.

== Egress safety

Export defaults to __Agent-only__. 'classifyResolved' classifies the resolved
endpoint against the same internal-range check the data plane's SSRF guard uses
("Ecluse.Security"): a loopback\/private target passes freely, a __public__ endpoint
is refused __fail-loud at boot__ unless the operator deliberately opts in with
@PROXY_TELEMETRY_ALLOW_PUBLIC_EGRESS=true@ (the supported route for remote\/agentless
export, authenticated out of band via @OTEL_EXPORTER_OTLP_HEADERS@). An endpoint that
cannot be resolved to a verifiably-private address at boot is __allowed with a
warning__ rather than blocked: telemetry must never make the inline proxy fail to
start, so only a /verified-public/ endpoint blocks boot.

== Export-failure routing

Telemetry failures must stay off the request path and out of raw stderr. The SDK's
batch exporter runs asynchronously, so an unreachable collector never touches a
served request; 'installThrottledErrorHandler' additionally routes the SDK's own
diagnostic stream through @katip@ under a throttle — the first failure logged
plainly, then a periodic heartbeat carrying the suppressed count — so a persistently
unreachable endpoint is one visible warning and a heartbeat, not a per-flush flood.

The configuration model is described in @docs\/architecture\/observability.md@.
-}
module Ecluse.Telemetry.Resolve (
    -- * The resolved telemetry identity
    ResolvedTelemetry (..),
    TelemetryEndpoint (..),
    EndpointSource (..),
    resolveTelemetry,

    -- * Canonical @OTEL_*@ projection
    otelEnvironmentOverrides,

    -- * Public-egress classification
    EndpointEgress (..),
    classifyResolved,
    EgressDecision (..),
    egressDecision,
    readAllowPublicEgress,

    -- * Export-failure throttle (pure core)
    ThrottleState (..),
    ThrottleEmit (..),
    initialThrottle,
    throttleInterval,
    throttleStep,

    -- * Boot wiring
    prepareTelemetry,
) where

import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import System.Environment (setEnv)

import Data.IP (IP, fromSockAddr)
import Katip (LogEnv, Severity (WarningS), logFM, ls)
import Katip.Monadic (runKatipContextT)
import Network.Socket (AddrInfo (addrAddress), defaultHints, getAddrInfo)
import OpenTelemetry.Internal.Logging (setGlobalErrorHandler)
import UnliftIO (tryAny)

import Ecluse.Log (moduleField)
import Ecluse.Security (hostAddress, isBlockedIP)

-- ── the resolved telemetry identity ──────────────────────────────────────────

{- | Where a resolved OTLP endpoint came from, so the boot path can distinguish a
deliberately-configured target from the silent default and warn on the latter.
-}
data EndpointSource
    = -- | Derived from @DD_AGENT_HOST@ (as @http:\/\/{host}:4318@).
      FromDdAgentHost
    | -- | Taken verbatim from @OTEL_EXPORTER_OTLP_ENDPOINT@.
      FromOtelEndpoint
    | -- | No endpoint was configured; the @http:\/\/localhost:4318@ default applies.
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
truth for both the SDK configuration and the @dd@ log object. 'rtEnvironment' and
'rtVersion' are 'Nothing' when the operator named neither dialect's form — they are
genuinely optional resource attributes, not defaulted to a placeholder.
-}
data ResolvedTelemetry = ResolvedTelemetry
    { rtServiceName :: Text
    -- ^ @service.name@ \/ @dd.service@ (defaults to @ecluse@).
    , rtEnvironment :: Maybe Text
    -- ^ @deployment.environment@ \/ @dd.env@, when configured.
    , rtVersion :: Maybe Text
    -- ^ @service.version@ \/ @dd.version@, when configured.
    , rtEndpoint :: TelemetryEndpoint
    -- ^ The resolved OTLP export endpoint.
    }
    deriving stock (Eq, Show)

{- | Resolve the telemetry identity from an environment list, each field
__Datadog-value-wins → vanilla OpenTelemetry → default__. @service.name@ falls
@DD_SERVICE@ → @OTEL_SERVICE_NAME@ → @service.name@ in @OTEL_RESOURCE_ATTRIBUTES@ →
@ecluse@; @deployment.environment@ and @service.version@ fall @DD_ENV@\/@DD_VERSION@
→ the matching @OTEL_RESOURCE_ATTRIBUTES@ key → unset; the endpoint is @DD_AGENT_HOST@
(as @http:\/\/{host}:4318@) → @OTEL_EXPORTER_OTLP_ENDPOINT@ → @http:\/\/localhost:4318@.

A value present but blank is treated as unset, so an empty @DD_ENV=@ does not stamp an
empty environment onto every signal. @DD_API_KEY@ and @DD_SITE@ are never consulted.

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
        , rtVersion = lk "DD_VERSION" <|> attr "service.version"
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

defaultEndpointUrl :: Text
defaultEndpointUrl = "http://localhost:4318"

-- The OTLP HTTP\/protobuf endpoint for a Datadog Agent host: the Agent's OTLP
-- receiver listens on 4318 for HTTP\/protobuf, the only transport we build.
agentHostUrl :: Text -> Text
agentHostUrl host = "http://" <> host <> ":4318"

-- ── canonical OTEL_* projection ──────────────────────────────────────────────

{- | Project the resolved identity back to the canonical @OTEL_*@ variables the
env-driven SDK reads, so a @DD_*@-only deployment still configures the exporter. The
overrides set @OTEL_SERVICE_NAME@, the OTLP endpoint, the @http\/protobuf@ protocol
(the only transport built — gRPC is behind a disabled cabal flag), and an
@OTEL_RESOURCE_ATTRIBUTES@ whose @service.name@\/@deployment.environment@\/
@service.version@ keys are overlaid by the resolution while any other operator-set
attributes are preserved.

Applied with 'System.Environment.setEnv' before the SDK initialises (see
'prepareTelemetry'); idempotent for a vanilla deployment that already set the same
@OTEL_*@ values.
-}
otelEnvironmentOverrides :: [(String, String)] -> [(String, String)]
otelEnvironmentOverrides environment =
    [ ("OTEL_SERVICE_NAME", toString (rtServiceName resolved))
    , ("OTEL_EXPORTER_OTLP_ENDPOINT", toString (teUrl (rtEndpoint resolved)))
    , ("OTEL_EXPORTER_OTLP_PROTOCOL", "http/protobuf")
    , ("OTEL_RESOURCE_ATTRIBUTES", toString (renderResourceAttributes mergedAttrs))
    ]
  where
    resolved :: ResolvedTelemetry
    resolved = resolveTelemetry environment

    existing :: Map Text Text
    existing =
        maybe
            Map.empty
            parseResourceAttributes
            (nonBlank . toText =<< lookup "OTEL_RESOURCE_ATTRIBUTES" environment)

    mergedAttrs :: Map Text Text
    mergedAttrs =
        foldl'
            (\acc (key, mValue) -> maybe acc (\value -> Map.insert key value acc) mValue)
            existing
            [ ("service.name", Just (rtServiceName resolved))
            , ("deployment.environment", rtEnvironment resolved)
            , ("service.version", rtVersion resolved)
            ]

-- Parse the @key1=value1,key2=value2@ resource-attribute string into a map,
-- trimming surrounding whitespace and dropping any entry that carries no @=@ or an
-- empty key. Lenient by design — this is operator-authored configuration, not a
-- wire format — so a stray trailing comma or spacing is tolerated.
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

-- ── public-egress classification ─────────────────────────────────────────────

{- | The egress classification of a resolved endpoint, decided from the addresses
its host resolves to (see 'classifyResolved').
-}
data EndpointEgress
    = -- | Resolves entirely to internal\/loopback addresses: safe, exports freely.
      EgressInternal
    | -- | Resolves to at least one public address: gated behind the opt-in.
      EgressPublic
    | {- | Could not be resolved to any address at boot, so it cannot be confirmed
      private. Allowed with a warning rather than blocked.
      -}
      EgressUnverified
    deriving stock (Eq, Show)

{- | Classify resolved endpoint addresses against the data plane's internal-range
block ("Ecluse.Security.isBlockedIP"). A name resolving entirely to internal
addresses (loopback, RFC1918, link-local, the cloud-metadata ranges) is
'EgressInternal'; any public address makes it 'EgressPublic'; no addresses at all
('Nothing' or an empty list — an unresolvable host) is 'EgressUnverified'.

Pure over the resolved addresses so the decision is unit-tested without DNS; the
resolution itself is performed once at boot by 'prepareTelemetry'.
-}
classifyResolved :: Maybe [IP] -> EndpointEgress
classifyResolved = \case
    Just addrs@(_ : _) -> if all isBlockedIP addrs then EgressInternal else EgressPublic
    _ -> EgressUnverified

{- | What the boot path should do about a classified endpoint, given whether public
egress was opted in. A 'EgressFailBoot' carries the operator-facing reason for the
fail-loud refusal; a 'EgressAllowWithWarning' the reason it is being allowed despite
not being a silent loopback target.
-}
data EgressDecision
    = -- | Export freely, no message (the loopback\/private common case).
      EgressAllow
    | -- | Export, but log the carried warning first.
      EgressAllowWithWarning Text
    | -- | Refuse to start; the carried message is the fail-loud boot reason.
      EgressFailBoot Text
    deriving stock (Eq, Show)

{- | Decide what to do about a classified endpoint. An internal target exports
silently; an unverifiable one exports with a warning (telemetry never blocks boot);
a public one fails boot unless @PROXY_TELEMETRY_ALLOW_PUBLIC_EGRESS@ opted in, in
which case it exports with a warning that off-cluster egress is deliberate.
-}
egressDecision :: Bool -> Text -> EndpointEgress -> EgressDecision
egressDecision allowPublic endpointUrl = \case
    EgressInternal -> EgressAllow
    EgressUnverified -> EgressAllowWithWarning (unverifiedMessage endpointUrl)
    EgressPublic
        | allowPublic -> EgressAllowWithWarning (publicAllowedMessage endpointUrl)
        | otherwise -> EgressFailBoot (publicBlockedMessage endpointUrl)

publicBlockedMessage :: Text -> Text
publicBlockedMessage url =
    "telemetry export endpoint "
        <> url
        <> " resolves to a public address; refusing to export off-cluster. Point it at a"
        <> " node-local collector or Agent, or set PROXY_TELEMETRY_ALLOW_PUBLIC_EGRESS=true to"
        <> " allow deliberate remote export (authenticate with OTEL_EXPORTER_OTLP_HEADERS)."

publicAllowedMessage :: Text -> Text
publicAllowedMessage url =
    "telemetry export endpoint "
        <> url
        <> " is a public address; exporting off-cluster because PROXY_TELEMETRY_ALLOW_PUBLIC_EGRESS=true."

unverifiedMessage :: Text -> Text
unverifiedMessage url =
    "telemetry export endpoint "
        <> url
        <> " could not be resolved to a verifiably-private address at boot; exporting anyway"
        <> " (telemetry never blocks proxy start-up). Confirm it is your node-local collector or Agent."

defaultedEndpointMessage :: Text -> Text
defaultedEndpointMessage url =
    "no telemetry export endpoint configured (DD_AGENT_HOST / OTEL_EXPORTER_OTLP_ENDPOINT unset); defaulting to "
        <> url
        <> "."

{- | Read the @PROXY_TELEMETRY_ALLOW_PUBLIC_EGRESS@ opt-in from the environment.
Absent or blank is 'False' (the secure default); a malformed value is a 'Left'
reason so the boot fails loud rather than silently coercing a typo to off.
-}
readAllowPublicEgress :: [(String, String)] -> Either Text Bool
readAllowPublicEgress environment =
    case nonBlank . toText =<< lookup "PROXY_TELEMETRY_ALLOW_PUBLIC_EGRESS" environment of
        Nothing -> Right False
        Just raw -> case T.toLower (T.strip raw) of
            "true" -> Right True
            "false" -> Right False
            "1" -> Right True
            "0" -> Right False
            "yes" -> Right True
            "no" -> Right False
            _ ->
                Left
                    ( "PROXY_TELEMETRY_ALLOW_PUBLIC_EGRESS: expected a boolean (true/false), got \""
                        <> raw
                        <> "\""
                    )

-- ── export-failure throttle ──────────────────────────────────────────────────

{- | The throttle state for SDK export-error routing: when an error was last
logged, and how many have been suppressed since. Exposed so the throttle decision
is unit-tested without wall-clock timing.
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

{- | Advance the throttle for one export error at @now@: surface the first error,
surface a heartbeat once the 'throttleInterval' has elapsed since the last surfaced
one (resetting the suppressed count), and otherwise suppress while counting. Pure,
so a sequence of @(time, decision)@ steps is asserted directly.
-}
throttleStep :: NominalDiffTime -> UTCTime -> ThrottleState -> (ThrottleState, ThrottleEmit)
throttleStep interval now st = case tsLastLogged st of
    Nothing -> (ThrottleState (Just now) 0, EmitFirst)
    Just lastLogged
        | diffUTCTime now lastLogged >= interval ->
            (ThrottleState (Just now) 0, EmitHeartbeat (tsSuppressed st + 1))
        | otherwise ->
            (st{tsSuppressed = tsSuppressed st + 1}, EmitSuppress)

-- ── boot wiring ──────────────────────────────────────────────────────────────

{- | Prepare the telemetry substrate at boot, before the SDK initialises: resolve
the identity, classify the endpoint's egress, and — on success — normalise the
@OTEL_*@ environment and install the throttled export-error handler.

Returns @'Left' reason@ when boot must fail loud: a malformed
@PROXY_TELEMETRY_ALLOW_PUBLIC_EGRESS@, or a public endpoint without the opt-in. On
@'Right' ()@ the environment has been normalised and the error handler installed, so
the caller may initialise the SDK. A defaulted endpoint and an allowed-but-warned
endpoint are surfaced through @katip@ as warnings, not failures.

Only the four mapped variables cross dialects here; @PROXY_TELEMETRY_ALLOW_PUBLIC_EGRESS@
is read directly in this boot phase (like @PROXY_CONFIG@) rather than through the
central environment parser, since it gates only this phase.
-}
prepareTelemetry :: LogEnv -> [(String, String)] -> IO (Either Text ())
prepareTelemetry logEnv environment =
    case readAllowPublicEgress environment of
        Left reason -> pure (Left reason)
        Right allowPublic -> do
            let resolved = resolveTelemetry environment
                endpointUrl = teUrl (rtEndpoint resolved)
            when (teSource (rtEndpoint resolved) == DefaultedEndpoint) $
                logResolve logEnv WarningS (defaultedEndpointMessage endpointUrl)
            egress <- classifyEndpointEgress endpointUrl
            case egressDecision allowPublic endpointUrl egress of
                EgressFailBoot reason -> pure (Left reason)
                EgressAllow -> commit
                EgressAllowWithWarning message -> logResolve logEnv WarningS message >> commit
  where
    commit :: IO (Either Text ())
    commit = do
        mapM_ (uncurry setEnv) (otelEnvironmentOverrides environment)
        installThrottledErrorHandler logEnv
        pure (Right ())

-- Resolve an endpoint URL's host and classify the addresses it resolves to. A
-- resolution failure (or a URL with no recognisable host) is 'EgressUnverified',
-- never an exception that aborts boot.
classifyEndpointEgress :: Text -> IO EndpointEgress
classifyEndpointEgress endpointUrl = do
    let host = hostAddress endpointUrl
    if T.null host
        then pure EgressUnverified
        else classifyResolved <$> resolveHostAddresses host

-- Resolve a host to its IP addresses, 'Nothing' on any resolution failure. Mirrors
-- the data plane's connection-time resolution ("Ecluse.Security.Egress"); the
-- non-IP socket addresses 'fromSockAddr' cannot decode are dropped.
resolveHostAddresses :: Text -> IO (Maybe [IP])
resolveHostAddresses host =
    tryAny (getAddrInfo (Just defaultHints) (Just (toString host)) Nothing) >>= \case
        Left _ -> pure Nothing
        Right addrs -> pure (Just (map fst (mapMaybe (fromSockAddr . addrAddress) addrs)))

{- Install a process-global handler for the SDK's diagnostic stream that routes its
export errors through @katip@ under a throttle, so a persistently unreachable
collector is one warning plus a periodic heartbeat rather than a per-flush flood.
The SDK exposes a single settable handler, so this replaces the default
stderr handler for the lifetime of the process. -}
installThrottledErrorHandler :: LogEnv -> IO ()
installThrottledErrorHandler logEnv = do
    throttle <- newIORef initialThrottle
    setGlobalErrorHandler $ \diagnostic -> do
        now <- getCurrentTime
        emit <- atomicModifyIORef' throttle (throttleStep throttleInterval now)
        case emit of
            EmitFirst ->
                logResolve logEnv WarningS (firstErrorMessage (toText diagnostic))
            EmitHeartbeat suppressed ->
                logResolve logEnv WarningS (heartbeatMessage suppressed (toText diagnostic))
            EmitSuppress -> pass

firstErrorMessage :: Text -> Text
firstErrorMessage diagnostic =
    "telemetry export error (subsequent identical errors are throttled): " <> diagnostic

heartbeatMessage :: Int -> Text -> Text
heartbeatMessage suppressed diagnostic =
    "telemetry export still failing: "
        <> show suppressed
        <> " export errors since the last report. Latest: "
        <> diagnostic

-- Log one line through the composition-root 'LogEnv', tagged with this module — the
-- plain-'IO' katip path the boot phase uses (it holds no 'Handler' reader).
logResolve :: LogEnv -> Severity -> Text -> IO ()
logResolve logEnv severity message =
    runKatipContextT logEnv (moduleField "Ecluse.Telemetry.Resolve") mempty $
        logFM severity (ls message)

-- A present-but-blank environment value is treated as unset.
nonBlank :: Text -> Maybe Text
nonBlank value = if T.null (T.strip value) then Nothing else Just value
