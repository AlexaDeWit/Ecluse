{- | The OpenTelemetry substrate: the tracer and meter providers the rest of the
proxy will hang spans and metrics on, behind a master switch that defaults to
__off__.

Écluse is an inline dependency in someone else's build, so observability is
__opt-in and vendor-neutral__: the substrate is OpenTelemetry, emitting OTLP that
any compatible backend can receive. The maintainer's choice of backend (Datadog)
must never become every consumer's obligation, so __with @PROXY_TELEMETRY@ unset
nothing is wired and no telemetry is emitted__ — the SDK is not even initialised.

This module is purely the __substrate__: it stands up (or, by default, declines to
stand up) the providers and brackets their lifecycle. The spans on the request
lifecycle and the metric instruments layer on top of it in later slices; nothing
here instruments the hot path.

== The switch and the handle

'TelemetrySwitch' is the @PROXY_TELEMETRY@ master switch, parsed at the
configuration boundary ("Ecluse.Config") in the same strict, fail-loud style as
the other enums. The 'Telemetry' handle it produces is one of two shapes:

* __'telemetryDisabled'__ — the off-by-default no-op. It holds no providers, the
  SDK is never initialised, and nothing is exported. This is what an unset
  @PROXY_TELEMETRY@ yields.
* an __enabled__ handle carrying the SDK's tracer and meter providers, built from
  the standard @OTEL_*@ environment variables the SDK reads directly
  (@OTEL_SERVICE_NAME@, @OTEL_RESOURCE_ATTRIBUTES@, @OTEL_EXPORTER_OTLP_ENDPOINT@,
  @OTEL_EXPORTER_OTLP_PROTOCOL@, the sampler). The OTLP exporter defaults to
  HTTP\/protobuf; gRPC stays behind the exporter's cabal flag, off.

'withTelemetry' is the lifecycle bracket the composition root ("Ecluse.Env") runs
the proxy within: when enabled it initialises the providers and tears them down —
flushing buffered spans and metrics — along every exit path; when disabled it is a
pure pass-through that opens nothing to tear down.

The configuration model and the signal catalogue are described in
@docs\/architecture\/observability.md@.
-}
module Ecluse.Telemetry (
    -- * Master switch
    TelemetrySwitch (..),
    parseTelemetrySwitch,
    renderTelemetrySwitch,

    -- * The telemetry handle
    Telemetry (..),
    TelemetryProviders (..),
    telemetryDisabled,
    telemetryEnabled,
    telemetryTracerProvider,
    telemetryMeterProvider,

    -- * Lifecycle
    withTelemetry,
) where

import OpenTelemetry.Metric (MeterProvider)
import OpenTelemetry.SDK (
    OTelSignals (otelMeterProvider, otelTracerProvider),
    withOpenTelemetry,
 )
import OpenTelemetry.Trace (TracerProvider)

-- ── master switch ────────────────────────────────────────────────────────────

{- | The @PROXY_TELEMETRY@ master switch: telemetry is opt-in, so 'TelemetryOff' is
the default and the FOSS posture. A sum type rather than a 'Bool' so each case
names its intent and a future mode (e.g. a metrics-only switch) is a new
constructor, not a second flag.
-}
data TelemetrySwitch
    = -- | Telemetry is disabled (the default): nothing is wired and nothing is emitted.
      TelemetryOff
    | {- | Telemetry is enabled: the SDK providers are built from the standard
      @OTEL_*@ environment and the OTLP exporter is active.
      -}
      TelemetryOn
    deriving stock (Eq, Show)

{- | Parse a 'TelemetrySwitch' from its wire name, naming the accepted set on
failure. The same strict, fail-loud style as the other configuration enums
("Ecluse.Config"): an unrecognised value is a loud failure, never a silent
fallback to one mode or the other.

>>> parseTelemetrySwitch "off"
Right TelemetryOff

>>> parseTelemetrySwitch "on"
Right TelemetryOn

>>> parseTelemetrySwitch "maybe"
Left "unknown telemetry switch \"maybe\" (expected one of: on, off)"
-}
parseTelemetrySwitch :: Text -> Either Text TelemetrySwitch
parseTelemetrySwitch = \case
    "off" -> Right TelemetryOff
    "on" -> Right TelemetryOn
    other ->
        Left
            ( "unknown telemetry switch \""
                <> other
                <> "\" (expected one of: on, off)"
            )

-- | The wire name of a 'TelemetrySwitch' (the inverse of 'parseTelemetrySwitch').
renderTelemetrySwitch :: TelemetrySwitch -> Text
renderTelemetrySwitch = \case
    TelemetryOff -> "off"
    TelemetryOn -> "on"

-- ── the telemetry handle ─────────────────────────────────────────────────────

{- | The telemetry handle held in the composition root: either the off-by-default
no-op or the enabled providers. Spans and metric instruments are derived from the
providers it carries; the disabled case carries none, so a layer that reaches for a
provider finds nothing to emit through — telemetry is genuinely inert, not merely
unsampled.
-}
data Telemetry
    = -- | The off-by-default no-op: no providers, nothing emitted.
      TelemetryDisabled
    | {- | The enabled handle carrying the SDK's providers, built from the standard
      @OTEL_*@ environment. The providers live in a 'TelemetryProviders' product so
      neither field is a partial record selector on this sum.
      -}
      TelemetryEnabled TelemetryProviders

{- | The SDK providers an enabled 'Telemetry' handle carries — a total product, so
its fields are not partial selectors over the 'Telemetry' sum.
-}
data TelemetryProviders = TelemetryProviders
    { tpTracerProvider :: TracerProvider
    -- ^ The SDK tracer provider spans are created through.
    , tpMeterProvider :: MeterProvider
    -- ^ The SDK meter provider metric instruments are created through.
    }

{- | The disabled telemetry handle: the off-by-default no-op that holds no
providers and emits nothing. This is what an unset @PROXY_TELEMETRY@ resolves to.
-}
telemetryDisabled :: Telemetry
telemetryDisabled = TelemetryDisabled

{- | Build an enabled telemetry handle from the SDK signals — the tracer and meter
providers. The disabled case has no constructor argument, so this is the only way
to obtain an enabled handle, keeping its providers' origin (the bracketed SDK
lifecycle) explicit.
-}
telemetryEnabled :: OTelSignals -> Telemetry
telemetryEnabled signals =
    TelemetryEnabled
        TelemetryProviders
            { tpTracerProvider = otelTracerProvider signals
            , tpMeterProvider = otelMeterProvider signals
            }

{- | The tracer provider a 'Telemetry' handle exposes, 'Nothing' when telemetry is
disabled. A caller that wants to create a span resolves this first; 'Nothing' is
the signal to emit nothing rather than to fabricate a no-op provider at the edge.
-}
telemetryTracerProvider :: Telemetry -> Maybe TracerProvider
telemetryTracerProvider = \case
    TelemetryDisabled -> Nothing
    TelemetryEnabled providers -> Just (tpTracerProvider providers)

{- | The meter provider a 'Telemetry' handle exposes, 'Nothing' when telemetry is
disabled (the dual of 'telemetryTracerProvider' for metric instruments).
-}
telemetryMeterProvider :: Telemetry -> Maybe MeterProvider
telemetryMeterProvider = \case
    TelemetryDisabled -> Nothing
    TelemetryEnabled providers -> Just (tpMeterProvider providers)

-- ── lifecycle ────────────────────────────────────────────────────────────────

{- | Run an action with a 'Telemetry' handle whose lifecycle is bracketed by the
'TelemetrySwitch', tearing the providers down — flushing buffered spans and
metrics — along every exit path.

* __'TelemetryOff'__ (the default) is a pure pass-through: the SDK is __never
  initialised__, the body runs against 'telemetryDisabled', and there is nothing
  to tear down. An unset @PROXY_TELEMETRY@ therefore opens no exporter and emits
  nothing.
* __'TelemetryOn'__ initialises the SDK from the standard @OTEL_*@ environment
  (via @hs-opentelemetry-sdk@'s @withOpenTelemetry@), runs the body against the
  enabled handle, and shuts the providers down on exit.

This is the scope the composition root ("Ecluse.Env") runs the server and worker
within, so telemetry is established once and flushed on shutdown.
-}
withTelemetry :: TelemetrySwitch -> (Telemetry -> IO a) -> IO a
withTelemetry = \case
    TelemetryOff -> \use -> use telemetryDisabled
    TelemetryOn -> \use -> withOpenTelemetry (use . telemetryEnabled)
