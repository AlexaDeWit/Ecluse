{- | The request-lifecycle tracing layer on top of the OpenTelemetry substrate
("Ecluse.Telemetry"): the WAI server span, the http-client child spans on the data
plane, and the hand-added domain spans that carry the decisions an operator cares
about — all __inert when telemetry is off__.

The substrate decides /whether/ telemetry is wired; this module decides /what/ is
traced. Every entry point takes the 'Telemetry' handle and, when it is
'Ecluse.Telemetry.TelemetryDisabled', adds nothing and emits nothing: the
middleware is 'id', the manager settings are returned untouched, and a domain-span
bracket runs its body against no span. When telemetry is enabled, the handle's
provider __is__ the process-global provider the substrate installed (when enabled,
"Ecluse.Telemetry.withTelemetry" calls @initializeGlobalTracerProvider@, which also
installs the global text-map propagator), so the WAI and http-client instrumentation — which read
the process globals — and the hand-added spans, which read the handle, all hang off
one coherent tracer and join into one trace.

== What is traced

* __Server span__ — one per request, from the WAI instrumentation, as the outermost
  middleware so it spans the whole request ('telemetryWaiMiddleware').
* __Client spans__ — one per upstream fetch, from instrumenting the data-plane
  'Network.HTTP.Client.Manager' settings ('instrumentDataPlaneManagerSettings'), which
  also injects W3C trace context into the outbound request so a downstream service
  continues the trace.
* __Domain spans__ — 'withRuleEvalSpan' (the per-version verdict, so a @403@ is
  explainable from the trace alone), 'withMirrorEnqueueSpan' (the synchronous serve
  handing off to the asynchronous mirror), and 'withMirrorJobSpan' (the worker's
  fetch → verify → publish).

== Secret discipline

The data-plane instrumentation uses 'dataPlaneInstrumentationConfig', which records
__no request or response headers__, so a forwarded client token or an @Authorization@
header is never captured on a client span; the WAI instrumentation likewise never
records @Authorization@. High-cardinality identifiers (package, version, the full
denial message) belong on these spans and are recorded here; secrets never are. The
attribute mapping and the scrub are covered by "Ecluse.Telemetry.TracingSpec".
-}
module Ecluse.Telemetry.Tracing (
    -- * WAI server span
    telemetryWaiMiddleware,

    -- * http-client data-plane instrumentation
    instrumentDataPlaneManagerSettings,
    dataPlaneInstrumentationConfig,

    -- * Domain spans
    withRuleEvalSpan,
    withMirrorEnqueueSpan,
    withMirrorJobSpan,
    JobSpanOutcome (..),

    -- * Verdict attribute mapping
    ruleVerdictFields,
) where

import Network.HTTP.Client (ManagerSettings)
import Network.Wai (Middleware)
import OpenTelemetry.Instrumentation.HttpClient (
    HttpClientInstrumentationConfig,
    httpClientInstrumentationConfig,
    instrumentManagerSettings,
 )
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware')
import OpenTelemetry.Metric.Core (getMeter)
import OpenTelemetry.Trace (
    Span,
    SpanArguments (kind),
    SpanKind (Consumer, Internal, Producer),
    SpanStatus (Error),
    addAttribute,
    defaultSpanArguments,
    inSpan',
    makeTracer,
    setStatus,
    tracerOptions,
 )
import UnliftIO (MonadUnliftIO)

import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Server.Response (
    RejectReason (BelowIntegrityFloor, ByPolicy, MissingIntegrity, Unavailable, UpstreamInvalid),
    Rejection (rejectionMessage, rejectionReason),
    RuleName (RuleName),
    ServeDecision (Admit, Reject),
 )
import Ecluse.Telemetry (
    Telemetry,
    telemetryMeterProvider,
    telemetryTracerProvider,
 )

-- ── WAI server span ───────────────────────────────────────────────────────────

{- | The WAI server-span middleware for the request stack: one server span per
request, built over the handle's tracer and meter providers. When telemetry is
disabled it is 'id' — the stack is unchanged and no span is opened — so it is
additive and inert exactly as the substrate's off posture requires.

It belongs __outermost__ in the stack so the span covers the whole request,
including the other middlewares (see "Ecluse.Server").
-}
telemetryWaiMiddleware :: Telemetry -> IO Middleware
telemetryWaiMiddleware telemetry =
    case (telemetryTracerProvider telemetry, telemetryMeterProvider telemetry) of
        (Just tracerProvider, Just meterProvider) -> do
            meter <- getMeter meterProvider ecluseScope
            newOpenTelemetryWaiMiddleware' tracerProvider meter
        _ -> pure id

-- ── http-client data-plane instrumentation ────────────────────────────────────

{- | Instrument a data-plane 'ManagerSettings' so every upstream fetch through the
resulting manager opens a client span and carries W3C trace-context headers, or
return the settings untouched when telemetry is disabled.

The gate is the handle, not a per-request check: when telemetry is enabled the
substrate has installed the process-global providers the http-client instrumentation
reads, so the spans hang off the same tracer as everything else; when disabled the
settings are returned verbatim and the data plane runs exactly as it would without
this layer.

The configuration is 'dataPlaneInstrumentationConfig', which records no headers, so a
forwarded client token never reaches a span.
-}
instrumentDataPlaneManagerSettings :: Telemetry -> ManagerSettings -> IO ManagerSettings
instrumentDataPlaneManagerSettings telemetry settings =
    case telemetryTracerProvider telemetry of
        Nothing -> pure settings
        Just _ -> instrumentManagerSettings dataPlaneInstrumentationConfig settings

{- | The http-client instrumentation configuration the data plane uses: the default,
which records __no__ request or response headers. This is the secret-scrub guarantee
at the configuration boundary — an @Authorization@ header is never lifted onto a span
— so it is named rather than inlined, and the scrub test pins the very same value.
-}
dataPlaneInstrumentationConfig :: HttpClientInstrumentationConfig
dataPlaneInstrumentationConfig = httpClientInstrumentationConfig

-- ── domain spans ──────────────────────────────────────────────────────────────

{- | Run a rule-evaluation domain span around an action that yields its result and
the verdict to record. The span carries the package and version and, from the
verdict, the decision and — on a denial — the deciding rule, the reason class, and
the human-readable message, so a refusal is explainable from the trace alone.

Inert when telemetry is disabled: the action runs against no span and its result is
returned unchanged.
-}
withRuleEvalSpan ::
    (MonadUnliftIO m) =>
    Telemetry ->
    PackageName ->
    Version ->
    m (a, ServeDecision) ->
    m a
withRuleEvalSpan telemetry name version action =
    withDomainSpan telemetry Internal "ecluse.rule.eval" $ \mSpan -> do
        recordFields mSpan (coordinateFields name version)
        (result, verdict) <- action
        recordFields mSpan (ruleVerdictFields verdict)
        pure result

{- | Run a mirror-enqueue domain span around the serve-time hand-off to the
asynchronous mirror, carrying the package, version, and the artifact's authoritative
URL. A 'Producer' span, since it produces the work the worker later consumes. Inert
when telemetry is disabled.
-}
withMirrorEnqueueSpan ::
    (MonadUnliftIO m) =>
    Telemetry ->
    PackageName ->
    Version ->
    Text ->
    m a ->
    m a
withMirrorEnqueueSpan telemetry name version artifactUrl action =
    withDomainSpan telemetry Producer "ecluse.mirror.enqueue" $ \mSpan -> do
        recordFields mSpan (coordinateFields name version <> [("ecluse.mirror.artifact_url", artifactUrl)])
        action

{- | Run a mirror-worker-job domain span around the worker's fetch → verify →
publish, carrying the package and version and, once the job finishes, its outcome.
A 'Consumer' span (it consumes the enqueued work); the outcome projection names the
bounded outcome label and, for a non-success, the detail that sets the span status to
'Error'. Inert when telemetry is disabled.
-}
withMirrorJobSpan ::
    (MonadUnliftIO m) =>
    Telemetry ->
    PackageName ->
    Version ->
    (a -> JobSpanOutcome) ->
    m a ->
    m a
withMirrorJobSpan telemetry name version project action =
    withDomainSpan telemetry Consumer "ecluse.mirror.job" $ \mSpan -> do
        recordFields mSpan (coordinateFields name version)
        result <- action
        let JobSpanOutcome label mDetail = project result
        recordFields mSpan [("ecluse.mirror.outcome", label)]
        whenJust mSpan $ \theSpan -> whenJust mDetail (setStatus theSpan . Error)
        pure result

{- | The projection a caller supplies for the mirror-job span: the bounded outcome
label always, and, for a job that did not publish, the detail that marks the span
'Error'. Kept a small record here — rather than the worker's own outcome type — so
the tracing layer does not depend on "Ecluse.Worker".
-}
data JobSpanOutcome = JobSpanOutcome
    { jobSpanLabel :: Text
    -- ^ The bounded outcome label (e.g. @succeeded@ \/ @dropped@ \/ @retried@).
    , jobSpanError :: Maybe Text
    -- ^ The failure detail when the job did not publish; 'Nothing' on success.
    }
    deriving stock (Eq, Show)

-- ── verdict attribute mapping ──────────────────────────────────────────────────

{- | Map a serve verdict to the rule-evaluation span's attribute fields. Pure and
total.

An 'Admit' records only the decision; a 'Reject' records the decision, the bounded
reason class, the human-readable message, and — for a policy denial — the deciding
'RuleName'. None of these fields can carry a secret: the rule name and reason class
are a closed vocabulary and the message is the rendered decision, never a credential.
-}
ruleVerdictFields :: ServeDecision -> [(Text, Text)]
ruleVerdictFields = \case
    Admit -> [("ecluse.rule.decision", "admit")]
    Reject rejection ->
        [ ("ecluse.rule.decision", "deny")
        , ("ecluse.rule.reason_class", reasonClass (rejectionReason rejection))
        , ("ecluse.rule.message", rejectionMessage rejection)
        ]
            <> ruleNameField (rejectionReason rejection)
  where
    reasonClass :: RejectReason -> Text
    reasonClass = \case
        ByPolicy _ -> "by_policy"
        Unavailable _ -> "unavailable"
        MissingIntegrity -> "missing_integrity"
        BelowIntegrityFloor -> "below_integrity_floor"
        UpstreamInvalid -> "upstream_invalid"

    ruleNameField :: RejectReason -> [(Text, Text)]
    ruleNameField = \case
        ByPolicy (RuleName ruleName) -> [("ecluse.rule.name", ruleName)]
        _ -> []

-- ── internals ──────────────────────────────────────────────────────────────────

{- Run an action within a domain span of the given kind, handing it the live 'Span'
when telemetry is enabled and 'Nothing' when it is disabled. The disabled branch
opens no span and creates no tracer, so the helper is genuinely inert off — not a
recording span that is later dropped. The span is parented on the ambient context
(the WAI server span on the request path), so a domain span nests under the request. -}
withDomainSpan ::
    (MonadUnliftIO m) =>
    Telemetry ->
    SpanKind ->
    Text ->
    (Maybe Span -> m a) ->
    m a
withDomainSpan telemetry spanKind name body =
    case telemetryTracerProvider telemetry of
        Nothing -> body Nothing
        Just tracerProvider ->
            let tracer = makeTracer tracerProvider ecluseScope tracerOptions
             in inSpan' tracer name defaultSpanArguments{kind = spanKind} (body . Just)

-- Record a set of text attribute fields on a span when one is present; a no-op when
-- telemetry is disabled (the 'Nothing' span).
recordFields :: (MonadIO m) => Maybe Span -> [(Text, Text)] -> m ()
recordFields Nothing _ = pass
recordFields (Just theSpan) fields = traverse_ (uncurry (addAttribute theSpan)) fields

-- The package and version of the request, as the coordinate fields every domain
-- span carries. High-cardinality identifiers, which belong on spans (never on metric
-- labels); neither rendering can contain a credential.
coordinateFields :: PackageName -> Version -> [(Text, Text)]
coordinateFields name version =
    [ ("ecluse.package", renderPackageName name)
    , ("ecluse.version", renderVersion version)
    ]

-- The instrumentation scope the hand-added spans and the WAI meter are created
-- under: this service's name, so the spans are attributed to Écluse rather than to a
-- third-party instrumentation library.
ecluseScope :: (IsString s) => s
ecluseScope = "ecluse"
