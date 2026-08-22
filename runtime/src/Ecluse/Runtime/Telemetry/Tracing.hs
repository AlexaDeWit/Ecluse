-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RankNTypes #-}

{- | The request-lifecycle tracing layer on top of the OpenTelemetry substrate
("Ecluse.Runtime.Telemetry"). It owns the WAI server span, the data plane's
http-client child spans, and the hand-added domain spans that carry the decisions an
operator cares about. All of it is __inert when telemetry is off__.

The substrate decides /whether/ telemetry is wired. This module decides /what/ is
traced. Every entry point takes the 'Telemetry' handle. When that handle is
'Ecluse.Runtime.Telemetry.TelemetryDisabled', the entry point adds nothing and emits
nothing. The middleware is 'id', the manager settings come back untouched, and a
domain-span bracket runs its body against no span.

When telemetry is enabled, the handle's provider __is__ the process-global provider
the substrate installed, because "Ecluse.Runtime.Telemetry.withTelemetry" calls
@initializeGlobalTracerProvider@, which also installs the global text-map propagator.
The WAI and http-client instrumentation read the process globals, and the hand-added
spans read the handle. All of them hang off one coherent tracer and join into one
trace.

== What is traced

* __Server span__: one per request, from the WAI instrumentation. It sits as the
  outermost middleware, so it spans the whole request ('telemetryWaiMiddleware').

* __Client spans__: one per upstream fetch, from instrumenting the data-plane
  'Network.HTTP.Client.Manager' settings ('instrumentDataPlaneManagerSettings'). That
  instrumentation also injects W3C trace context into the outbound request, so a
  downstream service continues the trace.

* __Domain spans__: 'withRuleEvalSpan' records the per-version verdict, so a @403@ is
  explainable from the trace alone. 'withMirrorEnqueueSpan' covers the synchronous
  serve handing off to the asynchronous mirror, and 'withMirrorJobSpan' the worker's
  fetch → verify → publish. 'withAdvisorySyncSpan' covers one advisory sync attempt and
  carries the ecosystem and the attempt's bounded result. The enqueue span captures its
  own W3C trace context onto the mirror job. The worker-job span re-establishes it as
  an OpenTelemetry __span link__ to that producer span. The asynchronous mirror
  hand-off is then navigable in a trace rather than only correlated by
  package\/version. A swallowed best-effort enqueue failure is recorded on the enqueue
  span's status, so the trace explains why the mirror did not happen.

== Secret discipline

The data-plane instrumentation uses 'dataPlaneInstrumentationConfig', which records
__no request or response headers__. A forwarded client token or an @Authorization@
header therefore never reaches a client span. The WAI instrumentation likewise never
records @Authorization@. High-cardinality identifiers (package, version, the full
denial message) belong on these spans and are recorded here. Secrets never are. The
attribute mapping and the scrub are covered by "Ecluse.Runtime.Telemetry.TracingSpec".
-}
module Ecluse.Runtime.Telemetry.Tracing (
    -- * WAI server span
    telemetryWaiMiddleware,

    -- * http-client data-plane instrumentation
    instrumentDataPlaneManagerSettings,
    dataPlaneInstrumentationConfig,

    -- * Domain spans
    withRuleEvalSpan,
    withMirrorEnqueueSpan,
    withPackumentGateSpan,
    withMetadataFetchSpan,
    withMetadataDecodeSpan,
    withMirrorJobSpan,
    withAdvisorySyncSpan,
    JobSpanOutcome (..),
    withDomainSpan,

    -- * The core tracing ports
    tracingPortOf,
    workerTracingPortOf,
    advisorySyncTracingPortOf,

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
import OpenTelemetry.Propagator.W3CTraceContext (decodeSpanContext, encodeSpanContext)
import OpenTelemetry.Trace (
    NewLink (NewLink, linkAttributes, linkContext),
    Span,
    SpanArguments (kind, links),
    SpanKind (Client, Consumer, Internal, Producer),
    SpanStatus (Error),
    addAttribute,
    defaultSpanArguments,
    inSpan',
    makeTracer,
    setStatus,
    tracerOptions,
 )
import UnliftIO (MonadUnliftIO, withRunInIO)

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Queue (RemoteSpanContext (RemoteSpanContext, rscTraceparent, rscTracestate))
import Ecluse.Core.Security.Authority (authorityLabel)
import Ecluse.Core.Server.Response (
    RejectReason (BelowIntegrityFloor, ByPolicy, MissingIntegrity, Unavailable, UpstreamInvalid),
    Rejection (rejectionMessage, rejectionReason),
    RuleName (RuleName),
    ServeDecision (Admit, Reject),
 )
import Ecluse.Core.Telemetry.Metrics (AdvisorySyncResult, advisorySyncResultName)
import Ecluse.Core.Telemetry.Span (AdvisorySyncTracingPort (..), JobSpanOutcome (..), TracingPort (..), WorkerTracingPort (..))
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Runtime.Telemetry (
    Telemetry,
    telemetryMeterProvider,
    telemetryTracerProvider,
 )

{- | The WAI server-span middleware for the request stack: one server span per
request, built over the handle's tracer and meter providers. When telemetry is
disabled it is 'id'. The stack is unchanged and no span opens, so the middleware is
additive and inert exactly as the substrate's off posture requires.

It belongs __outermost__ in the stack so the span covers the whole request,
including the other middlewares (see "Ecluse.Runtime.Server").
-}
telemetryWaiMiddleware :: Telemetry -> IO Middleware
telemetryWaiMiddleware telemetry =
    case (telemetryTracerProvider telemetry, telemetryMeterProvider telemetry) of
        (Just tracerProvider, Just meterProvider) -> do
            meter <- getMeter meterProvider ecluseScope
            newOpenTelemetryWaiMiddleware' tracerProvider meter
        _ -> pure id

{- | Instrument a data-plane 'ManagerSettings' so every upstream fetch through the
resulting manager opens a client span and carries W3C trace-context headers. Return the
settings untouched when telemetry is disabled.

The gate is the handle, not a per-request check. When telemetry is enabled, the
substrate holds the process-global providers the http-client instrumentation reads.
The spans then hang off the same tracer as everything else. When disabled the settings
come back verbatim and the data plane runs exactly as it would without this layer.

The configuration is 'dataPlaneInstrumentationConfig', which records no headers, so a
forwarded client token never reaches a span.
-}
instrumentDataPlaneManagerSettings :: Telemetry -> ManagerSettings -> IO ManagerSettings
instrumentDataPlaneManagerSettings telemetry settings =
    case telemetryTracerProvider telemetry of
        Nothing -> pure settings
        Just _ -> instrumentManagerSettings dataPlaneInstrumentationConfig settings

{- | The http-client instrumentation configuration the data plane uses: the default,
which records __no__ request or response headers. This is the secret-scrub guarantee at
the configuration boundary, since an @Authorization@ header is never lifted onto a
span. It is named rather than inlined, and the scrub test pins that same value.
-}
dataPlaneInstrumentationConfig :: HttpClientInstrumentationConfig
dataPlaneInstrumentationConfig = httpClientInstrumentationConfig

{- | Run a rule-evaluation domain span around an action that yields its result and
the verdict to record. The span carries the package and version, and from the verdict
the decision. On a denial it also carries the deciding rule, the reason class, and the
human-readable message, so a refusal is explainable from the trace alone.

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
    withDomainSpan telemetry Internal [] "ecluse.rule.eval" $ \mSpan -> do
        recordFields mSpan (coordinateFields name version)
        (result, verdict) <- action
        recordFields mSpan (ruleVerdictFields verdict)
        pure result

{- | Run a mirror-enqueue domain span around the serve-time hand-off to the
asynchronous mirror, carrying the package, version, and the authority the artifact is
fetched from. A 'Producer' span, since it produces the work the worker later consumes.

The artifact URL arrives whole. The span records @ecluse.mirror.artifact_host@, the host
and port alone ('authorityLabel'). The location comes from upstream and its userinfo or
query string can carry a credential, so the URL itself never reaches a span.

The body is handed this span's own W3C trace context ('RemoteSpanContext') to stamp
onto the mirror job, or 'Nothing' when telemetry is disabled. The worker's per-job span
can then __link__ back to this producer span across the asynchronous hop. The @project@
function maps the body's result onto an optional failure detail. A 'Just' sets the span
status to 'Error', so the trace still explains a swallowed best-effort enqueue
failure.

Inert when telemetry is disabled: the body runs against no span and is handed no trace
context.
-}
withMirrorEnqueueSpan ::
    (MonadUnliftIO m) =>
    Telemetry ->
    PackageName ->
    Version ->
    Text ->
    (a -> Maybe Text) ->
    (Maybe RemoteSpanContext -> m a) ->
    m a
withMirrorEnqueueSpan telemetry name version artifactUrl project body =
    withDomainSpan telemetry Producer [] "ecluse.mirror.enqueue" $ \mSpan -> do
        recordFields mSpan (coordinateFields name version <> [("ecluse.mirror.artifact_host", authorityLabel artifactUrl)])
        carrier <- traverse captureRemoteContext mSpan
        result <- body carrier
        whenJust mSpan $ \theSpan -> whenJust (project result) (setStatus theSpan . Error)
        pure result

{- | Run a mirror-worker-job domain span around the worker's fetch → verify →
publish, carrying the package and version and, once the job finishes, its outcome.
A 'Consumer' span, since it consumes the enqueued work. The outcome projection names
the bounded outcome label and, for a non-success, the detail that sets the span status
to 'Error'.

The carried trace context ('RemoteSpanContext') re-establishes the cross-async
relationship as a span __link__ to the enqueueing producer span.
'withMirrorEnqueueSpan' captures it and the job threads it through, so a trace
navigates from the request to the mirror it triggered and back. A 'Nothing' context, or
one that does not parse, yields no link. Inert when telemetry is disabled.
-}
withMirrorJobSpan ::
    (MonadUnliftIO m) =>
    Telemetry ->
    PackageName ->
    Version ->
    Maybe RemoteSpanContext ->
    (a -> JobSpanOutcome) ->
    m a ->
    m a
withMirrorJobSpan telemetry name version remoteContext project action =
    withDomainSpan telemetry Consumer (mirrorJobLinks remoteContext) "ecluse.mirror.job" $ \mSpan -> do
        recordFields mSpan (coordinateFields name version)
        result <- action
        let JobSpanOutcome label mDetail = project result
        recordFields mSpan [("ecluse.mirror.outcome", label)]
        whenJust mSpan $ \theSpan -> whenJust mDetail (setStatus theSpan . Error)
        pure result

{- | Run an advisory-sync domain span around one sync attempt, carrying the ecosystem it
syncs and, once the attempt finishes, the projected bounded result. An 'Internal' span:
the attempt is background work on a schedule, parented on no request.

The attributes are exactly the ecosystem and the result. That is the bounded vocabulary
the @ecluse.advisory.sync.*@ metrics carry as labels, so a trace and a series join on one
value. The artifact's bucket, key, ETag, and provenance stay off the span.

Inert when telemetry is disabled: the attempt runs against no span and returns its
result unchanged.
-}
withAdvisorySyncSpan ::
    (MonadUnliftIO m) =>
    Telemetry ->
    Ecosystem ->
    (a -> AdvisorySyncResult) ->
    m a ->
    m a
withAdvisorySyncSpan telemetry eco project action =
    withDomainSpan telemetry Internal [] "ecluse.advisory.sync.attempt" $ \mSpan -> do
        recordFields mSpan [("ecluse.ecosystem", ecosystemName eco)]
        result <- action
        recordFields mSpan [("ecluse.advisory.sync.result", advisorySyncResultName (project result))]
        pure result

-- | Run a packument-gate domain span around the rules and filter application for a public packument.
withPackumentGateSpan :: (MonadUnliftIO m) => Telemetry -> PackageName -> m a -> m a
withPackumentGateSpan telemetry name action =
    withDomainSpan telemetry Internal [] "ecluse.packument.gate" $ \mSpan -> do
        recordFields mSpan [("ecluse.package", renderPackageName name)]
        action

withMetadataFetchSpan :: (MonadUnliftIO m) => Telemetry -> PackageName -> m a -> m a
withMetadataFetchSpan telemetry name action =
    withDomainSpan telemetry Client [] "ecluse.metadata.fetch" $ \mSpan -> do
        recordFields mSpan [("ecluse.package", renderPackageName name)]
        action

withMetadataDecodeSpan :: (MonadUnliftIO m) => Telemetry -> PackageName -> m a -> m a
withMetadataDecodeSpan telemetry name action =
    withDomainSpan telemetry Internal [] "ecluse.metadata.decode" $ \mSpan -> do
        recordFields mSpan [("ecluse.package", renderPackageName name)]
        action

{- | Project the OpenTelemetry-backed domain spans onto the core 'TracingPort' the
serve path ("Ecluse.Core.Server.Pipeline") brackets through: the per-version rule
verdict and the serve-time mirror-enqueue hand-off. Each field is the matching
@with*Span@ bracket closed over the 'Telemetry' handle. The port is therefore exactly
this module's tracing behind the core interface, inert when telemetry is off. The worker's
mirror-job span is projected separately by 'workerTracingPortOf' onto a
'WorkerTracingPort', so this port carries only the two serve-path spans.
-}
tracingPortOf :: Telemetry -> TracingPort
tracingPortOf telemetry =
    TracingPort
        { spanRuleEval = withRuleEvalSpan telemetry
        , spanMirrorEnqueue = \n v url ok action -> withRunInIO $ \runInIO ->
            withMirrorEnqueueSpan telemetry n v url ok (runInIO . action)
        , spanPackumentGate = \n action -> withRunInIO $ \runInIO ->
            withPackumentGateSpan telemetry n (runInIO action)
        , spanMetadataFetch = \n action -> withRunInIO $ \runInIO ->
            withMetadataFetchSpan telemetry n (runInIO action)
        , spanMetadataDecode = \n action -> withRunInIO $ \runInIO ->
            withMetadataDecodeSpan telemetry n (runInIO action)
        }

{- | Project the OpenTelemetry-backed mirror-job span onto the core 'WorkerTracingPort'
the worker loop ("Ecluse.Core.Worker") brackets through. The single field is
'withMirrorJobSpan' closed over the 'Telemetry' handle. The port is therefore exactly
this module's tracing behind the core interface, inert when telemetry is off.
-}
workerTracingPortOf :: Telemetry -> WorkerTracingPort
workerTracingPortOf telemetry =
    WorkerTracingPort
        { wtpMirrorJobSpan = withMirrorJobSpan telemetry
        }

{- | Project the OpenTelemetry-backed advisory-sync span onto the core
'AdvisorySyncTracingPort' the sync loop ("Ecluse.Runtime.Cve.Sync") brackets through. The
single field is 'withAdvisorySyncSpan' closed over the 'Telemetry' handle. The port is
therefore exactly this module's tracing behind the core interface, inert when telemetry
is off.
-}
advisorySyncTracingPortOf :: Telemetry -> AdvisorySyncTracingPort
advisorySyncTracingPortOf telemetry =
    AdvisorySyncTracingPort
        { astpSyncAttemptSpan = withAdvisorySyncSpan telemetry
        }

{- | Map a serve verdict to the rule-evaluation span's attribute fields. Pure and
total.

An 'Admit' records only the decision. A 'Reject' records the decision, the bounded
reason class, the human-readable message, and, for a policy denial, the deciding
'RuleName'. None of these fields can carry a secret. The rule name and reason class
are a closed vocabulary, and the message is the rendered decision, never a credential.
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

{- Run an action within a domain span of the given kind and links. The action gets the
live 'Span' when telemetry is enabled, and 'Nothing' when it is disabled. The disabled
branch opens no span and creates no tracer, so the helper is genuinely inert off, not a
recording span that is later dropped. The span is parented on the ambient context (the
WAI server span on the request path), so a domain span nests under the request. The
links are independent cross-trace references (the producer→consumer mirror hop), set at
creation. -}
withDomainSpan ::
    (MonadUnliftIO m) =>
    Telemetry ->
    SpanKind ->
    [NewLink] ->
    Text ->
    (Maybe Span -> m a) ->
    m a
withDomainSpan telemetry spanKind spanLinks name body =
    case telemetryTracerProvider telemetry of
        Nothing -> body Nothing
        Just tracerProvider ->
            let tracer = makeTracer tracerProvider ecluseScope tracerOptions
             in inSpan' tracer name defaultSpanArguments{kind = spanKind, links = spanLinks} (body . Just)

-- Capture a live span's W3C trace context as the carrier stamped onto the mirror job, so
-- the worker can re-establish the link. The carrier is the standard
-- @traceparent@\/@tracestate@ wire encoding, the same one the http-client
-- instrumentation injects on outbound requests. The propagation is W3C all the way
-- through, with no Écluse-private format.
captureRemoteContext :: (MonadIO m) => Span -> m RemoteSpanContext
captureRemoteContext theSpan = do
    (traceparent, tracestate) <- liftIO (encodeSpanContext theSpan)
    pure
        RemoteSpanContext
            { rscTraceparent = decodeUtf8 traceparent
            , rscTracestate = decodeUtf8 tracestate
            }

-- The span links for a worker-job span, decoded from the carried trace context. The
-- link is the single producer (enqueue) span the job points back to. There is no link
-- when the job carried none, or when the carrier does not parse as a W3C context. An
-- untrusted carrier never fails the job. It just loses the link. The link target is a
-- remote span context, so the worker job stays the root of its own trace while still
-- referencing the originating request.
mirrorJobLinks :: Maybe RemoteSpanContext -> [NewLink]
mirrorJobLinks Nothing = []
mirrorJobLinks (Just remote) =
    case decodeSpanContext (Just (encodeUtf8 (rscTraceparent remote))) tracestateHeader of
        Nothing -> []
        Just ctx -> [NewLink{linkContext = ctx, linkAttributes = mempty}]
  where
    -- An empty tracestate is passed as absent rather than an empty header value.
    tracestateHeader :: Maybe ByteString
    tracestateHeader
        | rscTracestate remote == "" = Nothing
        | otherwise = Just (encodeUtf8 (rscTracestate remote))

-- Record a set of text attribute fields on a span when one is present. A no-op when
-- telemetry is disabled (the 'Nothing' span).
recordFields :: (MonadIO m) => Maybe Span -> [(Text, Text)] -> m ()
recordFields Nothing _ = pass
recordFields (Just theSpan) fields = traverse_ (uncurry (addAttribute theSpan)) fields

-- The package and version of the request, as the coordinate fields every domain
-- span carries. High-cardinality identifiers, which belong on spans and never on metric
-- labels. Neither rendering can contain a credential.
coordinateFields :: PackageName -> Version -> [(Text, Text)]
coordinateFields name version =
    [ ("ecluse.package", renderPackageName name)
    , ("ecluse.version", renderVersion version)
    ]

-- The instrumentation scope the hand-added spans and the WAI meter are created under:
-- this service's name. The spans are then attributed to Écluse rather than to a
-- third-party instrumentation library.
ecluseScope :: (IsString s) => s
ecluseScope = "ecluse"
