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

{- | Build the WAI server-span middleware for the request stack, or 'id' when telemetry is
disabled.

It belongs __outermost__ in the stack so the span covers the whole request, including the
other middlewares (see "Ecluse.Runtime.Server").
-}
telemetryWaiMiddleware :: Telemetry -> IO Middleware
telemetryWaiMiddleware telemetry =
    case (telemetryTracerProvider telemetry, telemetryMeterProvider telemetry) of
        (Just tracerProvider, Just meterProvider) -> do
            meter <- getMeter meterProvider ecluseScope
            newOpenTelemetryWaiMiddleware' tracerProvider meter
        _ -> pure id

{- | Instrument a data-plane 'ManagerSettings' so upstream fetches open a client span and
carry W3C trace-context headers. Return the settings untouched when telemetry is disabled.

'dataPlaneInstrumentationConfig' records no headers, so a forwarded client token never
reaches a span.
-}
instrumentDataPlaneManagerSettings :: Telemetry -> ManagerSettings -> IO ManagerSettings
instrumentDataPlaneManagerSettings telemetry settings =
    case telemetryTracerProvider telemetry of
        Nothing -> pure settings
        Just _ -> instrumentManagerSettings dataPlaneInstrumentationConfig settings

{- | The http-client instrumentation configuration for the data plane. It records __no__
request or response headers, so an @Authorization@ header never reaches a span.
-}
dataPlaneInstrumentationConfig :: HttpClientInstrumentationConfig
dataPlaneInstrumentationConfig = httpClientInstrumentationConfig

{- | Run a rule-evaluation domain span around an action that yields its result and the
verdict to record ('ruleVerdictFields'). Inert when telemetry is disabled.
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

{- | Run a mirror-enqueue 'Producer' span around the serve-time hand-off to the asynchronous
mirror, handing the body this span's trace context to stamp onto the job so the worker's span
links back. Inert when telemetry is disabled.

The span records the artifact authority alone ('authorityLabel'), never the URL, whose
userinfo or query string can carry a credential. A 'Just' from @project@ sets the span status
to 'Error', so the trace still explains a swallowed best-effort enqueue failure.
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

{- | Run a mirror-worker-job 'Consumer' span around the worker's fetch, verify, and publish,
linking back to the enqueueing producer span through the carried trace context
('mirrorJobLinks'). Inert when telemetry is disabled.
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

{- | Run an advisory-sync 'Internal' span around one sync attempt, carrying the ecosystem and
the projected result alone. Inert when telemetry is disabled.

Those two are the bounded vocabulary the @ecluse.advisory.sync.*@ metrics label with, so a
trace and a series join on one value.
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

{- | Project this module's serve-path spans onto the core 'TracingPort' that
"Ecluse.Core.Server.Pipeline" brackets through. Inert when telemetry is disabled.
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

{- | Project 'withMirrorJobSpan' onto the core 'WorkerTracingPort' that "Ecluse.Core.Worker"
brackets through. Inert when telemetry is disabled.
-}
workerTracingPortOf :: Telemetry -> WorkerTracingPort
workerTracingPortOf telemetry =
    WorkerTracingPort
        { wtpMirrorJobSpan = withMirrorJobSpan telemetry
        }

{- | Project 'withAdvisorySyncSpan' onto the core 'AdvisorySyncTracingPort' that
"Ecluse.Runtime.Cve.Sync" brackets through. Inert when telemetry is disabled.
-}
advisorySyncTracingPortOf :: Telemetry -> AdvisorySyncTracingPort
advisorySyncTracingPortOf telemetry =
    AdvisorySyncTracingPort
        { astpSyncAttemptSpan = withAdvisorySyncSpan telemetry
        }

{- | Map a serve verdict to the rule-evaluation span's attribute fields.

None of these fields can carry a secret. The rule name and reason class are a closed
vocabulary, and the message is the rendered decision, never a credential.
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

{- Run an action within a domain span of the given kind and links, or against 'Nothing' when
telemetry is disabled, which creates no tracer and opens no span. The span is parented on the
ambient context, so a domain span nests under the WAI server span on the request path. -}
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

-- Capture a live span's trace context as the carrier stamped onto the mirror job, so the
-- worker can link back. The encoding is the standard W3C @traceparent@\/@tracestate@ pair.
captureRemoteContext :: (MonadIO m) => Span -> m RemoteSpanContext
captureRemoteContext theSpan = do
    (traceparent, tracestate) <- liftIO (encodeSpanContext theSpan)
    pure
        RemoteSpanContext
            { rscTraceparent = decodeUtf8 traceparent
            , rscTracestate = decodeUtf8 tracestate
            }

-- The span links for a worker-job span, decoded from the carried trace context: the single
-- producer (enqueue) span the job points back to. A missing or unparsable carrier yields no
-- link and never fails the job. The link target is remote, so the job roots its own trace.
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

-- The instrumentation scope the hand-added spans and the WAI meter are created under, so
-- they are attributed to Écluse rather than to a third-party instrumentation library.
ecluseScope :: (IsString s) => s
ecluseScope = "ecluse"
