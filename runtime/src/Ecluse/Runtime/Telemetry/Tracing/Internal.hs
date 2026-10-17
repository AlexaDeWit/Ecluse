-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RankNTypes #-}

{- | The middleware, the manager settings, the domain-span brackets and the attribute mappings
behind "Ecluse.Runtime.Telemetry.Tracing", which documents the tracing layer and re-exports the
curated surface. Importing this module opts out of that stability promise, the convention @text@
and @bytestring@ use, so production code imports the public one.
-}
module Ecluse.Runtime.Telemetry.Tracing.Internal (
    -- * WAI server span
    telemetryWaiMiddleware,

    -- * http-client data-plane instrumentation
    instrumentDataPlaneManagerSettings,
    dataPlaneInstrumentationConfig,

    -- * Domain spans
    withRuleEvalSpan,
    withMirrorEnqueueSpan,
    withMirrorJobSpan,
    withAdvisorySyncSpan,
    JobSpanOutcome (..),

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
import Ecluse.Core.Telemetry.Span (AdvisorySyncTracingPort (..), JobSpanOutcome (..), TracingPort (..), WorkerTracingPort (..), ecluseScope)
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Runtime.Telemetry (
    Telemetry,
    telemetryMeterProvider,
    telemetryTracerProvider,
 )

-- | The WAI server-span middleware, or 'id' without telemetry. It belongs __outermost__, so the span covers the request.
telemetryWaiMiddleware :: Telemetry -> IO Middleware
telemetryWaiMiddleware telemetry =
    case (telemetryTracerProvider telemetry, telemetryMeterProvider telemetry) of
        (Just tracerProvider, Just meterProvider) -> do
            meter <- getMeter meterProvider ecluseScope
            newOpenTelemetryWaiMiddleware' tracerProvider meter
        _ -> pure id

-- | Make upstream fetches open a client span and carry W3C trace-context headers, when telemetry is enabled.
instrumentDataPlaneManagerSettings :: Telemetry -> ManagerSettings -> IO ManagerSettings
instrumentDataPlaneManagerSettings telemetry settings =
    case telemetryTracerProvider telemetry of
        Nothing -> pure settings
        Just _ -> instrumentManagerSettings dataPlaneInstrumentationConfig settings

-- | The data plane's http-client instrumentation. It records __no__ headers, so an @Authorization@ header never reaches a span.
dataPlaneInstrumentationConfig :: HttpClientInstrumentationConfig
dataPlaneInstrumentationConfig = httpClientInstrumentationConfig

-- | Run a rule-evaluation span around an action that yields its result and the verdict to record ('ruleVerdictFields').
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

-- | Run a mirror-enqueue 'Producer' span that hands the body its trace context. It records only the artifact authority, as a URL can carry a credential.
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

-- | Run a mirror-job 'Consumer' span around the worker's fetch, verify and publish, linked to the enqueueing span.
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

-- | Run an advisory-sync 'Internal' span around one attempt, with the labels the @ecluse.advisory.sync.*@ metrics use, so the two join.
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

-- A domain span over one package alone: the packument gate and the two metadata legs.
withPackageSpan :: (MonadUnliftIO m) => Telemetry -> SpanKind -> Text -> PackageName -> m a -> m a
withPackageSpan telemetry spanKind spanName name action =
    withDomainSpan telemetry spanKind [] spanName $ \mSpan -> do
        recordFields mSpan [("ecluse.package", renderPackageName name)]
        action

-- | Project this module's serve-path spans onto the core 'TracingPort' the pipeline brackets with.
tracingPortOf :: Telemetry -> TracingPort
tracingPortOf telemetry =
    TracingPort
        { spanRuleEval = withRuleEvalSpan telemetry
        , spanMirrorEnqueue = \n v url ok action -> withRunInIO $ \runInIO ->
            withMirrorEnqueueSpan telemetry n v url ok (runInIO . action)
        , spanPackumentGate = \n action -> withRunInIO $ \runInIO ->
            withPackageSpan telemetry Internal "ecluse.packument.gate" n (runInIO action)
        , spanMetadataFetch = \n action -> withRunInIO $ \runInIO ->
            withPackageSpan telemetry Client "ecluse.metadata.fetch" n (runInIO action)
        , spanMetadataDecode = \n action -> withRunInIO $ \runInIO ->
            withPackageSpan telemetry Internal "ecluse.metadata.decode" n (runInIO action)
        }

-- | Project 'withMirrorJobSpan' onto the core 'WorkerTracingPort' that "Ecluse.Core.Worker" uses.
workerTracingPortOf :: Telemetry -> WorkerTracingPort
workerTracingPortOf telemetry =
    WorkerTracingPort
        { wtpMirrorJobSpan = withMirrorJobSpan telemetry
        }

-- | Project 'withAdvisorySyncSpan' onto the 'AdvisorySyncTracingPort' "Ecluse.Runtime.Cve.Sync" brackets through.
advisorySyncTracingPortOf :: Telemetry -> AdvisorySyncTracingPort
advisorySyncTracingPortOf telemetry =
    AdvisorySyncTracingPort
        { astpSyncAttemptSpan = withAdvisorySyncSpan telemetry
        }

-- | A serve verdict's span fields. None carries a secret: the rule and class are closed sets, and the message is the rendered decision.
ruleVerdictFields :: ServeDecision -> [(Text, Text)]
ruleVerdictFields = \case
    Admit -> [("ecluse.rule.decision", "admit")]
    Reject rejection ->
        [ ("ecluse.rule.decision", "deny")
        , ("ecluse.rule.reason_class", reasonClass (rejectionReason rejection))
        , ("ecluse.rule.message", rejectionMessage rejection)
        ]
            <> ruleNameField (rejectionReason rejection)

reasonClass :: RejectReason -> Text
reasonClass = \case
    ByPolicy{} -> "by_policy"
    Unavailable _ -> "unavailable"
    MissingIntegrity -> "missing_integrity"
    BelowIntegrityFloor -> "below_integrity_floor"
    UpstreamInvalid -> "upstream_invalid"

-- Only a policy denial has a rule to attribute, so no other refusal carries the field.
ruleNameField :: RejectReason -> [(Text, Text)]
ruleNameField = \case
    ByPolicy (RuleName ruleName) _ -> [("ecluse.rule.name", ruleName)]
    Unavailable _ -> []
    MissingIntegrity -> []
    BelowIntegrityFloor -> []
    UpstreamInvalid -> []

{- Run an action within a domain span, or against 'Nothing' when telemetry is disabled, which
creates no tracer. The span parents on the ambient context, so it nests under the WAI server span. -}
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

-- Capture a live span's trace context as the carrier stamped onto the mirror job, encoded as the
-- standard W3C @traceparent@\/@tracestate@ pair.
captureRemoteContext :: (MonadIO m) => Span -> m RemoteSpanContext
captureRemoteContext theSpan = do
    (traceparent, tracestate) <- liftIO (encodeSpanContext theSpan)
    pure
        RemoteSpanContext
            { rscTraceparent = decodeUtf8 traceparent
            , rscTracestate = decodeUtf8 tracestate
            }

-- The producer span a worker job points back to. A missing or unparsable carrier yields no link
-- and never fails the job, and the remote target leaves the job rooting its own trace.
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

-- Record text attribute fields on a span when one is present.
recordFields :: (MonadIO m) => Maybe Span -> [(Text, Text)] -> m ()
recordFields Nothing _ = pass
recordFields (Just theSpan) fields = traverse_ (uncurry (addAttribute theSpan)) fields

-- The coordinate fields every domain span carries. They are high-cardinality, which belongs on a
-- span and never on a metric label, and neither rendering can contain a credential.
coordinateFields :: PackageName -> Version -> [(Text, Text)]
coordinateFields name version =
    [ ("ecluse.package", renderPackageName name)
    , ("ecluse.version", renderVersion version)
    ]
