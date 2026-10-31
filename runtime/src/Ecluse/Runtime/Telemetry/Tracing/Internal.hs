-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RankNTypes #-}

{- | The middleware, the exchange tracing, the domain-span brackets and the attribute mappings
behind "Ecluse.Runtime.Telemetry.Tracing", which documents the tracing layer and re-exports the
curated surface. Importing this module opts out of that stability promise, the convention @text@
and @bytestring@ use, so production code imports the public one.
-}
module Ecluse.Runtime.Telemetry.Tracing.Internal (
    -- * WAI server span
    telemetryWaiMiddleware,

    -- * The data plane's client spans
    dataPlaneTrace,
    tracedExchange,

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

import Network.HTTP.Client (HttpException (HttpExceptionRequest, InvalidUrlException))
import Network.Wai (Middleware)
import OpenTelemetry.Instrumentation.HttpClient (
    HttpClientInstrumentationConfig (httpClientMetrics),
    httpClientInstrumentationConfig,
    tracedHttpRequest,
 )
import OpenTelemetry.Instrumentation.HttpClient.Raw (createHttpClientMetricsFromMeter)
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
import Text.Show (showParen, showString, shows, showsPrec)
import UnliftIO (MonadUnliftIO, withRunInIO)
import UnliftIO.Exception (catch, mask, throwIO, withException)

import Ecluse.Core.Cve.Types (OsvDbFetchFault, renderOsvDbFetchFault)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Queue (RemoteSpanContext (RemoteSpanContext, rscTraceparent, rscTracestate))
import Ecluse.Core.Registry.Exchange.Types (TraceExchange, untracedExchange)
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
import Ecluse.Runtime.Telemetry.Escape (escapeRecordingMiddleware, recordEscape)

-- | The WAI server-span middleware, or 'id' without telemetry. It belongs __outermost__, so the span covers the request.
telemetryWaiMiddleware :: Telemetry -> IO Middleware
telemetryWaiMiddleware telemetry =
    case (telemetryTracerProvider telemetry, telemetryMeterProvider telemetry) of
        (Just tracerProvider, Just meterProvider) -> do
            meter <- getMeter meterProvider ecluseScope
            (. escapeRecordingMiddleware) <$> newOpenTelemetryWaiMiddleware' tracerProvider meter
        _ -> pure id

{- | Open each upstream exchange under one client span that carries W3C trace-context headers, when
telemetry is enabled. The span comes from the global tracer provider, which the library reads.
-}
dataPlaneTrace :: Telemetry -> IO TraceExchange
dataPlaneTrace telemetry =
    case (telemetryTracerProvider telemetry, telemetryMeterProvider telemetry) of
        (Just _, Just meterProvider) -> do
            -- The library's instruments under its own scope, built once for the process.
            metrics <- createHttpClientMetricsFromMeter =<< getMeter meterProvider "hs-opentelemetry-instrumentation-http-client"
            -- It records __no__ header, so an @Authorization@ header never reaches a span.
            pure (tracedExchange httpClientInstrumentationConfig{httpClientMetrics = Just metrics})
        _ -> pure untracedExchange

{- | One client span from the request to the response headers, with its bookkeeping masked. An
asynchronous exception lands in the opener, where the span records it and ends, or in a thread's first
context attach, which can wait on the context table's growth and leave the span unended.
-}
tracedExchange :: HttpClientInstrumentationConfig -> TraceExchange
tracedExchange config request open =
    mask $ \restore ->
        tracedHttpRequest config request (\sent -> restore (open sent) `catch` (throwIO . UntaggedFault))
            `catch` \(UntaggedFault original) -> throwIO original

{- An HTTP fault crossing the client span, which records its text, shown without the request it is
tagged with. StatusCodeException and TooManyRedirects show a response that still holds the request,
and stay unreachable only while requests keep redirectCount 0 and the default checkResponse. -}
newtype UntaggedFault = UntaggedFault HttpException

-- Shown as http-client shows a fault before it tags the fault with its request.
instance Show UntaggedFault where
    showsPrec precedence (UntaggedFault fault) = case fault of
        HttpExceptionRequest _ content ->
            showParen (precedence >= 11) (showString "HttpExceptionContentWrapper {unHttpExceptionContentWrapper = " . shows content . showString "}")
        InvalidUrlException{} -> showsPrec precedence fault

instance Exception UntaggedFault

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
    (a -> (AdvisorySyncResult, Maybe OsvDbFetchFault)) ->
    m a ->
    m a
withAdvisorySyncSpan telemetry eco project action =
    withDomainSpan telemetry Internal [] "ecluse.advisory.sync.attempt" $ \mSpan -> do
        recordFields mSpan [("ecluse.ecosystem", ecosystemName eco)]
        result <- action
        let (outcome, failure) = project result
        recordFields mSpan [("ecluse.advisory.sync.result", advisorySyncResultName outcome)]
        whenJust mSpan $ \theSpan -> whenJust failure (setStatus theSpan . Error . renderOsvDbFetchFault)
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
            withPackageSpan telemetry Client metadataFetchSpan n (runInIO action)
        , spanMetadataDecode = \n action -> withRunInIO $ \runInIO ->
            withPackageSpan telemetry Internal metadataDecodeSpan n (runInIO action)
        , spanDirectoryFetch = withDomainSpan telemetry Client [] metadataFetchSpan . const
        , spanDirectoryDecode = withDomainSpan telemetry Internal [] metadataDecodeSpan . const
        }

-- One name per metadata leg, whether the read is one package's or a registry-wide directory's.
metadataFetchSpan, metadataDecodeSpan :: Text
metadataFetchSpan = "ecluse.metadata.fetch"
metadataDecodeSpan = "ecluse.metadata.decode"

-- | Project 'withMirrorJobSpan' onto the core 'WorkerTracingPort' that "Ecluse.Core.Worker" uses.
workerTracingPortOf :: Telemetry -> WorkerTracingPort
workerTracingPortOf telemetry =
    WorkerTracingPort
        { wtpMirrorJobSpan = withMirrorJobSpan telemetry
        }

-- | Project 'withAdvisorySyncSpan' onto the 'AdvisorySyncTracingPort' "Ecluse.Runtime.Cve.Sync" brackets through. Inert when telemetry is disabled.
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

{- Run an action within a domain span, or against 'Nothing' without telemetry. The span nests under
the ambient one, and records an escaping request-carrying exception by its typed cause. -}
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
             in inSpan' tracer name defaultSpanArguments{kind = spanKind, links = spanLinks} $ \theSpan ->
                    body (Just theSpan) `withException` recordEscape theSpan

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
