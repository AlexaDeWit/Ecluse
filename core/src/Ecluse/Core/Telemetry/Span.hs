-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RankNTypes #-}

{- | The domain-span tracing ports, and the bracket Pilot opens its own spans through.

The serve path, the mirror worker, and the advisory sync task reach their hand-added spans
through a port: bracket operations as a record (the Handle pattern), parametric in the
bracketed action's result and naming no OpenTelemetry tracer. The application supplies the
OTel-backed implementations (see @Ecluse.Runtime.Telemetry.Tracing@), a test a pass-through
double. Pilot's compile, stream, and export passes run outside a request and hold the
'TracerProvider' themselves, so they bracket through 'withOptionalSpan' instead. Both
consumers create their tracer under 'ecluseScope'.
-}
module Ecluse.Core.Telemetry.Span (
    -- * The serve-path tracing port
    TracingPort (..),

    -- * The worker tracing port
    WorkerTracingPort (..),
    JobSpanOutcome (..),

    -- * The advisory sync tracing port
    AdvisorySyncTracingPort (..),

    -- * Bracketing a span against an optional tracer
    ecluseScope,
    withOptionalSpan,
    openOptionalSpan,
    closeOptionalSpan,
) where

import OpenTelemetry.Context qualified as Ctx
import OpenTelemetry.Trace.Core (
    Span,
    SpanKind,
    TracerProvider,
    createSpan,
    defaultSpanArguments,
    endSpan,
    kind,
    makeTracer,
    tracerOptions,
 )
import UnliftIO (MonadUnliftIO, bracket)

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Queue (RemoteSpanContext)
import Ecluse.Core.Server.Response (ServeDecision)
import Ecluse.Core.Telemetry.Metrics (AdvisorySyncResult)
import Ecluse.Core.Version (Version)

{- | The domain-span tracing port: bracket operations over a backend whose closure captures its
tracer. The implementation is inert when tracing is off, so a call site brackets unconditionally.
-}
data TracingPort = TracingPort
    { spanRuleEval :: forall a. PackageName -> Version -> IO (a, ServeDecision) -> IO a
    {- ^ Bracket the per-version rule evaluation. The 'ServeDecision' the body yields goes on the
    span, so a refusal is explainable from the trace alone.
    -}
    , spanMirrorEnqueue ::
        forall a.
        PackageName ->
        Version ->
        Text ->
        (a -> Maybe Text) ->
        (Maybe RemoteSpanContext -> IO a) ->
        IO a
    {- ^ Bracket the serve-time hand-off to the asynchronous mirror, carrying the artifact's
    authoritative URL. The body stamps the supplied span context ('Nothing' when tracing is
    off) onto the job, linking the worker's span across the async hop. A 'Just' from the
    projection marks the span errored.
    -}
    , spanPackumentGate ::
        forall a.
        PackageName ->
        IO a ->
        IO a
    , spanMetadataFetch ::
        forall a.
        PackageName ->
        IO a ->
        IO a
    , spanMetadataDecode ::
        forall a.
        PackageName ->
        IO a ->
        IO a
    {- ^ Bracket the gating phase of a packument request, which runs the rules and
    filter on the public upstream document.
    -}
    }

{- | The mirror worker's domain-span tracing port: the worker analogue of 'TracingPort'. The
implementation is inert when tracing is off, so the worker brackets unconditionally.
-}
newtype WorkerTracingPort = WorkerTracingPort
    { wtpMirrorJobSpan ::
        forall a.
        PackageName ->
        Version ->
        Maybe RemoteSpanContext ->
        (a -> JobSpanOutcome) ->
        IO a ->
        IO a
    {- ^ Bracket the worker's per-job fetch, verify, and publish. The supplied trace context
    links the span back to the enqueueing request, and is 'Nothing' when the job carried none.
    The projected 'JobSpanOutcome' marks the span errored when the job did not publish.
    -}
    }

{- | The outcome projection a caller supplies for the mirror-job span. It is a small record rather
than the worker's own outcome type, so the tracing port does not depend on the worker loop.
-}
data JobSpanOutcome = JobSpanOutcome
    { jobSpanLabel :: Text
    -- ^ The bounded outcome label (e.g. @succeeded@ \/ @dropped@ \/ @retried@).
    , jobSpanError :: Maybe Text
    -- ^ The failure detail when the job did not publish. 'Nothing' on success.
    }
    deriving stock (Eq, Show)

{- | The advisory sync task's domain-span tracing port: one span per sync attempt. The span
carries the same 'AdvisorySyncResult' that labels the attempt metrics, so a trace and a series
join on one vocabulary. The implementation is inert when tracing is off.
-}
newtype AdvisorySyncTracingPort = AdvisorySyncTracingPort
    { astpSyncAttemptSpan ::
        forall a.
        Ecosystem ->
        (a -> AdvisorySyncResult) ->
        IO a ->
        IO a
    -- ^ Bracket one advisory sync attempt for an ecosystem, recording the projected result.
    }

{- | The instrumentation scope the hand-added spans and the WAI meter are created under, so they
are attributed to Écluse rather than to a third-party instrumentation library.
-}
ecluseScope :: (IsString s) => s
ecluseScope = "ecluse"

{- | Run @body@ against a span opened on @mTracerProvider@, or against 'Nothing' when there is
none, which opens no span. The span roots its own trace: no ambient context is consulted.
-}
withOptionalSpan :: (MonadUnliftIO m) => Maybe TracerProvider -> SpanKind -> Text -> (Maybe Span -> m a) -> m a
withOptionalSpan mTracerProvider spanKind name =
    bracket (openOptionalSpan mTracerProvider spanKind name) closeOptionalSpan

{- | The acquire half of 'withOptionalSpan', for a @conduit@ pass that brackets through
@bracketP@ rather than 'bracket'.
-}
openOptionalSpan :: (MonadIO m) => Maybe TracerProvider -> SpanKind -> Text -> m (Maybe Span)
openOptionalSpan mTracerProvider spanKind name = case mTracerProvider of
    Nothing -> pure Nothing
    Just tracerProvider ->
        let tracer = makeTracer tracerProvider ecluseScope tracerOptions
         in Just <$> createSpan tracer Ctx.empty name defaultSpanArguments{kind = spanKind}

-- | The release half of 'withOptionalSpan'. Ending at the current instant, as 'Nothing' asks.
closeOptionalSpan :: (MonadIO m) => Maybe Span -> m ()
closeOptionalSpan mSpan = whenJust mSpan (`endSpan` Nothing)
