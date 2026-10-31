-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RankNTypes #-}

{- | The domain-span tracing ports, and the bracket Pilot opens its own spans through.

The serve path, the mirror worker, and the advisory sync task reach their spans through a port:
bracket operations naming no OpenTelemetry tracer, so the application supplies the OTel-backed
implementations and a test a pass-through double. Pilot's passes hold the 'TracerProvider'
themselves and bracket through 'withOptionalSpan'. Both create their tracer under 'ecluseScope'.
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

import Ecluse.Core.Cve.Types (OsvDbFetchFault)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Queue (RemoteSpanContext)
import Ecluse.Core.Server.Response (ServeDecision)
import Ecluse.Core.Telemetry.Metrics (AdvisorySyncResult)
import Ecluse.Core.Version (Version)

-- | The serve path's tracing port. The implementation is inert when tracing is off, so a call site brackets unconditionally.
data TracingPort = TracingPort
    { spanRuleEval :: forall a. PackageName -> Version -> IO (a, ServeDecision) -> IO a
    -- ^ Bracket the per-version rule evaluation. The 'ServeDecision' goes on the span, so a refusal is explainable from the trace.
    , spanMirrorEnqueue ::
        forall a.
        PackageName ->
        Version ->
        Text ->
        (a -> Maybe Text) ->
        (Maybe RemoteSpanContext -> IO a) ->
        IO a
    -- ^ Bracket the hand-off to the asynchronous mirror. The body stamps the span context onto the job, linking the worker's span.
    , spanPackumentGate ::
        forall a.
        PackageName ->
        IO a ->
        IO a
    -- ^ Bracket the gating phase of a packument request, which runs the rules and filter on the public upstream document.
    , spanMetadataFetch ::
        forall a.
        PackageName ->
        IO a ->
        IO a
    -- ^ Bracket one upstream metadata fetch, refusals included.
    , spanMetadataDecode ::
        forall a.
        PackageName ->
        IO a ->
        IO a
    -- ^ Bracket extraction after a successful metadata response. Streaming body reads can occur inside.
    , spanDirectoryFetch :: forall a. IO a -> IO a
    -- ^ Bracket one registry-wide directory fetch, refusals included. The directory names no package.
    , spanDirectoryDecode :: forall a. IO a -> IO a
    -- ^ Bracket directory extraction after a successful response. Streaming body reads can occur inside.
    }

-- | The mirror worker's tracing port, inert when tracing is off, so the worker brackets unconditionally.
newtype WorkerTracingPort = WorkerTracingPort
    { wtpMirrorJobSpan ::
        forall a.
        PackageName ->
        Version ->
        Maybe RemoteSpanContext ->
        (a -> JobSpanOutcome) ->
        IO a ->
        IO a
    -- ^ Bracket the worker's per-job fetch, verify and publish, linked to the enqueueing request when the job carried a context.
    }

-- | The outcome a caller projects for the mirror-job span, its own record so the port does not depend on the worker loop.
data JobSpanOutcome = JobSpanOutcome
    { jobSpanLabel :: Text
    -- ^ The bounded outcome label (e.g. @succeeded@ \/ @dropped@ \/ @retried@).
    , jobSpanError :: Maybe Text
    -- ^ The failure detail when the job did not publish. 'Nothing' on success.
    }
    deriving stock (Eq, Show)

-- | One span per advisory sync attempt, labelled with the 'AdvisorySyncResult' of the attempt metrics, so a trace and a series join.
newtype AdvisorySyncTracingPort = AdvisorySyncTracingPort
    { astpSyncAttemptSpan ::
        forall a.
        Ecosystem ->
        (a -> (AdvisorySyncResult, Maybe OsvDbFetchFault)) ->
        IO a ->
        IO a
    -- ^ Bracket one attempt, recording the projected result and, for a failed fetch, its typed fault as the error status.
    }

-- | The instrumentation scope of the hand-added spans and the WAI meter, so the signals are attributed to Écluse.
ecluseScope :: (IsString s) => s
ecluseScope = "ecluse"

-- | Run @body@ against a span on @mTracerProvider@, or against 'Nothing' without one. The span roots its own trace.
withOptionalSpan :: (MonadUnliftIO m) => Maybe TracerProvider -> SpanKind -> Text -> (Maybe Span -> m a) -> m a
withOptionalSpan mTracerProvider spanKind name =
    bracket (openOptionalSpan mTracerProvider spanKind name) closeOptionalSpan

-- | The acquire half of 'withOptionalSpan', for a @conduit@ pass that brackets through @bracketP@ rather than 'bracket'.
openOptionalSpan :: (MonadIO m) => Maybe TracerProvider -> SpanKind -> Text -> m (Maybe Span)
openOptionalSpan mTracerProvider spanKind name = case mTracerProvider of
    Nothing -> pure Nothing
    Just tracerProvider ->
        let tracer = makeTracer tracerProvider ecluseScope tracerOptions
         in Just <$> createSpan tracer Ctx.empty name defaultSpanArguments{kind = spanKind}

-- | The release half of 'withOptionalSpan'. Ending at the current instant, as 'Nothing' asks.
closeOptionalSpan :: (MonadIO m) => Maybe Span -> m ()
closeOptionalSpan mSpan = whenJust mSpan (`endSpan` Nothing)
