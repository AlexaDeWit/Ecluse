-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RankNTypes #-}

{- | The domain-span tracing ports, decoupled from any tracing backend. The core serve
path, the mirror worker, and the advisory sync task open their hand-added spans through
these abstract interfaces.

The serve path brackets two domain spans an operator cares about: the per-version rule
verdict, and the synchronous-to-asynchronous mirror hand-off. The mirror worker brackets
one, the per-job fetch → verify → publish. The advisory sync task brackets one span per
sync attempt.

This module defines those bracket operations as records of functions (the Handle
pattern). Each is parametric in the bracketed action's result, so the span wraps the real
work without seeing its shape. A consumer records through its port and never names an
OpenTelemetry tracer. The application supplies the OTel-backed implementations behind
them (see @Ecluse.Runtime.Telemetry.Tracing@), and a test supplies a pass-through double
that simply runs the body.

There are three ports. 'TracingPort' carries the serve path's two spans,
'WorkerTracingPort' the worker's mirror-job span, and 'AdvisorySyncTracingPort' the
advisory sync task's per-attempt span. Each port carries exactly the spans its consumer
opens.
-}
module Ecluse.Core.Telemetry.Span (
    -- * The serve-path tracing port
    TracingPort (..),

    -- * The worker tracing port
    WorkerTracingPort (..),
    JobSpanOutcome (..),

    -- * The advisory sync tracing port
    AdvisorySyncTracingPort (..),
) where

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
