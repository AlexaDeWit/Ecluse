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

{- | The domain-span tracing port: a record of bracket operations over a backend whose
closure captures its tracer. Each field runs a bracketed @IO@ action within a span and
returns its result. The fields are rank-2 (parametric in the result), so one port value
serves every call site whatever the body yields. The implementation is inert when
tracing is off, so the serve path brackets unconditionally.
-}
data TracingPort = TracingPort
    { spanRuleEval :: forall a. PackageName -> Version -> IO (a, ServeDecision) -> IO a
    {- ^ Bracket the per-version rule evaluation. The body yields its result and the
    verdict to record on the span. That verdict is the decision and, on a denial, the
    deciding rule, reason class, and message. A refusal is therefore explainable from
    the trace alone.
    -}
    , spanMirrorEnqueue ::
        forall a.
        PackageName ->
        Version ->
        Text ->
        (a -> Maybe Text) ->
        (Maybe RemoteSpanContext -> IO a) ->
        IO a
    {- ^ Bracket the serve-time hand-off to the asynchronous mirror, carrying the
    package, version, and the artifact's authoritative URL. The body receives the
    enqueueing span's trace context to stamp onto the mirror job, or 'Nothing' when
    tracing is off. The worker's per-job span can then link back across the async hop.
    The projection maps the body's result onto an optional failure detail. A 'Just'
    marks the span errored, so a swallowed best-effort enqueue failure is still
    explainable from the trace.
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

{- | The mirror worker's domain-span tracing port: the worker analogue of 'TracingPort',
kept a separate record so the worker brackets exactly its own span. The single field
brackets the per-job fetch → verify → publish, and projects the job's terminal result
onto the span's outcome ('JobSpanOutcome'). It is rank-2 (parametric in the result), so
one port value serves the call site whatever the body yields. The implementation is
inert when tracing is off, so the worker brackets unconditionally.
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
    {- ^ Bracket the worker's per-job fetch → verify → publish. It carries the package
    and version, the trace context the job was enqueued under, and the projected outcome
    once the job finishes. That trace context links the per-job span back to the
    enqueueing request across the async hop, and is 'Nothing' for a job that carried
    none. The outcome is the bounded outcome label always, plus a failure detail that
    marks the span errored when the job did not publish.
    -}
    }

{- | The projection a caller supplies for the mirror-job span. It carries the bounded
outcome label always, and, for a job that did not publish, the detail that marks the
span errored. A small record, rather than the worker's own outcome type, so the tracing
port does not depend on the worker loop.
-}
data JobSpanOutcome = JobSpanOutcome
    { jobSpanLabel :: Text
    -- ^ The bounded outcome label (e.g. @succeeded@ \/ @dropped@ \/ @retried@).
    , jobSpanError :: Maybe Text
    -- ^ The failure detail when the job did not publish. 'Nothing' on success.
    }
    deriving stock (Eq, Show)

{- | The advisory sync task's domain-span tracing port: one span per sync attempt. The
single field is rank-2 (parametric in the result), so one port value serves the call
site whatever the body yields. The projection names the attempt's bounded result once
the body finishes, so the span says which of the five outcomes it observed. That result
is the same 'AdvisorySyncResult' that labels the attempt metrics, so a trace and a
series join on one vocabulary. The implementation is inert when tracing is off, so the
sync loop brackets unconditionally.
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
