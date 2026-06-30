{-# LANGUAGE RankNTypes #-}

{- | The domain-span tracing ports: the abstract interfaces the core serve path and
mirror worker open their hand-added spans through, decoupled from any tracing backend.

The serve path brackets two domain spans an operator cares about -- the per-version
rule verdict and the synchronous-to-asynchronous mirror hand-off -- and the mirror
worker brackets one -- the per-job fetch → verify → publish. This module defines those
bracket operations as records of functions (the Handle pattern), each parametric in the
bracketed action's result so the span wraps the real work without seeing its shape. A
consumer records through its port and never names an OpenTelemetry tracer; the
application supplies the OTel-backed implementations behind them (see
@Ecluse.Telemetry.Tracing@), and a test supplies a pass-through double that simply runs
the body.

Two ports are defined: 'TracingPort' for the serve path's two spans and
'WorkerTracingPort' for the worker's mirror-job span; each carries exactly the spans its
consumer opens.
-}
module Ecluse.Core.Telemetry.Span (
    -- * The serve-path tracing port
    TracingPort (..),

    -- * The worker tracing port
    WorkerTracingPort (..),
    JobSpanOutcome (..),
) where

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Queue (RemoteSpanContext)
import Ecluse.Core.Server.Response (ServeDecision)
import Ecluse.Core.Version (Version)

{- | The domain-span tracing port -- a record of bracket operations over a backend
whose closure captures its tracer. Each field runs a bracketed @IO@ action within a
span and returns its result; the fields are rank-2 (parametric in the result) so one
port value serves every call site whatever the body yields. The implementation is
inert when tracing is off, so the serve path brackets unconditionally.
-}
data TracingPort = TracingPort
    { spanRuleEval :: forall a. PackageName -> Version -> IO (a, ServeDecision) -> IO a
    {- ^ Bracket the per-version rule evaluation: the body yields its result and the
    verdict to record on the span (the decision and, on a denial, the deciding rule,
    reason class, and message), so a refusal is explainable from the trace alone.
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
    package, version, and the artifact's authoritative URL. The body is handed the
    enqueueing span's trace context (or 'Nothing' when tracing is off) to stamp onto
    the mirror job, so the worker's per-job span can link back across the async hop.
    The projection maps the body's result onto an optional failure detail: a 'Just'
    marks the span errored, so a swallowed best-effort enqueue failure is still
    explainable from the trace.
    -}
    , spanPackumentGate ::
        forall a.
        PackageName ->
        IO a ->
        IO a
    -- ^ Bracket the gating phase of a packument request, which runs the rules and filter on the public upstream document.
    }

{- | The mirror worker's domain-span tracing port -- the worker analogue of 'TracingPort',
kept a separate record so the worker brackets exactly its own span. The single field
brackets the per-job fetch → verify → publish, projecting the job's terminal result onto
the span's outcome ('JobSpanOutcome'); it is rank-2 (parametric in the result) so one
port value serves the call site whatever the body yields. The implementation is inert
when tracing is off, so the worker brackets unconditionally.
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
    {- ^ Bracket the worker's per-job fetch → verify → publish, carrying the package and
    version, the trace context the job was enqueued under (to __link__ the per-job span
    back to the enqueueing request across the async hop, or 'Nothing' for a job that
    carried none), and, once the job finishes, the projected outcome (the bounded outcome
    label always, and a failure detail that marks the span errored when the job did not
    publish).
    -}
    }

{- | The projection a caller supplies for the mirror-job span: the bounded outcome label
always, and, for a job that did not publish, the detail that marks the span errored. A
small record (rather than the worker's own outcome type) so the tracing port does not
depend on the worker loop.
-}
data JobSpanOutcome = JobSpanOutcome
    { jobSpanLabel :: Text
    -- ^ The bounded outcome label (e.g. @succeeded@ \/ @dropped@ \/ @retried@).
    , jobSpanError :: Maybe Text
    -- ^ The failure detail when the job did not publish; 'Nothing' on success.
    }
    deriving stock (Eq, Show)
