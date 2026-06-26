{-# LANGUAGE RankNTypes #-}

{- | The domain-span tracing port: the abstract interface the core serve path opens
its hand-added spans through, decoupled from any tracing backend.

The serve path brackets two domain spans an operator cares about — the per-version
rule verdict and the synchronous-to-asynchronous mirror hand-off. This module defines
those two bracket operations as a record of functions (the Handle pattern), each
parametric in the bracketed action's result so the span wraps the real work without
seeing its shape. The core records through this port and never names an OpenTelemetry
tracer; the application supplies the OTel-backed implementation behind it (see
@Ecluse.Telemetry.Tracing@), and a test supplies a pass-through double that simply
runs the body.

Only the two serve-path spans are present; the worker's mirror-job span stays in the
application tracing layer, so the port carries exactly what the pipeline uses.
-}
module Ecluse.Core.Telemetry.Span (
    -- * The tracing port
    TracingPort (..),
) where

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Server.Response (ServeDecision)
import Ecluse.Core.Version (Version)

{- | The domain-span tracing port — a record of bracket operations over a backend
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
    , spanMirrorEnqueue :: forall a. PackageName -> Version -> Text -> IO a -> IO a
    {- ^ Bracket the serve-time hand-off to the asynchronous mirror, carrying the
    package, version, and the artifact's authoritative URL.
    -}
    }
