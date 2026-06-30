{- | The log↔trace correlation glue: read the active OpenTelemetry span off the
ambient context and stamp its ids onto the @dd@ log object ("Ecluse.Log"), so a JSONL
line can be joined to the trace it was emitted within.

"Ecluse.Log" owns the @dd@ object's /shape/ and the Datadog id format
('Ecluse.Log.formatDdTraceId' \/ 'Ecluse.Log.formatDdSpanId'), and stays free of any
OpenTelemetry dependency. This module is the IO half that "Ecluse.Log" deferred: it
reaches into the OpenTelemetry thread-local context for the active span, renders its
trace and span ids into a 'DdSpan', and fills it onto a 'DdContext'.

== The identity and the span

The @service@\/@env@\/@version@ identity is resolved once
("Ecluse.Telemetry.Resolve") and carried as a span-less 'DdContext' (the
__identity__); 'ddPayloadNow' fills the __active span__ onto a copy of it at log time.
With no span in scope -- outside a request, or with telemetry off -- the trace and span
ids are simply absent and the identity still stamps the line. A span whose context is
not valid (a dropped\/non-recording span carrying zero ids) likewise contributes no
ids, so a line never carries a meaningless all-zero trace id.

The identity is installed as the initial @katip@ context at the per-request and worker
entry points, so every log line carries the @dd@ object; the ids are read at that
point (the WAI server span is active by then) and re-read where a tighter span is
opened.
-}
module Ecluse.Telemetry.Correlation (
    -- * Identity
    ddIdentity,
    ddIdentityFromEnvironment,

    -- * Active-span correlation
    activeDdSpan,
    ddContextNow,
    ddPayloadNow,
) where

import System.Environment (getEnvironment)

import Katip (SimpleLogPayload)
import OpenTelemetry.Trace.Core (getActiveSpanContext, isValid)
import OpenTelemetry.Trace.Core qualified as OTel
import OpenTelemetry.Trace.Id (spanIdBytes, traceIdBytes)

import Ecluse.Log (
    DdContext (..),
    DdSpan (DdSpan),
    ddField,
    formatDdSpanId,
    formatDdTraceId,
 )
import Ecluse.Telemetry.Resolve (
    ResolvedTelemetry (rtEnvironment, rtServiceName, rtVersion),
    resolveTelemetry,
 )

{- | The span-less @dd@ identity from a resolved telemetry configuration: the
@service@\/@env@\/@version@ that stamp every line, with no active span yet
('ddPayloadNow' fills that at log time). The single resolved identity feeds both the
SDK and this object, so logs and traces share one identity whichever dialect was
configured.
-}
ddIdentity :: ResolvedTelemetry -> DdContext
ddIdentity resolved =
    DdContext
        { ddService = rtServiceName resolved
        , ddEnv = rtEnvironment resolved
        , ddVersion = rtVersion resolved
        , ddSpan = Nothing
        }

{- | Resolve the @dd@ identity from the process environment -- the same precedence
table the SDK configuration uses ("Ecluse.Telemetry.Resolve"), so the log identity
matches the exporter's. Read once at composition (the @OTEL_*@ environment is already
normalised by then), not per line.
-}
ddIdentityFromEnvironment :: IO DdContext
ddIdentityFromEnvironment = ddIdentity . resolveTelemetry <$> getEnvironment

{- | The active span's ids as a 'DdSpan', read from the ambient OpenTelemetry context
and rendered in the Datadog id format. 'Nothing' when no span is in scope or the
active span's context is not valid (a dropped\/non-recording span), so a line never
carries an all-zero trace id.
-}
activeDdSpan :: (MonadIO m) => m (Maybe DdSpan)
activeDdSpan = do
    mContext <- getActiveSpanContext
    pure $ case mContext of
        Just spanContext
            | isValid spanContext ->
                Just
                    ( DdSpan
                        (formatDdTraceId (traceIdBytes (OTel.traceId spanContext)))
                        (formatDdSpanId (spanIdBytes (OTel.spanId spanContext)))
                    )
        _ -> Nothing

{- | Fill the active span's ids onto a @dd@ identity, yielding the full 'DdContext' for
the current log site: the identity always, the trace\/span ids when a valid span is in
scope.
-}
ddContextNow :: (MonadIO m) => DdContext -> m DdContext
ddContextNow base = do
    mSpan <- activeDdSpan
    pure base{ddSpan = mSpan}

{- | The @dd@ object for the current log site as a @katip@ payload -- the identity plus
the active span's ids -- ready to compose into a log call or install as the initial
context of a request\/worker scope (so every line under it carries @dd@).
-}
ddPayloadNow :: (MonadIO m) => DdContext -> m SimpleLogPayload
ddPayloadNow base = ddField <$> ddContextNow base
