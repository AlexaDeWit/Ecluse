-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The log↔trace correlation glue: read the active OpenTelemetry span off the
ambient context and stamp its ids onto the @dd@ log object ("Ecluse.Runtime.Log"). A
reader then joins a JSONL line to the trace it was emitted within.

"Ecluse.Runtime.Log" owns the @dd@ object's /shape/ and the Datadog id format
('Ecluse.Runtime.Log.formatDdTraceId' \/ 'Ecluse.Runtime.Log.formatDdSpanId'), and
stays free of any OpenTelemetry dependency. This module is the IO half that
"Ecluse.Runtime.Log" deferred. It reaches into the OpenTelemetry thread-local context
for the active span. It renders the trace and span ids into a 'DdSpan' and fills that
onto a 'DdContext'.

== The identity and the span

"Ecluse.Runtime.Telemetry.Resolve" resolves the @service@\/@env@\/@version@ identity
once, and this module carries it as a span-less 'DdContext', the __identity__.
'ddPayloadNow' fills the __active span__ onto a copy of it at log time. With no span
in scope, outside a request or with telemetry off, the trace and span ids are simply
absent. The identity still stamps the line. A span whose context is not valid (a
dropped\/non-recording span carrying zero ids) likewise contributes no ids. A line
therefore never carries a meaningless all-zero trace id.

The identity is installed as the initial @katip@ context at the per-request and worker
entry points, so every log line carries the @dd@ object. The ids are read at that
point, since the WAI server span is active by then, and re-read where a tighter span
opens.
-}
module Ecluse.Runtime.Telemetry.Correlation (
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

import Ecluse.Runtime.Log (
    DdContext (..),
    DdSpan (DdSpan),
    ddField,
    formatDdSpanId,
    formatDdTraceId,
 )
import Ecluse.Runtime.Telemetry.Resolve (
    ResolvedTelemetry (rtEnvironment, rtServiceName, rtVersion),
    resolveTelemetry,
 )

{- | The span-less @dd@ identity from a resolved telemetry configuration: the
@service@\/@env@\/@version@ that stamp every line. There is no active span yet, and
'ddPayloadNow' fills one at log time. The single resolved identity feeds both the
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

{- | Resolve the @dd@ identity from the process environment, the same precedence table
the SDK configuration uses ("Ecluse.Runtime.Telemetry.Resolve"), so the log identity
matches the exporter's. Read once at composition (the @OTEL_*@ environment is already
normalised by then), not per line.
-}
ddIdentityFromEnvironment :: IO DdContext
ddIdentityFromEnvironment = ddIdentity . resolveTelemetry <$> getEnvironment

{- | The active span's ids as a 'DdSpan', read from the ambient OpenTelemetry context
and rendered in the Datadog id format. 'Nothing' when no span is in scope, and
'Nothing' when the active span's context is not valid (a dropped\/non-recording span).
A line therefore never carries an all-zero trace id.
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
the current log site. The identity is always present, and the trace\/span ids when a
valid span is in scope.
-}
ddContextNow :: (MonadIO m) => DdContext -> m DdContext
ddContextNow base = do
    mSpan <- activeDdSpan
    pure base{ddSpan = mSpan}

{- | The @dd@ object for the current log site as a @katip@ payload: the identity plus
the active span's ids. Ready to compose into a log call, or to install as the initial
context of a request\/worker scope so every line under it carries @dd@.
-}
ddPayloadNow :: (MonadIO m) => DdContext -> m SimpleLogPayload
ddPayloadNow base = ddField <$> ddContextNow base
