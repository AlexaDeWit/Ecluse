-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The log-to-trace correlation glue: read the active OpenTelemetry span off the ambient
context and stamp its ids onto the @dd@ log object ("Ecluse.Runtime.Log"), so a reader joins a
JSONL line to the trace it was emitted within. This is the IO half "Ecluse.Runtime.Log" defers,
which is why that module needs no OpenTelemetry dependency. The ids take
@hs-opentelemetry-propagator-datadog@'s form: the unsigned decimal of the low 64 bits,
big-endian. No span in scope, or one whose context is not valid, contributes no ids, so a line
never carries a meaningless all-zero trace id. The identity still stamps it.
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
import OpenTelemetry.Propagator.Datadog (
    convertOpenTelemetrySpanIdToDatadogSpanId,
    convertOpenTelemetryTraceIdToDatadogTraceId,
 )
import OpenTelemetry.Trace.Core (getActiveSpanContext, isValid)
import OpenTelemetry.Trace.Core qualified as OTel

import Ecluse.Runtime.Log (
    DdContext (..),
    DdSpan (DdSpan),
    ddField,
 )
import Ecluse.Runtime.Telemetry.Resolve (
    ResolvedTelemetry (rtEnvironment, rtServiceName, rtVersion),
    resolveTelemetry,
 )

{- | The span-less @dd@ identity that stamps every log line. 'ddPayloadNow' fills the span at
log time. The one resolved configuration feeds the SDK too, so logs and traces share an identity.
-}
ddIdentity :: ResolvedTelemetry -> DdContext
ddIdentity resolved =
    DdContext
        { ddService = rtServiceName resolved
        , ddEnv = rtEnvironment resolved
        , ddVersion = rtVersion resolved
        , ddSpan = Nothing
        }

{- | Resolve the @dd@ identity from the environment on "Ecluse.Runtime.Telemetry.Resolve"'s
precedence, so the log identity matches the exporter's. Call once at composition, never per line.
-}
ddIdentityFromEnvironment :: IO DdContext
ddIdentityFromEnvironment = ddIdentity . resolveTelemetry <$> getEnvironment

{- | The active span's ids in Datadog format, 'Nothing' when no span is in scope or its context
is not valid, so a log line never carries an all-zero trace id.
-}
activeDdSpan :: (MonadIO m) => m (Maybe DdSpan)
activeDdSpan = do
    mContext <- getActiveSpanContext
    pure $ case mContext of
        Just spanContext
            | isValid spanContext ->
                Just
                    ( DdSpan
                        (show (convertOpenTelemetryTraceIdToDatadogTraceId (OTel.traceId spanContext)))
                        (show (convertOpenTelemetrySpanIdToDatadogSpanId (OTel.spanId spanContext)))
                    )
        _ -> Nothing

{- | Fill the active span's ids onto a @dd@ identity, yielding the 'DdContext' for the current
log site.
-}
ddContextNow :: (MonadIO m) => DdContext -> m DdContext
ddContextNow base = do
    mSpan <- activeDdSpan
    pure base{ddSpan = mSpan}

{- | The @dd@ object for the current log site as a @katip@ payload. Install it as a request or
worker scope's initial context so every line under that scope carries @dd@.
-}
ddPayloadNow :: (MonadIO m) => DdContext -> m SimpleLogPayload
ddPayloadNow base = ddField <$> ddContextNow base
