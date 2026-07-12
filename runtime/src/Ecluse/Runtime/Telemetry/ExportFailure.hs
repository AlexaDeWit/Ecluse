-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Export-failure routing: the shared throttle every telemetry export failure
coalesces through.

Telemetry failures must stay off the request path and out of raw stderr. The SDK's
batch exporter runs asynchronously, so an unreachable collector never touches a served
request. This module owns the __shared throttle__ those failures coalesce through: an
'ExportFailureSink' carries one throttle plus a @katip@ target, and 'routeExportFailure'
surfaces the first failure plainly, then a periodic heartbeat carrying the suppressed
count, so a persistently unreachable endpoint is one visible warning and a heartbeat,
not a per-flush flood.

The exporter wrappers ("Ecluse.Runtime.Telemetry") feed the sink through
'observeExportResult'; 'installExportErrorHandler' routes the SDK's own diagnostic
stream through the same sink. The mechanism is described in
@docs\/architecture\/observability.md@.
-}
module Ecluse.Runtime.Telemetry.ExportFailure (
    -- * Export-failure throttle (pure core)
    ThrottleState (..),
    ThrottleEmit (..),
    initialThrottle,
    throttleInterval,
    throttleStep,

    -- * Export-failure routing
    ExportFailureSink,
    newExportFailureSink,
    exportFailureSink,
    routeExportFailure,
    observeExportResult,
    installExportErrorHandler,
) where

import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

import Katip (LogEnv, Severity (WarningS), logFM, ls)
import Katip.Monadic (runKatipContextT)
import OpenTelemetry.Exporter.Span (ExportResult (..))
import OpenTelemetry.Internal.Logging (setGlobalErrorHandler)

import Ecluse.Runtime.Log (moduleField)

{- | The throttle state for SDK export-error routing: when an error was last
logged, and how many have been suppressed since. Exposed so the throttle decision
is unit-tested without wall-clock timing.
-}
data ThrottleState = ThrottleState
    { tsLastLogged :: Maybe UTCTime
    -- ^ When an error was last surfaced ('Nothing' before the first).
    , tsSuppressed :: Int
    -- ^ Errors suppressed since the last surfaced one.
    }
    deriving stock (Eq, Show)

-- | What 'throttleStep' decided to do with an export error.
data ThrottleEmit
    = -- | The first error: surface it plainly.
      EmitFirst
    | {- | The throttle window elapsed: surface a heartbeat carrying the count of
      errors since the last surfaced one (this one included).
      -}
      EmitHeartbeat Int
    | -- | Within the window: suppress and count.
      EmitSuppress
    deriving stock (Eq, Show)

-- | The initial throttle state: nothing logged, nothing suppressed.
initialThrottle :: ThrottleState
initialThrottle = ThrottleState Nothing 0

-- | How long export errors are coalesced between surfaced heartbeats.
throttleInterval :: NominalDiffTime
throttleInterval = 60

{- | Advance the throttle for one export error at @now@: surface the first error,
surface a heartbeat once the 'throttleInterval' has elapsed since the last surfaced
one (resetting the suppressed count), and otherwise suppress while counting. Pure,
so a sequence of @(time, decision)@ steps is asserted directly.
-}
throttleStep :: NominalDiffTime -> UTCTime -> ThrottleState -> (ThrottleState, ThrottleEmit)
throttleStep interval now st = case tsLastLogged st of
    Nothing -> (ThrottleState (Just now) 0, EmitFirst)
    Just lastLogged
        | diffUTCTime now lastLogged >= interval ->
            (ThrottleState (Just now) 0, EmitHeartbeat (tsSuppressed st + 1))
        | otherwise ->
            (st{tsSuppressed = tsSuppressed st + 1}, EmitSuppress)

{- | The shared export-failure sink: a single throttle plus the @katip@ target that
every export failure feeds -- the span exporter, the metric exporter, and the SDK's own
diagnostic stream -- so a persistently unreachable collector is one coalesced stream (the
first failure plainly, then a periodic heartbeat) rather than several independent floods.

The clock and the surfacing action are injected so the throttle decision is unit-tested
without wall-clock timing or a live @katip@ scribe (mirroring the pure 'throttleStep'
tests); 'exportFailureSink' wires the production clock and @katip@ target.
-}
data ExportFailureSink = ExportFailureSink
    { sinkNow :: IO UTCTime
    , sinkState :: IORef ThrottleState
    , sinkSurface :: Severity -> Text -> IO ()
    }

-- | Build an export-failure sink over an injected clock and surfacing action.
newExportFailureSink :: IO UTCTime -> (Severity -> Text -> IO ()) -> IO ExportFailureSink
newExportFailureSink now surface = do
    throttleRef <- newIORef initialThrottle
    pure ExportFailureSink{sinkNow = now, sinkState = throttleRef, sinkSurface = surface}

{- | The production sink: the wall clock and the composition-root 'LogEnv' as the @katip@
target, tagged with this module (the plain-'IO' @katip@ path the boot phase uses).
-}
exportFailureSink :: LogEnv -> IO ExportFailureSink
exportFailureSink logEnv = newExportFailureSink getCurrentTime (logExportFailure logEnv)

{- | Route one export-failure diagnostic through the shared throttle into @katip@: the
first surfaced plainly, a heartbeat carrying the suppressed count once 'throttleInterval'
has elapsed since the last surfaced one, otherwise suppressed and counted.
-}
routeExportFailure :: ExportFailureSink -> Text -> IO ()
routeExportFailure sink diagnostic = do
    now <- sinkNow sink
    emit <- atomicModifyIORef' (sinkState sink) (throttleStep throttleInterval now)
    case emit of
        EmitFirst -> sinkSurface sink WarningS (firstErrorMessage diagnostic)
        EmitHeartbeat suppressed -> sinkSurface sink WarningS (heartbeatMessage suppressed diagnostic)
        EmitSuppress -> pass

{- | Observe one exporter's 'ExportResult', routing a 'Failure' through the sink and
ignoring a 'Success'. This only /observes/ the failure -- the inner result is the
caller's to return unchanged, so export semantics are untouched (a failed export stays
off the request path). @signal@ names the failing exporter (@span@ \/ @metric@).
-}
observeExportResult :: ExportFailureSink -> Text -> ExportResult -> IO ()
observeExportResult sink signal = \case
    Success -> pass
    Failure mErr -> routeExportFailure sink (signal <> " export failed" <> maybe "" ((": " <>) . show) mErr)

{- | Install a process-global handler for the SDK's own diagnostic stream, routed through
the shared sink so it coalesces with the exporter-failure feed. In @hs-opentelemetry
1.0.0.0@ the only caller of this handler is the SDK's internal logging -- a failed OTLP
export is dropped there rather than routed here -- so the export-failure feed comes from
the exporter wrappers ('observeExportResult'); this handler is kept for the SDK-internal
diagnostics it still serves.

The forwarded diagnostic 'String' is the SDK's own text and is trusted not to carry
secrets: this module never reads the credential-bearing telemetry inputs
(@OTEL_EXPORTER_OTLP_HEADERS@, @DD_API_KEY@, @DD_SITE@), so the only residual channel is
whatever the SDK itself chooses to log, which the upstream exporter keeps to
endpoint/status diagnostics.
-}
installExportErrorHandler :: ExportFailureSink -> IO ()
installExportErrorHandler sink = setGlobalErrorHandler (routeExportFailure sink . toText)

firstErrorMessage :: Text -> Text
firstErrorMessage diagnostic =
    "telemetry export error (subsequent identical errors are throttled): " <> diagnostic

heartbeatMessage :: Int -> Text -> Text
heartbeatMessage suppressed diagnostic =
    "telemetry export still failing: "
        <> show suppressed
        <> " export errors since the last report. Latest: "
        <> diagnostic

-- Log one line through the composition-root 'LogEnv', tagged with this module -- the
-- plain-'IO' katip path the boot phase uses (it holds no 'Handler' reader).
logExportFailure :: LogEnv -> Severity -> Text -> IO ()
logExportFailure logEnv severity message =
    runKatipContextT logEnv (moduleField "Ecluse.Runtime.Telemetry.ExportFailure") mempty $
        logFM severity (ls message)
