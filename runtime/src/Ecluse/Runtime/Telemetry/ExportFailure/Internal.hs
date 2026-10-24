-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The throttle and the sink behind "Ecluse.Runtime.Telemetry.ExportFailure", which documents
the routing and re-exports the curated surface. Importing this module opts out of that stability
promise, the convention @text@ and @bytestring@ use, so production code imports the public one.
-}
module Ecluse.Runtime.Telemetry.ExportFailure.Internal (
    -- * The throttle (pure core)
    ThrottleState (..),
    ThrottleEmit (..),
    initialThrottle,
    throttleStep,

    -- * Routing
    ExportFailureSink,
    newExportFailureSink,
    exportFailureSink,
    observeExportResult,
    observeFlushFault,
    installExportErrorHandler,
) where

import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

import Katip (LogEnv, Severity (WarningS))
import OpenTelemetry.Exporter.Span (ExportResult (..))
import OpenTelemetry.Internal.Logging (setGlobalErrorHandler)

import Ecluse.Core.Fault.Http (displayFaultT)
import Ecluse.Runtime.Log (moduleLog)

-- | The throttle's state, exposed so a test steps the throttle without a wall clock.
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
    | -- | The window elapsed: surface a heartbeat with the count of errors since the last surfaced one, this one included.
      EmitHeartbeat Int
    | -- | Within the window: suppress and count.
      EmitSuppress
    deriving stock (Eq, Show)

-- | The initial throttle state: nothing logged, nothing suppressed.
initialThrottle :: ThrottleState
initialThrottle = ThrottleState Nothing 0

-- How long export errors are coalesced between surfaced heartbeats.
throttleInterval :: NominalDiffTime
throttleInterval = 60

-- | Advance the throttle for one export error at @now@, with a heartbeat due once @interval@ has passed since the last surfaced error.
throttleStep :: NominalDiffTime -> UTCTime -> ThrottleState -> (ThrottleState, ThrottleEmit)
throttleStep interval now st = case tsLastLogged st of
    Nothing -> (ThrottleState (Just now) 0, EmitFirst)
    Just lastLogged
        | diffUTCTime now lastLogged >= interval ->
            (ThrottleState (Just now) 0, EmitHeartbeat (tsSuppressed st + 1))
        | otherwise ->
            (st{tsSuppressed = tsSuppressed st + 1}, EmitSuppress)

-- | One throttle and one @katip@ target that every export-failure feed shares.
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

-- | The production sink: the wall clock and the composition-root 'LogEnv' as the @katip@ target.
exportFailureSink :: LogEnv -> IO ExportFailureSink
exportFailureSink logEnv = newExportFailureSink getCurrentTime (moduleLog logEnv sinkModule)

-- The @module@ field these lines carry. Operators filter on it, so it names the resolver that
-- owns the telemetry configuration rather than this module.
sinkModule :: Text
sinkModule = "Ecluse.Runtime.Telemetry.Resolve"

-- | Route a failed export from the @signal@ exporter through the sink. The exporter's request holds every collector header, so the error renders through 'displayFaultT'.
observeExportResult :: ExportFailureSink -> Text -> ExportResult -> IO ()
observeExportResult sink signal = \case
    Success -> pass
    Failure mErr -> routeExportFailure sink (signal <> " export failed" <> maybe "" ((": " <>) . displayFaultT) mErr)

-- | Route a metric series the batching flush dropped because its replay into the SDK threw.
observeFlushFault :: ExportFailureSink -> SomeException -> IO ()
observeFlushFault sink fault = routeExportFailure sink ("metric flush dropped a series: " <> displayFaultT fault)

-- | Install a process-global handler that forwards the SDK's own diagnostic text verbatim. Failed exports reach the sink separately, through 'observeExportResult'.
installExportErrorHandler :: ExportFailureSink -> IO ()
installExportErrorHandler sink = setGlobalErrorHandler (routeExportFailure sink . toText)

routeExportFailure :: ExportFailureSink -> Text -> IO ()
routeExportFailure sink diagnostic = do
    now <- sinkNow sink
    emit <- atomicModifyIORef' (sinkState sink) (throttleStep throttleInterval now)
    case emit of
        EmitFirst -> sinkSurface sink WarningS (firstErrorMessage diagnostic)
        EmitHeartbeat suppressed -> sinkSurface sink WarningS (heartbeatMessage suppressed diagnostic)
        EmitSuppress -> pass

firstErrorMessage :: Text -> Text
firstErrorMessage diagnostic =
    "telemetry export error (subsequent identical errors are throttled): " <> diagnostic

heartbeatMessage :: Int -> Text -> Text
heartbeatMessage suppressed diagnostic =
    "telemetry export still failing: "
        <> show suppressed
        <> " export errors since the last report. Latest: "
        <> diagnostic
