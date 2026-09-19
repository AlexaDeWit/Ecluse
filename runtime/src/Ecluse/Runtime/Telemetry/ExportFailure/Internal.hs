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
    installExportErrorHandler,
) where

import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

import Katip (LogEnv, Severity (WarningS))
import OpenTelemetry.Exporter.Span (ExportResult (..))
import OpenTelemetry.Internal.Logging (setGlobalErrorHandler)

import Ecluse.Runtime.Log (moduleLog)

{- | The throttle state for SDK export-error routing. Exposed so a test asserts the throttle
decision without wall-clock timing.
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

-- How long export errors are coalesced between surfaced heartbeats.
throttleInterval :: NominalDiffTime
throttleInterval = 60

{- | Advance the throttle for one export error at @now@: the first surfaces, a heartbeat once
@interval@ has elapsed since the last surfaced error, and anything between is suppressed and counted.
-}
throttleStep :: NominalDiffTime -> UTCTime -> ThrottleState -> (ThrottleState, ThrottleEmit)
throttleStep interval now st = case tsLastLogged st of
    Nothing -> (ThrottleState (Just now) 0, EmitFirst)
    Just lastLogged
        | diffUTCTime now lastLogged >= interval ->
            (ThrottleState (Just now) 0, EmitHeartbeat (tsSuppressed st + 1))
        | otherwise ->
            (st{tsSuppressed = tsSuppressed st + 1}, EmitSuppress)

{- | One throttle and one @katip@ target shared by every export-failure feed. The clock and the
surfacing action are injected, so a test asserts the throttle decision without wall-clock timing.
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

-- | The production sink: the wall clock and the composition-root 'LogEnv' as the @katip@ target.
exportFailureSink :: LogEnv -> IO ExportFailureSink
exportFailureSink logEnv = newExportFailureSink getCurrentTime (moduleLog logEnv sinkModule)

-- The @module@ field these lines carry. Operators filter on it, so it names the resolver that
-- owns the telemetry configuration rather than this module.
sinkModule :: Text
sinkModule = "Ecluse.Runtime.Telemetry.Resolve"

{- Route one export-failure diagnostic through the shared throttle into @katip@. The first
error surfaces plainly and later ones fold into a heartbeat carrying the suppressed count. -}
routeExportFailure :: ExportFailureSink -> Text -> IO ()
routeExportFailure sink diagnostic = do
    now <- sinkNow sink
    emit <- atomicModifyIORef' (sinkState sink) (throttleStep throttleInterval now)
    case emit of
        EmitFirst -> sinkSurface sink WarningS (firstErrorMessage diagnostic)
        EmitHeartbeat suppressed -> sinkSurface sink WarningS (heartbeatMessage suppressed diagnostic)
        EmitSuppress -> pass

{- | Observe one exporter's 'ExportResult', routing a 'Failure' through the sink. @signal@ names the
exporter (@span@ \/ @metric@). @hs-opentelemetry 1.0.0.0@ drops a failed OTLP export, so only this feed reports one.
-}
observeExportResult :: ExportFailureSink -> Text -> ExportResult -> IO ()
observeExportResult sink signal = \case
    Success -> pass
    Failure mErr -> routeExportFailure sink (signal <> " export failed" <> maybe "" ((": " <>) . show) mErr)

{- | Install a process-global handler for the SDK's own diagnostic stream, forwarded verbatim. Ecluse
reads none of @OTEL_EXPORTER_OTLP_HEADERS@, @DD_API_KEY@, @DD_SITE@, so the SDK's own text is the only leak channel.
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
