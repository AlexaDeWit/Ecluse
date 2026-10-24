-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cleanup when provider acquisition fails after a meter starts.
module Ecluse.Runtime.Telemetry.InternalSpec (spec) where

import Control.Exception qualified as Exception
import OpenTelemetry.Exporter.Metric (MetricExporter (..))
import OpenTelemetry.Exporter.Span (ExportResult (Success))
import OpenTelemetry.Metric (FlushResult (FlushSuccess), MeterProvider (meterProviderShutdown), ShutdownResult (ShutdownSuccess), createMeterProvider, defaultPeriodicMetricReaderOptions, defaultSdkMeterProviderOptions, forkPeriodicMetricReader, getGlobalMeterProvider, setGlobalMeterProvider, shutdownMeterProvider)
import OpenTelemetry.Propagator (Propagator (propagatorFields), getGlobalTextMapPropagator, setGlobalTextMapPropagator)
import OpenTelemetry.Resource (emptyMaterializedResources)
import OpenTelemetry.Trace (getGlobalTracerProvider, setGlobalTracerProvider)
import System.Timeout qualified as Timeout
import Test.Hspec
import UnliftIO.Exception (bracket, finally, throwIO, tryAny)

import Ecluse.Runtime.Telemetry.Batched (batchMeterProvider)
import Ecluse.Runtime.Telemetry.Internal (initializeMeterWorkers, initializeObservedOpenTelemetryWith)
import Ecluse.Test.Env (withEnvVars)
import Ecluse.Test.Log (newTestLogEnv)

spec :: Spec
spec = describe "provider acquisition" $ do
    it "shuts down an acquired batched meter and restores globals after logger failure" $
        bracket getGlobalTextMapPropagator setGlobalTextMapPropagator $ \_ -> do
            setGlobalTextMapPropagator mempty{propagatorFields = ["ecluse-test-propagator"]}
            bracket ((,,) <$> getGlobalTracerProvider <*> getGlobalMeterProvider <*> getGlobalTextMapPropagator) (\(tracer, meter, propagator) -> setGlobalTracerProvider tracer >> setGlobalMeterProvider meter >> setGlobalTextMapPropagator propagator) $ \_ ->
                withEnvVars ["OTEL_SDK_DISABLED"] [("OTEL_TRACES_EXPORTER", "none"), ("OTEL_PROPAGATORS", "none")] $ do
                    shutdowns <- newIORef (0 :: Int)
                    let acquire = do
                            (sdk, _) <- createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions
                            (meter, _) <- batchMeterProvider throwIO sdk
                            let observed = meter{meterProviderShutdown = \timeout -> modifyIORef' shutdowns (+ 1) >> meterProviderShutdown meter timeout}
                            pure (observed, Nothing)
                    result <- tryAny (initializeObservedOpenTelemetryWith acquire (throwIO LoggerSetupFailed))
                    either fromException (const Nothing) result `shouldBe` Just LoggerSetupFailed
                    readIORef shutdowns `shouldReturn` 1
            propagatorFields <$> getGlobalTextMapPropagator `shouldReturn` ["ecluse-test-propagator"]

    for_ [(False, "stops the real reader, flusher and SDK when final setup fails"), (True, "keeps the setup failure and cancels the flusher when exporter shutdown throws")] $ \(stopFails, label) ->
        it label $ do
            logEnv <- newTestLogEnv
            started <- newEmptyMVar
            cancelled <- newEmptyMVar
            blocked <- newEmptyMVar
            exporterStops <- newIORef (0 :: Int)
            sdkStops <- newIORef (0 :: Int)
            let exporter =
                    MetricExporter
                        { metricExporterExport = const (pure Success)
                        , metricExporterForceFlush = pure FlushSuccess
                        , metricExporterShutdown = modifyIORef' exporterStops (+ 1) >> if stopFails then throwIO ExporterStopFailed else pure ShutdownSuccess
                        }
                flushStep = (putMVar started () >> takeMVar blocked) `finally` putMVar cancelled ()
                release (sdk, _) = modifyIORef' sdkStops (+ 1) >> void (shutdownMeterProvider sdk Nothing)
            result <- tryAny $
                Exception.bracketOnError (createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions) release $ \(sdk, meterEnv) -> do
                    void (batchMeterProvider throwIO sdk)
                    initializeMeterWorkers logEnv flushStep (forkPeriodicMetricReader meterEnv exporter defaultPeriodicMetricReaderOptions) $ \_ -> do
                        Timeout.timeout 5_000_000 (takeMVar started) `shouldReturn` Just ()
                        throwIO LoggerSetupFailed
            either fromException (const Nothing) result `shouldBe` Just LoggerSetupFailed
            readIORef exporterStops `shouldReturn` 1
            readIORef sdkStops `shouldReturn` 1
            Timeout.timeout 5_000_000 (takeMVar cancelled) `shouldReturn` Just ()

data LoggerSetupFailed = LoggerSetupFailed
    deriving stock (Eq, Show)

instance Exception LoggerSetupFailed

data ExporterStopFailed = ExporterStopFailed
    deriving stock (Show)

instance Exception ExporterStopFailed
