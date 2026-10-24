-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Provider ownership, worker shutdown, and cleanup after failed acquisition.
module Ecluse.Runtime.Telemetry.InternalSpec (spec) where

import Control.Exception qualified as Exception
import OpenTelemetry.Attributes (emptyAttributes)
import OpenTelemetry.Exporter.InMemory.Metric (inMemoryMetricExporter)
import OpenTelemetry.Exporter.Metric (MetricExporter (..))
import OpenTelemetry.Exporter.Span (ExportResult (Success))
import OpenTelemetry.Log.Core (LoggerProvider (loggerProviderIsShutdown), createLoggerProvider, emptyLoggerProviderOptions)
import OpenTelemetry.Metric (FlushResult (FlushError, FlushSuccess), MeterProvider (meterProviderShutdown), ShutdownResult (ShutdownSuccess), createMeterProvider, defaultPeriodicMetricReaderOptions, defaultSdkMeterProviderOptions, forkPeriodicMetricReader, getGlobalMeterProvider, getMeter, setGlobalMeterProvider, shutdownMeterProvider)
import OpenTelemetry.Metric.Core (Counter (counterAdd), defaultAdvisoryParameters, meterCreateCounterInt64)
import OpenTelemetry.Propagator (Propagator (propagatorFields), getGlobalTextMapPropagator, setGlobalTextMapPropagator)
import OpenTelemetry.Resource (emptyMaterializedResources)
import OpenTelemetry.SDK (OTelSignals (otelLoggerProvider, otelShutdown, otelTracerProvider))
import OpenTelemetry.Trace (forceFlushTracerProvider, getGlobalTracerProvider, setGlobalTracerProvider)
import System.Timeout qualified as Timeout
import Test.Hspec
import UnliftIO.Exception (bracket, finally, throwIO, tryAny)
import UnliftIO.MVar (isEmptyMVar)

import Ecluse.Runtime.Telemetry.Batched (batchMeterProvider)
import Ecluse.Runtime.Telemetry.Internal (initializeMeterWorkers, initializeObservedOpenTelemetryWith)
import Ecluse.Runtime.Test.Telemetry (sumPointsIn)
import Ecluse.Test.Env (withEnvVars)
import Ecluse.Test.Log (newTestLogEnv)

spec :: Spec
spec = describe "provider acquisition" $ do
    it "shuts down an acquired batched meter and restores globals after logger failure" $
        bracket getGlobalTextMapPropagator setGlobalTextMapPropagator $ \_ -> do
            setGlobalTextMapPropagator mempty{propagatorFields = ["ecluse-test-propagator"]}
            withIsolatedProviders $ do
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

    it "keeps workers live until stop and exports pending records before SDK shutdown" $ do
        logEnv <- newTestLogEnv
        started <- newEmptyMVar
        cancelled <- newEmptyMVar
        blocked <- newEmptyMVar
        exporterStops <- newIORef (0 :: Int)
        sdkStops <- newIORef (0 :: Int)
        releases <- newIORef ([] :: [Text])
        (inner, exported) <- inMemoryMetricExporter
        let exporter = inner{metricExporterShutdown = modifyIORef' exporterStops (+ 1) >> modifyIORef' releases (<> ["reader"]) >> metricExporterShutdown inner}
            flushStep = (putMVar started () >> takeMVar blocked) `finally` (modifyIORef' releases (<> ["flusher"]) >> putMVar cancelled ())
            release (sdk, _) = modifyIORef' sdkStops (+ 1) >> modifyIORef' releases (<> ["sdk"]) >> void (shutdownMeterProvider sdk Nothing)
        bracket (createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions) release $ \(sdk, meterEnv) -> do
            (provider, _) <- batchMeterProvider throwIO sdk
            meter <- getMeter provider "lifecycle-test"
            requests <- meterCreateCounterInt64 meter "lifecycle.requests" Nothing Nothing defaultAdvisoryParameters
            let acquire = initializeMeterWorkers logEnv flushStep (forkPeriodicMetricReader meterEnv exporter defaultPeriodicMetricReaderOptions) $ \stop -> do
                    Timeout.timeout 5_000_000 (takeMVar started) `shouldReturn` Just ()
                    pure stop
            bracket acquire id $ \_ -> do
                readIORef exporterStops `shouldReturn` 0
                readIORef sdkStops `shouldReturn` 0
                isEmptyMVar cancelled `shouldReturn` True
                counterAdd requests 7 emptyAttributes
        readIORef exporterStops `shouldReturn` 1
        readIORef sdkStops `shouldReturn` 1
        Timeout.timeout 5_000_000 (takeMVar cancelled) `shouldReturn` Just ()
        readIORef releases `shouldReturn` ["reader", "flusher", "sdk"]
        finalExport <- take 1 . reverse <$> readIORef exported
        sumPointsIn "lifecycle.requests" finalExport `shouldBe` [(emptyAttributes, 7)]

    for_ [(False, "shuts down successfully acquired providers"), (True, "continues to logger shutdown when meter shutdown throws")] $ \(meterFails, label) ->
        it label $ withIsolatedProviders $ do
            shutdowns <- newIORef (0 :: Int)
            let acquire = do
                    (sdk, _) <- createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions
                    let observed =
                            sdk
                                { meterProviderShutdown = \timeout -> do
                                    modifyIORef' shutdowns (+ 1)
                                    result <- meterProviderShutdown sdk timeout
                                    when meterFails (throwIO ExporterStopFailed)
                                    pure result
                                }
                    pure (observed, Nothing)
            (signals, _) <- initializeObservedOpenTelemetryWith acquire (createLoggerProvider [] emptyLoggerProviderOptions)
            let flushTracer = forceFlushTracerProvider (otelTracerProvider signals) Nothing
                loggerStopped = readIORef (loggerProviderIsShutdown (otelLoggerProvider signals))
            flushTracer `shouldReturn` FlushSuccess
            loggerStopped `shouldReturn` False
            readIORef shutdowns `shouldReturn` 0
            otelShutdown signals
            flushTracer `shouldReturn` FlushError
            loggerStopped `shouldReturn` True
            readIORef shutdowns `shouldReturn` 1

withIsolatedProviders :: IO a -> IO a
withIsolatedProviders use =
    bracket ((,,) <$> getGlobalTracerProvider <*> getGlobalMeterProvider <*> getGlobalTextMapPropagator) (\(tracer, meter, propagator) -> setGlobalTracerProvider tracer >> setGlobalMeterProvider meter >> setGlobalTextMapPropagator propagator) $ \_ ->
        withEnvVars ["OTEL_SDK_DISABLED"] [("OTEL_TRACES_EXPORTER", "none"), ("OTEL_PROPAGATORS", "none")] use

data LoggerSetupFailed = LoggerSetupFailed
    deriving stock (Eq, Show)

instance Exception LoggerSetupFailed

data ExporterStopFailed = ExporterStopFailed
    deriving stock (Show)

instance Exception ExporterStopFailed
