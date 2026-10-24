-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cleanup when provider acquisition fails after a meter starts.
module Ecluse.Runtime.Telemetry.InternalSpec (spec) where

import OpenTelemetry.Metric (MeterProvider (meterProviderShutdown), createMeterProvider, defaultSdkMeterProviderOptions, getGlobalMeterProvider, setGlobalMeterProvider)
import OpenTelemetry.Resource (emptyMaterializedResources)
import OpenTelemetry.Trace (getGlobalTracerProvider, setGlobalTracerProvider)
import Test.Hspec
import UnliftIO.Exception (bracket, throwIO, tryAny)

import Ecluse.Runtime.Telemetry.Batched (batchMeterProvider)
import Ecluse.Runtime.Telemetry.Internal (initializeObservedOpenTelemetryWith)
import Ecluse.Test.Env (withEnvVars)

spec :: Spec
spec = describe "provider acquisition" $ do
    it "shuts down an acquired batched meter when logger initialisation fails" $
        bracket ((,) <$> getGlobalTracerProvider <*> getGlobalMeterProvider) (\(tracer, meter) -> setGlobalTracerProvider tracer >> setGlobalMeterProvider meter) $ \_ ->
            withEnvVars ["OTEL_SDK_DISABLED"] [("OTEL_TRACES_EXPORTER", "none")] $ do
                shutdowns <- newIORef (0 :: Int)
                let acquire = do
                        (sdk, _) <- createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions
                        (meter, _) <- batchMeterProvider throwIO sdk
                        let observed = meter{meterProviderShutdown = \timeout -> modifyIORef' shutdowns (+ 1) >> meterProviderShutdown meter timeout}
                        pure (observed, Nothing)
                result <- tryAny (initializeObservedOpenTelemetryWith acquire (throwIO LoggerSetupFailed))
                either fromException (const Nothing) result `shouldBe` Just LoggerSetupFailed
                readIORef shutdowns `shouldReturn` 1

data LoggerSetupFailed = LoggerSetupFailed
    deriving stock (Eq, Show)

instance Exception LoggerSetupFailed
