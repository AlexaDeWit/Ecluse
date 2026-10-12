-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Service telemetry laws reject incomplete export and readiness observations.
module Ecluse.Test.Contract.TelemetrySpec (spec) where

import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec

import Ecluse.Test.Contract.Install (ClientResult (..))
import Ecluse.Test.Contract.Telemetry

spec :: Spec
spec = describe "read telemetry evidence" $ do
    it "requires both a service metric and an exported span" $
        exportsSignals "package" traffic (collector "ecluse.serve.decision Span #1")
    it "rejects a metric-only collector" $
        exportsSignals "package" traffic (collector "ecluse.serve.decision") `shouldThrow` anyException
    it "rejects a trace-only collector" $
        exportsSignals "package" traffic (collector "Span #1") `shouldThrow` anyException
    it "rejects missing logs when telemetry is disabled" $
        logsWithoutCollector "package" traffic (logs "") `shouldThrow` anyException
    it "requires readiness after exporter failure" $
        servesAfterExportFailure
            "first"
            "second"
            traffic{telemetryReadiness = pure 503}
            (logs "\"message\":\"telemetry export error\"")
            `shouldThrow` anyException
    it "requires another fresh client after exporter failure" $ do
        requests <- newIORef []
        let observed = traffic{telemetryInstall = \package -> modifyIORef' requests (<> [package]) >> pure success}
        servesAfterExportFailure "first" "second" observed (logs "\"message\":\"telemetry export error\"")
        readIORef requests `shouldReturn` ["first", "second"]
  where
    success = ClientResult "client" ExitSuccess "" ""
    traffic = TrafficDriver (const (pure success)) (pure 200)
    collector output = CollectorDriver (\_ _ -> pure output)
    logs output = LogDriver (\_ _ -> pure output)
