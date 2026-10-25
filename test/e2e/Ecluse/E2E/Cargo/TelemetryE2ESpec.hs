-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo's native clients supply the common read and mirror telemetry observations.
module Ecluse.E2E.Cargo.TelemetryE2ESpec (cargoReadTelemetryFamily, cargoMirrorTelemetryFamily) where

import Data.Text qualified as T

import Ecluse.E2E.Cargo.InstallE2ESpec (cargoInstallInto)
import Ecluse.E2E.Cargo.MirrorInstallE2ESpec (cargoMirrorConfig, withCargoMirrorFixture)
import Ecluse.E2E.Fixtures.Cargo
import Ecluse.E2E.Fixtures.Contracts (concreteFamily)
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Cargo (currentCargo)
import Ecluse.E2E.Harness.CargoRegistry
import Ecluse.E2E.TelemetryE2ESpec (collectorDriver, readDriver)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture)
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Telemetry
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Registry.Cargo.Fixtures (cargoVersion)

-- | Read telemetry retains enabled, disabled, failed-exporter and identity configurations.
cargoReadTelemetryFamily :: FamilyFixture ReadTelemetryDriver
cargoReadTelemetryFamily = concreteFamily (cargoReadFixture "telemetry" ["read", "telemetry", "client-install"]) $ \recorder action plane ->
    action (readDriver recorder plane nativeTraffic "ecluse-e2e-build" "ecluse-e2e-unix" "ecluse-e2e-macro")

-- | Mirror telemetry uses native target observations, never a fabricated completion callback.
cargoMirrorTelemetryFamily :: FamilyFixture MirrorTelemetryFixture
cargoMirrorTelemetryFamily = concreteFamily (cargoWriteFixture "telemetry-write" ["read", "mirror", "client-install", "telemetry"]) $ \recorder action plane ->
    timedFixture recorder (withCargoMirrorFixture plane . curry) $ \(registry, _) -> do
        queue <- sharedMirrorQueue plane
        let config = cargoMirrorConfig{ecCollector = True, ecQueueUrl = Just queue, ecExtraEnv = ecExtraEnv cargoMirrorConfig <> otlpCollectorEnv}
        withE2EWith
            config
            ( \proxy ->
                action
                    MirrorTelemetryFixture
                        { mirrorTelemetryTraffic = nativeTraffic proxy
                        , mirrorTelemetryStore =
                            MirrorTelemetryDriver
                                (cargoRegistryVersions registry "cargo-mirror")
                                (\name version -> pollUntil 120 500000 (elem version) (cargoRegistryVersions registry "cargo-mirror" name))
                        , mirrorTelemetryCollector = collectorDriver proxy
                        , mirrorTelemetryPackage = "ecluse-e2e-build"
                        , mirrorTelemetryVersion = cargoVersion
                        , mirrorTelemetryPrivatePackage = "ecluse-e2e-macro"
                        , mirrorTelemetryPrivateVersion = cargoVersion
                        , mirrorTelemetryHost = "cargo-mirror"
                        , mirrorTelemetryTarget = T.drop (T.length "/cargo") (cargoArchiveRoute "ecluse-e2e-macro" cargoVersion)
                        }
            )
            plane

nativeTraffic :: E2E -> TrafficDriver
nativeTraffic proxy = TrafficDriver (\name -> cargoInstallInto proxy currentCargo name cargoVersion) (proxyStatus proxy "/readyz")
