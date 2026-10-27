-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Concrete native Cargo providers satisfy every full ecosystem contract family.
module Ecluse.E2E.Drivers.Cargo (cargoDriver) where

import Ecluse.E2E.Cargo.AdvisoriesE2ESpec (cargoAdvisoryFamily)
import Ecluse.E2E.Cargo.DredgerE2ESpec (cargoDredgerFixture, cargoDredgerSafetyContract, cargoSweepDriver)
import Ecluse.E2E.Cargo.InstallE2ESpec (cargoInstallationFamily)
import Ecluse.E2E.Cargo.MirrorInstallE2ESpec (CargoCoordinates, cargoMirrorFamily)
import Ecluse.E2E.Cargo.MirrorProofE2ESpec (cargoMirrorProofContract)
import Ecluse.E2E.Cargo.NativeE2ESpec (cargoNativeContract)
import Ecluse.E2E.Cargo.PublishE2ESpec (cargoPublicationFamily)
import Ecluse.E2E.Cargo.TelemetryE2ESpec (cargoMirrorTelemetryFamily, cargoReadTelemetryFamily)
import Ecluse.E2E.Fixtures.Cargo (cargoNativeFixture)
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (fixtureMetadata))
import Ecluse.E2E.Harness.Cargo (CargoProject)
import Ecluse.E2E.Harness.CargoDredger (CargoSweepPlane, CargoSweepStore)
import Ecluse.Test.Contract.Ecosystem

-- | Native clients, writes, maintenance and telemetry share the same completion laws as npm and PyPI.
cargoDriver :: EcosystemDriver Bool CargoCoordinates Text CargoSweepPlane CargoSweepStore CargoProject
cargoDriver =
    EcosystemDriver
        { ecosystemName = "cargo"
        , ecosystemProxying = Available (ServingEvidence "integration/cargo")
        , ecosystemInstallation = Available cargoInstallationFamily
        , ecosystemPolicy = Available (ServingEvidence "integration/cargo")
        , ecosystemAdvisories = Available cargoAdvisoryFamily
        , ecosystemMirroring = Available cargoMirrorFamily
        , ecosystemPublication = Available cargoPublicationFamily
        , ecosystemDredger = Available (fixtureMetadata cargoDredgerFixture, cargoSweepDriver cargoDredgerFixture)
        , ecosystemTelemetry = Available (cargoReadTelemetryFamily, Available cargoMirrorTelemetryFamily)
        , ecosystemNativeExtensions = [cargoNativeContract cargoNativeFixture, cargoMirrorProofContract] <> cargoDredgerSafetyContract
        }
