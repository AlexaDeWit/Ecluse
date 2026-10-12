-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | npm's concrete operations for every required ecosystem family.
module Ecluse.E2E.Drivers.Npm (npmDriver) where

import Data.Aeson (Value)

import Ecluse.E2E.AdvisoriesE2ESpec (npmAdvisoryFamily)
import Ecluse.E2E.Fixtures.Contracts
import Ecluse.E2E.Harness (E2E, GlobalDataPlane, NpmProject, Resolved)
import Ecluse.E2E.InstallationE2ESpec (installationFamily, npmInstallationDriver)
import Ecluse.E2E.Npm.DredgerE2ESpec (npmSweepDriver)
import Ecluse.E2E.Npm.InstallE2ESpec (nativeInstallationContract)
import Ecluse.E2E.Npm.MirrorInstallE2ESpec (npmMirrorFamily)
import Ecluse.E2E.Npm.PublishE2ESpec (npmPublicationFamily)
import Ecluse.E2E.TelemetryE2ESpec (npmMirrorTelemetryFamily, npmReadTelemetryFamily)
import Ecluse.Test.Contract.Ecosystem

-- | Lower-tier serving evidence and real-client operations form one completion definition.
npmDriver :: EcosystemDriver (Maybe Text) (Set Resolved) Value GlobalDataPlane E2E NpmProject
npmDriver =
    EcosystemDriver
        { ecosystemName = "npm"
        , ecosystemProxying = Available (ServingEvidence "integration/npm-metadata")
        , ecosystemInstallation = Available (installationFamily npmInstallationFixture npmInstallationDriver)
        , ecosystemPolicy = Available (ServingEvidence "integration/npm-metadata")
        , ecosystemAdvisories = Available (npmAdvisoryFamily npmPilotFixture)
        , ecosystemMirroring = Available (npmMirrorFamily npmMirrorFixture)
        , ecosystemPublication = Available (npmPublicationFamily npmPublicationFixture)
        , ecosystemDredger = Available (fixtureMetadata npmDredgerFixture, npmSweepDriver npmDredgerFixture)
        , ecosystemTelemetry = Available (npmReadTelemetryFamily npmTelemetryFixture, Available (npmMirrorTelemetryFamily npmMirrorTelemetryFixture))
        , ecosystemNativeExtensions = [nativeInstallationContract npmRegistryFixture]
        }
