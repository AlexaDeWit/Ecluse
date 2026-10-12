-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Concrete PyPI operations for every required ecosystem family.
module Ecluse.E2E.Drivers.PyPI (pypiDriver) where

import Data.Aeson (Value)
import Ecluse.E2E.AdvisoriesE2ESpec (pypiAdvisoryFamily)
import Ecluse.E2E.Fixtures.Contracts
import Ecluse.E2E.Harness.Pip (PythonGraph)
import Ecluse.E2E.Harness.PyPIDredger (PyPISweepPlane, PyPISweepStore)
import Ecluse.E2E.Harness.Types (PipProject)
import Ecluse.E2E.InstallationE2ESpec (installationFamily, pypiInstallationDriver)
import Ecluse.E2E.PyPI.AdvisoriesE2ESpec (nativeAdvisoryContract)
import Ecluse.E2E.PyPI.DredgerE2ESpec (pypiDredgerFixture, pypiSweepDriver)
import Ecluse.E2E.PyPI.InstallE2ESpec (nativeInstallationContract)
import Ecluse.E2E.PyPI.MirrorInstallE2ESpec (nativeMirrorContract, pypiMirrorFamily)
import Ecluse.E2E.PyPI.PublishE2ESpec (nativePublicationContract, pypiPublicationFamily)
import Ecluse.E2E.PyPI.TelemetryE2ESpec (pypiMirrorTelemetryFamily)
import Ecluse.E2E.TelemetryE2ESpec (pypiReadTelemetryFamily)
import Ecluse.Test.Contract.Ecosystem

-- | Native read, write, install, maintenance and telemetry providers share the full ecosystem laws.
pypiDriver :: EcosystemDriver PythonGraph Value PyPISweepPlane PyPISweepStore PipProject
pypiDriver =
    EcosystemDriver
        { ecosystemName = "pypi"
        , ecosystemProxying = Available (ServingEvidence "integration/pypi")
        , ecosystemInstallation = Available (installationFamily pypiInstallationFixture pypiInstallationDriver)
        , ecosystemPolicy = Available (ServingEvidence "integration/pypi")
        , ecosystemAdvisories = Available (pypiAdvisoryFamily pypiPilotFixture)
        , ecosystemMirroring = Available (pypiMirrorFamily pypiMirrorFixture)
        , ecosystemPublication = Available (pypiPublicationFamily pypiPublicationFixture)
        , ecosystemDredger = Available (fixtureGroup pypiDredgerFixture, pypiSweepDriver pypiDredgerFixture)
        , ecosystemTelemetry =
            Available
                ( pypiReadTelemetryFamily pypiTelemetryFixture
                , Available (pypiMirrorTelemetryFamily pypiMirrorTelemetryFixture)
                )
        , ecosystemNativeExtensions = [nativeInstallationContract pypiPublicFixture, nativeAdvisoryContract pypiAdvisoryNativeFixture, nativeMirrorContract pypiMirrorNativeFixture, nativePublicationContract pypiPublicationNativeFixture]
        }
