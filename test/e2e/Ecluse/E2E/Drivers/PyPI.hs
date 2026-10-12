-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | PyPI read operations and explicit production write absences.
module Ecluse.E2E.Drivers.PyPI (pypiDriver) where

import Ecluse.E2E.AdvisoriesE2ESpec (pypiAdvisoryFamily)
import Ecluse.E2E.Fixtures.Contracts
import Ecluse.E2E.InstallationE2ESpec (installationFamily, pypiInstallationDriver)
import Ecluse.E2E.PyPI.AdvisoriesE2ESpec (nativeAdvisoryContract)
import Ecluse.E2E.PyPI.InstallE2ESpec (nativeInstallationContract)
import Ecluse.E2E.TelemetryE2ESpec (pypiReadTelemetryFamily)
import Ecluse.Test.Contract.Ecosystem

-- | Full parity retains each missing write law. The foundation explicitly selects read families.
pypiDriver :: EcosystemDriver () () () () ()
pypiDriver =
    EcosystemDriver
        { ecosystemName = "pypi"
        , ecosystemProxying = Available (ServingEvidence "integration/pypi")
        , ecosystemInstallation = Available (installationFamily pypiInstallationFixture pypiInstallationDriver)
        , ecosystemPolicy = Available (ServingEvidence "integration/pypi")
        , ecosystemAdvisories = Available (pypiAdvisoryFamily pypiPilotFixture)
        , ecosystemMirroring = Unavailable (MissingCapability "the PyPI adapter has no publication operation for the mirror worker")
        , ecosystemPublication = Unavailable (MissingCapability "the PyPI adapter has no native upload operation")
        , ecosystemDredger = Unavailable (MissingCapability "boot refuses a PyPI mirror target before the Dredger role can start")
        , ecosystemTelemetry =
            Available
                ( pypiReadTelemetryFamily pypiTelemetryFixture
                , Unavailable (MissingCapability "the PyPI mirror worker cannot publish a private copy for worker and private-fetch telemetry")
                )
        , ecosystemNativeExtensions = [nativeInstallationContract pypiPublicFixture, nativeAdvisoryContract pypiAdvisoryNativeFixture]
        }
