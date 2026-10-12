-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | PyPI read operations and explicit gaps in the required write drivers.
module Ecluse.E2E.Drivers.PyPI (pypiDriver) where

import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterPublish))
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
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
        , ecosystemMirroring = Unavailable (writeGap "mirroring")
        , ecosystemPublication = Unavailable (writeGap "publication")
        , ecosystemDredger = Unavailable (writeGap "Dredger")
        , ecosystemTelemetry =
            Available
                ( pypiReadTelemetryFamily pypiTelemetryFixture
                , Unavailable (writeGap "mirror telemetry")
                )
        , ecosystemNativeExtensions = [nativeInstallationContract pypiPublicFixture, nativeAdvisoryContract pypiAdvisoryNativeFixture]
        }

writeGap :: Text -> MissingCapability
writeGap family = MissingCapability $ case adapterPublish pypiAdapter of
    Nothing -> "the PyPI adapter has no publisher required by the " <> family <> " conformance driver"
    Just _ -> "the PyPI adapter advertises a publisher, but the " <> family <> " conformance driver is not bound"
