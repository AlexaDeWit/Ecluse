-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Actual worker and private-fetch telemetry for the complete PyPI ecosystem contract.
module Ecluse.E2E.PyPI.TelemetryE2ESpec (pypiMirrorTelemetryFamily) where

import Test.Hspec (shouldReturn)

import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture, concreteFamily)
import Ecluse.E2E.Fixtures.PyPI.Mirror
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.PyPIMirror
import Ecluse.E2E.PyPI.MirrorEvidence (canonicalVersion, mirrorCompleted)
import Ecluse.E2E.TelemetryE2ESpec (collectorDriver)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture, FamilyMetadata)
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Telemetry

-- | Bind two independent projects to the shared domain-span and private-miss-to-hit laws.
pypiMirrorTelemetryFamily :: ConcreteFixture FamilyMetadata -> FamilyFixture MirrorTelemetryFixture
pypiMirrorTelemetryFamily fixture = concreteFamily fixture $ \recorder action plane ->
    timedFixture recorder (withMirrorProjects PreserveUploadFields [domainProject, privateProject] plane . curry) $ \(target, distributions) -> do
        ((_, platform), _) <- either (fail . toString) pure mirrorArchitectures
        version <- canonicalVersion mirrorVersion
        let filename = mirrorWheelFile privateProject platform
            config = nativeMirrorConfig{ecCollector = True, ecExtraEnv = ecExtraEnv nativeMirrorConfig <> otlpCollectorEnv}
        advertiseMissingFile target filename
        timedFixture recorder (\use -> withE2EWith config use plane) $ \proxy ->
            action
                MirrorTelemetryFixture
                    { mirrorTelemetryTraffic = TrafficDriver (\name -> fst <$> pipInstallAndInspect proxy name mirrorVersion) (proxyStatus proxy "/readyz")
                    , mirrorTelemetryStore = nativeTelemetryStore proxy target distributions
                    , mirrorTelemetryCollector = collectorDriver proxy
                    , mirrorTelemetryPackage = domainProject
                    , mirrorTelemetryVersion = version
                    , mirrorTelemetryPrivatePackage = privateProject
                    , mirrorTelemetryPrivateVersion = version
                    , mirrorTelemetryHost = "pypi-mirror"
                    , mirrorTelemetryTarget = "/simple/" <> privateProject <> "/" <> filename
                    , mirrorTelemetryPrivateFetch = ConventionalArtifact
                    }

nativeTelemetryStore :: E2E -> NativeTarget -> [Distribution] -> MirrorTelemetryDriver
nativeTelemetryStore proxy target distributions =
    MirrorTelemetryDriver
        { telemetryVersions = versions
        , telemetryAwaitVersion = \project version -> do
            awaitProjectFiles target distributions project `shouldReturn` True
            awaitProxyLog proxy (mirrorCompleted project version) 120 `shouldReturn` True
            versions project
        }
  where
    versions project = nativeVersions target distributions project >>= traverse canonicalVersion

domainProject :: Text
domainProject = "e2e-telemetry-domain"

privateProject :: Text
privateProject = "e2e-telemetry-private"
