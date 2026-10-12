-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Actual worker and private-fetch telemetry for the complete PyPI ecosystem contract.
module Ecluse.E2E.PyPI.TelemetryE2ESpec (pypiMirrorTelemetryFamily) where

import Data.Text qualified as T
import Test.Hspec (shouldReturn)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture, concreteFamily)
import Ecluse.E2E.Fixtures.PyPI.Mirror
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.PyPIMirror
import Ecluse.E2E.PyPI.MirrorEvidence (canonicalVersion)
import Ecluse.E2E.TelemetryE2ESpec (collectorDriver)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture)
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Telemetry

-- | Bind two independent projects to the shared domain-span and private-miss-to-hit laws.
pypiMirrorTelemetryFamily :: ConcreteFixture -> FamilyFixture MirrorTelemetryFixture
pypiMirrorTelemetryFamily fixture = concreteFamily fixture $ \recorder action plane ->
    timedFixture recorder (withMirrorProjects PreserveUploadFields [domainProject, privateProject] plane . curry) $ \(target, distributions) -> do
        ((_, platform), _) <- either (fail . toString) pure mirrorArchitectures
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
                    , mirrorTelemetryVersion = canonicalVersion
                    , mirrorTelemetryPrivatePackage = privateProject
                    , mirrorTelemetryPrivateVersion = canonicalVersion
                    , mirrorTelemetryHost = "pypi-mirror"
                    , mirrorTelemetryTarget = "/simple/" <> privateProject <> "/" <> filename
                    }

nativeTelemetryStore :: E2E -> NativeTarget -> [Distribution] -> MirrorTelemetryDriver
nativeTelemetryStore proxy target distributions =
    MirrorTelemetryDriver
        { telemetryVersions = fmap canonical . nativeVersions target distributions
        , telemetryAwaitVersion = \project version -> do
            awaitProjectFiles target distributions project `shouldReturn` True
            awaitProxyLog proxy (T.isInfixOf ("mirror version complete: " <> project <> "@" <> version)) 120 `shouldReturn` True
            canonical <$> nativeVersions target distributions project
        }
  where
    canonical = map (renderVersion . mkVersion PyPI)

domainProject :: Text
domainProject = "e2e-telemetry-domain"

privateProject :: Text
privateProject = "e2e-telemetry-private"
