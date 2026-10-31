-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native traffic for the shared read telemetry and npm mirror telemetry contracts.
module Ecluse.E2E.TelemetryE2ESpec (npmReadTelemetryFamily, pypiReadTelemetryFamily, npmMirrorTelemetryFamily, collectorDriver, readDriver) where

import Data.List (lookup)
import Data.Text qualified as T

import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture, concreteFamily)
import Ecluse.E2E.Fixtures.Npm (allowPkg, mirrorPkg, psName, psVersion, telemetryDdPkg, telemetryPkg, telemetryPrivatePkg)
import Ecluse.E2E.Fixtures.PyPI (pypiProject, pypiVersion, pypiWheelFile)
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture, FamilyMetadata)
import Ecluse.Test.Contract.Phase (PhaseRecorder)
import Ecluse.Test.Contract.Telemetry
import Ecluse.Test.Poll (pollUntil)

-- | npm traffic and configuration acquisition for the canonical read telemetry scenarios.
npmReadTelemetryFamily :: ConcreteFixture FamilyMetadata -> FamilyFixture ReadTelemetryDriver
npmReadTelemetryFamily fixture = concreteFamily fixture $ \recorder action plane ->
    action (npmReadDriver recorder plane)

-- | pip traffic uses the same four required service telemetry scenarios.
pypiReadTelemetryFamily :: ConcreteFixture FamilyMetadata -> FamilyFixture ReadTelemetryDriver
pypiReadTelemetryFamily fixture = concreteFamily fixture $ \recorder action plane ->
    action (pypiReadDriver recorder plane)

-- | npm supplies worker and private-fetch observations to the common write telemetry laws.
npmMirrorTelemetryFamily :: ConcreteFixture FamilyMetadata -> FamilyFixture MirrorTelemetryFixture
npmMirrorTelemetryFamily fixture = concreteFamily fixture $ \recorder action ->
    timedFixtureWith recorder (withE2EWith (telemetryConfig EnabledTelemetry)) $ \e2e ->
        action
            MirrorTelemetryFixture
                { mirrorTelemetryTraffic = npmTraffic e2e
                , mirrorTelemetryStore = mirrorDriver e2e
                , mirrorTelemetryCollector = collectorDriver e2e
                , mirrorTelemetryPackage = psName telemetryPkg
                , mirrorTelemetryVersion = psVersion telemetryPkg
                , mirrorTelemetryPrivatePackage = psName telemetryPrivatePkg
                , mirrorTelemetryPrivateVersion = psVersion telemetryPrivatePkg
                , mirrorTelemetryHost = mirrorHost
                , mirrorTelemetryTarget = T.drop (T.length "/npm") (npmTarballPath (psName telemetryPrivatePkg) (psVersion telemetryPrivatePkg))
                , mirrorTelemetryPrivateFetch = ConventionalArtifact
                }

npmReadDriver :: PhaseRecorder -> GlobalDataPlane -> ReadTelemetryDriver
npmReadDriver recorder plane =
    readDriver recorder plane npmTraffic (psName allowPkg) (psName mirrorPkg) (psName telemetryDdPkg)

pypiReadDriver :: PhaseRecorder -> GlobalDataPlane -> ReadTelemetryDriver
pypiReadDriver recorder plane = readDriver recorder plane pypiTraffic pypiProject pypiProject pypiProject

readDriver :: PhaseRecorder -> GlobalDataPlane -> (E2E -> TrafficDriver) -> Text -> Text -> Text -> ReadTelemetryDriver
readDriver recorder plane traffic primary secondary identityPackage =
    ReadTelemetryDriver
        { withReadTelemetry = \mode action ->
            timedFixtureWith
                recorder
                (withClientProxyWith (telemetryConfig mode))
                (\e2e -> action (TelemetrySession (traffic e2e) (logDriver e2e) (collectorDriver e2e)))
                plane
        , readTelemetryPackage = primary
        , readTelemetrySecondPackage = secondary
        , readTelemetryIdentityPackage = identityPackage
        , readTelemetryIdentity = TelemetryIdentity ddTagService ddTagEnv ddTagVersion
        }

{- The disabled proxy keeps the exporter's endpoint and tuning beside a running collector, so only
the switch stands between it and an export. Debug logs carry the request-scoped lines it checks. -}
telemetryConfig :: TelemetryMode -> E2EConfig
telemetryConfig = \case
    EnabledTelemetry -> defaultE2EConfig{ecCollector = True, ecExtraEnv = otlpCollectorEnv}
    DisabledTelemetry ->
        defaultE2EConfig
            { ecCollector = True
            , ecExtraEnv =
                [("ECLUSE_OBSERVABILITY__TELEMETRY", "off"), ("ECLUSE_OBSERVABILITY__LOG_LEVEL", "debug")]
                    <> filter ((/= "ECLUSE_OBSERVABILITY__TELEMETRY") . fst) otlpCollectorEnv
            }
    FailedExporter -> defaultE2EConfig{ecExtraEnv = otlpCollectorEnv}
    IdentityTelemetry -> defaultE2EConfig{ecCollector = True, ecExtraEnv = ("ECLUSE_OBSERVABILITY__LOG_LEVEL", "debug") : datadogCollectorEnv}

npmTraffic :: E2E -> TrafficDriver
npmTraffic e2e = TrafficDriver (npmInstall e2e) (proxyStatus e2e "/readyz")

pypiTraffic :: E2E -> TrafficDriver
pypiTraffic e2e = TrafficDriver install (proxyStatus e2e "/readyz")
  where
    install name = do
        files <- advertisedFiles e2e name
        digest <- maybe (fail "telemetry index advertised no wheel digest") pure (lookup pypiWheelFile files)
        withPipProject e2e name pypiVersion digest pipInstallIn

mirrorDriver :: E2E -> MirrorTelemetryDriver
mirrorDriver e2e =
    MirrorTelemetryDriver
        { telemetryVersions = verdaccioVersions e2e
        , telemetryAwaitVersion = \name version -> verdaccioAwaitVersions e2e name [version]
        }

logDriver :: E2E -> LogDriver
logDriver e2e = LogDriver $ \matches attempts -> pollUntil attempts 250000 matches (proxyContainerLogs e2e)

-- | Observe actual collector output using the common bounded polling contract.
collectorDriver :: E2E -> CollectorDriver
collectorDriver e2e = CollectorDriver $ \matches attempts -> do
    name <- maybe (fail "telemetry profile requires a collector") pure (e2eCollectorContainer e2e)
    pollUntil attempts 250000 matches (containerLogs name)
