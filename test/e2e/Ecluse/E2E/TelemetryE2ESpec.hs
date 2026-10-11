-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Product-image telemetry under OTLP, disabled, failed-exporter, and Datadog configurations.
module Ecluse.E2E.TelemetryE2ESpec (telemetryContract) where

import Data.Text qualified as T

import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Phase (PhaseRecorder, timedFixture)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Test.Hspec

import Ecluse.E2E.Fixtures.Npm (allowPkg, mirrorPkg, psName, psVersion, telemetryDdPkg, telemetryPkg, telemetryPrivatePkg)
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.Test.Contract.Telemetry
import Ecluse.Test.Poll (pollUntil)

-- | Drive the product image under each telemetry configuration and read what it exported.
telemetryContract :: ConcreteFixture -> DeclaredGroup
telemetryContract fixture = DeclaredGroup (fixtureGroup fixture) $ \recorder -> aroundAll (timedFixture recorder (withFixture fixture)) (scenarios recorder)

scenarios :: PhaseRecorder -> SpecWith GlobalDataPlane
scenarios recorder = do
    describe "telemetry with an OTLP collector" $
        aroundAllWith (timedFixtureWith recorder (withE2EWith defaultE2EConfig{ecCollector = True, ecExtraEnv = otlpCollectorEnv})) $ do
            it (caseLabel "telemetry/signals" "exports metrics and a span") $ \e2e ->
                exportsSignals (psName allowPkg) (trafficDriver e2e) (collectorDriver e2e)
            it (caseLabel "telemetry/domain-spans" "correlates rule, enqueue and worker spans") $ \e2e ->
                exportsDomainSpans
                    (psName telemetryPkg)
                    (psVersion telemetryPkg)
                    (trafficDriver e2e)
                    (mirrorDriver e2e)
                    (collectorDriver e2e)
            it (caseLabel "telemetry/private-fetch" "records the private miss and subsequent success") $ \e2e ->
                exportsPrivateFetch
                    (psName telemetryPrivatePkg)
                    (psVersion telemetryPrivatePkg)
                    mirrorHost
                    (T.drop (T.length "/npm") (npmTarballPath (psName telemetryPrivatePkg) (psVersion telemetryPrivatePkg)))
                    (trafficDriver e2e)
                    (mirrorDriver e2e)
                    (collectorDriver e2e)
    describe "telemetry disabled" $
        aroundAllWith (timedFixtureWith recorder (withE2EWith defaultE2EConfig{ecExtraEnv = [("ECLUSE_OBSERVABILITY__TELEMETRY", "off")]})) $
            it (caseLabel "telemetry/disabled" "serves and logs without a collector") $ \e2e ->
                logsWithoutCollector (psName allowPkg) (trafficDriver e2e) (logDriver e2e)
    describe "telemetry exporter unavailable" $
        aroundAllWith (timedFixtureWith recorder (withE2EWith defaultE2EConfig{ecExtraEnv = otlpCollectorEnv})) $
            it (caseLabel "telemetry/export-failure" "warns and keeps serving after export failure") $ \e2e ->
                servesAfterExportFailure (psName allowPkg) (psName mirrorPkg) (trafficDriver e2e) (logDriver e2e)
    describe "Datadog identity" $
        aroundAllWith (timedFixtureWith recorder (withE2EWith defaultE2EConfig{ecCollector = True, ecExtraEnv = datadogCollectorEnv})) $
            it (caseLabel "telemetry/datadog" "exports the configured identity and log correlation") $ \e2e ->
                exportsIdentity
                    (psName telemetryDdPkg)
                    (TelemetryIdentity ddTagService ddTagEnv ddTagVersion)
                    (trafficDriver e2e)
                    (logDriver e2e)
                    (collectorDriver e2e)

trafficDriver :: E2E -> TrafficDriver
trafficDriver e2e = TrafficDriver (npmInstall e2e) (proxyStatus e2e "/readyz")

mirrorDriver :: E2E -> MirrorTelemetryDriver
mirrorDriver e2e =
    MirrorTelemetryDriver
        { telemetryVersions = verdaccioVersions e2e
        , telemetryAwaitVersion = \name version -> verdaccioAwaitVersions e2e name [version]
        }

logDriver :: E2E -> LogDriver
logDriver e2e = LogDriver $ \matches attempts -> pollUntil attempts 250000 matches (proxyContainerLogs e2e)

collectorDriver :: E2E -> CollectorDriver
collectorDriver e2e = CollectorDriver $ \matches attempts -> do
    name <- maybe (fail "telemetry profile requires a collector") pure (e2eCollectorContainer e2e)
    pollUntil attempts 250000 matches (containerLogs name)
