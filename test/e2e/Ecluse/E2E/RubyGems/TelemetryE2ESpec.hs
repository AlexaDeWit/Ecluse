-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native Bundler traffic supplies actual service signals, worker spans and private fetch observations.
module Ecluse.E2E.RubyGems.TelemetryE2ESpec (rubyReadTelemetryFamily, rubyMirrorTelemetryFamily) where

import Ecluse.E2E.Fixtures.Contracts (concreteFamily)
import Ecluse.E2E.Fixtures.RubyGems
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.RubyGems
import Ecluse.E2E.Harness.RubyGemsRegistry
import Ecluse.E2E.RubyGems.InstallE2ESpec (rubyConsumer)
import Ecluse.E2E.RubyGems.MirrorInstallE2ESpec (rubyMirrorConfig, withRubyMirrorFixture)
import Ecluse.E2E.TelemetryE2ESpec (collectorDriver, readDriver)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture)
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Telemetry
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Registry.RubyGems.Fixtures (rubyLeaf, rubyRoot, rubyVersion)

-- | Enabled, disabled, exporter-failure and Datadog identity cases retain their separate process lifetimes.
rubyReadTelemetryFamily :: FamilyFixture ReadTelemetryDriver
rubyReadTelemetryFamily = concreteFamily (rubyReadFixture "telemetry" ["read", "telemetry", "client-install"]) $ \recorder action plane ->
    action (readDriver recorder plane nativeTraffic rubyRoot rubyLeaf "ecluse-fixture-platform")

-- | The private-fetch package starts absent even after the domain-span package reaches the mirror.
rubyMirrorTelemetryFamily :: FamilyFixture MirrorTelemetryFixture
rubyMirrorTelemetryFamily = concreteFamily (rubyWriteFixture "telemetry-write" ["read", "mirror", "client-install", "telemetry"]) $ \recorder action plane ->
    timedFixture recorder (withRubyMirrorFixture plane . curry) $ \(registry, _) -> do
        queue <- sharedMirrorQueue plane
        let config = rubyMirrorConfig{ecCollector = True, ecQueueUrl = Just queue, ecExtraEnv = ecExtraEnv rubyMirrorConfig <> otlpCollectorEnv}
        withClientProxyWith
            config
            ( \proxy ->
                action
                    MirrorTelemetryFixture
                        { mirrorTelemetryTraffic = nativeTraffic proxy
                        , mirrorTelemetryStore =
                            MirrorTelemetryDriver
                                (rubyRegistryVersions registry "rubygems-mirror")
                                (\name version -> pollUntil 120 500000 (elem version) (rubyRegistryVersions registry "rubygems-mirror" name))
                        , mirrorTelemetryCollector = collectorDriver proxy
                        , mirrorTelemetryPackage = rubyLeaf
                        , mirrorTelemetryVersion = rubyVersion
                        , mirrorTelemetryPrivatePackage = rubyRoot
                        , mirrorTelemetryPrivateVersion = rubyVersion
                        , mirrorTelemetryHost = "rubygems-mirror"
                        , mirrorTelemetryTarget = "/info/" <> rubyRoot
                        , mirrorTelemetryPrivateFetch = IndexedLookup
                        }
            )
            plane

nativeTraffic :: E2E -> TrafficDriver
nativeTraffic proxy = TrafficDriver install (proxyStatus proxy "/readyz")
  where
    install name = rubyConsumer proxy currentBundler $ \project -> do
        rubyDependency project name ("= " <> rubyVersion)
        rubyInstall project False
