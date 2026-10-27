-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Concrete RubyGems operations supply every full ecosystem family and native recovery obligation.
module Ecluse.E2E.Drivers.RubyGems (rubyDriver) where

import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (fixtureMetadata))
import Ecluse.E2E.Harness.RubyGems (RubyProject)
import Ecluse.E2E.Harness.RubyGemsCodeArtifact (RubySweepPlane, RubySweepStore)
import Ecluse.E2E.RubyGems.AdvisoriesE2ESpec (rubyAdvisoryFamily)
import Ecluse.E2E.RubyGems.DredgerE2ESpec (rubyDredgerFixture, rubyDredgerSafetyContract, rubySweepDriver)
import Ecluse.E2E.RubyGems.InstallE2ESpec (rubyInstallationFamily)
import Ecluse.E2E.RubyGems.MirrorInstallE2ESpec (RubyCoordinates, rubyMirrorFamily)
import Ecluse.E2E.RubyGems.MirrorProofE2ESpec (rubyMirrorProofContract)
import Ecluse.E2E.RubyGems.NativeE2ESpec (rubyNativeContract)
import Ecluse.E2E.RubyGems.PublishE2ESpec (rubyPublicationFamily)
import Ecluse.E2E.RubyGems.RelayE2ESpec (rubyRelayContract)
import Ecluse.E2E.RubyGems.TelemetryE2ESpec (rubyMirrorTelemetryFamily, rubyReadTelemetryFamily)
import Ecluse.Test.Contract.Ecosystem

-- | Availability flags cannot remove independently required native laws or executed reports.
rubyDriver :: EcosystemDriver Bool RubyCoordinates Text RubySweepPlane RubySweepStore RubyProject
rubyDriver =
    EcosystemDriver
        { ecosystemName = "rubygems"
        , ecosystemProxying = Available (ServingEvidence "integration/rubygems")
        , ecosystemInstallation = Available rubyInstallationFamily
        , ecosystemPolicy = Available (ServingEvidence "integration/rubygems")
        , ecosystemAdvisories = Available rubyAdvisoryFamily
        , ecosystemMirroring = Available rubyMirrorFamily
        , ecosystemPublication = Available rubyPublicationFamily
        , ecosystemDredger = Available (fixtureMetadata rubyDredgerFixture, rubySweepDriver rubyDredgerFixture)
        , ecosystemTelemetry = Available (rubyReadTelemetryFamily, Available rubyMirrorTelemetryFamily)
        , ecosystemNativeExtensions = [rubyNativeContract, rubyMirrorProofContract, rubyRelayContract] <> rubyDredgerSafetyContract
        }
