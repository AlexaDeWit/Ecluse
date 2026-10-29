-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native RubyGems OSV generations drive real Bundler advisory and remediation observations.
module Ecluse.E2E.RubyGems.AdvisoriesE2ESpec (rubyAdvisoryFamily) where

import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.E2E.AdvisoriesE2ESpec (awaitDenials, scenarioFamily)
import Ecluse.E2E.Fixtures.Advisories
import Ecluse.E2E.Fixtures.RubyGems
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.RubyGems
import Ecluse.E2E.RubyGems.InstallE2ESpec (rubyConsumer)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture)
import Ecluse.Test.Contract.Install.Advisory
import Ecluse.Test.Registry.RubyGems.Fixtures
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceInfo)

-- | CVSS, EPSS, malicious denial and exact/partial fixes keep independent source and client lifetimes.
rubyAdvisoryFamily :: FamilyFixture AdvisoryScenarioDriver
rubyAdvisoryFamily = scenarioFamily fixture RubyGems rubyGemsTopology{topologyAws = True} rubyAdvisoryExportBase expected session
  where
    fixture = rubyWriteFixture "advisories" ["read", "advisories", "client-install"]
    expected policy = AdvisoryCase "ecluse-fixture-advisory" "1.18.0" "1.19.0" (identifier policy) (const "Could not find gem") False
    identifier = \case
        KnownMaliciousDefault -> maliciousAdvisoryId
        PartialFix -> partialFixAdvisoryId
        PartialFixAbstaining -> partialFixRemainingAdvisoryId
        _ -> rubyAdvisoryId
    session _ proxy = AdvisorySession (map number <$> infoCoordinates proxy) (releaseDriver proxy) Nothing
    number (_, version, _) = version

infoCoordinates :: E2E -> IO [(Text, Text, Text)]
infoCoordinates proxy = do
    (status, bytes) <- proxyGet proxy "/rubygems/info/ecluse-fixture-advisory"
    unless (status == 200) (fail "Ruby advisory info listing failed")
    rows <- either (fail . toString) pure (referenceInfo (LBS.toStrict bytes))
    pure [("ecluse-fixture-advisory", rowVersion row, rowPlatform row) | row <- rows]

releaseDriver :: E2E -> Text -> IO AdvisoryDriver
releaseDriver proxy version = do
    releases <- loadRubyReleases
    release <- rubyRelease releases "ecluse-fixture-advisory" version "ruby"
    rows <- infoCoordinates proxy
    let artifact name number platform = "/rubygems/gems/" <> name <> "-" <> number <> (if platform == "ruby" then "" else "-" <> platform) <> ".gem"
    pure
        AdvisoryDriver
            { advisoryExpectedArtifact = rubyArchiveRoute release
            , advisoryAdvertisedArtifacts = [(artifact name number platform, platform) | (name, number, platform) <- rows, number == version]
            , advisoryInstall = rubyConsumer proxy currentBundler $ \project -> do
                rubyDependency project "ecluse-fixture-advisory" ("= " <> version)
                result <- rubyInstall project False
                tree <- rubyInstalledTree project
                pure (result, any (T.isSuffixOf "ecluse_fixture_advisory.rb" . toText) (Map.keys tree))
            , advisoryArtifactStatus = proxyStatus proxy (rubyArchiveRoute release)
            , advisoryAwaitDenials = awaitDenials proxy
            }
