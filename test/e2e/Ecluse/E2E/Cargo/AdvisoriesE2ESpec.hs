-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native Cargo fixtures use the shared advisory scenarios and real sparse clients.
module Ecluse.E2E.Cargo.AdvisoriesE2ESpec (cargoAdvisoryFamily) where

import Data.Aeson (Value (String))
import Data.Aeson.KeyMap qualified as KeyMap
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.E2E.AdvisoriesE2ESpec (awaitDenials, scenarioFamily)
import Ecluse.E2E.Cargo.InstallE2ESpec (cargoConsumer)
import Ecluse.E2E.Fixtures.Advisories
import Ecluse.E2E.Fixtures.Cargo
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Cargo
import Ecluse.Test.Contract.Ecosystem (FamilyFixture)
import Ecluse.Test.Contract.Install.Advisory

-- | CVSS, EPSS, MAL and remediation cases keep separate source and client lifetimes.
cargoAdvisoryFamily :: FamilyFixture AdvisoryScenarioDriver
cargoAdvisoryFamily = scenarioFamily fixture Cargo cargoTopology{topologyAws = True} cargoAdvisoryExportBase expected session
  where
    fixture = cargoWriteFixture "advisories" ["read", "advisories", "client-install"]
    expected policy = AdvisoryCase "ecluse-e2e-advisory" "1.18.0" "1.19.0" (identifier policy) (const "failed to select a version") False
    identifier = \case
        KnownMaliciousDefault -> maliciousAdvisoryId
        PartialFix -> partialFixAdvisoryId
        PartialFixAbstaining -> partialFixRemainingAdvisoryId
        _ -> cargoAdvisoryId
    session _ proxy = AdvisorySession (cargoReadVersions proxy "/cargo/" "ecluse-e2e-advisory") (releaseDriver proxy) Nothing

releaseDriver :: E2E -> Text -> IO AdvisoryDriver
releaseDriver proxy version = do
    rows <- cargoReadIndex proxy "/cargo/" "ecluse-e2e-advisory"
    let artifacts = [(archive, checksum) | row <- rows, Just (String nativeVersion) <- [KeyMap.lookup "vers" row], nativeVersion == version, Just (String checksum) <- [KeyMap.lookup "cksum" row]]
        archive = cargoArchiveRoute "ecluse-e2e-advisory" version
    pure
        AdvisoryDriver
            { advisoryExpectedArtifact = archive
            , advisoryAdvertisedArtifacts = artifacts
            , advisoryInstall = cargoConsumer proxy currentCargo $ \project -> do
                cargoSelectDependency "ecluse-e2e-advisory" version project
                result <- runCargo project ["build"]
                installed <- doesFileExist (cargoDirectory project </> "target/debug/ecluse-e2e-consumer")
                pure (result, installed)
            , advisoryArtifactStatus = proxyStatus proxy archive
            , advisoryAwaitDenials = awaitDenials proxy
            }
