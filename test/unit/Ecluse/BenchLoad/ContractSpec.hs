-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Missing load laws cannot disappear when a provider shortens its registration list.
module Ecluse.BenchLoad.ContractSpec (spec) where

import Data.Set qualified as Set
import Test.Hspec

import Ecluse.BenchLoad.Contract
import Ecluse.BenchLoad.Floors (Pass (ConcurrencyOne, Loaded))
import Ecluse.BenchLoad.Harness (Scenario (scenarioName), UpstreamFixture (..))
import Ecluse.BenchLoad.Npm (npmFixture)
import Ecluse.BenchLoad.Pod (PodShape (Unlimited), scheduledPodShapes)
import Ecluse.BenchLoad.PyPI (pypiFixture)
import Ecluse.BenchLoad.Scenarios (checkFixtureCoverage, checkedCounts, fixtures)
import Ecluse.BenchLoad.Support (twoCores)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))

-- | Audit real registrations without booting a proxy or starting a measurement.
spec :: Spec
spec = describe "canonical load contract" $ do
    it "rejects each omitted read measurement even when other scenarios remain" $
        for_ fixtures $ \fixture ->
            for_ (requiredLoadLaws ReadLoadFoundation) $ \law -> do
                let name = loadScenarioName (fixtureEcosystem fixture) law
                    shortened = fixture{fixtureScenarios = filter ((/= name) . Just . scenarioName) (fixtureScenarios fixture)}
                    providers = shortened : filter ((/= fixtureEcosystem fixture) . fixtureEcosystem) fixtures
                checkFixtureCoverage providers `shouldSatisfy` isLeft
    it "requires every managed PyPI read comparison even when the provider omits it" $
        for_ [ManagedMirrorBaseline, ManagedMirrorWarm, ManagedMirrorRevision] $ \law -> do
            let selected = loadScenarioName PyPI law
                shortened = filter ((/= selected) . Just) (names pypiFixture)
            loadCoverageGaps ReadLoadFoundation PyPI shortened `shouldSatisfy` (not . null)
    it "preserves the npm complete load sequence" $
        loadCoverageGaps FullLoad Npm (names npmFixture) `shouldBe` []
    it "requires the complete PyPI worker measurement beside its reads" $ do
        loadCoverageGaps ReadLoadFoundation PyPI (names pypiFixture) `shouldBe` []
        loadCoverageGaps FullLoad PyPI (names pypiFixture) `shouldBe` []
        loadCoverageGaps FullLoad PyPI (filter (/= "worker-mirroring") (names pypiFixture))
            `shouldBe` [MissingLoadScenario MirrorWorker "worker-mirroring"]
    it "rejects duplicated extensions as well as duplicated read laws" $
        loadCoverageGaps FullLoad Npm (names npmFixture <> ["worker-mirroring"])
            `shouldBe` [DuplicateLoadScenario "worker-mirroring"]
    it "does not count a native npm artifact name as PyPI coverage" $
        loadCoverageGaps ReadLoadFoundation PyPI ("tarball-hot-path" : filter (/= "wheel-hot-path") (names pypiFixture))
            `shouldBe` [MissingLoadScenario PrivateArtifact "wheel-hot-path"]
    it "refuses a protocol without load drivers" $
        loadCoverageGaps FullLoad RubyGems [] `shouldBe` [NoLoadProtocol RubyGems]
    it "requires the exact passes the complete fixtures execute under each shape" $
        for_ scheduledPodShapes $ \shape ->
            fmap (Set.fromList . map snd) ((<>) <$> requiredLoadPasses FullLoad Npm shape <*> requiredLoadPasses FullLoad PyPI shape)
                `shouldBe` Right (checkedCounts shape)
    it "retains both worker passes only under the unlimited shape" $
        for_ [(Npm, "npm/worker-mirroring"), (PyPI, "pypi/worker-mirroring")] $ \(ecosystem, key) -> do
            fmap (filter ((== MirrorWorker) . fst)) (requiredLoadPasses FullLoad ecosystem Unlimited)
                `shouldBe` Right [(MirrorWorker, (key, Loaded)), (MirrorWorker, (key, ConcurrencyOne))]
            fmap (filter ((== MirrorWorker) . fst)) (requiredLoadPasses FullLoad ecosystem twoCores) `shouldBe` Right []
  where
    names = map scenarioName . fixtureScenarios
