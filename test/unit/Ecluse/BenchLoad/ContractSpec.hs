-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Missing load laws cannot disappear when a provider shortens its registration list.
module Ecluse.BenchLoad.ContractSpec (spec) where

import Data.Set qualified as Set
import Test.Hspec

import Ecluse.BenchLoad.Cargo (cargoFixture)
import Ecluse.BenchLoad.Contract
import Ecluse.BenchLoad.Floors (Pass (ConcurrencyOne, Loaded))
import Ecluse.BenchLoad.Harness (Scenario (scenarioName), UpstreamFixture (..))
import Ecluse.BenchLoad.Npm (npmFixture)
import Ecluse.BenchLoad.Pod (PodShape (Unlimited), scheduledPodShapes)
import Ecluse.BenchLoad.PyPI (pypiFixture)
import Ecluse.BenchLoad.RubyGems (rubyGemsFixture)
import Ecluse.BenchLoad.Scenarios (checkFixtureCoverage, checkedCounts, fixtures)
import Ecluse.BenchLoad.Support (twoCores)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))

-- | Audit real registrations without booting a proxy or starting a measurement.
spec :: Spec
spec = describe "canonical load contract" $ do
    it "rejects each omitted read measurement even when other scenarios remain" $
        for_ fixtures $ \fixture ->
            for_ (requiredLoadLaws ReadLoadFoundation (fixtureEcosystem fixture)) $ \law -> do
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
    it "requires the complete Cargo native reads and worker measurement" $ do
        loadCoverageGaps FullLoad Cargo (names cargoFixture) `shouldBe` []
        loadCoverageGaps FullLoad Cargo (filter (/= "worker-mirroring") (names cargoFixture))
            `shouldBe` [MissingLoadScenario MirrorWorker "worker-mirroring"]
        loadCoverageGaps ReadLoadFoundation Cargo ("wheel-hot-path" : filter (/= "crate-hot-path") (names cargoFixture))
            `shouldBe` [MissingLoadScenario PrivateArtifact "crate-hot-path"]
    it "rejects duplicated extensions as well as duplicated read laws" $
        loadCoverageGaps FullLoad Npm (names npmFixture <> ["worker-mirroring"])
            `shouldBe` [DuplicateLoadScenario "worker-mirroring"]
    it "does not count a native npm artifact name as PyPI coverage" $
        loadCoverageGaps ReadLoadFoundation PyPI ("tarball-hot-path" : filter (/= "wheel-hot-path") (names pypiFixture))
            `shouldBe` [MissingLoadScenario PrivateArtifact "wheel-hot-path"]
    it "requires native info and compact specification work beside whole-version copying" $ do
        loadCoverageGaps FullLoad RubyGems (names rubyGemsFixture) `shouldBe` []
        for_ [(RepeatedInfo, "info-repeated-corpus"), (OneProjectInfo, "info-working-set-one"), (ManyProjectInfo, "info-working-set-many"), (QuickSpecification, "quick-gemspec"), (MirrorWorker, "worker-mirroring")] $ \(law, name) ->
            loadCoverageGaps FullLoad RubyGems (filter (/= name) (names rubyGemsFixture)) `shouldBe` [MissingLoadScenario law name]
        loadCoverageGaps FullLoad RubyGems [] `shouldSatisfy` (not . null)
        for_ [AssembledResponseHit, CacheFits, CacheEvicts] $ \law -> do
            requiredLoadLaws FullLoad RubyGems `shouldSatisfy` notElem law
            loadScenarioName RubyGems law `shouldBe` Nothing
    it "requires the exact passes the full profile executes under each shape" $
        for_ scheduledPodShapes $ \shape -> do
            fmap (Set.fromList . map snd . concat) (traverse (\ecosystem -> requiredLoadPasses FullLoad ecosystem shape) [Npm, PyPI, Cargo, RubyGems])
                `shouldBe` Right (checkedCounts shape)
            fmap length (requiredLoadPasses FullLoad RubyGems shape) `shouldBe` Right (if shape == Unlimited then 42 else 40)
    it "keeps optional variants out of full obligations while retaining private-copy controls" $
        for_ [Npm, PyPI, Cargo, RubyGems] $ \ecosystem ->
            case requiredLoadPasses FullLoad ecosystem twoCores of
                Left gaps -> expectationFailure (toString (show gaps :: Text))
                Right passes -> do
                    map fst passes `shouldSatisfy` notElem PrivateCopyFivePercent
                    map fst passes `shouldSatisfy` notElem PatternRestart
                    for_ [PrivateCopyQuarter, PrivateCopyWhole, PatternCiFleet] $ \law ->
                        map fst passes `shouldSatisfy` elem law
    it "retains both worker passes only under the unlimited shape" $
        for_ [(Npm, "npm/worker-mirroring"), (PyPI, "pypi/worker-mirroring"), (Cargo, "cargo/worker-mirroring"), (RubyGems, "rubygems/worker-mirroring")] $ \(ecosystem, key) -> do
            fmap (filter ((== MirrorWorker) . fst)) (requiredLoadPasses FullLoad ecosystem Unlimited)
                `shouldBe` Right [(MirrorWorker, (key, Loaded)), (MirrorWorker, (key, ConcurrencyOne))]
            fmap (filter ((== MirrorWorker) . fst)) (requiredLoadPasses FullLoad ecosystem twoCores) `shouldBe` Right []
  where
    names = map scenarioName . fixtureScenarios
