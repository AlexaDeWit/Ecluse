-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Registered load scenarios and the counts required by the routine full profile.
module Ecluse.BenchLoad.Scenarios (
    fixtures,
    checkFixtureCoverage,
    scenariosForSelection,
    findScenario,
    runsUnder,
    checkedCounts,
) where

import Data.Set qualified as Set

import Ecluse.BenchLoad.Advisories (MaliciousSource (MaliciousHit, MaliciousMixedHeavy), maliciousQualification)
import Ecluse.BenchLoad.Cargo (cargoFixture)
import Ecluse.BenchLoad.Contract (LoadScope (FullLoad), loadCoverageGaps, loadScenarioName, onRequestLoadLaws)
import Ecluse.BenchLoad.Floors (FloorKey, Pass (ConcurrencyOne, Loaded))
import Ecluse.BenchLoad.Harness (Scenario (scenarioInProcess, scenarioName, scenarioServiceTime), UpstreamFixture (fixtureEcosystem, fixtureScenarios))
import Ecluse.BenchLoad.Npm (npmFixture)
import Ecluse.BenchLoad.Pod (PodShape (Unlimited))
import Ecluse.BenchLoad.PyPI (pypiFixture)
import Ecluse.BenchLoad.RubyGems (rubyGemsFixture)
import Ecluse.BenchLoad.Selection (scenarioKey, selectScenario)
import Ecluse.Test.EcosystemBench (checkBenchmarkCoverage)

-- | Each ecosystem's fixture, in report order.
fixtures :: [UpstreamFixture]
fixtures = [npmFixture, pypiFixture, cargoFixture, rubyGemsFixture]

-- | Reject omitted load laws and ambiguous scenario identities before loading work.
checkFixtureCoverage :: [UpstreamFixture] -> Either Text ()
checkFixtureCoverage registered = do
    checkBenchmarkCoverage (map fixtureEcosystem registered)
    for_ registered $ \fixture -> do
        let gaps = loadCoverageGaps FullLoad (fixtureEcosystem fixture) (map scenarioName (fixtureScenarios fixture))
        unless (null gaps) (Left ("load contract gaps for " <> show (fixtureEcosystem fixture) <> ": " <> show gaps))

-- | The scenario an ecosystem-qualified key names.
findScenario :: Text -> Maybe Scenario
findScenario name =
    selectScenario
        name
        [ (fixtureEcosystem fixture, [(scenarioName s, s) | s <- fixtureScenarios fixture <> qualificationScenarios fixture])
        | fixture <- fixtures
        ]

-- | Whether a run under the shape measures the scenario. No pod shape bounds in-process work, so only an unlimited run does.
runsUnder :: PodShape -> Scenario -> Bool
runsUnder shape s = shape == Unlimited || not (scenarioInProcess s)

-- | Every loaded and concurrency-one count the full profile checks under the shape.
checkedCounts :: PodShape -> Set FloorKey
checkedCounts shape =
    Set.fromList
        [ (scenarioKey (fixtureEcosystem fixture) (scenarioName s), whichPass)
        | fixture <- fixtures
        , s <- scenariosForSelection Nothing fixture
        , runsUnder shape s
        , whichPass <- Loaded : [ConcurrencyOne | scenarioServiceTime s]
        ]

-- | Full runs omit optional variants. Explicit selections can reach every native and qualification row.
scenariosForSelection :: Maybe [Text] -> UpstreamFixture -> [Scenario]
scenariosForSelection selected fixture = case selected of
    Nothing -> filter ((`notElem` optionalNames) . scenarioName) (fixtureScenarios fixture)
    Just _ -> fixtureScenarios fixture <> qualificationScenarios fixture
  where
    optionalNames = mapMaybe (loadScenarioName (fixtureEcosystem fixture)) onRequestLoadLaws

qualificationScenarios :: UpstreamFixture -> [Scenario]
qualificationScenarios fixture =
    [ maliciousQualification (fixtureEcosystem fixture) source scenario
    | scenario <- fixtureScenarios fixture
    , scenarioName scenario `elem` ["merge-cold", "index-cold", "info-cold"]
    , source <- [MaliciousHit, MaliciousMixedHeavy]
    ]
