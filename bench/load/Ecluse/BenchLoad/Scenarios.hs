-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Every scenario the harness defines, and the counts of successes a run of all of them checks.
module Ecluse.BenchLoad.Scenarios (
    fixtures,
    findScenario,
    runsUnder,
    checkedCounts,
) where

import Data.Set qualified as Set

import Ecluse.BenchLoad.Floors (FloorKey, Pass (ConcurrencyOne, Loaded))
import Ecluse.BenchLoad.Harness (Scenario (scenarioInProcess, scenarioName, scenarioServiceTime), UpstreamFixture (fixtureEcosystem, fixtureScenarios))
import Ecluse.BenchLoad.Npm (npmFixture)
import Ecluse.BenchLoad.Pod (PodShape (Unlimited))
import Ecluse.BenchLoad.PyPI (pypiFixture)
import Ecluse.BenchLoad.Selection (scenarioKey, selectScenario)

-- | Each ecosystem's fixture, in report order.
fixtures :: [UpstreamFixture]
fixtures = [npmFixture, pypiFixture]

-- | The scenario an ecosystem-qualified key names.
findScenario :: Text -> Maybe Scenario
findScenario name =
    selectScenario
        name
        [ (fixtureEcosystem fixture, [(scenarioName s, s) | s <- fixtureScenarios fixture])
        | fixture <- fixtures
        ]

-- | Whether a run under the shape measures the scenario. No pod shape bounds in-process work, so only an unlimited run does.
runsUnder :: PodShape -> Scenario -> Bool
runsUnder shape s = shape == Unlimited || not (scenarioInProcess s)

-- | Every count a run of every scenario checks under the shape: each loaded pass, and each concurrency-one pass.
checkedCounts :: PodShape -> Set FloorKey
checkedCounts shape =
    Set.fromList
        [ (scenarioKey (fixtureEcosystem fixture) (scenarioName s), whichPass)
        | fixture <- fixtures
        , s <- fixtureScenarios fixture
        , runsUnder shape s
        , whichPass <- Loaded : [ConcurrencyOne | scenarioServiceTime s]
        ]
