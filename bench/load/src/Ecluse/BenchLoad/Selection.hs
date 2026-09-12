-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Ecosystem selection and report grouping for isolated load scenarios.
The eviction bound reserves room for churn even when the corpus is small.
-}
module Ecluse.BenchLoad.Selection (
    scenarioKey,
    selectScenario,
    fixtureSection,
    fixtureBaseline,
    evictionEntries,
) where

import Data.List (lookup)
import Data.Text qualified as T

import Ecluse.BenchLoad.Normalise (BaselineSource (InjectedFallback))
import Ecluse.Core.Ecosystem (Ecosystem (Npm), ecosystemName)

-- | Qualify a child identifier so equal scenario names in different ecosystems stay distinct.
scenarioKey :: Ecosystem -> Text -> Text
scenarioKey ecosystem name = ecosystemName ecosystem <> "/" <> name

-- | Resolve only an ecosystem-qualified scenario identifier.
selectScenario :: Text -> [(Ecosystem, [(Text, a)])] -> Maybe a
selectScenario key fixtures =
    lookup key [(scenarioKey ecosystem name, value) | (ecosystem, scenarios) <- fixtures, (name, value) <- scenarios]

-- | Keep every analysis table under its ecosystem in the summary and downloadable artifact.
fixtureSection :: Ecosystem -> [Text] -> Text
fixtureSection ecosystem sections =
    T.intercalate "\n" (("# " <> ecosystemName ecosystem <> " load scenarios\n") : sections)

-- | Only npm has a live probe. Other fixtures use the configured latency in microseconds.
fixtureBaseline :: Ecosystem -> Int -> BaselineSource -> BaselineSource
fixtureBaseline Npm _ measured = measured
fixtureBaseline _ configuredMicros _ = InjectedFallback (fromIntegral configuredMicros / 1_000)

-- | Keep an eviction cache below its working set, refusing a set too small to churn.
evictionEntries :: Int -> Int -> Either Text Int
evictionEntries configured workingSet
    | workingSet < 2 = Left "cache-evicts-large requires at least two corpus projects"
    | otherwise = Right (min (max 1 configured) (workingSet - 1))
