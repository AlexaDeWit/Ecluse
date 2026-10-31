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
    measurementConditionVersion,
    evictionEntries,
) where

import Data.List (lookup)
import Data.Text qualified as T

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)

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

-- | Identify the shared conditions independently of scenario names and workload settings.
measurementConditionVersion :: Text
measurementConditionVersion = "unified-v3"

-- | Keep an eviction cache below its working set, refusing a set too small to churn.
evictionEntries :: Int -> Int -> Either Text Int
evictionEntries configured workingSet
    | workingSet < 2 = Left "cache-evicts-large requires at least two corpus projects"
    | otherwise = Right (min (max 1 configured) (workingSet - 1))
