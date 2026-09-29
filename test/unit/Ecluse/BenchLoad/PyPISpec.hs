-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The order of the PyPI fixture's scenarios.
module Ecluse.BenchLoad.PyPISpec (spec) where

import Data.List (isInfixOf)
import Test.Hspec

import Ecluse.BenchLoad.Harness (Scenario (scenarioName), UpstreamFixture (fixtureScenarios))
import Ecluse.BenchLoad.PyPI (pypiFixture)

spec :: Spec
spec = do
    describe "PyPI advisory variants" $
        it "run right after their no-database counterparts" $ do
            names `shouldSatisfy` isInfixOf ["index-cold", "index-cold-advisories", "index-cold-all-advisory-rules"]
            names `shouldSatisfy` isInfixOf ["revalidate-not-modified", "revalidate-not-modified-advisories"]
    describe "PyPI private copies" $
        it "run from the smallest share to the complete capture" $
            names `shouldSatisfy` isInfixOf ["heavy-private-5pct", "heavy-private-25pct", "heavy-private"]
  where
    names = map scenarioName (fixtureScenarios pypiFixture)
