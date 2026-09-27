-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The PyPI advisory variants' order in the fixture.
module Ecluse.BenchLoad.PyPISpec (spec) where

import Data.List (isInfixOf)
import Test.Hspec

import Ecluse.BenchLoad.Harness (Scenario (scenarioName), UpstreamFixture (fixtureScenarios))
import Ecluse.BenchLoad.PyPI (pypiFixture)

spec :: Spec
spec = describe "PyPI advisory variants" $
    it "run right after their no-database counterparts" $ do
        let names = map scenarioName (fixtureScenarios pypiFixture)
        names `shouldSatisfy` isInfixOf ["index-cold", "index-cold-advisories", "index-cold-all-advisory-rules"]
        names `shouldSatisfy` isInfixOf ["revalidate-not-modified", "revalidate-not-modified-advisories"]
