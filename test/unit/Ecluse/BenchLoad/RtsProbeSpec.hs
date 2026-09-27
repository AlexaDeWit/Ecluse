-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The statistics check a scenario child and a proxy run before they measure anything.
module Ecluse.BenchLoad.RtsProbeSpec (spec) where

import Test.Hspec

import Ecluse.BenchLoad.RtsProbe (rtsStatsRefusal)

spec :: Spec
spec = describe "rtsStatsRefusal" $ do
    it "refuses a process launched without the statistics, naming the flag to relaunch it with" $
        rtsStatsRefusal "the proxy" False `shouldBe` Just "bench-load needs the RTS statistics. Run the proxy with GHCRTS=-T."
    it "lets a process with the statistics on measure" $
        rtsStatsRefusal "the proxy" True `shouldBe` Nothing
