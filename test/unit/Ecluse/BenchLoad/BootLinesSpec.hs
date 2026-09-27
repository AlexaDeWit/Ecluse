-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin how the report reads the limits a proxy logged at boot.
module Ecluse.BenchLoad.BootLinesSpec (spec) where

import Test.Hspec

import Ecluse.BenchLoad.BootLines (BootLimits (..), admittedListings, bootLimits, bootMessages)

messages :: [Text]
messages =
    [ "runtime: capabilities 2 (derived from the cgroup limit)"
    , "runtime: serve admission 20 (computed from 2 capabilities)"
    , "memory plan: material estimate budget 125628416 (computed from heap ceiling 348966912, derived from the cgroup limit)"
    , "metadata admission estimates: cold selected 8524800, retained selected 209920, full origin 38797312, listing output 11534336 bytes"
    , "memory plan: cache byte bound 83752140 (computed from heap ceiling 348966912, derived from the cgroup limit)"
    , "memory plan: cache entry bound 512 (computed from heap ceiling 348966912, derived from the cgroup limit)"
    ]

spec :: Spec
spec = do
    describe "bootMessages" $
        it "keeps the runtime and admission decisions from the JSON log" $
            bootMessages
                [ "{\"message\":\"runtime: capabilities 2 (derived from the cgroup limit)\",\"status\":\"info\"}"
                , "{\"message\":\"serving packument request for lodash\",\"status\":\"info\"}"
                , "{\"message\":\"metadata admission estimates: full origin 1, listing output 2 bytes\"}"
                , "not json"
                ]
                `shouldBe` ["runtime: capabilities 2 (derived from the cgroup limit)", "metadata admission estimates: full origin 1, listing output 2 bytes"]
    describe "bootLimits" $ do
        it "reads each limit from its line" $
            bootLimits messages
                `shouldBe` BootLimits
                    { blCpuAdmission = Just 20
                    , blMaterialBudgetBytes = Just 125628416
                    , blFullOriginBytes = Just 38797312
                    , blListingOutputBytes = Just 11534336
                    , blCacheBytes = Just 83752140
                    , blCacheEntries = Just 512
                    }
        it "leaves a limit no line states unknown" $
            blMaterialBudgetBytes (bootLimits (take 2 messages)) `shouldBe` Nothing
    describe "admittedListings" $ do
        it "divides the budget by a two-origin listing's weight" $
            admittedListings (bootLimits messages) `shouldBe` Just 1
        it "admits several when the budget allows" $
            admittedListings (bootLimits messages){blMaterialBudgetBytes = Just 571_400_000} `shouldBe` Just 6
        it "still admits one listing heavier than the budget" $
            admittedListings (bootLimits messages){blMaterialBudgetBytes = Just 1} `shouldBe` Just 1
        it "is unknown without the budget" $
            admittedListings (bootLimits (take 2 messages)) `shouldBe` Nothing
