-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin how the report reads the limits and the rule policy a proxy logged at boot.
module Ecluse.BenchLoad.BootLinesSpec (spec) where

import Test.Hspec

import Ecluse.BenchLoad.BootLines (
    BootLimits (..),
    LoggedRule (..),
    admittedListings,
    bootLimits,
    bootMessages,
    loggedRules,
    ruleBootOrders,
    ruleMessages,
 )

messages :: [Text]
messages =
    [ "runtime: capabilities 2 (derived from the cgroup limit)"
    , "runtime: serve admission 20 (computed from 2 capabilities)"
    , "memory plan: material estimate budget 125628416 (computed from heap ceiling 348966912, derived from the cgroup limit)"
    , "metadata admission estimates: cold selected 8524800, retained selected 209920, full origin 38797312, listing output 11534336 bytes"
    , "memory plan: cache byte bound 83752140 (computed from heap ceiling 348966912, derived from the cgroup limit)"
    , "memory plan: cache entry bound 512 (computed from heap ceiling 348966912, derived from the cgroup limit)"
    ]

ruleLines :: [Text]
ruleLines =
    [ "config: mounts.npm.rules.pin.identity = left-pad (document)"
    , "config: mounts.npm.rules.pin.type = AllowByIdentity (document)"
    , "config: rules.min-age.ageSeconds = 3600 (environment)"
    , "config: rules.min-age.precedence = 100 (default)"
    , "config: rules.min-age.type = AllowIfOlderThan (default)"
    , "config: rules.remediation-fast-track.type = AllowIfRemediatesCve (default)"
    , "rule boot order for mount npm:"
    , "rule 1: AllowByIdentity (precedence 250)"
    , "rule 2: AllowIfOlderThan (precedence 100)"
    , "rule boot order for mount pypi:"
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
    describe "ruleMessages" $
        it "keeps the rule keys and the boot order from the JSON log" $
            ruleMessages
                [ "{\"message\":\"config: rules.min-age.type = AllowIfOlderThan (default)\"}"
                , "{\"message\":\"config: server.port = 8080 (environment)\"}"
                , "{\"message\":\"rule boot order for mount npm:\"}"
                , "{\"message\":\"rule 1: AllowIfOlderThan (precedence 100)\"}"
                , "{\"message\":\"runtime: capabilities 2 (derived from the cgroup limit)\"}"
                ]
                `shouldBe` ["config: rules.min-age.type = AllowIfOlderThan (default)", "rule boot order for mount npm:", "rule 1: AllowIfOlderThan (precedence 100)"]
    describe "loggedRules" $ do
        it "groups each rule's keys under its name, with the layers they came from" $
            loggedRules ruleLines
                `shouldBe` [ LoggedRule "mounts.npm.rules.pin" (Just "AllowByIdentity") [("identity", "left-pad")] ["document"]
                           , LoggedRule "min-age" (Just "AllowIfOlderThan") [("ageSeconds", "3600"), ("precedence", "100")] ["environment", "default"]
                           , LoggedRule "remediation-fast-track" (Just "AllowIfRemediatesCve") [] ["default"]
                           ]
        it "reads the last parenthesis as the layer" $
            map lrSettings (loggedRules ["config: rules.pin.identity = a (b) (document)"]) `shouldBe` [[("identity", "a (b)")]]
        it "keeps quoted, JSON array, empty, and assignment-shaped values whole" $
            map lrSettings (loggedRules [line "say \"hi\"", line "[\"@a\",\"@b\"]", line "", line "a = b"])
                `shouldBe` [[("note", "say \"hi\""), ("note", "[\"@a\",\"@b\"]"), ("note", ""), ("note", "a = b")]]
        it "reads every segment before the last key as the rule name" $
            loggedRules ["config: rules.a.b.type = DenyByIdentity (document)"]
                `shouldBe` [LoggedRule "a.b" (Just "DenyByIdentity") [] ["document"]]
    describe "ruleBootOrders" $
        it "lists each mount's rules in the order it logged them" $
            ruleBootOrders ruleLines
                `shouldBe` [("npm", ["AllowByIdentity (precedence 250)", "AllowIfOlderThan (precedence 100)"]), ("pypi", [])]
  where
    line value = "config: rules.pin.note = " <> value <> " (document)"
