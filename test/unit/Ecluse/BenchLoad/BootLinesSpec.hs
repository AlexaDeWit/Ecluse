-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin how the report reads the limits and the rule policy a proxy logged at boot.
module Ecluse.BenchLoad.BootLinesSpec (spec) where

import Test.Hspec

import Ecluse.BenchLoad.BootLines (
    BootLimits (..),
    LoggedRule (..),
    bootLimits,
    bootMessages,
    loggedRules,
    ruleBootOrders,
    ruleMessages,
 )

messages :: [Text]
messages =
    [ "runtime: capabilities 4 (derived from the cgroup limit)"
    , "runtime: serve admission 40 (computed from 4 capabilities)"
    , "memory plan: cache byte bound 67108864 (computed from heap ceiling 905969664, derived from the cgroup limit)"
    , "memory plan: cache entry bound 4096 (computed from heap ceiling 905969664, derived from the cgroup limit)"
    , "memory plan: transient budget 110880768 (live target 192937984 less 82057216 for the idle process and the other tenants, floor 16777216, live ceiling 257250645)"
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
        it "keeps the runtime and memory-plan decisions from the JSON log" $
            bootMessages
                [ "{\"message\":\"runtime: capabilities 2 (derived from the cgroup limit)\",\"status\":\"info\"}"
                , "{\"message\":\"serving packument request for lodash\",\"status\":\"info\"}"
                , "{\"message\":\"memory plan: transient budget 1 (built-in default, no heap-ceiling datapoint)\"}"
                , "not json"
                ]
                `shouldBe` ["runtime: capabilities 2 (derived from the cgroup limit)", "memory plan: transient budget 1 (built-in default, no heap-ceiling datapoint)"]
    describe "bootLimits" $ do
        it "reads each limit from its line" $
            bootLimits messages
                `shouldBe` BootLimits
                    { blCpuAdmission = Just 40
                    , blMemoryBudgetBytes = Just 110880768
                    , blCacheBytes = Just 67108864
                    , blCacheEntries = Just 4096
                    }
        it "leaves a limit no line states unknown" $
            blMemoryBudgetBytes (bootLimits (take 2 messages)) `shouldBe` Nothing
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
