-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.BenchLoad.PatternsSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.BenchLoad.Patterns

spec :: Spec
spec = describe "finite request patterns" $ do
    let names = ["a", "b", "c", "d", "e", "f", "g", "h"]
        knobs = defaultPatternKnobs{pkNames = 4, pkClients = 2, pkRounds = 3}
        trace family settings = fromRight (RequestTrace [] []) (makeTrace family settings names)
    it "visits each cold-install name once with one client regardless of fleet size" $ do
        let result = trace ColdInstall knobs
        length (rtClients result) `shouldBe` 1
        map (length . ctNames) (rtClients result) `shouldBe` [4]
        map (length . ordNub . ctNames) (rtClients result) `shouldBe` [4]
        map ctStartMicros (rtClients result) `shouldBe` [0]
    it "replays the same lockfile with exact inter-client skew" $ do
        let clients = rtClients (trace CiFleet knobs{pkSkewMicros = 137})
        map ctStartMicros clients `shouldBe` [0, 137]
        length (ordNub (map ctNames clients)) `shouldBe` 1
    it "reserves disjoint private names around the requested common fraction" $ do
        let result = trace Heterogeneous knobs{pkOverlap = 0.5}
        length (rtNames result) `shouldBe` 6
        map (length . ctNames) (rtClients result) `shouldBe` [4, 4]
    it "refuses impossible overlap rather than wrapping identities" $
        makeTrace Heterogeneous knobs{pkNames = 8, pkOverlap = 0} names `shouldSatisfy` isLeft
    it "keeps scan finite and visits the entire space every round" $ do
        let result = trace Scan knobs
        map (length . ctNames) (rtClients result) `shouldBe` [12]
        length (rtNames result) `shouldBe` 4
    it "schedules restart arrivals independently of client service times" $
        map ctStartMicros (rtClients (trace Restart knobs{pkArrivalMicros = 500_000})) `shouldBe` [0, 500_000]
    it "reproduces Zipf draws with a seed and changes draws with another seed" $ do
        trace Zipf knobs `shouldNotBe` trace Zipf knobs{pkSeed = 1234}
        map (length . ctNames) (rtClients (trace Zipf knobs)) `shouldBe` [12, 12]
    it "counts distinct bytes once despite repeated traffic and keeps the fat tail" $ do
        let result = trace Scan knobs{pkNames = 8}
            sizes = Map.fromList [(name, if name == "h" then 10_000 else 1) | name <- names]
        workingBytes sizes result `shouldBe` Right 10_007
        workingBytes Map.empty result `shouldSatisfy` isLeft
    it "rejects malformed counts and non-finite exponents" $ do
        makeTrace ColdInstall knobs{pkNames = 9} names `shouldSatisfy` isLeft
        makeTrace Zipf knobs{pkExponent = 0 / 0} names `shouldSatisfy` isLeft
        makeTrace CiFleet knobs{pkSkewMicros = -1} names `shouldSatisfy` isLeft
