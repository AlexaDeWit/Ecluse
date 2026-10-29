-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native report evidence must retain missing measurements and failed guards.
module Ecluse.BenchReport.PerformanceSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchReport.Performance
import Ecluse.Test.Contract.Performance
import Ecluse.Test.Contract.Result (ExecutionIdentity (ExecutionIdentity))

spec :: Spec
spec = do
    describe "benchmarkObservations" $ do
        it "keeps a zero-deviation measurement available" $
            benchmarkObservations NativePassed Map.empty (csv "100,0")
                `shouldBe` Right (NativePerformance NativePassed [PerformanceObservation "ecosystem: npm.route.match.mixed requests" NativePassed] Map.empty)
        it "does not invent measurements from an empty CSV" $
            (npObservations <$> benchmarkObservations NativePassed Map.empty "Name,Mean (ps),2*Stdev (ps)\n") `shouldBe` Right []
        it "rejects malformed CSV" $
            benchmarkObservations NativePassed Map.empty "not a CSV" `shouldSatisfy` isLeft
        it "marks a negative measurement as failed" $
            (npObservations <$> benchmarkObservations NativePassed Map.empty (csv "-1,0"))
                `shouldBe` Right [PerformanceObservation "ecosystem: npm.route.match.mixed requests" NativeFailed]
    describe "benchmarkConsoleObservations" $ do
        it "reads actual outcomes with their native group paths" $
            benchmarkConsoleObservations console
                `shouldBe` [ PerformanceObservation "native/ecosystem: npm.rules.evalRules.scales linearly in version count" NativePassed
                           , PerformanceObservation "native/synthetic generators.npm.decodes every generated release" NativeFailed
                           ]
        it "does not accept a declaration without an outcome" $
            benchmarkConsoleObservations "All\n  synthetic generators\n    npm\n      decodes every generated release\n" `shouldBe` []
    describe "canonical measurement obligations" $ do
        it "requires every allocation leg independently of reported legs" $
            allocationRequiredRows "pypi" ["numpy"]
                `shouldBe` ["pypi/numpy/full", "pypi/numpy/singleVersion", "pypi/numpy/fullShippedAdvisories", "pypi/numpy/fullAllAdvisoryRules"]
        it "requires the native generator and complexity outcomes missing from CSV" $ do
            let rows = benchmarkRequiredRows "npm" ["react (large)"]
            rows `shouldContain` ["native/synthetic generators.npm.decodes every generated release"]
            rows `shouldContain` ["native/ecosystem: npm.rules.evalRules.scales linearly in version count"]
            rows `shouldContain` ["ecosystem: npm.wire+project (per package).react (large).full metadata projection"]
        it "preserves embedded-URL generator identities and requires native served-field observations" $ do
            for_ ["npm", "pypi", "cargo"] $ \ecosystem ->
                benchmarkRequiredRows ecosystem [] `shouldContain` ["native/synthetic generators." <> ecosystem <> ".rewrites every artifact onto the proxy origin"]
            benchmarkRequiredRows "rubygems" [] `shouldContain` ["native/synthetic generators.rubygems.preserves native served records and proxy artifact routes"]
        it "requires native route, request and coordinate-complexity outcomes independently" $ do
            let rows = benchmarkRequiredRows "rubygems" ["nokogiri (large)"]
            rows `shouldContain` ["native/ecosystem: rubygems.route.match.malformed native coordinate"]
            for_ ["gem action", "quick specification action", "native info action", "request building metadata request", "request building artifact request", "request building compact specification request"] $ \nativeRow ->
                rows `shouldContain` ["ecosystem: rubygems.route.match." <> nativeRow]
    describe "makePerformanceReceipt" $ do
        it "keeps an invocation failure despite successful measured rows" $
            npOutcome (prNative (makePerformanceReceipt request{requestOutcome = NativeFailed} Map.empty native)) `shouldBe` NativeFailed
        it "keeps unavailability despite a zero process exit" $
            npOutcome (prNative (makePerformanceReceipt request Map.empty native{npOutcome = NativeUnavailable})) `shouldBe` NativeUnavailable
        it "selects a subject without hiding unexpected rows" $
            npObservations (prNative (makePerformanceReceipt request Map.empty native))
                `shouldBe` [PerformanceObservation "npm/unexpected/full" NativePassed]
        it "cannot supply native floor enforcement from wrapper declarations" $
            Map.lookup "floorEnforcement" (npConditions (prNative (makePerformanceReceipt request{requestRecordedConditions = Map.fromList [("floorEnforcement", "held"), ("floorEnforcementReasons", "")]} Map.empty native))) `shouldBe` Nothing
        it "keeps conditions observed by the harness ahead of wrapper declarations" $
            npConditions (prNative (makePerformanceReceipt request{requestRecordedConditions = Map.singleton "passes" "1"} Map.empty native)) `shouldBe` Map.singleton "passes" "5"

csv :: Text -> Text
csv figures = "Name,Mean (ps),2*Stdev (ps)\nAll.ecluse-core (work-per-request).ecosystem: npm.route.match.mixed requests," <> figures <> "\n"

console :: Text
console =
    T.unlines
        [ "Build profile: -w ghc-9.10.3 -O1"
        , "All"
        , "  ecluse-core (work-per-request)"
        , "    ecosystem: npm"
        , "      rules.evalRules"
        , "        scales linearly in version count: OK (1.0s)"
        , ""
        , "  synthetic generators"
        , "    npm"
        , "      decodes every generated release: FAIL"
        ]

request :: PerformanceRequest
request = PerformanceRequest (ExecutionIdentity "source" "image" "arm64" "plan" "run") "1" (PerformanceObligation "allocation" "ecosystem-npm" ["npm/react/full"] Map.empty) [] NativePassed Map.empty

native :: NativePerformance
native = NativePerformance NativePassed [PerformanceObservation "npm/unexpected/full" NativePassed, PerformanceObservation "pypi/numpy/full" NativePassed] (Map.singleton "passes" "5")
