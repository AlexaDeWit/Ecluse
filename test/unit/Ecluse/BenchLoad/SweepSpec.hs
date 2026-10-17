-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the sweep file's reader and the committed capability sweep: its job balance and its boot values.
module Ecluse.BenchLoad.SweepSpec (spec) where

import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Hspec

import Ecluse.BenchLoad.Floors (PassSelection (LoadedPassOnly))
import Ecluse.BenchLoad.Pod (ProxyTuning (..), cpuListMembers, mib, podCpuQuota, podMemoryLimit)
import Ecluse.BenchLoad.Sweep (Arm (..), BootValues (..), Phase (..), Sweep (..), SweepJob (..), decodeSweep, loadSweep, sweepJob)
import Ecluse.Composition.Sizing (resolveServeAdmission)
import Ecluse.Rts (CgroupLimits (CgroupLimits), RtsPosture (..), RuntimeOverrides (RuntimeOverrides), RuntimePlan (..), resolveRuntimePlan)

spec :: Spec
spec = do
    describe "the committed capability sweep" $ do
        it "runs the validation and measured phases by default, and the one-CPU phase only on request" $
            withSweep $ \sweep ->
                [(phaseName phase, phaseOnRequest phase) | phase <- toList (swPhases sweep)]
                    `shouldBe` [("A", False), ("B", False), ("C", False), ("D", False), ("E", False), ("F", True)]
        it "puts each test arm of a measured phase in three of its jobs, at a different position each time" $
            withSweep $ \sweep ->
                for_ (NE.drop 1 (swPhases sweep)) $ \phase ->
                    (phaseName phase, Map.filter (not . balanced) (placements phase)) `shouldBe` (phaseName phase, mempty)
        it "expects of every arm the posture and admission the runtime resolves for it on the four-processor runner" $
            withSweep $ \sweep ->
                for_ (Map.toList (swArms sweep)) $ \(name, arm) ->
                    (name, resolved arm) `shouldBe` (name, armExpected arm)
        it "sets no CPU-time quota on a measured arm, and places each on CPUs of its own" $
            withSweep $ \sweep ->
                for_ [(name, arm) | phase <- NE.drop 1 (swPhases sweep), job <- toList (phaseJobs phase), name <- toList (jobArms job), Just arm <- [Map.lookup name (swArms sweep)]] $ \(name, arm) -> do
                    (name, podCpuQuota (armShape arm)) `shouldBe` (name, Nothing)
                    (name, isJust (ptCpus (armTuning arm))) `shouldBe` (name, True)
        it "runs the loaded pass alone at a fixed 5 ms, and scores the cold listing, the 25% private copy, and the assembled hit" $
            withSweep $ \sweep -> do
                (swPasses sweep, swUpstreamLatencyMs sweep) `shouldBe` (LoadedPassOnly, 5)
                swScored sweep `shouldMatchList` [ecosystem <> "/" <> name | ecosystem <- ["npm", "pypi"], name <- ["heavy-private-25pct", "assembled-response-hit"]] <> ["npm/merge-cold", "pypi/index-cold"]
        it "finds a job by name in any phase" $
            withSweep $ \sweep -> do
                jobArms <$> sweepJob sweep "D1" `shouldBe` Just ("anchor" :| ["d-n8", "d-n4-area32", "d-n4", "anchor"])
                sweepJob sweep "Z9" `shouldBe` Nothing

    describe "decodeSweep" $ do
        it "reads a sweep whose jobs open and close with the anchor" $
            decoded [] [] `shouldSatisfy` isRight
        it "refuses a job that names no arm, or that does not open and close with the anchor" $
            for_ [["anchor", "missing", "anchor"], ["test", "anchor"], ["anchor", "test"], ["anchor", "anchor"]] $ \arms ->
                decoded [] [jobValue "J1" arms] `shouldSatisfy` isLeft
        it "refuses a name used twice and a name the runner cannot put in a path" $ do
            decoded [] [jobValue "A" ["anchor", "test", "anchor"]] `shouldSatisfy` isLeft
            decoded ["arms" .= object ["anchor" .= armValue "1gib", "test" .= armValue "2gib", "test arm" .= armValue "1gib"]] [] `shouldSatisfy` isLeft
        it "refuses a scored scenario the sweep does not run, and an anchor that names no arm" $ do
            decoded ["scored" .= ["npm/herd" :: Text]] [] `shouldSatisfy` isLeft
            decoded ["anchor" .= ("absent" :: Text)] [] `shouldSatisfy` isLeft

withSweep :: (Sweep -> Expectation) -> Expectation
withSweep check = loadSweep "capability-1634" >>= either (expectationFailure . toString) check

-- Each test arm's jobs and positions between a job's two anchor slots.
placements :: Phase -> Map Text [(Text, Int)]
placements phase =
    Map.fromListWith (<>) [(name, [(jobName j, position)]) | j <- toList (phaseJobs phase), (position, name) <- zip [1 ..] (testSlots (toList (jobArms j)))]
  where
    testSlots arms = take (length arms - 2) (drop 1 arms)

balanced :: [(Text, Int)] -> Bool
balanced seen = length seen == 3 && Set.size (Set.fromList (map fst seen)) == 3 && Set.size (Set.fromList (map snd seen)) == 3

-- The posture the runtime resolves for an arm's cgroup and settings, with the shipped -A64m.
resolved :: Arm -> BootValues
resolved arm =
    BootValues
        { bvCapabilities = capabilities
        , bvAllocAreaMib = fst (planAllocAreaBytes plan) `div` mib
        , bvMaxHeapMib = (`div` mib) <$> fst (planMaxHeapBytes plan)
        , bvAdmission = fst (resolveServeAdmission (armServeMaxInFlight arm) capabilities)
        }
  where
    tuning = armTuning arm
    processors = maybe 4 (Set.size . cpuListMembers) (ptCpus tuning)
    posture = RtsPosture{rpCapabilities = processors, rpProcessors = processors, rpAllocAreaBytes = fromMaybe 64 (ptAllocAreaMib tuning) * mib, rpNurseryChunkBytes = Just (4 * mib), rpMaxHeapBytes = Nothing}
    limits = CgroupLimits (fromIntegral <$> podCpuQuota (armShape arm)) (podMemoryLimit (armShape arm))
    plan = resolveRuntimePlan (RuntimeOverrides (ptCores tuning) Nothing (ptMaxHeapBytes tuning)) limits posture
    capabilities = fst (planCapabilities plan)

-- A sweep with an anchor arm and one test arm, the given keys replaced, and the given jobs added to its one phase.
decoded :: [Pair] -> [Value] -> Either Text Sweep
decoded overrides jobs = decodeSweep (encode (object (overrides <> filter ((`notElem` map fst overrides) . fst) base)))
  where
    base =
        [ "name" .= ("test" :: Text)
        , "upstreamLatencyMs" .= (5 :: Int)
        , "passes" .= ("loaded" :: Text)
        , "scenarios" .= ["npm/merge-cold" :: Text]
        , "scored" .= ["npm/merge-cold" :: Text]
        , "anchor" .= ("anchor" :: Text)
        , "arms" .= object ["anchor" .= armValue "1gib", "test" .= armValue "2gib"]
        , "phases" .= [object ["phase" .= ("A" :: Text), "jobs" .= (jobValue "A1" ["anchor", "test", "anchor"] : jobs)]]
        ]

armValue :: Text -> Value
armValue shape =
    object
        [ "shape" .= shape
        , "cpus" .= ("2-3" :: Text)
        , "cores" .= (2 :: Int)
        , "podMemoryMib" .= (1024 :: Int)
        , "referenceShape" .= ("2cpu-1gib" :: Text)
        , "expect" .= object ["capabilities" .= (2 :: Int), "allocAreaMib" .= (64 :: Int), "maxHeapMib" .= (832 :: Int), "admission" .= (40 :: Int)]
        ]

jobValue :: Text -> [Text] -> Value
jobValue name arms = object ["job" .= name, "arms" .= arms]
