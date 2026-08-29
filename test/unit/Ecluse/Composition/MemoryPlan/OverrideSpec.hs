-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.MemoryPlan.OverrideSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.MemoryPlan.Internal (OverridePins (..), TenantDemands (..))
import Ecluse.Composition.MemoryPlan.Override (attributeOverrideViolations, noOverridePins, overrideMinShedSum, overrideSubstitutions)

spec :: Spec
spec = do
    describe "attributeOverrideViolations (the refusal decision, in isolation)" $ do
        it "refuses on the joint check, naming the single culprit whose removal fits" $ do
            -- The pinned plan overshoots by 40 and fits once the pin is removed, so the message
            -- names the pin and reports only those 40 bytes.
            let violations = attributeOverrideViolations 1000 40 0 [("cache.maxBytes", 0)]
            violations `shouldSatisfy` (not . null)
            violations `shouldSatisfy` any (T.isInfixOf "cache.maxBytes")
            violations `shouldSatisfy` any (T.isInfixOf "40")
            violations `shouldSatisfy` any (T.isInfixOf "override-free minimum fits")
            -- The message never claims bytes the pins do not contribute.
            violations `shouldNotSatisfy` any (T.isInfixOf "even after every computed tenant")

        it "names only the pins whose individual removal flips the verdict" $ do
            -- Removing the cache fits (0). Removing the depth does not (12 over).
            let violations = attributeOverrideViolations 1000 30 0 [("cache.maxBytes", 0), ("queue.memoryMaxDepth", 12)]
            violations `shouldSatisfy` any (T.isInfixOf "cache.maxBytes")
            violations `shouldNotSatisfy` any (T.isInfixOf "queue.memoryMaxDepth")

        it "names all pins when none alone flips the verdict (they overshoot only jointly)" $ do
            -- The override-free minimum fits, but neither single removal does (each
            -- leaves 10 over): the pins overshoot only together, so blame both.
            let violations = attributeOverrideViolations 1000 50 0 [("cache.maxBytes", 10), ("queue.memoryMaxDepth", 10)]
            violations `shouldSatisfy` any (T.isInfixOf "cache.maxBytes")
            violations `shouldSatisfy` any (T.isInfixOf "queue.memoryMaxDepth")

        it "does not refuse when the override-free minimum also overshoots (the pod is too small)" $
            -- freeOvershoot > 0: the shed ladder's degradation owns this, not a refusal.
            attributeOverrideViolations 1000 60 25 [("cache.maxBytes", 40)] `shouldBe` []

        it "does not refuse when the pinned plan already fits" $
            attributeOverrideViolations 1000 0 0 [("cache.maxBytes", 0)] `shouldBe` []

    describe "overrideMinShedSum (the substitution arithmetic, in isolation)" $ do
        it "charges nothing extra for a queue depth pinned to the floor the ladder would compute" $ do
            -- queue.memoryMaxDepth at the queue-depth floor (5000) is zero-delta.
            let demands = baseDemands{tdMemoryBacked = True}
                atFloor = overrideMinShedSum demands noOverridePins{opDepth = Just 5000}
                unpinned = overrideMinShedSum demands noOverridePins
            atFloor `shouldBe` unpinned

        it "charges nothing for a queue depth under a non-memory backend (queueCharge is zero)" $ do
            -- With no memory-backed queue the depth pin cannot move a byte, however large.
            let huge = overrideMinShedSum baseDemands noOverridePins{opDepth = Just 100000}
                unpinned = overrideMinShedSum baseDemands noOverridePins
            huge `shouldBe` unpinned

        it "charges an explicit cache its full value where a computed cache sheds to zero" $ do
            -- The reclaimable share is zero for an explicit cache: it adds its whole value.
            let withCache = overrideMinShedSum baseDemands noOverridePins{opCache = Just 12345678}
                without = overrideMinShedSum baseDemands noOverridePins
            withCache - without `shouldBe` 12345678

        it "charges an explicit artifact cap its envelope (cap x multiplier) when mirroring, nothing otherwise" $ do
            -- An explicit limits.maxArtifactBytes never sheds. It adds cap x 4 to the
            -- minimum when mirroring, and nothing when no mount mirrors.
            let mirroringDemands = baseDemands{tdMirrors = True}
                mirroring = overrideMinShedSum mirroringDemands noOverridePins{opArtifact = Just 10000000}
                notMirroring = overrideMinShedSum baseDemands noOverridePins{opArtifact = Just 10000000}
                unpinned = overrideMinShedSum mirroringDemands noOverridePins
            mirroring - unpinned `shouldBe` 40000000
            notMirroring `shouldBe` unpinned

    describe "overrideSubstitutions (the per-pin substitution)" $ do
        it "pairs each present override with the pin set that substitutes only it out" $
            overrideSubstitutions allPins
                `shouldBe` [ ("cache.maxBytes", allPins{opCache = Nothing})
                           , ("runtime.serveMaxInFlight", allPins{opAdmission = Nothing})
                           , ("limits.maxResponseBytes", allPins{opResponse = Nothing})
                           , ("limits.maxRequestBytes", allPins{opRequest = Nothing})
                           , ("queue.memoryMaxDepth", allPins{opDepth = Nothing})
                           , ("limits.maxArtifactBytes", allPins{opArtifact = Nothing})
                           ]

        it "yields no substitution for an absent override" $
            map fst (overrideSubstitutions noOverridePins{opAdmission = Just 2}) `shouldBe` ["runtime.serveMaxInFlight"]
  where
    -- The substitution arithmetic reads only the base bytes no pin moves, the computed
    -- request floor, and the tenant-presence flags. Every other demand is inert here.
    baseDemands :: TenantDemands
    baseDemands =
        TenantDemands
            { tdCeiling = 0
            , tdReserve = 100000000
            , tdFixedBuffers = 0
            , tdPins = noOverridePins
            , tdCacheDesired = 0
            , tdCacheEntriesExplicit = Nothing
            , tdMaterialDesired = 0
            , tdMaterialMinimum = 0
            , tdAdmissionDesired = 1
            , tdPublishConfigured = False
            , tdPublishDesired = 0
            , tdRequestFinal = 26214400
            , tdRequestComputed = 26214400
            , tdDepthDesired = 0
            , tdMemoryBacked = False
            , tdMirrors = False
            , tdArtifactCapDesired = 0
            , tdMirrorChargeDesired = 0
            }

    allPins :: OverridePins
    allPins =
        OverridePins{opCache = Just 1, opAdmission = Just 2, opResponse = Just 3, opRequest = Just 4, opDepth = Just 5, opArtifact = Just 6}
