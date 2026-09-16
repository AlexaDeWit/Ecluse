-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.Sweep.PacingSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Registry.Maintenance.Budget (
    QuotaDimension (AccountReads, AccountWrites, NameListing, StoreRequests, TokenReads, VersionListing),
    QuotaOrigin (QuotaDeclared, QuotaDocumented),
    RequestKind (DeleteBatch, ListingPage, ManifestRead, VersionPage),
    RequestTally,
    StoreBudget (StoreBudget, bgCosts, bgOrigin, bgQuotas, bgScope),
    freePace,
    mkQuotaScope,
    oneRequest,
    paceSeconds,
    undeclaredBudget,
 )
import Ecluse.Core.Registry.Sweep.Pacing (
    BudgetShortfall (NeedsFraction, WorkFillsAllowance),
    PaceDecision (pdFraction, pdPace, pdShortfall),
    budgetFraction,
    ceilingsFor,
    cycleAllowance,
    cycleDemand,
    decidePace,
    defaultCycleWindow,
    renderPaceDecision,
 )
import Ecluse.Core.Registry.Sweep.Types (
    SweepPacing (SweepPacing, swpBudgetFraction, swpChunkPause, swpChunkSize, swpCyclePause, swpCycleWindow, swpDeletionCap, swpShape),
    SweepShape (SweepCandidates),
 )

spec :: Spec
spec = do
    windowSpec
    fractionSpec
    demandSpec
    decisionSpec
    warningSpec

{- The window covers the rest of the running cycle, the pause, and the next cycle, so a complete
cycle gets half of what the pause leaves. -}
windowSpec :: Spec
windowSpec = describe "the target cycle window" $ do
    it "grants each active cycle the same allowance as the idle interval" $ do
        defaultCycleWindow 3600 `shouldBe` 10800
        cycleAllowance shipped `shouldBe` 3600

    it "leaves a window no wider than the cycle pause with nothing to spend" $
        cycleAllowance shipped{swpCycleWindow = 3600} `shouldBe` 0

fractionSpec :: Spec
fractionSpec = describe "the request budget fraction" $ do
    it "derives the share from the tightest quota and the existing package pace" $
        budgetFraction shipped codeArtifact `shouldBe` 1 % 4

    it "never takes more than half a pool, whatever the pace implies" $
        budgetFraction shipped (declaredAt 10) `shouldBe` 1 % 2

    it "takes the operator's own share over the derived one" $
        budgetFraction shipped{swpBudgetFraction = Just (1 % 10)} codeArtifact `shouldBe` 1 % 10

    it "holds the sweep to its share of every declared quota" $
        ceilingsFor (1 % 4) codeArtifact
            `shouldBe` Map.fromList
                [(NameListing, 50), (VersionListing, 50), (AccountReads, 200), (AccountWrites, 25), (TokenReads, 300)]

{- One attempt costs the longest its own dimensions hold it to, and a cycle makes them one at a
time, so the costs add. -}
demandSpec :: Spec
demandSpec = describe "what a cycle's requests demand" $ do
    it "adds each kind's own serial cost at the ceiling" $
        cycleDemand (ceilingsFor (1 % 4) codeArtifact) codeArtifact sampleCycle `shouldBe` 1544 % 10

    it "demands nothing of a store that declared no capacity" $
        cycleDemand (ceilingsFor (1 % 2) undeclaredBudget) undeclaredBudget sampleCycle `shouldBe` 0

decisionSpec :: Spec
decisionSpec = describe "the pace the next cycle runs at" $ do
    it "runs at the ceiling with no sample to pace from" $ do
        let decision = decidePace shipped codeArtifact Nothing
        pdShortfall decision `shouldBe` Nothing
        pdFraction decision `shouldBe` 1 % 4
        paceSeconds (pdPace decision) ListingPage `shouldBe` 0.02

    it "imposes no wait at all on a store that declared no capacity" $
        pdPace (decidePace shipped undeclaredBudget (Just (sampleCycle, 600))) `shouldBe` freePace

    it "stretches every request by the share the measured cycle needs" $ do
        let decision = decidePace shipped codeArtifact (Just (sampleCycle, 600))
        pdShortfall decision `shouldBe` Nothing
        -- The cycle demanded 154.4 of the 3000 seconds it had left, so each request takes
        -- 3000/154.4 times its own cost at the ceiling.
        paceSeconds (pdPace decision) ListingPage `shouldBe` fromRational (75 % 193)

    it "warns and stays at the ceiling when the window needs more than the share allows" $ do
        let decision = decidePace shipped{swpCycleWindow = 5000} codeArtifact (Just (sampleCycle, 600))
        pdShortfall decision `shouldBe` Just (NeedsFraction (193 % 500))
        paceSeconds (pdPace decision) ListingPage `shouldBe` 0.02

    it "warns that no request share reaches a window its own work already fills" $
        pdShortfall (decidePace shipped codeArtifact (Just (sampleCycle, 4000)))
            `shouldBe` Just WorkFillsAllowance

warningSpec :: Spec
warningSpec = describe "the warning an unattainable window earns" $ do
    it "says nothing about a cycle the window fits" $
        renderPaceDecision shipped (decidePace shipped codeArtifact (Just (sampleCycle, 600)))
            `shouldBe` Nothing

    it "names the share the window would need and the one in force" $ do
        let tight = shipped{swpCycleWindow = 5000}
            line = renderPaceDecision tight (decidePace tight codeArtifact (Just (sampleCycle, 600)))
        fmap (T.isInfixOf "it would need 0.39 of the store's request capacity") line `shouldBe` Just True
        fmap (T.isInfixOf "above the ceiling of 0.25") line `shouldBe` Just True
        fmap (T.isInfixOf "runs on at that ceiling") line `shouldBe` Just True

    it "says no request share reaches a window the work alone fills" $
        fmap
            (T.isInfixOf "no request budget reaches the window")
            (renderPaceDecision shipped (decidePace shipped codeArtifact (Just (sampleCycle, 4000))))
            `shouldBe` Just True

-- The shipped dredger defaults: a chunk of 50 every two seconds, an hour between cycles.
shipped :: SweepPacing
shipped =
    SweepPacing
        { swpChunkSize = 50
        , swpChunkPause = 2
        , swpCyclePause = 3600
        , swpCycleWindow = 10800
        , swpBudgetFraction = Nothing
        , swpDeletionCap = 100
        , swpShape = SweepCandidates
        }

-- The documented CodeArtifact account quotas and the pools each call is charged to.
codeArtifact :: StoreBudget
codeArtifact =
    StoreBudget
        { bgScope = mkQuotaScope "acme-123456789012.d.codeartifact.us-east-1.amazonaws.com"
        , bgQuotas =
            Map.fromList
                [(NameListing, 200), (VersionListing, 200), (AccountReads, 800), (AccountWrites, 100), (TokenReads, 1200)]
        , bgOrigin = QuotaDocumented
        , bgCosts =
            Map.fromList
                [ (ListingPage, Map.fromList [(NameListing, 1), (AccountReads, 1)])
                , (VersionPage, Map.fromList [(VersionListing, 1), (AccountReads, 1)])
                , (ManifestRead, Map.fromList [(AccountReads, 1), (TokenReads, 1)])
                , (DeleteBatch, Map.fromList [(AccountWrites, 1)])
                ]
        }

-- A backend that publishes no account quota, at the capacity an operator declared for it.
declaredAt :: Rational -> StoreBudget
declaredAt capacity =
    undeclaredBudget
        { bgQuotas = Map.fromList [(StoreRequests, capacity)]
        , bgOrigin = QuotaDeclared
        , bgCosts = Map.fromList [(kind, Map.fromList [(StoreRequests, 1)]) | kind <- [minBound .. maxBound]]
        }

{- A measured cycle over six thousand candidate packages: a hundred name pages, one version page
and one manifest read per package, and sixty delete batches. -}
sampleCycle :: RequestTally
sampleCycle =
    mconcat
        [ times 100 ListingPage
        , times 6000 VersionPage
        , times 6000 ManifestRead
        , times 60 DeleteBatch
        ]
  where
    times n kind = mconcat (replicate n (oneRequest kind))
