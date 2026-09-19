-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.Sweep.PacingSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Text qualified as T
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Ecluse.Core.Registry.Maintenance.Budget (
    QuotaDimension (AccountReads, AccountWrites, NameListing, StoreRequests, TokenReads, VersionListing),
    QuotaOrigin (QuotaDeclared, QuotaDerived, QuotaDocumented),
    RequestKind (DeleteBatch, ListingPage, ManifestRead, VersionPage),
    RequestTally,
    StoreBudget (StoreBudget, bgCosts, bgOrigin, bgQuotas, bgScope),
    mkQuotaScope,
    oneRequest,
    requestKinds,
    undeclaredBudget,
 )
import Ecluse.Core.Registry.Maintenance.Budget.Internal (freePace, paceSeconds)
import Ecluse.Core.Registry.Sweep.Pacing (
    PaceDecision (pdFraction, pdPace, pdShortfall),
    decidePace,
    defaultCycleWindow,
    derivedCapacity,
    nominalPackagePace,
    renderPaceDecision,
    renderScopeBudget,
 )
import Ecluse.Core.Registry.Sweep.Pacing.Internal (
    BudgetShortfall (NeedsFraction, WorkFillsAllowance),
    budgetFraction,
    ceilingsFor,
    cycleAllowance,
    cycleDemand,
 )
import Ecluse.Core.Registry.Sweep.Types (
    SweepPacing (SweepPacing, swpBudgetFraction, swpChunkPause, swpChunkSize, swpCyclePause, swpCycleWindow, swpDeletionCap, swpShape),
    SweepShape (SweepCandidates),
 )

spec :: Spec
spec = do
    windowSpec
    derivedSpec
    fractionSpec
    demandSpec
    decisionSpec
    warningSpec
    bootLineSpec

{- The window covers the rest of the running cycle, the pause, and the next cycle, so a complete
cycle gets half of what the pause leaves. -}
windowSpec :: Spec
windowSpec = describe "the target cycle window" $ do
    it "grants each active cycle the same allowance as the idle interval" $ do
        defaultCycleWindow 3600 `shouldBe` 10800
        cycleAllowance shipped `shouldBe` 3600

    it "leaves a window no wider than the cycle pause with nothing to spend" $
        cycleAllowance shipped{swpCycleWindow = 3600} `shouldBe` 0

{- A backend that publishes no quota is paced at the sweep's own package pace, so the chunk keys
stay the one dial an operator has over how hard a cycle leans on such a store. -}
derivedSpec :: Spec
derivedSpec = describe "the capacity a backend publishing no quota is derived" $ do
    it "reads the nominal package pace off the chunk keys, under the chunk-pause floor" $ do
        nominalPackagePace 50 2 `shouldBe` 25
        nominalPackagePace 50 10 `shouldBe` 5
        nominalPackagePace 50 1 `shouldBe` 25

    it "gives an undeclared store that pace on the one request dimension" $ do
        let derived = derivedCapacity 25 undeclaredBudget
        bgQuotas derived `shouldBe` Map.singleton StoreRequests 25
        bgOrigin derived `shouldBe` QuotaDerived

    it "leaves a store that declares its own capacity alone" $
        derivedCapacity 25 (declaredAt 10) `shouldBe` declaredAt 10

    it "yields half the derived capacity as the ceiling, so 12.5 a second at the defaults" $ do
        let derived = derivedCapacity (nominalPackagePace 50 2) undeclaredBudget
        budgetFraction shipped derived `shouldBe` 1 % 2
        ceilingsFor (budgetFraction shipped derived) derived `shouldBe` Map.singleton StoreRequests (25 % 2)

    it "lowers the derived ceiling when the operator raises the chunk pause" $ do
        let slower = derivedCapacity (nominalPackagePace 50 10) undeclaredBudget
        ceilingsFor (budgetFraction shipped{swpChunkPause = 10} slower) slower
            `shouldBe` Map.singleton StoreRequests (5 % 2)

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

    it "holds no one request longer than a whole cycle's allowance" $ do
        let tiny = decidePace shipped (declaredAt 1) (Just (oneRequest ListingPage, 0))
        paceSeconds (pdPace tiny) ListingPage `shouldSatisfy` (<= fromRational (cycleAllowance shipped))

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

{- The boot line every store gets: where its capacity came from, the share in force and where that
came from, and the ceilings the share yields. -}
bootLineSpec :: Spec
bootLineSpec = describe "the capacity line a boot records per store" $ do
    it "names the derived inputs, the computed share, and the ceilings" $ do
        let line = renderScopeBudget shipped (derivedCapacity (nominalPackagePace 50 2) undeclaredBudget)
        line `shouldSatisfy` T.isInfixOf "capacity derived from dredger.chunkSize 50 every 2s"
        line `shouldSatisfy` T.isInfixOf "storeRequests 25.0/s"
        line `shouldSatisfy` T.isInfixOf "fraction 0.5 (computed)"
        line `shouldSatisfy` T.isInfixOf "ceilings storeRequests 12.5/s"

    it "names the backend's documented quotas and the share they imply" $ do
        let line = renderScopeBudget shipped codeArtifact
        line `shouldSatisfy` T.isInfixOf "the backend's documented quotas"
        line `shouldSatisfy` T.isInfixOf "fraction 0.25 (computed)"
        line `shouldSatisfy` T.isInfixOf "accountWrites 25.0/s"

    it "says when the share came from the configuration rather than the derivation" $
        renderScopeBudget shipped{swpBudgetFraction = Just (1 % 10)} codeArtifact
            `shouldSatisfy` T.isInfixOf "fraction 0.1 (from configuration)"

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
        , bgCosts = Map.fromList [(kind, Map.fromList [(StoreRequests, 1)]) | kind <- requestKinds]
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
