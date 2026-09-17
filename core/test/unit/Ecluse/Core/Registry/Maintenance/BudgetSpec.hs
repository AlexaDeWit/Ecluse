-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.Maintenance.BudgetSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Time (NominalDiffTime)
import Test.Hspec

import Ecluse.Core.Clock (monoSecondsBetween, monotonicNow, waitSeconds)
import Ecluse.Core.Registry.Maintenance.Budget (
    BudgetPort (budgetClose, budgetOpen, budgetPaced),
    CycleCost (ccRequests, ccWorkSeconds),
    QuotaDimension (AccountReads, AccountWrites, StoreRequests),
    QuotaOrigin (QuotaUndeclared),
    QuotaScope,
    RequestGate (gateSpend),
    RequestKind (DeleteBatch, ListingPage, ManifestRead),
    StoreBudget (bgOrigin, bgQuotas),
    budgetDeclared,
    mkQuotaScope,
    newBudgetMeter,
    oneRequest,
    paceOf,
    paceSeconds,
    parseQuotaDimension,
    parseRequestKind,
    quotaDimensionName,
    renderRequestTally,
    requestKindName,
    smallestQuota,
    tallyCounts,
    undeclaredBudget,
 )

spec :: Spec
spec = do
    vocabularySpec
    tallySpec
    meterSpec

vocabularySpec :: Spec
vocabularySpec = describe "the budget vocabulary" $ do
    it "reads back every dimension and request kind it can spell" $ do
        traverse (parseQuotaDimension . quotaDimensionName) [minBound .. maxBound]
            `shouldBe` Just [minBound .. maxBound]
        traverse (parseRequestKind . requestKindName) [minBound .. maxBound]
            `shouldBe` Just [minBound .. maxBound]

    it "refuses a dimension and a request kind this build meters nothing under" $ do
        parseQuotaDimension "diskBytes" `shouldBe` Nothing
        parseRequestKind "tarballFetch" `shouldBe` Nothing

    it "reports a store with no declared capacity as undeclared" $ do
        budgetDeclared undeclaredBudget `shouldBe` False
        bgOrigin undeclaredBudget `shouldBe` QuotaUndeclared
        smallestQuota undeclaredBudget `shouldBe` Nothing

    it "takes the tightest declared quota as the pool's own" $
        smallestQuota declaredBudget `shouldBe` Just 100

tallySpec :: Spec
tallySpec = describe "the request tally" $ do
    it "adds counts of one kind and keeps kinds apart" $
        tallyCounts (oneRequest ListingPage <> oneRequest ListingPage <> oneRequest ManifestRead)
            `shouldBe` [(ListingPage, 2), (ManifestRead, 1)]

    it "names the kinds it counted, and says so when it counted none" $ do
        renderRequestTally (oneRequest DeleteBatch) `shouldBe` "deleteBatch 1"
        renderRequestTally mempty `shouldBe` "no requests"

    it "holds a request to the seconds its pace names, and to none where it names none" $ do
        paceSeconds (paceOf (Map.fromList [(ListingPage, 1 % 4)])) ListingPage `shouldBe` 0.25
        paceSeconds (paceOf (Map.fromList [(ListingPage, 1 % 4)])) ManifestRead `shouldBe` 0

meterSpec :: Spec
meterSpec = describe "the cycle meter" $ do
    it "counts each scope's own requests and forgets them when the next cycle opens" $ do
        (port, gateFor) <- newRecordingMeter
        budgetOpen port
        gateSpend (gateFor mirror) ListingPage
        gateSpend (gateFor cache) ManifestRead
        counted <- ccRequests <$> budgetClose port
        Map.lookup mirror counted `shouldBe` Just (oneRequest ListingPage)
        Map.lookup cache counted `shouldBe` Just (oneRequest ManifestRead)
        budgetOpen port
        fresh <- ccRequests <$> budgetClose port
        Map.keys fresh `shouldBe` []

    it "waits the installed pace for the scope that installed it, and not for another" $ do
        waits <- newIORef []
        (port, gateFor) <- newMeterRecording waits
        budgetPaced port (Map.fromList [(mirror, paceOf (Map.fromList [(ListingPage, 1 % 2)]))])
        gateSpend (gateFor mirror) ListingPage
        gateSpend (gateFor mirror) ManifestRead
        gateSpend (gateFor cache) ListingPage
        readIORef waits `shouldReturn` [0.5]

    it "serves a sub-second pace through the wait the boot wires in" $ do
        (port, gateFor) <- newBudgetMeter waitSeconds
        budgetPaced port (Map.fromList [(mirror, paceOf (Map.fromList [(ListingPage, 1 % 20)]))])
        before <- monotonicNow
        gateSpend (gateFor mirror) ListingPage
        served <- monoSecondsBetween before <$> monotonicNow
        served `shouldSatisfy` (> 0.02)

    it "counts the waits it imposed out of the work it measured" $ do
        waits <- newIORef []
        (port, gateFor) <- newMeterRecording waits
        budgetOpen port
        budgetPaced port (Map.fromList [(mirror, paceOf (Map.fromList [(ListingPage, 30)]))])
        gateSpend (gateFor mirror) ListingPage
        cost <- budgetClose port
        ccWorkSeconds cost `shouldSatisfy` (< 30)

declaredBudget :: StoreBudget
declaredBudget =
    undeclaredBudget
        { bgQuotas = Map.fromList [(AccountReads, 800), (AccountWrites, 100), (StoreRequests, 200)]
        }

mirror :: QuotaScope
mirror = mkQuotaScope "mirror.example.test"

cache :: QuotaScope
cache = mkQuotaScope "cache.example.test"

-- A meter whose wait returns at once, for a case about the counts alone.
newRecordingMeter :: IO (BudgetPort, QuotaScope -> RequestGate)
newRecordingMeter = newBudgetMeter (const pass)

-- A meter that records the waits it was asked for instead of serving them.
newMeterRecording :: IORef [NominalDiffTime] -> IO (BudgetPort, QuotaScope -> RequestGate)
newMeterRecording waits = newBudgetMeter (\seconds -> modifyIORef' waits (<> [seconds]))
