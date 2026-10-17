-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Advisory sync composed with policy evaluation and mirror sweeping.
module Ecluse.Runtime.Cve.SyncSpec (spec) where

import Data.Aeson (Value (String))
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime, UTCTime (UTCTime), addUTCTime, fromGregorian)
import Network.HTTP.Types.Status (status404)
import System.Directory (copyFile)
import System.FilePath (takeDirectory, (</>))
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)

import Ecluse.Core.Cve (AdvisoryRange (arCveId), CveDbRejected (CveDbEpssNotEstablished), CveLookup (..))
import Ecluse.Core.Cve.Slot (currentAdvisoryEtag, generationInstalledAt, withSlotGeneration)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Osv.Schema (EpssRequirement (..))
import Ecluse.Core.Osv.Stream (IngestStats (IngestStats), PilotIngestAborted (PilotIngestAborted))
import Ecluse.Core.Registry.Maintenance (StoredVersion (StoredVersion), VersionPresence (VersionServed))
import Ecluse.Core.Registry.Sweep.Package (sweepPackageGroup)
import Ecluse.Core.Registry.Sweep.Types (SweepMount (smFirstParty, smStore), newSweepState)
import Ecluse.Core.Rules (RuleDeps (..), prepare)
import Ecluse.Core.Rules.Types (DenyIfCveParams (DenyIfCveParams), DenyIfEpssParams (DenyIfEpssParams), FailureAlignment (FailDeny, FailNoDecision), RemainingAdvisories (AbstainOnRemaining), Rule (..), RuleVerdict (..), completeEvidence, mkEvalContext)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Runtime.Cve.Sync.Step (CveFetch (..), DbEtag (..), SyncEnv (..), SyncOutcome (..), syncStep)
import Ecluse.Runtime.Test.Cve (fetchServing, fetchServingAt, installedSource, publishedAt, withSyncEnv)
import Ecluse.Test.Cve (namesFix)
import Ecluse.Test.Maintenance (FakeStore (fakeMaintenance, readFakeContents), FakeStoreConfig (fakeContents, fakeManifests), defaultFakeStoreConfig, newFakeStore)
import Ecluse.Test.Osv (CorpusVersion (CorpusV1, CorpusV2), osvCorpusZip, osvZipOf)
import Ecluse.Test.Osv.Withdrawal (withdrawalBytes, withdrawalZip)
import Ecluse.Test.OsvDb (compileOsvZipDbTo, compileOsvZipDbWithFeedTo, withOsvZipDb)
import Ecluse.Test.Package (sampleDetails, sampleManifest, unscopedNpm)
import Ecluse.Test.Rules (atDefaultPrecedence, evalRule, isAllow, isDeny, isNoDecision, remediationWith, shippedRemediation, slotRuleDeps)
import Ecluse.Test.Sweep (RecordedSweep (recPorts), recordingPorts, testMount, testPacing)

withdrawalSpec :: Spec
withdrawalSpec = describe "compiled withdrawal through sync and shared policy" $ do
    it "refuses a withdrawal-only replacement and keeps the last accepted evidence" $ do
        active <- withdrawalBytes Nothing >>= \bytes -> osvZipOf [("active.json", LBS.fromStrict bytes)]
        withdrawn <- withdrawalBytes (Just (String "2024-05-14T20:15:44Z")) >>= \bytes -> osvZipOf [("active.json", LBS.fromStrict bytes)]
        withOsvZipDb Npm active $ \path ->
            withSyncEnv $ \_ slot envWith -> do
                let env = envWith (fetchServing (Just "active") (copyFile path))
                syncStep env Nothing >>= \case
                    SyncSwapped actual meta -> do
                        actual `shouldBe` DbEtag "active"
                        lookup "row_count" meta `shouldBe` Just "4"
                    other -> expectationFailure ("expected active generation swap, got " <> show other)
                accepted <- readFileBS path
                compileOsvZipDbTo Npm withdrawn (takeDirectory path)
                    `shouldThrow` (\(PilotIngestAborted stats) -> stats == IngestStats 1 0 0 0 0)
                readFileBS path `shouldReturn` accepted
                syncStep env (Just (DbEtag "active")) >>= \case
                    SyncUnchanged -> pass
                    other -> expectationFailure ("expected unchanged generation, got " <> show other)
                withSlotGeneration slot $ \case
                    Nothing -> expectationFailure "withdrawal refusal lost the synced database"
                    Just (_, lookup', _) -> do
                        namesFix lookup' "withdrawal-only" "2.0.0" `shouldReturn` True
                        rows <- cveAdvisoriesFor lookup' "withdrawal-only"
                        map arCveId rows `shouldBe` replicate 2 "GHSA-withdrawal"

    it "removes withdrawn evidence while retaining independent denies, fixes, and deletion guards" $
        withSyncEnv $ \_ slot envWith -> do
            let deps = slotRuleDeps slot
                cvss = DenyIfCve (DenyIfCveParams 5 FailDeny)
                epss = DenyIfEpss (DenyIfEpssParams 0.25 FailDeny)
            ctx <- mkEvalContext (pure (UTCTime (fromGregorian 2026 1 1) 0)) (currentAdvisoryEtag slot)
            for_ [(Nothing, "active", Nothing, True), (Just (String "2024-05-14T20:15:44Z"), "withdrawn", Just (DbEtag "active"), False)] $ \(withdrawn, etag, previous, active) -> do
                archive <- withdrawalZip withdrawn
                withOsvZipDb Npm archive $ \path -> do
                    let env = envWith (fetchServing (Just etag) (copyFile path))
                    syncStep env previous >>= \case
                        SyncSwapped actual meta -> do
                            actual `shouldBe` DbEtag etag
                            lookup "row_count" meta `shouldBe` Just (if active then "6" else "2")
                        other -> expectationFailure ("expected withdrawal generation swap, got " <> show other)
                withSlotGeneration slot $ \case
                    Nothing -> expectationFailure "withdrawal test has no synced database"
                    Just (_, lookup', _) -> do
                        namesFix lookup' "withdrawal-only" "2.0.0" `shouldReturn` active
                        namesFix lookup' "withdrawal-overlap" "2.0.0" `shouldReturn` active
                        namesFix lookup' "withdrawal-overlap" "3.0.0" `shouldReturn` True
                        namesFix lookup' "corpus-vuln" "1.2.0" `shouldReturn` True
                        rows <- cveAdvisoriesFor lookup' "withdrawal-only"
                        map arCveId rows `shouldBe` replicate (if active then 2 else 0) "GHSA-withdrawal"
                        overlapping <- cveAdvisoriesFor lookup' "withdrawal-overlap"
                        sort (ordNub (map arCveId overlapping))
                            `shouldBe` if active then ["GHSA-independent", "GHSA-withdrawal"] else ["GHSA-independent"]
                        names <- cveCoveredNames lookup'
                        ("withdrawal-only" `elem` names) `shouldBe` active
                for_ [cvss, epss] $ \rule -> do
                    for_ ["1.0.0", "4.0.0"] $ \version -> do
                        verdict <- evalRule deps ctx rule (completeEvidence (sampleDetails (unscopedNpm "withdrawal-only") (mkVersion Npm version)))
                        case verdict of
                            Deny _ _ -> active `shouldBe` True
                            NoDecision _ -> active `shouldBe` False
                            other -> expectationFailure ("unexpected withdrawal denial verdict: " <> show other)
                    for_ ["withdrawal-overlap", "corpus-vuln"] $ \name -> do
                        verdict <- evalRule deps ctx rule (completeEvidence (sampleDetails (unscopedNpm name) (mkVersion Npm "1.0.0")))
                        verdict `shouldSatisfy` \case
                            Deny _ _ -> True
                            _ -> False
                    survivesSweep deps rule False "withdrawal-only" `shouldReturn` not active
                    survivesSweep deps rule False "withdrawal-overlap" `shouldReturn` False
                    survivesSweep deps rule True "withdrawal-overlap" `shouldReturn` True
                for_ [shippedRemediation, remediationWith FailNoDecision AbstainOnRemaining] $ \remediation -> do
                    fixVerdict <- evalRule deps ctx remediation (completeEvidence (sampleDetails (unscopedNpm "withdrawal-only") (mkVersion Npm "2.0.0")))
                    case fixVerdict of
                        Allow _ -> active `shouldBe` True
                        NoDecision _ -> active `shouldBe` False
                        other -> expectationFailure ("unexpected withdrawal remediation verdict: " <> show other)
            survivesSweep deps (DenyByIdentity "withdrawal-only") False "withdrawal-only" `shouldReturn` False

-- Sweep one stored version, 1.0.0, under a single rule, and report whether it is still stored.
survivesSweep :: RuleDeps -> Rule -> Bool -> Text -> IO Bool
survivesSweep deps rule firstParty rawName = do
    let name = unscopedNpm rawName
        version = mkVersion Npm "1.0.0"
        stored = [StoredVersion version VersionServed Nothing]
    store <-
        newFakeStore
            defaultFakeStoreConfig
                { fakeContents = Map.singleton name stored
                , fakeManifests = Map.singleton name (sampleManifest name [version])
                }
    rules <- prepare deps [atDefaultPrecedence rule]
    generation <- rdCurrentAdvisoryEtag deps
    recorded <- recordingPorts generation
    counters <- newSweepState
    let handle = fakeMaintenance store
        mount = (testMount handle rules [rule]){smFirstParty = const firstParty}
    sweepPackageGroup testPacing (recPorts recorded) counters mount name [(smStore mount, stored)] `shouldReturn` Nothing
    contents <- readFakeContents store
    pure (maybe False (not . null) (Map.lookup name contents))

-- The consumer half of an optional EPSS failure: Pilot published CorpusV2 with no scores.
unavailableEnrichmentSpec :: Spec
unavailableEnrichmentSpec = describe "an artifact published with unavailable EPSS enrichment" $ do
    it "leaves a consumer that requires EPSS on its last qualified generation until an available one arrives" $
        withSyncEnv $ \dir slot envWith -> do
            (qualified, unavailable) <- corpusGenerations dir
            let required etag offset path = (servingAt envWith etag offset path){syncEpssRequirement = EpssRequired}
            syncStep (required "qualified" 0 qualified) Nothing >>= expectSwap "qualified"
            bytes <- readFileBS (syncDbPath (required "qualified" 0 qualified))
            source <- installedSource slot
            installed <- generationInstalledAt slot
            syncStep (required "unavailable" 60 unavailable) (Just (DbEtag "qualified")) >>= \case
                SyncRejected etag rejection -> do
                    etag `shouldBe` DbEtag "unavailable"
                    rejection `shouldBe` CveDbEpssNotEstablished
                other -> expectationFailure ("expected the unavailable artifact refused, got " <> show other)
            readFileBS (syncDbPath (required "qualified" 0 qualified)) `shouldReturn` bytes
            installedSource slot `shouldReturn` source
            generationInstalledAt slot `shouldReturn` installed
            currentAdvisoryEtag slot `shouldReturn` Just (DbEtag "qualified")
            withSlotGeneration slot $ \case
                Nothing -> expectationFailure "the refusal lost the qualified generation"
                Just (_, lookup', _) -> do
                    namesFix lookup' "corpus-vuln" "1.2.0" `shouldReturn` True
                    cveAdvisoriesFor lookup' "corpus-revoked" `shouldReturn` []
            available <- osvCorpusZip CorpusV2 >>= \archive -> compileOsvZipDbTo Npm archive (dir </> "available")
            syncStep (required "available" 120 available) (Just (DbEtag "unavailable")) >>= expectSwap "available"
            withSlotGeneration slot $ \case
                Nothing -> expectationFailure "the available generation was not installed"
                Just (_, lookup', _) -> cveAdvisoriesFor lookup' "corpus-revoked" >>= (`shouldSatisfy` not . null)

    it "gives a consumer without EPSS rules the new advisory and its fix" $
        withSyncEnv $ \dir slot envWith -> do
            (qualified, unavailable) <- corpusGenerations dir
            let deps = slotRuleDeps slot
                deny = DenyIfCve (DenyIfCveParams 5 FailDeny)
                -- A consumer without EPSS rules runs remediation in abstain mode.
                abstaining = remediationWith FailDeny AbstainOnRemaining
                verdictOn rule name version = do
                    ctx <- mkEvalContext (pure publishedAt) (currentAdvisoryEtag slot)
                    evalRule deps ctx rule (completeEvidence (sampleDetails (unscopedNpm name) (mkVersion Npm version)))
            syncStep (servingAt envWith "qualified" 0 qualified) Nothing >>= expectSwap "qualified"
            verdictOn deny "corpus-revoked" "1.0.0" >>= (`shouldSatisfy` isNoDecision)
            verdictOn abstaining "corpus-revoked" "1.2.0" >>= (`shouldSatisfy` isNoDecision)
            verdictOn deny "corpus-clean" "1.0.0" >>= (`shouldSatisfy` isNoDecision)
            survivesSweep deps deny False "corpus-revoked" `shouldReturn` True
            syncStep (servingAt envWith "unavailable" 60 unavailable) (Just (DbEtag "qualified")) >>= \case
                SyncSwapped etag meta -> do
                    etag `shouldBe` DbEtag "unavailable"
                    lookup "epss_status" meta `shouldBe` Just "unavailable"
                other -> expectationFailure ("expected the unavailable artifact installed, got " <> show other)
            verdictOn deny "corpus-revoked" "1.0.0" >>= (`shouldSatisfy` isDeny)
            verdictOn abstaining "corpus-revoked" "1.2.0" >>= (`shouldSatisfy` isAllow)
            verdictOn deny "corpus-clean" "1.0.0" >>= (`shouldSatisfy` isDeny)
            survivesSweep deps deny False "corpus-revoked" `shouldReturn` False

-- CorpusV1 compiled with the feed, then CorpusV2 compiled while an optional feed answers 404.
corpusGenerations :: FilePath -> IO (FilePath, FilePath)
corpusGenerations dir = do
    qualified <- osvCorpusZip CorpusV1 >>= \archive -> compileOsvZipDbTo Npm archive (dir </> "qualified")
    unavailable <- osvCorpusZip CorpusV2 >>= \archive -> compileOsvZipDbWithFeedTo Npm EpssOptional (status404, "") archive (dir </> "unavailable")
    pure (qualified, unavailable)

-- A sync environment serving a compiled artifact under an ETag, published this many seconds late.
servingAt :: (CveFetch -> SyncEnv) -> Text -> NominalDiffTime -> FilePath -> SyncEnv
servingAt envWith etag offset path = envWith (fetchServingAt (Just (addUTCTime offset publishedAt)) (Just etag) (copyFile path))

expectSwap :: Text -> SyncOutcome -> Expectation
expectSwap expected = \case
    SyncSwapped etag _ -> etag `shouldBe` DbEtag expected
    other -> expectationFailure ("expected a swap to " <> toString expected <> ", got " <> show other)

spec :: Spec
spec = do
    withdrawalSpec
    unavailableEnrichmentSpec
