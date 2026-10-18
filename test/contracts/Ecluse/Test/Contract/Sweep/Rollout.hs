-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RecordWildCards #-}

-- | Eventual cleanup and accepted byte loss when process policies diverge.
module Ecluse.Test.Contract.Sweep.Rollout (rolloutScenarios) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Install (shouldFail, shouldSucceed)
import Ecluse.Test.Contract.Sweep (inventoriesEmpty, refusedNext)
import Ecluse.Test.Contract.Sweep.Types
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec

-- | Roles can disagree until a later sweep, and no role can recreate absent source bytes.
rolloutScenarios :: RolloutFixture -> StoreOps store -> ClientOps store project -> SweepOps plane -> RecoveryOps plane store -> RolloutOps plane store -> SpecWith (plane, store, store)
rolloutScenarios fixture@RolloutFixture{..} stores@StoreOps{..} clients@ClientOps{..} SweepOps{..} RecoveryOps{..} roles@RolloutOps{..} = describe "eventual cleanup after out-of-order role updates" $ do
    it (caseLabel (rolloutCasePrefix <> "old-worker") "removes the copies an old worker mirrored after a stricter proxy started") $ \(plane, _, cache) -> do
        queueUrl <- rolloutQueue plane
        let target = packageName rolloutTarget
            version = packageVersion rolloutTarget
            unaffected = packageName rolloutUnaffected
            denied = IdentityPolicy [target, rolloutFirstPartyName <> "@" <> rolloutFirstPartyVersion]
        withServingRole plane queueUrl PermissivePolicy $ \old -> do
            for_ [target, unaffected] $ \name -> void (clientInstall old name Nothing >>= shouldSucceed)
            void $ clientPublish old rolloutFirstPartyName rolloutFirstPartyVersion >>= shouldSucceed

            storeVersions old target `shouldReturn` []
        withServingRole plane queueUrl (IdentityPolicy [target]) $ \strict -> do
            refusedNext clients strict target version
            -- One worker spans the sweep. The emulator hands the next job to a stopped worker's
            -- pending long poll, so a fresh worker would wait out the visibility timeout.
            withWorkerRole plane queueUrl $ \worker -> do
                outsideSweeps plane $ do
                    for_ [target <> "@" <> version, unaffected <> "@" <> packageVersion rolloutUnaffected] (\release -> awaitPublished worker release `shouldReturn` True)
                    storeAwaitVersions strict target [version] `shouldReturn` [version]
                    storeVersions cache target `shouldReturn` [version]

                    clientWithProject strict $ \project -> do
                        void $ clientInstallIn project target (Just version) >>= shouldSucceed
                        clientInstalledVersion project target `shouldReturn` Just version
                    served <- clientArtifact strict target version
                    second LBS.length served `shouldSatisfy` servedBytes
                swept <- runSweep plane Apply denied
                sweepExit swept `shouldBe` ExitSuccess
                outsideSweeps plane $ do
                    inventoriesEmpty stores strict cache target
                    storeVersions strict unaffected `shouldReturn` [packageVersion rolloutUnaffected]
                    storeVersions strict rolloutFirstPartyName `shouldReturn` [rolloutFirstPartyVersion]
                    refusedNext clients strict target version
                    nothingQueued fixture clients roles worker strict (target <> "@" <> version)
            rescanned <- runSweep plane Apply denied
            sweepExit rescanned `shouldBe` ExitSuccess
            inventoriesEmpty stores strict cache target
            storeVersions strict rolloutFirstPartyName `shouldReturn` [rolloutFirstPartyVersion]

    it (caseLabel (rolloutCasePrefix <> "old-dredger-refill") "restores a version an old Dredger removed while the newer roles permitted it") $ \(plane, proxy, cache) -> do
        let name = packageName rolloutRestored
            version = packageVersion rolloutRestored
        storeArtifact cache name version >>= (`shouldSatisfy` servedBytes)
        (retainedStatus, retained) <- storeArtifactBytes proxy name version
        retainedStatus `shouldBe` 200
        removed <- runSweep plane Apply (IdentityPolicy [name])
        sweepExit removed `shouldBe` ExitSuccess
        inventoriesEmpty stores proxy cache name

        converged <- runSweep plane Apply PermissivePolicy
        sweepExit converged `shouldBe` ExitSuccess
        inventoriesEmpty stores proxy cache name
        clientWithProject proxy $ \project -> do
            void $ clientInstallIn project name Nothing >>= shouldSucceed
            clientInstalledVersion project name `shouldReturn` Just version
        storeAwaitVersions proxy name [version] `shouldReturn` [version]
        (restoredStatus, restored) <- storeArtifactBytes proxy name version
        (restoredStatus, restored) `shouldBe` (200, retained)
        (fst <$> clientArtifact proxy name version) `shouldReturn` 200

    it (caseLabel (rolloutCasePrefix <> "old-dredger-loss") "leaves the version lost when an old Dredger removed the only remaining bytes") $ \(plane, proxy, cache) -> do
        let name = packageName rolloutLost
            version = packageVersion rolloutLost
        withSourceWithheld plane name version $ do
            (fst <$> clientArtifact proxy name version) `shouldReturn` 200
            removed <- runSweep plane Apply (IdentityPolicy [name])
            sweepExit removed `shouldBe` ExitSuccess
            inventoriesEmpty stores proxy cache name
            converged <- runSweep plane Apply PermissivePolicy
            sweepExit converged `shouldBe` ExitSuccess
            inventoriesEmpty stores proxy cache name

            (fst <$> clientArtifact proxy name version) `shouldReturn` 404
            clientWithProject proxy (\project -> void (clientInstallIn project name Nothing >>= shouldFail))
            awaitWrites
            inventoriesEmpty stores proxy cache name

-- The sentinel proves a drain after the refusal. The refused release completes once, in the drain before the sweep.
nothingQueued :: RolloutFixture -> ClientOps store project -> RolloutOps plane store -> String -> store -> Text -> IO ()
nothingQueued RolloutFixture{..} ClientOps{..} RolloutOps{..} worker proxy denied = do
    void $ clientInstall proxy (packageName rolloutSentinel) Nothing >>= shouldSucceed
    awaitPublished worker (packageName rolloutSentinel <> "@" <> packageVersion rolloutSentinel) `shouldReturn` True
    coordinate <- publicationCoordinate denied
    (T.count ("mirror version complete: " <> coordinate) <$> workerLogs worker) `shouldReturn` 1
