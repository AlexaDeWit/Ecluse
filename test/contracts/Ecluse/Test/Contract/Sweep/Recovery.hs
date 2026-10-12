-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RecordWildCards #-}

-- | Post-cleanup reads and recovery across mirror and private-cache copies.
module Ecluse.Test.Contract.Sweep.Recovery (recoveryScenarios) where

import Data.Text qualified as T
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Install (shouldFail, shouldSucceed)
import Ecluse.Test.Contract.Sweep (listed, refusedNext)
import Ecluse.Test.Contract.Sweep.Types
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec

-- | Preserve denied siblings, residual deletes and source-dependent refill outcomes.
recoveryScenarios :: RecoveryFixture -> StoreOps store -> ClientOps store project -> SweepOps plane -> RecoveryOps plane store -> SpecWith (plane, store, store)
recoveryScenarios RecoveryFixture{..} stores@StoreOps{..} clients@ClientOps{..} SweepOps{..} RecoveryOps{..} = describe "next private reads and recovery after a grouped cleanup" $ do
    it (caseLabel (recoveryCasePrefix <> "denied-install") "serves the sibling and refuses the denied version once both stores lose it") $ \(plane, proxy, cache) -> do
        let name = packageName recoverySibling
            sibling = packageVersion recoverySibling
        storeVersions cache name `shouldReturn` sort (packageVersions recoverySibling)
        storeArtifact cache name recoveryDeniedVersion >>= (`shouldSatisfy` servedBytes)
        run <- runSweep plane Apply (IdentityPolicy [name <> "@" <> recoveryDeniedVersion])
        sweepExit run `shouldBe` ExitSuccess
        storeVersions proxy name `shouldReturn` [sibling]
        storeVersions cache name `shouldReturn` [sibling]
        (fst <$> storeArtifact cache name recoveryDeniedVersion) `shouldReturn` 404
        clientMetadataStatus proxy name `shouldReturn` 200
        (fst <$> clientArtifact proxy name sibling) `shouldReturn` 200
        refusedNext clients proxy name recoveryDeniedVersion
        clientWithProject proxy $ \project -> do
            void $ clientInstallIn project name (Just sibling) >>= shouldSucceed
            clientInstalledVersion project name `shouldReturn` Just sibling
        storeVersions proxy name `shouldReturn` [sibling]
        storeVersions cache name `shouldReturn` [sibling]

    it (caseLabel (recoveryCasePrefix <> "delete-recovery") "keeps the cache copy its backend refused and removes it on a later run") $ \(plane, proxy, cache) -> do
        let name = packageName recoveryRefused
            version = packageVersion recoveryRefused
        refused <- withDeletesRefused plane (runSweep plane Apply (IdentityPolicy [name]))
        sweepAudit refused
            `shouldSatisfy` any (T.isInfixOf (refusedDeleteTarget <> ": " <> name <> "@" <> version <> ": the backend refused the delete, HTTP 503"))
        storeVersions proxy name `shouldReturn` []
        storeVersions cache name `shouldReturn` [version]
        storeArtifact cache name version >>= (`shouldSatisfy` servedBytes)
        residual <- runSweep plane Apply (IdentityPolicy [name])
        sweepExit residual `shouldBe` ExitSuccess
        storeVersions cache name `shouldReturn` []
        refusedNext clients proxy name version
        storeVersions proxy name `shouldReturn` []
        storeVersions cache name `shouldReturn` []

    it (caseLabel (recoveryCasePrefix <> "refill") "mirrors the version again when the policy lifts and the public source still holds bytes") $ \(plane, proxy, cache) -> do
        let name = packageName recoveryReadmitted
            version = packageVersion recoveryReadmitted
        run <- runSweep plane Apply (IdentityPolicy [name])
        sweepExit run `shouldBe` ExitSuccess
        storeVersions proxy name `shouldReturn` []
        storeVersions cache name `shouldReturn` []
        withRelaxedClient plane $ \relaxed -> do
            clientWithProject relaxed $ \project -> do
                void $ clientInstallIn project name Nothing >>= shouldSucceed
                clientInstalledVersion project name `shouldReturn` Just version
            storeAwaitVersions relaxed name [version] `shouldReturn` [version]
            storeArtifact relaxed name version >>= (`shouldSatisfy` servedBytes)

    it (caseLabel (recoveryCasePrefix <> "missing-source") "leaves both stores empty when the policy lifts and no source bytes remain") $ \(plane, proxy, cache) -> do
        let name = packageName recoveryMissing
            version = packageVersion recoveryMissing
        run <- runSweep plane Apply (IdentityPolicy [name])
        sweepExit run `shouldBe` ExitSuccess
        storeVersions proxy name `shouldReturn` []
        storeVersions cache name `shouldReturn` []
        withSourceWithheld plane name version $
            withRelaxedClient plane $ \relaxed -> do
                (fst <$> clientArtifact relaxed name version) `shouldReturn` 404
                clientWithProject relaxed (\project -> void (clientInstallIn project name Nothing >>= shouldFail))
                awaitWrites
                storeVersions relaxed name `shouldReturn` []
                storeVersions cache name `shouldReturn` []

    it (caseLabel (recoveryCasePrefix <> "late-copy") "removes the copies each store received after the cycle that reported them gone") $ \(plane, proxy, cache) -> do
        let name = packageName recoveryLate
            version = packageVersion recoveryLate
        removed <- runSweep plane Apply (IdentityPolicy [name])
        sweepExit removed `shouldBe` ExitSuccess
        storeVersions proxy name `shouldReturn` []
        storeVersions cache name `shouldReturn` []
        direct <- directPublisher proxy
        for_ [direct, cache] $ \store ->
            void $ clientPublish store name version >>= shouldSucceed
        for_ [proxy, cache] $ \store -> do
            storeAwaitVersions store name [version] `shouldReturn` [version]
            listed stores store [name]
        rediscovered <- runSweep plane Apply (IdentityPolicy [name])
        sweepExit rediscovered `shouldBe` ExitSuccess
        storeVersions proxy name `shouldReturn` []
        storeVersions cache name `shouldReturn` []
