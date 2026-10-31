-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

-- | Persisted project records and companion objects recover through real conditional S3 exchanges.
module Ecluse.Runtime.Mirror.State.S3IntegrationSpec (spec) where

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Conduit (runResourceT)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec
import UnliftIO.Async (concurrently)
import UnliftIO.STM (checkSTM)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Internal (CompanionRef (..), projectKey)
import Ecluse.Core.Package (Artifact (..), HashAlg (SHA256), PackageDetails (..), mkHash)
import Ecluse.Core.Registry.MirrorUpload (inventoryDetails, inventoryFiles)
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorInventory)
import Ecluse.Core.Registry.PyPI.Mirror (pypiStateFormat)
import Ecluse.Core.Security (Limits (..), defaultLimits)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Integration.Ministack (MinistackPool, endpointFor, withMinistack)
import Ecluse.Runtime.Aws.S3 (buildS3Env)
import Ecluse.Runtime.Mirror.State.S3 (newS3MirrorState)
import Ecluse.Runtime.Mirror.State.S3.Internal (ObjectRead (..), ObjectWrite (..), readObject, writeObject)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Mirror.State
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Poll (retryingIO)
import Ecluse.Test.Support (expectRight)

spec :: MinistackPool -> Spec
spec pool = aroundAll (withMinistack pool) $ describe "durable project mirror state" $ do
    it (caseLabel "integration/mirror-state-s3/native-001" "recovers pending companions and preserves concurrent versions, additions, and references") $ \container -> withSweeper $ \deadlines -> do
        let endpoint = Just (endpointFor container)
            bucket = "mirror-state-recovery"
            target = stateTarget "https://private.test/simple"
            fresh = newS3MirrorState endpoint deadlines defaultLimits pypiStateFormat bucket "recovery" target
        env <- buildS3Env endpoint
        retryingIO 21 500_000 (void (runResourceT (AWS.send env (S3.newCreateBucket (S3.BucketName bucket)))))
        store <- fresh
        inventory <- mirrorSample [1, 2, 3] True "https://original.test/" "original"
        (_, initial) <- recordExpectedInventory pypiStateFormat defaultLimits (const pass) target store inventory >>= expectRight
        restarted <- fresh
        (_, observed) <- loaded restarted
        observed `shouldBe` initial
        let file :| _ = inventoryFiles inventory
        verified <- expectRight (verifyCompanionBytes defaultLimits file "metadata")
        let refused = restarted{msRetainCompanion = \_ _ -> pure (Left (StateInvalid "injected storage failure"))}
        retainProjectCompanion defaultLimits (const pass) refused mirrorPackage mirrorVersion verified `shouldReturn` Left (StateInvalid "injected storage failure")
        (_, pendingRecord) <- loaded restarted
        pendingRecord `shouldBe` initial
        reference <- msRetainCompanion restarted (const pass) verified >>= expectRight
        afterObject <- fresh
        (_, withoutReference) <- loaded afterObject
        withoutReference `shouldBe` initial
        msReadCompanion afterObject (const pass) reference `shouldReturn` Right "metadata"
        reduced <- mirrorSample [1, 2] True "https://changed.test/" "changed"
        (_, retained) <- recordExpectedInventory pypiStateFormat defaultLimits (const pass) target afterObject reduced >>= expectRight
        retained `shouldBe` initial
        additions <- mirrorSample [1, 2, 3, 4] True "https://changed.test/" "changed"
        another <- mirrorSample [1] False "https://second.test/" "second"
        let details = inventoryDetails another
            versionTwo = mkVersion PyPI "2"
            renamed = fmap (\artifact -> artifact{artFilename = T.replace "1.0" "2.0" (artFilename artifact), artUrl = T.replace "1.0" "2.0" (artUrl artifact)}) (pkgArtifacts details)
        nextVersion <- expectRight (restoreMirrorInventory details{pkgVersion = versionTwo, pkgArtifacts = renamed} (mirrorSource renamed "second"))
        arrivals <- newTVarIO (0 :: Int)
        conflicts <- newIORef (0 :: Int)
        additionStore <- firstWriteBarrier arrivals conflicts afterObject
        versionStore <- firstWriteBarrier arrivals conflicts afterObject
        companionStore <- firstWriteBarrier arrivals conflicts afterObject
        (added, (versionAdded, companionAdded)) <-
            concurrently
                (recordExpectedInventory pypiStateFormat defaultLimits (const pass) target additionStore additions)
                ( concurrently
                    (recordExpectedInventory pypiStateFormat defaultLimits (const pass) target versionStore nextVersion)
                    (retainProjectCompanion defaultLimits (const pass) companionStore mirrorPackage mirrorVersion verified)
                )
        traverse_ expectRight [added, versionAdded, companionAdded]
        readIORef conflicts >>= (`shouldSatisfy` (>= 2))
        final <- fresh
        (_, completeRecord) <- loaded final
        Map.keys (recordVersions completeRecord) `shouldBe` ["1", "2"]
        version <- maybe (fail "missing first version") pure (recordVersion mirrorVersion completeRecord)
        Map.keys (versionFiles version) `shouldBe` map mirrorFilename [1, 2, 3, 4]
        Map.lookup (mirrorFilename 1) (versionCompanions version) `shouldBe` Just (CompanionRetained reference)
        Map.lookup (mirrorFilename 3) (versionCompanions version) `shouldBe` Just CompanionPending
        msReadCompanion final (const pass) reference `shouldReturn` Right "metadata"
        missingDigest <- expectRight (mkHash SHA256 (hexSha256Of "never retained"))
        msReadCompanion final (const pass) (CompanionRef missingDigest 14) `shouldReturn` Left StateMissingCompanion
        let CompanionRef digest size = reference
        msReadCompanion final (const pass) (CompanionRef digest (size + 1)) >>= (`shouldSatisfy` invalidState)
        let objectKey = "recovery/companions/sha256/" <> companionDigest reference
        object <- readObject (const pass) deadlines env defaultLimits bucket objectKey Nothing >>= expectRight
        case object of
            ObjectLoaded revision _ -> do
                writeObject (const pass) deadlines env defaultLimits bucket objectKey (Just revision) "corrupt" >>= expectRight >>= \case
                    ObjectStored _ -> pass
                    ObjectConflict -> expectationFailure "the corruption fixture lost its known revision"
                msReadCompanion final (const pass) reference >>= (`shouldSatisfy` invalidState)
            other -> expectationFailure ("missing retained companion: " <> show other)

    it (caseLabel "integration/mirror-state-s3/native-002" "reconciles a lost successful write and rejects malformed or oversized persisted records") $ \container -> withSweeper $ \deadlines -> do
        let endpoint = Just (endpointFor container)
            bucket = "mirror-state-faults"
            target = stateTarget "https://private.test/simple"
            fresh prefix limits = newS3MirrorState endpoint deadlines limits pypiStateFormat bucket prefix target
        env <- buildS3Env endpoint
        retryingIO 21 500_000 (void (runResourceT (AWS.send env (S3.newCreateBucket (S3.BucketName bucket)))))
        store <- fresh "lost" defaultLimits
        lost <- newIORef True
        writes <- newIORef (0 :: Int)
        let uncertain =
                store
                    { msWriteProject = \charge name revision record -> do
                        modifyIORef' writes (+ 1)
                        result <- msWriteProject store charge name revision record
                        case result of
                            Right (StateStored _) -> do
                                discard <- atomicModifyIORef' lost (False,)
                                pure (if discard then Left (StateTransport (transportFault TransportUnreachable "injected lost successful reply")) else result)
                            _ -> pure result
                    }
        inventory <- mirrorSample [1, 2, 3] False "https://original.test/" "original"
        (_, persisted) <- recordExpectedInventory pypiStateFormat defaultLimits (const pass) target uncertain inventory >>= expectRight
        readIORef writes `shouldReturn` 1
        restarted <- fresh "lost" defaultLimits
        snd <$> loaded restarted `shouldReturn` persisted
        let malformedKey = "malformed/" <> projectKey target mirrorPackage <> "/project.json"
        writeObject (const pass) deadlines env defaultLimits bucket malformedKey Nothing "{broken" >>= expectRight >>= \case
            ObjectStored _ -> pass
            ObjectConflict -> expectationFailure "malformed fixture unexpectedly exists"
        malformed <- fresh "malformed" defaultLimits
        msReadProject malformed (const pass) mirrorPackage Nothing >>= (`shouldSatisfy` invalidState)
        limited <- fresh "lost" defaultLimits{maxMetadataBytes = 8}
        msReadProject limited (const pass) mirrorPackage Nothing >>= (`shouldSatisfy` boundedState)

loaded :: MirrorState -> IO (StateRevision, ProjectMirrorRecord)
loaded store =
    msReadProject store (const pass) mirrorPackage Nothing >>= expectRight >>= \case
        StateLoaded revision record -> pure (revision, record)
        other -> fail ("expected a loaded project record, got " <> show other)

firstWriteBarrier :: TVar Int -> IORef Int -> MirrorState -> IO MirrorState
firstWriteBarrier arrivals conflicts store = do
    firstWrite <- newIORef True
    pure
        store
            { msWriteProject = \charge name revision record -> do
                initialWrite <- atomicModifyIORef' firstWrite (False,)
                when initialWrite $ do
                    atomically (modifyTVar' arrivals (+ 1))
                    atomically (readTVar arrivals >>= checkSTM . (>= 3))
                result <- msWriteProject store charge name revision record
                when (result == Right StateConflict) (atomicModifyIORef' conflicts (\count -> (count + 1, ())))
                pure result
            }

invalidState :: Either MirrorStateFault a -> Bool
invalidState (Left (StateInvalid _)) = True
invalidState _ = False

boundedState :: Either MirrorStateFault a -> Bool
boundedState (Left (StateBound _)) = True
boundedState _ = False
