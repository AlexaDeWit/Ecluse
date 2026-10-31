-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Durable expected inventories distinguish complete mirrors from interrupted physical copies.
module Ecluse.Core.Mirror.State (
    StateTarget,
    stateTarget,
    StateRevision,
    ProjectMirrorRecord,
    recordTarget,
    recordPackage,
    recordVersions,
    recordEnvelope,
    recordVersion,
    StoredMirrorFragment,
    fragmentDetails,
    fragmentReadData,
    MirrorVersion,
    versionFiles,
    versionFragments,
    versionCompanions,
    versionVerifiedIdentities,
    CompanionState (..),
    CompanionRef,
    companionDigest,
    companionSize,
    VerifiedCompanionBytes,
    companionBody,
    MirrorStateFault (..),
    StateRead (..),
    StateWrite (..),
    MirrorState (..),
    StateCharge,
    extendProjectRecord,
    retainCompanionReference,
    verifyCompanionBytes,
    verifySourceCompanionBytes,
    verifyRetainedCompanion,
    checkMirrorCompletion,
    MirrorCompletion (..),
    MirrorCompletionFault (..),
    encodeProjectRecord,
    projectRecordWeight,
    recordExpectedInventory,
    retainProjectCompanion,
    recordVerifiedArtifact,
) where

import Ecluse.Core.Mirror.State.Codec
import Ecluse.Core.Mirror.State.Format (MirrorStateFormat)
import Ecluse.Core.Mirror.State.Internal
import Ecluse.Core.Package (PackageDetails, PackageName)
import Ecluse.Core.Registry.MirrorUpload (MirrorInventory, MirrorReadData, VerifiedArtifactBytes, inventoryPackage)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Server.MemoryModel (saturatingCharge)
import Ecluse.Core.Version (Version)

-- | Original admission facts remain attached to their retained source entries.
fragmentDetails :: StoredMirrorFragment -> PackageDetails
fragmentDetails (StoredMirrorFragment details _ _) = details

-- | Coordinates in this source value are local to this fragment.
fragmentReadData :: StoredMirrorFragment -> MirrorReadData
fragmentReadData (StoredMirrorFragment _ source _) = source

-- | Record additions before any new distribution upload. Collisions reread and union within a fixed budget.
recordExpectedInventory :: MirrorStateFormat -> Limits -> StateCharge -> StateTarget -> MirrorState -> MirrorInventory -> IO (Either MirrorStateFault (StateRevision, ProjectMirrorRecord))
recordExpectedInventory format limits charge target store inventory =
    updateRecord limits charge (mirrorInventoryWeight inventory) store (inventoryPackage inventory) (extendProjectRecord format limits target inventory)

-- | Store the target's usable digest before upload without changing the original served hash fields.
recordVerifiedArtifact :: Limits -> StateCharge -> MirrorState -> PackageName -> Version -> VerifiedArtifactBytes -> IO (Either MirrorStateFault (StateRevision, ProjectMirrorRecord))
recordVerifiedArtifact limits charge store name version verified =
    updateRecord limits charge 0 store name (maybeToRight (StateInvalid "missing expected inventory") >=> retainVerifiedIdentity version verified)

-- | Durable bytes precede their reference. A collision cannot drop another version or companion update.
retainProjectCompanion :: Limits -> StateCharge -> MirrorState -> PackageName -> Version -> VerifiedCompanionBytes -> IO (Either MirrorStateFault (StateRevision, ProjectMirrorRecord))
retainProjectCompanion limits charge store name version bytes =
    msRetainCompanion store charge bytes >>= \case
        Left fault -> pure (Left fault)
        Right reference -> updateRecord limits charge 0 store name (maybeToRight StateMissingCompanion >=> retainCompanionReference version bytes reference)

updateRecord :: Limits -> StateCharge -> Int -> MirrorState -> PackageName -> (Maybe ProjectMirrorRecord -> Either MirrorStateFault ProjectMirrorRecord) -> IO (Either MirrorStateFault (StateRevision, ProjectMirrorRecord))
updateRecord limits charge inputWeight store name change = go (8 :: Int) StateRetryExhausted
  where
    go 0 lastFault = pure (Left lastFault)
    go attempts _ =
        msReadProject store charge name Nothing >>= \case
            Left fault -> pure (Left fault)
            Right (StateUnchanged _) -> pure (Left StateRevisionMismatch)
            Right StateAbsent -> apply attempts Nothing Nothing
            Right (StateLoaded revision record) -> apply attempts (Just revision) (Just record)
    apply attempts revision previous = case traverse (projectRecordWorkWeight limits) previous of
        Left fault -> pure (Left fault)
        Right retained -> do
            charge (saturatingCharge (2 * (toInteger inputWeight + maybe 0 toInteger retained) + 4096))
            case change previous of
                Left fault -> pure (Left fault)
                Right updated -> case revision of
                    Just current | previous == Just updated -> pure (Right (current, updated))
                    _ ->
                        msWriteProject store charge name revision updated >>= \case
                            Right (StateStored current) -> pure (Right (current, updated))
                            Right StateConflict -> go (attempts - 1) StateRetryExhausted
                            Left fault -> go (attempts - 1) fault
