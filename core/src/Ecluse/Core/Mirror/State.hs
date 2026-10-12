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
    extendProjectRecord,
    retainCompanionReference,
    verifyCompanionBytes,
    verifyRetainedCompanion,
    checkMirrorCompletion,
    MirrorCompletion (..),
    MirrorCompletionFault (..),
    encodeProjectRecord,
    decodeProjectRecord,
    projectRecordWeight,
    recordExpectedInventory,
    retainProjectCompanion,
    recordVerifiedArtifact,
) where

import Ecluse.Core.Mirror.State.Codec
import Ecluse.Core.Mirror.State.Internal
import Ecluse.Core.Package (PackageDetails, PackageName)
import Ecluse.Core.Registry.MirrorUpload (MirrorInventory, MirrorReadData, VerifiedArtifactBytes, inventoryPackage)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Version (Version)

-- | Original admission facts remain attached to their retained source entries.
fragmentDetails :: StoredMirrorFragment -> PackageDetails
fragmentDetails (StoredMirrorFragment details _ _) = details

-- | Coordinates in this source value are local to this fragment.
fragmentReadData :: StoredMirrorFragment -> MirrorReadData
fragmentReadData (StoredMirrorFragment _ source _) = source

-- | Record additions before any new distribution upload. Collisions reread and union within a fixed budget.
recordExpectedInventory :: Limits -> StateTarget -> MirrorState -> MirrorInventory -> IO (Either MirrorStateFault (StateRevision, ProjectMirrorRecord))
recordExpectedInventory limits target store inventory =
    updateRecord limits store (inventoryPackage inventory) (extendProjectRecord limits target inventory)

-- | Store the target's usable digest before upload without changing the original served hash fields.
recordVerifiedArtifact :: Limits -> MirrorState -> PackageName -> Version -> VerifiedArtifactBytes -> IO (Either MirrorStateFault (StateRevision, ProjectMirrorRecord))
recordVerifiedArtifact limits store name version verified =
    updateRecord limits store name (maybeToRight (StateInvalid "missing expected inventory") >=> retainVerifiedIdentity version verified)

-- | Durable bytes precede their reference. A collision cannot drop another version or companion update.
retainProjectCompanion :: Limits -> MirrorState -> PackageName -> Version -> VerifiedCompanionBytes -> IO (Either MirrorStateFault (StateRevision, ProjectMirrorRecord))
retainProjectCompanion limits store name version bytes =
    msRetainCompanion store bytes >>= \case
        Left fault -> pure (Left fault)
        Right reference -> updateRecord limits store name (maybeToRight StateMissingCompanion >=> retainCompanionReference version bytes reference)

updateRecord :: Limits -> MirrorState -> PackageName -> (Maybe ProjectMirrorRecord -> Either MirrorStateFault ProjectMirrorRecord) -> IO (Either MirrorStateFault (StateRevision, ProjectMirrorRecord))
updateRecord limits store name change = go (8 :: Int) StateRetryExhausted
  where
    go 0 lastFault = pure (Left lastFault)
    go attempts _ =
        msReadProject store name Nothing >>= \case
            Left fault -> pure (Left fault)
            Right (StateUnchanged _) -> pure (Left StateRevisionMismatch)
            Right StateAbsent -> apply attempts Nothing Nothing
            Right (StateLoaded revision record) -> apply attempts (Just revision) (Just record)
    apply attempts revision previous = case change previous >>= checked of
        Left fault -> pure (Left fault)
        Right updated -> case revision of
            Just current | previous == Just updated -> pure (Right (current, updated))
            _ ->
                msWriteProject store name revision updated >>= \case
                    Right (StateStored current) -> pure (Right (current, updated))
                    Right StateConflict -> go (attempts - 1) StateRetryExhausted
                    Left fault -> go (attempts - 1) fault
    checked record = encodeProjectRecord limits record $> record
