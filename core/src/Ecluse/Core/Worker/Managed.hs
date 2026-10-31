-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Durable worker records and transient inventories use the same monotonic completion facts.
module Ecluse.Core.Worker.Managed (
    readMirrorRecord,
    persistInventory,
    persistVerified,
    retainCompanion,
    verifyCompanions,
    fragmentDocument,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Codec (projectRecordWorkWeight)
import Ecluse.Core.Mirror.State.Format (MirrorStateFormat)
import Ecluse.Core.Mirror.State.Internal (retainVerifiedIdentity)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Metadata (VersionDoc (VersionDoc))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Version (Version)
import Ecluse.Core.Worker.Types (WorkerMirrorState (..), WorkerStateBudget (..))

-- | Read after the native inventory. Unchanged responses require a payload that this caller holds.
readMirrorRecord :: WorkerStateBudget -> WorkerMirrorState -> PackageName -> IO (Either MirrorStateFault (Maybe ProjectMirrorRecord))
readMirrorRecord charge binding name = do
    result <- fmap (>>= resolve) (msReadProject (wmsStore binding) (wsbState charge) name Nothing)
    case result of
        Right (Just record) -> fmap Just <$> chargeRecord charge (wmsLimits binding) (Right record)
        other -> pure other
  where
    resolve StateAbsent = Right Nothing
    resolve (StateUnchanged _) = Left StateRevisionMismatch
    resolve (StateLoaded _ record)
        | recordPackage record == name && recordTarget record == wmsTarget binding = Right (Just record)
        | otherwise = Left StateIdentityMismatch

-- | A native upload follows this write, including pending companion declarations.
persistInventory :: WorkerStateBudget -> MirrorStateFormat -> Limits -> Maybe WorkerMirrorState -> MirrorInventory -> Maybe ProjectMirrorRecord -> IO (Either MirrorStateFault ProjectMirrorRecord)
persistInventory charge format limits binding inventory previous = case binding of
    Nothing -> pure (extendProjectRecord format limits (stateTarget "transient worker") inventory previous)
    Just managed -> recordExpectedInventory format (wmsLimits managed) (wsbState charge) (wmsTarget managed) (wmsStore managed) inventory >>= chargeRecord charge (wmsLimits managed) . fmap snd

-- | Retain the bridge from a verified source digest to the digest the native target advertises.
persistVerified :: WorkerStateBudget -> Maybe WorkerMirrorState -> Version -> VerifiedArtifactBytes -> ProjectMirrorRecord -> IO (Either MirrorStateFault ProjectMirrorRecord)
persistVerified charge binding version verified record = case binding of
    Nothing -> pure (retainVerifiedIdentity version verified record)
    Just managed -> recordVerifiedArtifact (wmsLimits managed) (wsbState charge) (wmsStore managed) (recordPackage record) version verified >>= chargeRecord charge (wmsLimits managed) . fmap snd

-- | Companion objects become durable before the project points at them.
retainCompanion :: WorkerStateBudget -> WorkerMirrorState -> Version -> VerifiedCompanionBytes -> ProjectMirrorRecord -> IO (Either MirrorStateFault ProjectMirrorRecord)
retainCompanion charge managed version bytes record =
    retainProjectCompanion (wmsLimits managed) (wsbState charge) (wmsStore managed) (recordPackage record) version bytes >>= chargeRecord charge (wmsLimits managed) . fmap snd

-- | Revalidate immutable objects before acknowledgement, including retries after a lost response.
verifyCompanions :: WorkerStateBudget -> IO () -> Maybe WorkerMirrorState -> MirrorVersion -> IO (Either MirrorStateFault ())
verifyCompanions charge progress binding version = go (Map.elems (versionCompanions version))
  where
    go [] = pure (Right ())
    go (CompanionPending : _) = pure (Left StateMissingCompanion)
    go (CompanionRetained reference : rest) = case binding of
        Nothing -> pure (Left StateMissingCompanion)
        Just managed ->
            msReadCompanion (wmsStore managed) (wsbState charge) reference >>= \case
                Left fault -> pure (Left fault)
                Right bytes -> case verifyRetainedCompanion reference bytes of
                    Left fault -> pure (Left fault)
                    Right _ -> progress >> go rest

-- The initial, copied, and latest records can coexist during final validation.
chargeRecord :: WorkerStateBudget -> Limits -> Either MirrorStateFault ProjectMirrorRecord -> IO (Either MirrorStateFault ProjectMirrorRecord)
chargeRecord budget limits result = case result of
    Left fault -> pure (Left fault)
    Right record -> case projectRecordWorkWeight limits record of
        Left fault -> pure (Left fault)
        Right bytes -> do
            wsbRecords budget (fromInteger (min (toInteger (maxBound :: Int)) (3 * toInteger bytes)))
            pure (Right record)

-- | Restore the original admitted source fields for a missing historic file.
fragmentDocument :: StoredMirrorFragment -> VersionDoc
fragmentDocument fragment = VersionDoc (fragmentDetails fragment) (Just (mirrorReadDocument (fragmentReadData fragment)))
