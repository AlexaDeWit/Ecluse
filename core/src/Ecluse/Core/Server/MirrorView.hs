-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Reconcile durable source fields against this caller's current private inventory.
module Ecluse.Core.Server.MirrorView (
    MirrorView,
    MirrorRetention,
    newMirrorView,
    mirrorViewDocuments,
    newMirrorRetention,
    mirrorRetentionWeight,
    validateMirrorProject,
    mirrorProjectViews,
    mirrorVersionView,
    publicMirrorCoverage,
    readMirrorCompanion,
) where

import Crypto.Hash (hashInit, hashUpdate)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as TE
import Data.Time (NominalDiffTime)
import Data.Traversable (for)
import UnliftIO (evaluate, tryAny)

import Ecluse.Core.Mirror.Documents (MirrorDocuments (..))
import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Format (MirrorStateFormat)
import Ecluse.Core.Mirror.State.Internal (StateRevision (..), targetKey)
import Ecluse.Core.Package (Artifact (..), PackageDetails (..), PackageInfo (..), PackageName)
import Ecluse.Core.Registry.CachedDocument (weighCachedDoc)
import Ecluse.Core.Registry.Metadata (Manifest (..), VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload (expectedTargetMatchesWith)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Server.Admission.Meter (MemoryTicket, charge, newChargeRegion)
import Ecluse.Core.Server.Cache.Backend (Recency (RefreshRecency), RetentionOperations (..))
import Ecluse.Core.Server.Cache.Backend.Local (LocalPool, newPooledRetention)
import Ecluse.Core.Server.Cache.Types (CacheKey, Source (Source), StoreBudget, cacheKeyIdentity, fullKey)
import Ecluse.Core.Server.Cache.VersionWeight.Fields (nameWeight, textWeight)
import Ecluse.Core.Server.MemoryModel (saturatingCharge)
import Ecluse.Core.Snapshot (digestBytes, digestFromContext)
import Ecluse.Core.Version (Version)

-- | This store holds source-derived records only, never private metadata or access decisions.
type MirrorRetention = RetentionOperations CacheKey (StateRevision, ProjectMirrorRecord, Int)

-- | One configured target partitions a bounded store from every other target.
data MirrorView = MirrorView Limits StateTarget MirrorState MirrorRetention MirrorStateFormat MirrorDocuments

-- | Bind a store dedicated to this target and its adapter's state format and document operations. Its pool must share the metadata retention budget.
newMirrorView :: Limits -> StateTarget -> MirrorState -> MirrorRetention -> MirrorStateFormat -> MirrorDocuments -> MirrorView
newMirrorView = MirrorView

-- | The bound adapter's operations for restoring and assembling managed documents.
mirrorViewDocuments :: MirrorView -> MirrorDocuments
mirrorViewDocuments (MirrorView _ _ _ _ _ documents) = documents

-- | TTL controls retention only. Every request still validates the retained revision.
newMirrorRetention :: LocalPool -> NominalDiffTime -> StoreBudget -> IO MirrorRetention
newMirrorRetention pool ttl budget = newPooledRetention pool ttl budget (\(_, _, weight) -> weight)

-- | Call only after private authorisation. A cached absence or an unsolicited 304 cannot prove state.
validateMirrorProject :: MirrorView -> MemoryTicket -> PackageName -> IO (Either MirrorStateFault (Maybe (StateRevision, ProjectMirrorRecord)))
validateMirrorProject (MirrorView limits target store cache format _) ticket name = do
    let key = fullKey (Source (show target)) name
    attempted <- tryAny $ do
        held <- roLookup cache (const pass) RefreshRecency key
        for_ held (\(_, _, weight) -> charge ticket weight)
        stateCharge <- newChargeRegion (charge ticket)
        msReadProject store stateCharge name (fmap (\(revision, _, _) -> revision) held) >>= \case
            Left fault -> pure (Left fault)
            Right StateAbsent -> pure (Right Nothing)
            Right (StateUnchanged revision) -> case held of
                Just (previous, record, _) | revision == previous -> pure (Right (Just (revision, record)))
                _ -> pure (Left StateRevisionMismatch)
            Right (StateLoaded revision record)
                | recordTarget record /= target || recordPackage record /= name -> pure (Left StateIdentityMismatch)
                | otherwise -> case projectRecordWeight format limits record of
                    Left fault -> pure (Left fault)
                    Right weight -> do
                        let StateRevision revisionTarget revisionName bytes = revision
                            retained = mirrorRetentionWeight key revision weight
                        charge ticket retained
                        owned <- evaluate (StateRevision revisionTarget revisionName (BS.copy bytes))
                        roInsert cache (const pass) pass key (owned, record, retained)
                        pure (Right (Just (owned, record)))
    pure (fromRight (Left (StateInvalid "mirror state read failed")) attempted)

-- | The revision bytes must be owned. Saturated entries remain uncacheable.
mirrorRetentionWeight :: CacheKey -> StateRevision -> Int -> Int
mirrorRetentionWeight key (StateRevision target name bytes) recordWeight =
    saturatingCharge
        ( toInteger recordWeight
            + 256
            + toInteger (BS.length bytes)
            + toInteger (SBS.length (cacheKeyIdentity key))
            + textWeight (targetKey target)
            + nameWeight name
        )

-- | Keep unmanaged packed input separate. All restored entries receive fresh project-wide coordinates.
mirrorProjectViews :: MirrorDocuments -> StateRevision -> ProjectMirrorRecord -> Maybe Manifest -> Either MirrorStateFault (Maybe Manifest, Maybe Manifest, Map Text MirrorVersion)
mirrorProjectViews documents revision record private = do
    let current = maybe Map.empty (infoVersions . manifestInfo) private
        complete version mirror = checkMirrorCompletion mirror (maybe [] (toList . pkgArtifacts) (Map.lookup version current)) == MirrorComplete
        (ready, pending) = Map.partitionWithKey complete (recordVersions record)
        unmanaged = fmap (\manifest -> manifest{manifestInfo = (manifestInfo manifest){infoVersions = Map.withoutKeys current (Map.keysSet (recordVersions record))}}) private
    restored <- for (Map.toAscList ready) $ \(version, mirror) -> do
        let files = maybe [] (toList . pkgArtifacts) (Map.lookup version current)
        doc <- maybeToRight (StateInvalid "cannot restore paired mirror source") (mdRestore documents mirror files)
        pure (version, doc)
    let (_, rebased) = mapAccumL (rekeyVersion documents) 0 (mdOrder documents restored)
        details = Map.fromList [(key, vdDetails doc) | (key, doc) <- rebased]
        managedRaw = mdAssemble documents record (map snd rebased)
        fingerprint = TE.encodeUtf8 (show revision) <> maybe mempty (digestBytes . manifestDigest) private
        managed = Manifest (PackageInfo (recordPackage record) details Map.empty []) managedRaw (fromIntegral (weighCachedDoc managedRaw)) mempty (digestFromContext (hashUpdate hashInit fingerprint))
    pure (unmanaged, managed <$ guard (not (Map.null details)), pending)

rekeyVersion :: MirrorDocuments -> Int -> (Text, VersionDoc) -> (Int, (Text, VersionDoc))
rekeyVersion documents start (version, doc) =
    let details = vdDetails doc
        artifacts = pkgArtifacts details
        indexed = zip (map (mdEntryKey documents) [start ..]) (toList artifacts)
        keys = Map.fromList [(artEntryKey artifact, key) | (key, artifact) <- indexed]
        rekey key = Map.findWithDefault key key keys
        changed = fmap (\artifact -> artifact{artEntryKey = rekey (artEntryKey artifact)}) artifacts
     in (start + length artifacts, (version, doc{vdDetails = details{pkgArtifacts = changed}, vdRaw = vdRaw doc >>= mdRekey documents rekey}))

-- | An unmanaged version keeps its existing path. A managed version requires the shared complete proof.
mirrorVersionView :: MirrorDocuments -> Version -> ProjectMirrorRecord -> Maybe VersionDoc -> Either MirrorStateFault (Maybe VersionDoc)
mirrorVersionView documents version record current = case recordVersion version record of
    Nothing -> Right current
    Just mirror -> do
        let files = maybe [] (toList . pkgArtifacts . vdDetails) current
        unless (checkMirrorCompletion mirror files == MirrorComplete) (Left (StateInvalid "managed mirror version is incomplete"))
        Just <$> maybeToRight (StateInvalid "cannot restore paired mirror source") (mdRestore documents mirror files)

-- | Public fallback must cover every retained expected file with matching identity after policy admission.
publicMirrorCoverage :: Map Text MirrorVersion -> PackageInfo -> Bool
publicMirrorCoverage expected info = all covered (Map.toList expected)
  where
    covered (version, mirror) = case Map.lookup version (infoVersions info) of
        Nothing -> False
        Just details ->
            let files = Map.fromListWith (<>) [(artFilename artifact, [artifact]) | artifact <- toList (pkgArtifacts details)]
             in all (matches mirror files) (Map.toList (versionFiles mirror))
    matches mirror files (name, expectedFile) = case Map.lookup name files of
        Nothing -> False
        Just candidates -> all (expectedTargetMatchesWith expectedFile (Map.lookup name (versionVerifiedIdentities mirror))) candidates

-- | Read a retained declared companion only after the parent version passed current private proof.
readMirrorCompanion :: MirrorView -> MemoryTicket -> ProjectMirrorRecord -> Version -> Text -> IO (Either MirrorStateFault ByteString)
readMirrorCompanion (MirrorView _ _ store _ _ _) ticket record version filename =
    case recordVersion version record >>= Map.lookup filename . versionCompanions of
        Just (CompanionRetained reference) -> do
            stateCharge <- newChargeRegion (charge ticket)
            attempted <- tryAny (msReadCompanion store stateCharge reference)
            pure (either (const (Left StateMissingCompanion)) (>>= verifyRetainedCompanion reference) attempted)
        _ -> pure (Left StateMissingCompanion)
