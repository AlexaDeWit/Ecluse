-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Reconcile durable source fields against this caller's current private inventory.
module Ecluse.Core.Server.MirrorView (
    MirrorView,
    MirrorRetention,
    newMirrorView,
    newMirrorRetention,
    mirrorRetentionWeight,
    validateMirrorProject,
    mirrorProjectViews,
    mirrorVersionView,
    publicMirrorCoverage,
    readMirrorCompanion,
) where

import Crypto.Hash (hashInit, hashUpdate)
import Data.Aeson (Value (Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Map.Strict qualified as Map
import Data.Primitive.SmallArray (smallArrayFromList)
import Data.Text.Encoding qualified as TE
import Data.Time (NominalDiffTime)
import Data.Traversable (for)
import UnliftIO (evaluate, tryAny)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, RubyGems))
import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Internal (StateRevision (..), targetKey)
import Ecluse.Core.Package (Artifact (..), PackageDetails (..), PackageInfo (..), PackageName, pkgEcosystem)
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry, RecordEntry))
import Ecluse.Core.Registry.CachedDocument (cargoCached, cargoPacked, pypiPacked, pypiSimpleCached, rubyGemsCached, weighCachedDoc)
import Ecluse.Core.Registry.Cargo.Document (cargoDocument, cargoRecords, packCargoDocument)
import Ecluse.Core.Registry.Json.Packed (docTable)
import Ecluse.Core.Registry.Json.Writer (packTree)
import Ecluse.Core.Registry.Metadata (Manifest (..), VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData (..), expectedTargetMatchesWith)
import Ecluse.Core.Registry.PyPI.Document (packedSimple, simpleDocument, simpleEnvelope, simpleFiles, urlHole)
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (..), GemRow (..))
import Ecluse.Core.Registry.RubyGems.SourceState (gemSourcePublishedAt)
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
data MirrorView = MirrorView Limits StateTarget MirrorState MirrorRetention

-- | Bind a store dedicated to this target. Its pool must share the metadata retention budget.
newMirrorView :: Limits -> StateTarget -> MirrorState -> MirrorRetention -> MirrorView
newMirrorView = MirrorView

-- | TTL controls retention only. Every request still validates the retained revision.
newMirrorRetention :: LocalPool -> NominalDiffTime -> StoreBudget -> IO MirrorRetention
newMirrorRetention pool ttl budget = newPooledRetention pool ttl budget (\(_, _, weight) -> weight)

-- | Call only after private authorisation. A cached absence or an unsolicited 304 cannot prove state.
validateMirrorProject :: MirrorView -> MemoryTicket -> PackageName -> IO (Either MirrorStateFault (Maybe (StateRevision, ProjectMirrorRecord)))
validateMirrorProject (MirrorView limits target store cache) ticket name = do
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
                | otherwise -> case projectRecordWeight limits record of
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
mirrorProjectViews :: StateRevision -> ProjectMirrorRecord -> Maybe Manifest -> Either MirrorStateFault (Maybe Manifest, Maybe Manifest, Map Text MirrorVersion)
mirrorProjectViews revision record private = do
    let current = maybe Map.empty (infoVersions . manifestInfo) private
        complete version mirror = checkMirrorCompletion mirror (maybe [] (toList . pkgArtifacts) (Map.lookup version current)) == MirrorComplete
        (ready, pending) = Map.partitionWithKey complete (recordVersions record)
        unmanaged = fmap (\manifest -> manifest{manifestInfo = (manifestInfo manifest){infoVersions = Map.withoutKeys current (Map.keysSet (recordVersions record))}}) private
    restored <- for (Map.toAscList ready) $ \(version, mirror) -> do
        let files = maybe [] (toList . pkgArtifacts) (Map.lookup version current)
        doc <- maybeToRight (StateInvalid "cannot restore paired mirror source") (restoreVersion mirror files)
        pure (version, doc)
    let ordered = if pkgEcosystem (recordPackage record) == Cargo then sortOn ((\(artifact :| _) -> artEntryKey artifact) . pkgArtifacts . vdDetails . snd) restored else restored
        (_, rebased) = mapAccumL rekeyVersion 0 ordered
        details = Map.fromList [(key, vdDetails doc) | (key, doc) <- rebased]
        files = concat [simpleFiles document | (_, doc) <- rebased, Just raw <- [vdRaw doc], Just document <- [snd pypiSimpleCached raw]]
        packed = packedSimple (recordEnvelope record) (docTable (smallArrayFromList [])) [(key, packTree urlHole value) | (key, value) <- files]
        cargo = packCargoDocument (cargoDocument (concat [cargoRecords document | (_, doc) <- rebased, Just raw <- [vdRaw doc], Just document <- [snd cargoCached raw]]))
        rubyDocuments = [document | (_, doc) <- rebased, Just raw <- [vdRaw doc], Just document <- [snd rubyGemsCached raw]]
        ruby = GemDocument (recordPackage record) (maybe mempty gemHeader (listToMaybe rubyDocuments)) (concatMap gemRows rubyDocuments)
        managedRaw = case pkgEcosystem (recordPackage record) of
            Cargo -> fst cargoPacked cargo
            RubyGems -> fst rubyGemsCached ruby
            _ -> fst pypiPacked packed
        fingerprint = TE.encodeUtf8 (show revision) <> maybe mempty (digestBytes . manifestDigest) private
        managed = Manifest (PackageInfo (recordPackage record) details Map.empty []) managedRaw (fromIntegral (weighCachedDoc managedRaw)) (digestFromContext (hashUpdate hashInit fingerprint))
    pure (unmanaged, managed <$ guard (not (Map.null details)), pending)

rekeyVersion :: Int -> (Text, VersionDoc) -> (Int, (Text, VersionDoc))
rekeyVersion start (version, doc) =
    let details = vdDetails doc
        constructor = if pkgEcosystem (pkgName details) `elem` [Cargo, RubyGems] then RecordEntry else ArrayEntry
        artifacts = pkgArtifacts details
        indexed = zip (map constructor [start ..]) (toList artifacts)
        keys = Map.fromList [(artEntryKey artifact, key) | (key, artifact) <- indexed]
        rekey key = Map.findWithDefault key key keys
        changed = fmap (\artifact -> artifact{artEntryKey = rekey (artEntryKey artifact)}) artifacts
        raw =
            vdRaw doc >>= \old ->
                case (snd cargoCached old, snd pypiSimpleCached old, snd rubyGemsCached old) of
                    (Just native, _, _) -> Just (fst cargoCached (cargoDocument [(rekey key, value) | (key, value) <- cargoRecords native]))
                    (_, Just simple, _) -> Just (fst pypiSimpleCached (simpleDocument (simpleEnvelope simple) [(rekey key, value) | (key, value) <- simpleFiles simple]))
                    (_, _, Just native) -> Just (fst rubyGemsCached native{gemRows = [row{grEntryKey = rekey (grEntryKey row)} | row <- gemRows native]})
                    _ -> Nothing
     in (start + length artifacts, (version, doc{vdDetails = details{pkgArtifacts = changed}, vdRaw = raw}))

-- | An unmanaged version keeps its existing path. A managed version requires the shared complete proof.
mirrorVersionView :: Version -> ProjectMirrorRecord -> Maybe VersionDoc -> Either MirrorStateFault (Maybe VersionDoc)
mirrorVersionView version record current = case recordVersion version record of
    Nothing -> Right current
    Just mirror -> do
        let files = maybe [] (toList . pkgArtifacts . vdDetails) current
        unless (checkMirrorCompletion mirror files == MirrorComplete) (Left (StateInvalid "managed mirror version is incomplete"))
        Just <$> maybeToRight (StateInvalid "cannot restore paired mirror source") (restoreVersion mirror files)

restoreVersion :: MirrorVersion -> [Artifact] -> Maybe VersionDoc
restoreVersion mirror current
    | CargoMirrorRead _ <- fragmentReadData firstFragment = restoreCargoVersion mirror current
    | RubyGemsMirrorRead _ <- fragmentReadData firstFragment = restoreRubyVersion mirror current
    | otherwise = restoreSimpleVersion mirror current
  where
    firstFragment :| _ = versionFragments mirror

restoreRubyVersion :: MirrorVersion -> [Artifact] -> Maybe VersionDoc
restoreRubyVersion mirror current = do
    let target = Map.fromList [(artFilename artifact, artifact) | artifact <- current]
        (_, tasks) = mapAccumL (\start fragment -> (start + length (pkgArtifacts (fragmentDetails fragment)), restoreRubyFragment target start fragment)) 0 (toList (versionFragments mirror))
    fragments <- sequenceA tasks
    (firstDetails, _, firstSource) <- listToMaybe fragments
    artifacts <- nonEmpty (concat [toList files | (_, files, _) <- fragments])
    let rows = concat [gemRows source | (_, _, source) <- fragments]
        published = gemSourcePublishedAt firstSource{gemRows = rows}
        details = firstDetails{pkgArtifacts = artifacts, pkgPublishedAt = published}
    pure (VersionDoc details (Just (fst rubyGemsCached firstSource{gemRows = rows})))

restoreRubyFragment :: Map Text Artifact -> Int -> StoredMirrorFragment -> Maybe (PackageDetails, NonEmpty Artifact, GemDocument)
restoreRubyFragment target start fragment = do
    RubyGemsMirrorRead source <- pure (fragmentReadData fragment)
    let original = fragmentDetails fragment
        sourceRows = Map.fromList [(grEntryKey row, row) | row <- gemRows source]
    restored <- for (zip [start ..] (toList (pkgArtifacts original))) $ \(position, artifact) -> do
        native <- Map.lookup (artFilename artifact) target
        row <- Map.lookup (artEntryKey artifact) sourceRows
        let key = RecordEntry position
        pure (artifact{artEntryKey = key, artUrl = artUrl native}, row{grEntryKey = key})
    artifacts <- nonEmpty (map fst restored)
    pure (original, artifacts, source{gemRows = map snd restored})

restoreCargoVersion :: MirrorVersion -> [Artifact] -> Maybe VersionDoc
restoreCargoVersion mirror current = do
    fragment :| [] <- pure (versionFragments mirror)
    CargoMirrorRead source <- pure (fragmentReadData fragment)
    artifact :| [] <- pure (pkgArtifacts (fragmentDetails fragment))
    [(key, _)] <- pure (cargoRecords source)
    native <- find ((== artFilename artifact) . artFilename) current
    guard (key == artEntryKey artifact)
    pure (VersionDoc (fragmentDetails fragment){pkgArtifacts = artifact{artUrl = artUrl native} :| []} (Just (fst cargoCached source)))

restoreSimpleVersion :: MirrorVersion -> [Artifact] -> Maybe VersionDoc
restoreSimpleVersion mirror current = do
    let target = Map.fromList [(artFilename artifact, artifact) | artifact <- current]
    let (_, tasks) = mapAccumL (\start fragment -> (start + length (pkgArtifacts (fragmentDetails fragment)), restoreFragment target start fragment)) 0 (toList (versionFragments mirror))
    fragments <- traverse (fmap snd) tasks
    firstDoc <- listToMaybe fragments
    artifacts <- nonEmpty (concatMap (toList . pkgArtifacts . vdDetails) fragments)
    documents <- traverse (vdRaw >=> snd pypiSimpleCached) fragments
    pure VersionDoc{vdDetails = (vdDetails firstDoc){pkgArtifacts = artifacts}, vdRaw = Just (fst pypiSimpleCached (simpleDocument mempty (concatMap simpleFiles documents)))}

restoreFragment :: Map Text Artifact -> Int -> StoredMirrorFragment -> Maybe (Int, VersionDoc)
restoreFragment current start fragment = do
    source <- case fragmentReadData fragment of
        PyPIMirrorRead document -> Just document
        NpmMirrorRead _ -> Nothing
        CargoMirrorRead _ -> Nothing
        RubyGemsMirrorRead _ -> Nothing
    let original = fragmentDetails fragment
        fields = Map.fromList (simpleFiles source)
    restored <- for (zip [start ..] (toList (pkgArtifacts original))) $ \(position, artifact) -> do
        native <- Map.lookup (artFilename artifact) current
        Object value <- Map.lookup (artEntryKey artifact) fields
        let key = ArrayEntry position
        pure (artifact{artEntryKey = key, artUrl = artUrl native}, (key, Object (KeyMap.insert "url" (String (artUrl native)) value)))
    artifacts <- nonEmpty (map fst restored)
    pure (start + length restored, VersionDoc original{pkgArtifacts = artifacts} (Just (fst pypiSimpleCached (simpleDocument mempty (map snd restored)))))

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
readMirrorCompanion (MirrorView _ _ store _) ticket record version filename =
    case recordVersion version record >>= Map.lookup filename . versionCompanions of
        Just (CompanionRetained reference) -> do
            stateCharge <- newChargeRegion (charge ticket)
            attempted <- tryAny (msReadCompanion store stateCharge reference)
            pure (either (const (Left StateMissingCompanion)) (>>= verifyRetainedCompanion reference) attempted)
        _ -> pure (Left StateMissingCompanion)
