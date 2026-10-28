-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Monotonic expected files retain their first admitted source fragment and companion identity.
module Ecluse.Core.Mirror.State.Internal (
    StateTarget (..),
    stateTarget,
    StateRevision (..),
    ProjectMirrorRecord (..),
    StoredMirrorFragment (..),
    MirrorVersion (..),
    CompanionState (..),
    CompanionRef (..),
    VerifiedCompanionBytes (..),
    MirrorStateFault (..),
    StateRead (..),
    StateWrite (..),
    MirrorState (..),
    StateCharge,
    extendProjectRecord,
    retainCompanionReference,
    retainVerifiedIdentity,
    verifyCompanionBytes,
    verifyRetainedCompanion,
    versionFiles,
    recordVersion,
    recordTarget,
    recordPackage,
    recordVersions,
    recordEnvelope,
    versionFragments,
    versionCompanions,
    versionVerifiedIdentities,
    companionDigest,
    companionSize,
    companionBody,
    targetKey,
    projectKey,
    checkMirrorCompletion,
    MirrorCompletion (..),
    MirrorCompletionFault (..),
    declaredCompanion,
    declaredSourceCompanion,
    verifySourceCompanionBytes,
) where

import Data.Aeson (Object)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text.Short qualified as TS
import Data.Traversable (for)

import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Fault (TransportFault)
import Ecluse.Core.Package
import Ecluse.Core.Package.Hash (sha256Hex)
import Ecluse.Core.Package.Integrity (IntegrityResult (..), verifyIntegrity)
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorFragment)
import Ecluse.Core.Registry.PyPI.Document (simpleDocument, simpleEnvelope, simpleFiles)
import Ecluse.Core.Security (LimitError, Limits (..), checkArtifactCount, checkVersionCountOf)
import Ecluse.Core.Version (Version, renderVersion)

-- | A digest of the configured target, never a credential or caller authorisation.
newtype StateTarget = StateTarget Text
    deriving stock (Eq, Ord, Show)

-- | Bind records and object keys to the exact configured target identity.
stateTarget :: Text -> StateTarget
stateTarget = StateTarget . sha256Hex . encodeUtf8

-- | Opaque storage revision bound to the project it came from.
data StateRevision = StateRevision StateTarget PackageName ByteString
    deriving stock (Eq, Show)

-- | Fragments retain only newly owned filenames, with original admission facts and coordinates.
data StoredMirrorFragment = StoredMirrorFragment PackageDetails MirrorReadData (NonEmpty ExpectedMirrorFile)
    deriving stock (Eq, Show)

-- | Files grow monotonically. Each companion starts pending before any distribution upload.
data MirrorVersion = MirrorVersion
    { storedFragments :: NonEmpty StoredMirrorFragment
    , storedIdentities :: Map Text Hash
    , storedCompanions :: Map Text CompanionState
    }
    deriving stock (Eq, Show)

-- | The target and package fields are checked on every decode, before state becomes usable.
data ProjectMirrorRecord = ProjectMirrorRecord
    { projectTarget :: StateTarget
    , projectPackage :: PackageName
    , projectEnvelope :: Object
    , projectVersions :: Map Text MirrorVersion
    }
    deriving stock (Eq, Show)

-- | A retained reference exists only after its immutable content object was written.
data CompanionState = CompanionPending | CompanionRetained CompanionRef
    deriving stock (Eq, Show)

-- | Content-addressed metadata, with no arbitrary URL or object path.
data CompanionRef = CompanionRef Hash Int
    deriving stock (Eq, Show)

-- | Checked source companion bytes are bounded and tied to a declared parent.
data VerifiedCompanionBytes = VerifiedCompanionBytes Artifact CompanionRef ByteString
    deriving stock (Eq, Show)

-- | State uncertainty never means absence or permission to present a smaller expected set.
data MirrorStateFault
    = StateTransport TransportFault
    | StateBound LimitError
    | StateInvalid Text
    | StateIdentityMismatch
    | StateFileConflict Text
    | StateCompanionConflict Text
    | StateRevisionMismatch
    | StateMissingCompanion
    | StateRetryExhausted
    deriving stock (Eq, Show)

-- | A 304 is useful only with the exact supplied, already charged project payload.
data StateRead = StateAbsent | StateUnchanged StateRevision | StateLoaded StateRevision ProjectMirrorRecord
    deriving stock (Eq, Show)

-- | The caller rereads and unions after a collision instead of repeating an old replacement.
data StateWrite = StateStored StateRevision | StateConflict
    deriving stock (Eq, Show)

-- | An absolute high-water requirement for sequential state work under one caller-owned lifetime.
type StateCharge = Int -> IO ()

-- | Composition binds a target and storage prefix. No operation accepts an arbitrary object key.
data MirrorState = MirrorState
    { msReadProject :: StateCharge -> PackageName -> Maybe StateRevision -> IO (Either MirrorStateFault StateRead)
    , msWriteProject :: StateCharge -> PackageName -> Maybe StateRevision -> ProjectMirrorRecord -> IO (Either MirrorStateFault StateWrite)
    , msRetainCompanion :: StateCharge -> VerifiedCompanionBytes -> IO (Either MirrorStateFault CompanionRef)
    , msReadCompanion :: StateCharge -> CompanionRef -> IO (Either MirrorStateFault ByteString)
    }

-- | The configured target identity carried by this validated record.
recordTarget :: ProjectMirrorRecord -> StateTarget
recordTarget = projectTarget

-- | The canonical package identity carried by this validated record.
recordPackage :: ProjectMirrorRecord -> PackageName
recordPackage = projectPackage

-- | Project fields follow the latest committed source admission, independently of file-byte updates.
recordEnvelope :: ProjectMirrorRecord -> Object
recordEnvelope = projectEnvelope

-- | Versions contain accumulated admission fragments without a mutable completion flag.
recordVersions :: ProjectMirrorRecord -> Map Text MirrorVersion
recordVersions = projectVersions

-- | Each fragment retains only its original owned source entries.
versionFragments :: MirrorVersion -> NonEmpty StoredMirrorFragment
versionFragments = storedFragments

-- | Pending declarations cannot be mistaken for durably retained metadata.
versionCompanions :: MirrorVersion -> Map Text CompanionState
versionCompanions = storedCompanions

-- | These digests came from verified bytes, independently of preserved source digest fields.
versionVerifiedIdentities :: MirrorVersion -> Map Text Hash
versionVerifiedIdentities = storedIdentities

-- | Add only new filenames, retaining the old read fields for every unchanged physical identity.
extendProjectRecord :: Limits -> StateTarget -> MirrorInventory -> Maybe ProjectMirrorRecord -> Either MirrorStateFault ProjectMirrorRecord
extendProjectRecord limits target inventory previous = do
    let name = inventoryPackage inventory
        initial = ProjectMirrorRecord target name mempty Map.empty
        record = fromMaybe initial previous
        key = renderVersion (inventoryVersion inventory)
    unless (recordTarget record == target && recordPackage record == name) (Left StateIdentityMismatch)
    updated <- extendVersion inventory (Map.lookup key (recordVersions record))
    let envelope = case inventoryReadData inventory of
            PyPIMirrorRead document -> simpleEnvelope document
            NpmMirrorRead _ -> mempty
            CargoMirrorRead _ -> mempty
            RubyGemsMirrorRead _ -> mempty
        result = record{projectEnvelope = envelope, projectVersions = Map.insert key updated (recordVersions record)}
    first StateBound (checkVersionCountOf limits (Map.size (recordVersions result)))
    first StateBound (checkArtifactCount limits (sum (map (Map.size . versionFiles) (Map.elems (recordVersions result)))))
    pure result

extendVersion :: MirrorInventory -> Maybe MirrorVersion -> Either MirrorStateFault MirrorVersion
extendVersion inventory previous = do
    let oldFiles = maybe Map.empty versionFiles previous
        incoming = Map.fromList [(artFilename (expectedArtifact file), file) | file <- toList (inventoryFiles inventory)]
    for_ (Map.toList incoming) $ \(name, file) -> for_ (Map.lookup name oldFiles) $ \old ->
        unless (expectedIdentityMatches old file) (Left (StateFileConflict name))
    let additions = Map.difference incoming oldFiles
    case nonEmpty (Map.elems additions) of
        Nothing -> maybe (Left (StateInvalid "empty expected inventory")) Right previous
        Just _ -> do
            (details, source) <-
                maybeToRight (StateInvalid "cannot retain paired source fragment") $
                    splitMirrorInventory (Map.keysSet additions) inventory
            let ownedSource = case source of
                    PyPIMirrorRead document -> PyPIMirrorRead (simpleDocument mempty (simpleFiles document))
                    NpmMirrorRead _ -> source
                    CargoMirrorRead _ -> source
                    RubyGemsMirrorRead _ -> source
            files <- first (const (StateInvalid "invalid source fragment")) (restoreMirrorFragment details ownedSource)
            let fragment = StoredMirrorFragment details ownedSource files
                companions = Map.fromList [(name, CompanionPending) | (name, file) <- Map.toList additions, declaredSourceCompanion ownedSource (expectedArtifact file)]
            pure $ case previous of
                Nothing -> MirrorVersion (fragment :| []) Map.empty companions
                Just old -> old{storedFragments = versionFragments old <> (fragment :| []), storedCompanions = versionCompanions old <> companions}

-- | Original source coordinates remain scoped to their fragment until the renderer assigns new ones.
versionFiles :: MirrorVersion -> Map Text ExpectedMirrorFile
versionFiles version =
    Map.fromList
        [(artFilename (expectedArtifact file), file) | StoredMirrorFragment _ _ files <- toList (versionFragments version), file <- toList files]

-- | A valid record without the requested version is unmanaged private content.
recordVersion :: Version -> ProjectMirrorRecord -> Maybe MirrorVersion
recordVersion version = Map.lookup (renderVersion version) . recordVersions

-- | A pending expectation cannot become retained until its actual bytes satisfy the declaration.
verifyCompanionBytes :: Limits -> ExpectedMirrorFile -> ByteString -> Either MirrorStateFault VerifiedCompanionBytes
verifyCompanionBytes limits file bytes = do
    let artifact = expectedArtifact file
    unless (declaredCompanion artifact) (Left (StateInvalid "companion was not declared"))
    when (BS.length bytes > maxMetadataBytes limits) (Left (StateInvalid "companion exceeds metadata byte bound"))
    digests <- case artCoreMetadata artifact of
        Just (MetadataDigests values) -> fmap catMaybes . for (Map.toList values) $ \(algorithm, value) ->
            case parseHashAlg algorithm of
                Left _ -> Right Nothing
                Right alg -> Just <$> first StateInvalid (mkHash alg value)
        _ -> pure []
    for_ (nonEmpty digests) $ \expected -> case verifyIntegrity expected bytes of
        IntegrityVerified -> Right ()
        IntegrityMismatch detail -> Left (StateInvalid detail)
    digest <- first StateInvalid (mkHash SHA256 (sha256Hex bytes))
    pure (VerifiedCompanionBytes artifact (CompanionRef digest (BS.length bytes)) bytes)

-- | Actual reads verify immutable content identity, including the recorded size.
verifyRetainedCompanion :: CompanionRef -> ByteString -> Either MirrorStateFault ByteString
verifyRetainedCompanion (CompanionRef digest size) bytes = do
    unless (size == BS.length bytes && hashValue digest == sha256Hex bytes) (Left (StateInvalid "retained companion identity mismatch"))
    pure bytes

-- | Bind durable bytes only to the same admitted declaration and parent identity.
retainCompanionReference :: Version -> VerifiedCompanionBytes -> CompanionRef -> ProjectMirrorRecord -> Either MirrorStateFault ProjectMirrorRecord
retainCompanionReference version (VerifiedCompanionBytes parent verified _) reference record = do
    unless (verified == reference) (Left (StateCompanionConflict (artFilename parent)))
    old <- maybeToRight StateMissingCompanion (recordVersion version record)
    file <- maybeToRight StateMissingCompanion (Map.lookup (artFilename parent) (versionFiles old))
    unless (expectedArtifact file == parent) (Left (StateCompanionConflict (artFilename parent)))
    retained <- maybeToRight StateMissingCompanion (Map.lookup (artFilename parent) (versionCompanions old))
    case retained of
        CompanionRetained existing | existing /= reference -> Left (StateCompanionConflict (artFilename parent))
        _ ->
            let updated = old{storedCompanions = Map.insert (artFilename parent) (CompanionRetained reference) (versionCompanions old)}
             in Right record{projectVersions = Map.insert (renderVersion version) updated (recordVersions record)}

-- | Retain a digest computed only from bytes that passed the admitted source integrity check.
retainVerifiedIdentity :: Version -> VerifiedArtifactBytes -> ProjectMirrorRecord -> Either MirrorStateFault ProjectMirrorRecord
retainVerifiedIdentity version verified record = do
    old <- maybeToRight (StateInvalid "missing expected version") (recordVersion version record)
    let file = verifiedFile verified
        name = artFilename (expectedArtifact file)
    expected <- maybeToRight (StateFileConflict name) (Map.lookup name (versionFiles old))
    unless (expectedIdentityMatches expected file) (Left (StateFileConflict name))
    let digest = verifiedTargetHash verified
    for_ (Map.lookup name (versionVerifiedIdentities old)) $ \previous ->
        unless (previous == digest) (Left (StateFileConflict name))
    let updated = old{storedIdentities = Map.insert name digest (versionVerifiedIdentities old)}
    pure record{projectVersions = Map.insert (renderVersion version) updated (recordVersions record)}

-- | A failed proof reports each missing file, conflicting identity, or pending companion.
data MirrorCompletion = MirrorComplete | MirrorIncomplete (NonEmpty MirrorCompletionFault)
    deriving stock (Eq, Show)

-- | The worker can retry absent files without republishing a conflicting existing filename.
data MirrorCompletionFault
    = MissingMirrorFile Text
    | ConflictingMirrorFile Text
    | MissingMirrorProof Text
    | PendingMirrorCompanion Text
    deriving stock (Eq, Show)

-- | Completion uses current authorised target identities and retained companions, never a stored Boolean.
checkMirrorCompletion :: MirrorVersion -> [Artifact] -> MirrorCompletion
checkMirrorCompletion version targetFiles =
    maybe MirrorComplete MirrorIncomplete (nonEmpty (fileFaults <> companionFaults))
  where
    current = Map.fromListWith (<>) [(artFilename file, [file]) | file <- targetFiles]
    fileFaults = mapMaybe checkFile (Map.toList (versionFiles version))
    checkFile (name, expected) = case Map.lookup name current of
        Nothing -> Just (MissingMirrorFile name)
        Just files
            | all (expectedTargetMatchesWith expected verified) files -> Nothing
            | any (targetIdentityConflict expected verified) files -> Just (ConflictingMirrorFile name)
            | otherwise -> Just (MissingMirrorProof name)
          where
            verified = Map.lookup name (versionVerifiedIdentities version)
    companionFaults = [PendingMirrorCompanion name | (name, CompanionPending) <- Map.toList (versionCompanions version)]

-- | The content identity used in storage keys and byte verification.
companionDigest :: CompanionRef -> Text
companionDigest (CompanionRef digest _) = hashValue digest

-- | The retained response size in bytes.
companionSize :: CompanionRef -> Int
companionSize (CompanionRef _ size) = size

-- | Only checked declared companion bytes reach the object writer.
companionBody :: VerifiedCompanionBytes -> ByteString
companionBody (VerifiedCompanionBytes _ _ bytes) = bytes

-- | The target component has a fixed-length hexadecimal encoding.
targetKey :: StateTarget -> Text
targetKey (StateTarget digest) = digest

-- | Length-independent components bind the schema, target, ecosystem, and canonical package.
projectKey :: StateTarget -> PackageName -> Text
projectKey target name = "v1/" <> targetKey target <> "/" <> ecosystemName (pkgEcosystem name) <> "/" <> sha256Hex (encodeUtf8 (TS.toText (pkgCanonical name)))

-- | A missing or false declaration creates no retention authority.
declaredCompanion :: Artifact -> Bool
declaredCompanion artifact = case artCoreMetadata artifact of
    Just (MetadataAvailable available) -> available
    Just (MetadataDigests values) -> not (Map.null values)
    Nothing -> False

-- | Native source payloads can require metadata beyond artifact-advertised sidecars.
declaredSourceCompanion :: MirrorReadData -> Artifact -> Bool
declaredSourceCompanion source artifact = case source of
    RubyGemsMirrorRead _ -> True
    _ -> declaredCompanion artifact

-- | The native codec checks meaning before this step binds its bytes to the declared parent.
verifySourceCompanionBytes :: Limits -> MirrorReadData -> ExpectedMirrorFile -> ByteString -> Either MirrorStateFault VerifiedCompanionBytes
verifySourceCompanionBytes limits source file bytes = case source of
    RubyGemsMirrorRead _ -> do
        unless (declaredSourceCompanion source (expectedArtifact file)) (Left (StateInvalid "native companion was not declared"))
        when (BS.length bytes > maxMetadataBytes limits) (Left (StateInvalid "companion exceeds metadata byte bound"))
        digest <- first StateInvalid (mkHash SHA256 (sha256Hex bytes))
        pure (VerifiedCompanionBytes (expectedArtifact file) (CompanionRef digest (BS.length bytes)) bytes)
    _ -> verifyCompanionBytes limits file bytes
