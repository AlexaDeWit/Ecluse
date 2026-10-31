-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bounded durable records recheck stored source fragments rather than reconstructing policy facts.
module Ecluse.Core.Mirror.State.Codec (
    encodeProjectRecord,
    projectRecordWeight,
    projectRecordWorkWeight,
    chargedDecodeProjectRecord,
    chargedEncodeProjectRecord,
    stateExchangeCharge,
    mirrorInventoryWeight,

    -- * Stored source entries
    sourceEntries,
    parseSourceEntries,
    sourceEntriesCharge,
    forceSourceEntries,
) where

import Control.Exception (evaluate)
import Data.Aeson (Object, Value (..), encode, object, (.:), (.:?), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair, Parser, parseEither, withObject)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.JsonStream.Parser qualified as J
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Traversable (for)

import Ecluse.Core.Ecosystem (ecosystemName, parseEcosystem)
import Ecluse.Core.Mirror.State.Format (MirrorStateFormat (..), ProjectEnvelope (envelopeCheck))
import Ecluse.Core.Mirror.State.Internal
import Ecluse.Core.Mirror.State.Weight (retainedRecordWeight)
import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey (..), distinctKeys, validEntryKey)
import Ecluse.Core.Registry.CachedDocument (estimateValueBytes)
import Ecluse.Core.Registry.JsonStream (StreamResult (..), readJsonStream, retainedValue)
import Ecluse.Core.Registry.Metadata (VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.MirrorUpload (MirrorInventory, MirrorReadData, expectedArtifact, inventoryDetails, inventoryReadData, mirrorReadDocument)
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorFragment)
import Ecluse.Core.Security
import Ecluse.Core.Server.Cache.VersionWeight (weighVersion)
import Ecluse.Core.Server.MemoryModel (bufferedChunkCharge, decodedDocumentCharge, expandWireBytes, saturatingCharge)
import Ecluse.Core.Text (textStorageBytes)
import Ecluse.Core.Version (mkVersion, renderVersion)

-- | Account for declared companion bytes as well as encoded source records before writing. Another format's source refuses the write.
encodeProjectRecord :: MirrorStateFormat -> Limits -> ProjectMirrorRecord -> Either MirrorStateFault ByteString
encodeProjectRecord format limits record = do
    let remaining = toInteger (maxMetadataBytes limits) - companionBytes record
    when (remaining < 0) (Left (StateBound (BodyTooLarge (MetadataBodyLimit (maxMetadataBytes limits)))))
    value <- maybeToRight (StateInvalid "a stored source belongs to another format") (recordValue format record)
    let bytes = LBS.toStrict (LBS.take (fromInteger remaining + 1) (encode value))
    checkRecord limits (BS.length bytes) record
    void (boundedValue limits bytes)
    pure bytes

decodeProjectValue :: Limits -> ByteString -> Either MirrorStateFault Value
decodeProjectValue limits bytes = checkDimensions limits bytes >> boundedValue limits bytes

restoreProjectValue :: MirrorStateFormat -> Limits -> StateTarget -> PackageName -> Int -> Value -> Either MirrorStateFault ProjectMirrorRecord
restoreProjectValue format limits target name size value = do
    record <- first (StateInvalid . toText) (parseEither (parseRecord format) value)
    unless (recordTarget record == target && recordPackage record == name) (Left StateIdentityMismatch)
    checkRecord limits size record
    pure record

boundedValue :: Limits -> ByteString -> Either MirrorStateFault Value
boundedValue limits bytes
    | BS.length bytes > maxMetadataBytes limits = Left (StateBound (BodyTooLarge (MetadataBodyLimit (maxMetadataBytes limits))))
    | otherwise = case J.runParser' (retainedValue (maxNestingDepth limits)) bytes of
        J.ParseYield value (J.ParseDone rest) | BS.all (\byte -> byte `elem` [9, 10, 13, 32]) rest -> Right value
        _ -> Left (StateInvalid "malformed or excessively nested state JSON")

data Dimension = VersionStart | ArtifactStart | FragmentStart | SourceStart | CompanionStart | VerifiedStart | ContainerEnd

checkDimensions :: Limits -> ByteString -> Either MirrorStateFault ()
checkDimensions limits bytes = do
    result <-
        first StateBound $
            evalState
                (readJsonStream (MetadataBodyLimit (maxMetadataBytes limits)) dimensions count (0, 0, 0, 0, 0, 0) next)
                [bytes]
    first (const (StateInvalid "malformed state dimensions")) (streamValue result) $> ()
  where
    next = state $ \case
        [] -> (BS.empty, [])
        chunk : rest -> (chunk, rest)
    dimensions = J.objectWithKey "versions" (J.arrayOf (J.objectFound VersionStart ContainerEnd (J.objectKeyValues versionField)))
    versionField "fragments" = J.arrayOf (J.objectFound FragmentStart ContainerEnd (J.objectKeyValues fragmentField))
    versionField "companions" = J.arrayOf (J.objectFound CompanionStart ContainerEnd mempty)
    versionField "verified" = J.arrayOf (J.objectFound VerifiedStart ContainerEnd mempty)
    versionField _ = mempty
    fragmentField "details" = J.objectWithKey "artifacts" (J.arrayOf (J.objectFound ArtifactStart ContainerEnd mempty))
    fragmentField "source" = J.objectWithKey "entries" (J.arrayOf (J.objectFound SourceStart ContainerEnd mempty))
    fragmentField _ = mempty
    count (versions, artifacts, fragments, sources, companions, verified) dimension = do
        let updated@(v, a, f, s, c, h) = case dimension of
                VersionStart -> (versions + 1, artifacts, fragments, sources, companions, verified)
                ArtifactStart -> (versions, artifacts + 1, fragments, sources, companions, verified)
                FragmentStart -> (versions, artifacts, fragments + 1, sources, companions, verified)
                SourceStart -> (versions, artifacts, fragments, sources + 1, companions, verified)
                CompanionStart -> (versions, artifacts, fragments, sources, companions + 1, verified)
                VerifiedStart -> (versions, artifacts, fragments, sources, companions, verified + 1)
                ContainerEnd -> (versions, artifacts, fragments, sources, companions, verified)
        checkVersionCountOf limits v
        traverse_ (checkStateFiles limits) [a, f, s, c, h]
        pure updated

-- | Charge the durable graph separately from active-work reservations.
projectRecordWeight :: MirrorStateFormat -> Limits -> ProjectMirrorRecord -> Either MirrorStateFault Int
projectRecordWeight format limits record = checkRecord limits 0 record $> saturatingCharge (retainedRecordWeight format record)

-- | Preserve the conservative selected-release allowance for update, worker and encoding overlap.
projectRecordWorkWeight :: Limits -> ProjectMirrorRecord -> Either MirrorStateFault Int
projectRecordWorkWeight limits record = do
    checkRecord limits 0 record
    pure . saturatingCharge $
        4096
            + toInteger (expandWireBytes (fromIntegral (estimateValueBytes (Object (recordEnvelope record)))))
            + sum
                [ 256
                    + toInteger (textStorageBytes key)
                    + sum [256 + toInteger (fragmentWeight details source) | StoredMirrorFragment details source _ <- toList (versionFragments version)]
                    + 256 * toInteger (Map.size (versionVerifiedIdentities version) + Map.size (versionCompanions version))
                | (key, version) <- Map.toList (recordVersions record)
                ]

-- | The admitted input already belongs to the caller. This estimates state-update scratch from its shape.
mirrorInventoryWeight :: MirrorInventory -> Int
mirrorInventoryWeight inventory = fragmentWeight (inventoryDetails inventory) (inventoryReadData inventory)

fragmentWeight :: PackageDetails -> MirrorReadData -> Int
fragmentWeight details source = weighVersion (VersionRead (Just (VersionDoc details (Just (mirrorReadDocument source)))) 0 Nothing)

-- | Materialise source fields and domain weights under the reservation. Callers keep it until they release the returned record.
chargedDecodeProjectRecord :: MirrorStateFormat -> StateCharge -> Limits -> StateTarget -> PackageName -> ByteString -> IO (Either MirrorStateFault ProjectMirrorRecord)
chargedDecodeProjectRecord format charge limits target name bytes = do
    let treeCharge = toInteger stateExchangeCharge + toInteger (bufferedChunkCharge (BS.length bytes)) + toInteger (decodedDocumentCharge bytes)
    charge (saturatingCharge treeCharge)
    evaluate (decodeProjectValue limits bytes) >>= \case
        Left fault -> pure (Left fault)
        Right value -> do
            materialised <- evaluate (force value)
            charge (saturatingCharge (treeCharge + domainDecodeCharge format materialised))
            evaluate (restoreProjectValue format limits target name (BS.length bytes) materialised) >>= \case
                Left fault -> pure (Left fault)
                Right record -> case projectRecordWeight format limits record of
                    Left fault -> pure (Left fault)
                    Right retained -> do
                        void (evaluate retained)
                        evaluate (forceRecordSources format record)
                        pure (Right record)

-- Reuse VersionWeight's allowances at schema-owned coordinates, never at arbitrary publisher fields with the same name.
domainDecodeCharge :: MirrorStateFormat -> Value -> Integer
domainDecodeCharge format value = 4096 + 8 * toInteger (estimateValueBytes value) + sumArray "versions" value versionCharge
  where
    versionCharge version = sumArray "fragments" version fragmentCharge
    fragmentCharge fragment = maybe 0 detailsCharge (fieldValue "details" fragment) + maybe 0 (msfDecodeCharge format) (fieldValue "source" fragment)
    detailsCharge details = 16 * 1024 + textCharge "version" details

-- | Charge each stored entry's @filename@ at the retained text allowance.
sourceEntriesCharge :: Value -> Integer
sourceEntriesCharge source = sumArray "entries" source (maybe 0 (textCharge "filename") . fieldValue "value")

textCharge :: Key -> Value -> Integer
textCharge key parent = case fieldValue key parent of
    Just (String text) -> 256 * toInteger (textStorageBytes text)
    _ -> 0

sumArray :: Key -> Value -> (Value -> Integer) -> Integer
sumArray key parent itemCharge = case fieldValue key parent of
    Just (Array values) -> foldl' (\total child -> total + itemCharge child) 0 values
    _ -> 0

fieldValue :: Key -> Value -> Maybe Value
fieldValue key (Object fields) = KeyMap.lookup key fields
fieldValue _ _ = Nothing

-- | Reserve retained input, encoded output and validation overlap before creating the encoding tree.
chargedEncodeProjectRecord :: MirrorStateFormat -> StateCharge -> Limits -> ProjectMirrorRecord -> IO (Either MirrorStateFault ByteString)
chargedEncodeProjectRecord format charge limits record = case projectRecordWorkWeight limits record of
    Left fault -> pure (Left fault)
    Right retained -> do
        charge (saturatingCharge (4 * toInteger retained + 32768))
        result <- evaluate (encodeProjectRecord format limits record)
        traverse (\bytes -> evaluate (BS.length bytes) $> bytes) result

forceRecordSources :: MirrorStateFormat -> ProjectMirrorRecord -> ()
forceRecordSources format record = force (recordEnvelope record) `seq` foldl' forceVersion () (Map.elems (recordVersions record))
  where
    forceVersion () version = foldl' forceFragment () (versionFragments version)
    forceFragment () (StoredMirrorFragment _ source _) = msfForceSource format source

-- | Materialise stored entries and their source coordinates.
forceSourceEntries :: [(EntryKey, Value)] -> ()
forceSourceEntries = foldl' (\() (key, value) -> forceKey key `seq` force value `seq` ()) ()
  where
    forceKey (ArrayEntry position) = force position `seq` ()
    forceKey (RecordEntry position) = force position `seq` ()
    forceKey (ObjectEntry name) = force name `seq` ()
    forceKey SingletonEntry = ()

-- | Fixed per-exchange metadata bookkeeping, independent of the configured maximum object size.
stateExchangeCharge :: Int
stateExchangeCharge = 4096

checkRecord :: Limits -> Int -> ProjectMirrorRecord -> Either MirrorStateFault ()
checkRecord limits size record = do
    when (toInteger size + companionBytes record > toInteger (maxMetadataBytes limits)) $
        Left (StateBound (BodyTooLarge (MetadataBodyLimit (maxMetadataBytes limits))))
    first StateBound (checkVersionCountOf limits (Map.size (recordVersions record)))
    first StateBound (checkStateFiles limits (sum (map countFiles (Map.elems (recordVersions record)))))
  where
    countFiles version = sum [length (pkgArtifacts details) | StoredMirrorFragment details _ _ <- toList (versionFragments version)]

companionBytes :: ProjectMirrorRecord -> Integer
companionBytes record =
    sum
        [toInteger (companionSize reference) | version <- Map.elems (recordVersions record), CompanionRetained reference <- Map.elems (versionCompanions version)]

-- Each fragment's source encodes before the record does, so a source of another format refuses the whole record.
recordValue :: MirrorStateFormat -> ProjectMirrorRecord -> Maybe Value
recordValue format record = do
    versions <- for (Map.toList (recordVersions record)) $ \(key, version) -> do
        fragments <- traverse fragmentValue (toList (versionFragments version))
        pure (object ["version" .= key, "fragments" .= fragments, "verified" .= [object ["filename" .= name, "digest" .= hashValueWire digest] | (name, digest) <- Map.toList (versionVerifiedIdentities version)], "companions" .= map companionValue (Map.toList (versionCompanions version))])
    pure $
        object
            [ "schema" .= (2 :: Int)
            , "target" .= targetKey (recordTarget record)
            , "package" .= nameValue (recordPackage record)
            , "envelope" .= recordEnvelope record
            , "versions" .= versions
            ]
  where
    fragmentValue (StoredMirrorFragment details source _) = (\stored -> object ["details" .= detailsValue details, "source" .= stored]) <$> msfEncodeSource format source

parseRecord :: MirrorStateFormat -> Value -> Parser ProjectMirrorRecord
parseRecord format = withObject "project mirror state" $ \value -> do
    schema <- value .: "schema"
    unless (schema == (2 :: Int)) (fail "unsupported mirror state schema")
    target <- StateTarget <$> value .: "target"
    name <- value .: "package" >>= parseName
    envelope <- value .: "envelope"
    versions <- value .: "versions" >>= traverse (parseVersion format name)
    ensureDistinct (map fst versions)
    for_ (listToMaybe versions) $ \(_, version) -> do
        let StoredMirrorFragment details source _ :| _ = versionFragments version
        either (fail . toString) pure (envelopeCheck (msfEnvelope format) envelope details source)
    pure (ProjectMirrorRecord target name envelope (Map.fromList versions))

parseVersion :: MirrorStateFormat -> PackageName -> Value -> Parser (Text, MirrorVersion)
parseVersion format name = withObject "mirror version" $ \value -> do
    key <- value .: "version"
    fragments <- value .: "fragments" >>= traverse (parseFragment format name key) >>= nonempty "empty version fragments"
    verified <-
        value .: "verified"
            >>= traverse
                ( withObject
                    "verified artifact identity"
                    ( \fields -> do
                        filename <- fields .: "filename"
                        digest <- fields .: "digest" >>= parseHash
                        unless (hashAlg digest == SHA256) (fail "unsupported verified artifact digest")
                        pure (filename, digest)
                    )
                )
    ensureDistinct (map fst verified)
    companions <- value .: "companions" >>= traverse parseCompanion
    ensureDistinct (map fst companions)
    let files = [expectedArtifact file | StoredMirrorFragment _ _ expected <- toList fragments, file <- toList expected]
        required = Set.fromList [artFilename (expectedArtifact file) | StoredMirrorFragment _ _ expected <- toList fragments, file <- toList expected, declaredSourceCompanion (msfCompanions format) (expectedArtifact file)]
    ensureDistinct (map artFilename files)
    unless (Set.fromList (map fst companions) == required) (fail "companion set does not match declared source companions")
    unless (Set.fromList (map fst verified) `Set.isSubsetOf` Set.fromList (map artFilename files)) (fail "verified digest has no expected file")
    pure (key, MirrorVersion fragments (Map.fromList verified) (Map.fromList companions))

parseFragment :: MirrorStateFormat -> PackageName -> Text -> Value -> Parser StoredMirrorFragment
parseFragment format name key = withObject "source fragment" $ \value -> do
    details <- value .: "details" >>= parseDetails
    unless (pkgName details == name && renderVersion (pkgVersion details) == key) (fail "fragment identity mismatch")
    source <- value .: "source" >>= parseSource format
    files <- either (const (fail "invalid source fragment pairing")) pure (restoreMirrorFragment details source)
    pure (StoredMirrorFragment details source files)

nameValue :: PackageName -> Value
nameValue name = object ["ecosystem" .= ecosystemName (pkgEcosystem name), "scope" .= fmap unScope (pkgNamespace name), "name" .= unscopedName name]

parseName :: Value -> Parser PackageName
parseName = withObject "package identity" $ \value -> do
    rawEco <- value .: "ecosystem"
    eco <- maybe (fail "unknown ecosystem") pure (parseEcosystem rawEco)
    scope <- fmap mkScope <$> value .: "scope"
    name <- value .: "name"
    pure (mkPackageName eco scope name)

detailsValue :: PackageDetails -> Value
detailsValue details =
    object $
        [ "package" .= nameValue (pkgName details)
        , "version" .= renderVersion (pkgVersion details)
        , "published" .= pkgPublishedAt details
        , "install" .= installValue (pkgInstallCode details)
        , "availability" .= availabilityValue (pkgAvailability details)
        , "artifacts" .= map artifactValue (toList (pkgArtifacts details))
        ]
            <> dependencyValue (pkgDependencySources details)

parseDetails :: Value -> Parser PackageDetails
parseDetails = withObject "admission facts" $ \value -> do
    name <- value .: "package" >>= parseName
    version <- mkVersion (pkgEcosystem name) <$> value .: "version"
    PackageDetails name version
        <$> value .: "published"
        <*> (value .: "install" >>= parseInstall)
        <*> (value .:? "dependencies" >>= maybe (pure DependencySourcesUnknown) parseDependencies)
        <*> (value .: "availability" >>= parseAvailability)
        <*> (value .: "artifacts" >>= traverse parseArtifact >>= nonempty "empty source fragment")

installValue :: CodeExecSignal -> Value
installValue NoCodeOnInstall = object ["kind" .= ("none" :: Text)]
installValue CodeExecUnknown = object ["kind" .= ("unknown" :: Text)]
installValue (RunsCodeOnInstall reason) = object ["kind" .= ("runs" :: Text), "reason" .= reason]

parseInstall :: Value -> Parser CodeExecSignal
parseInstall = withObject "install fact" $ \value ->
    value .: "kind" >>= \case
        "none" -> pure NoCodeOnInstall
        "unknown" -> pure CodeExecUnknown
        "runs" -> RunsCodeOnInstall <$> value .: "reason"
        (_ :: Text) -> fail "unknown install fact"

-- Absent when undetermined, as in every record written before the fact existed.
dependencyValue :: DependencySources -> [Pair]
dependencyValue DependencySourcesUnknown = []
dependencyValue RegistryDependencies = ["dependencies" .= object ["kind" .= ("registry" :: Text)]]
dependencyValue (FetchesOutsideRegistry reason) = ["dependencies" .= object ["kind" .= ("outside" :: Text), "reason" .= reason]]

parseDependencies :: Value -> Parser DependencySources
parseDependencies = withObject "dependency fact" $ \value ->
    value .: "kind" >>= \case
        "registry" -> pure RegistryDependencies
        "outside" -> FetchesOutsideRegistry <$> value .: "reason"
        (_ :: Text) -> fail "unknown dependency fact"

availabilityValue :: Availability -> Text
availabilityValue Available = "available"
availabilityValue Deprecated = "deprecated"
availabilityValue Yanked = "yanked"

parseAvailability :: Text -> Parser Availability
parseAvailability "available" = pure Available
parseAvailability "deprecated" = pure Deprecated
parseAvailability "yanked" = pure Yanked
parseAvailability _ = fail "unknown availability fact"

artifactValue :: Artifact -> Value
artifactValue artifact =
    object
        [ "key" .= keyValue (artEntryKey artifact)
        , "filename" .= artFilename artifact
        , "url" .= artUrl artifact
        , "hashes" .= map hashValueWire (artHashes artifact)
        , "size" .= artSize artifact
        , "companion" .= fmap metadataValue (artCoreMetadata artifact)
        ]

parseArtifact :: Value -> Parser Artifact
parseArtifact = withObject "artifact" $ \value -> do
    key <- value .: "key" >>= parseKey
    name <- value .: "filename"
    url <- value .: "url"
    metadata <- value .: "companion" >>= traverse parseMetadata
    hashes <- value .: "hashes" >>= traverse parseHash
    size <- value .: "size"
    when (any (< 0) size) (fail "negative artifact size")
    pure Artifact{artEntryKey = key, artFilename = name, artUrl = url, artHashes = hashes, artSize = size, artCoreMetadata = metadata}

hashValueWire :: Hash -> Value
hashValueWire digest = object ["algorithm" .= renderHashAlg (hashAlg digest), "value" .= hashValue digest]

parseHash :: Value -> Parser Hash
parseHash = withObject "digest" $ \value -> do
    alg <- value .: "algorithm" >>= either (fail . toString) pure . parseHashAlg
    raw <- value .: "value"
    either (fail . toString) pure (mkHash alg raw)

metadataValue :: CoreMetadata -> Value
metadataValue (MetadataAvailable available) = Bool available
metadataValue (MetadataDigests values) = object ["digests" .= values]

parseMetadata :: Value -> Parser CoreMetadata
parseMetadata (Bool available) = pure (MetadataAvailable available)
parseMetadata value = withObject "companion declaration" (fmap MetadataDigests . (.: "digests")) value

keyValue :: EntryKey -> Value
keyValue (ArrayEntry position) = object ["array" .= position]
keyValue (RecordEntry position) = object ["record" .= position]
keyValue (ObjectEntry text) = object ["object" .= text]
keyValue SingletonEntry = Null

parseKey :: Value -> Parser EntryKey
parseKey Null = pure SingletonEntry
parseKey value =
    withObject
        "source coordinate"
        ( \fields ->
            ( do
                key <- ArrayEntry <$> fields .: "array"
                unless (validEntryKey key) (fail "negative source coordinate")
                pure key
            )
                <|> ( do
                        key <- RecordEntry <$> fields .: "record"
                        unless (validEntryKey key) (fail "negative source coordinate")
                        pure key
                    )
                <|> (ObjectEntry <$> fields .: "object")
        )
        value

parseSource :: MirrorStateFormat -> Value -> Parser MirrorReadData
parseSource format = withObject "read source" $ \value -> do
    kind <- value .: "kind"
    unless (kind == msfKind format) (fail "unknown source shape")
    msfDecodeSource format value

-- | Stored entries keep each value under its source coordinate.
sourceEntries :: [(EntryKey, Value)] -> [Value]
sourceEntries entries = [object ["key" .= keyValue key, "value" .= value] | (key, value) <- entries]

-- | Read stored entries under @label@, refusing a repeated coordinate.
parseSourceEntries :: String -> Object -> Parser [(EntryKey, Value)]
parseSourceEntries label value = do
    entries <- value .: "entries" >>= traverse (withObject label (\entry -> (,) <$> (entry .: "key" >>= parseKey) <*> entry .: "value"))
    entries <$ ensureDistinct (map fst entries)

companionValue :: (Text, CompanionState) -> Value
companionValue (name, stateValue) = object ["filename" .= name, "retained" .= reference]
  where
    reference = case stateValue of
        CompanionPending -> Null
        CompanionRetained (CompanionRef digest size) -> object ["digest" .= hashValueWire digest, "size" .= size]

parseCompanion :: Value -> Parser (Text, CompanionState)
parseCompanion = withObject "companion state" $ \value -> do
    name <- value .: "filename"
    retained <- value .: "retained"
    status <- case retained of
        Null -> pure CompanionPending
        other ->
            withObject
                "retained companion"
                ( \fields -> do
                    digest <- fields .: "digest" >>= parseHash
                    size <- fields .: "size"
                    unless (hashAlg digest == SHA256 && size >= 0) (fail "invalid retained companion identity")
                    pure (CompanionRetained (CompanionRef digest size))
                )
                other
    pure (name, status)

ensureDistinct :: (Ord a) => [a] -> Parser ()
ensureDistinct values = unless (distinctKeys values) (fail "duplicate state entry")

nonempty :: String -> [a] -> Parser (NonEmpty a)
nonempty message = maybe (fail message) pure . nonEmpty
