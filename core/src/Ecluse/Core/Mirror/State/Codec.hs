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
) where

import Control.Exception (evaluate)
import Data.Aeson (Value (..), encode, object, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither, withObject)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.JsonStream.Parser qualified as J
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Ecluse.Core.Ecosystem (ecosystemName, parseEcosystem)
import Ecluse.Core.Mirror.State.Internal
import Ecluse.Core.Mirror.State.Weight (retainedRecordWeight)
import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey (..), distinctKeys, validEntryKey)
import Ecluse.Core.Registry.CachedDocument (estimateValueBytes)
import Ecluse.Core.Registry.Cargo.Document (cargoDocument, cargoRecords)
import Ecluse.Core.Registry.Cargo.Manifest (cratesIoIndex)
import Ecluse.Core.Registry.JsonStream (StreamResult (..), readJsonStream, retainedValue)
import Ecluse.Core.Registry.Metadata (VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.MirrorUpload (MirrorInventory, MirrorReadData (..), expectedArtifact, inventoryDetails, inventoryReadData, mirrorReadDocument)
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorFragment)
import Ecluse.Core.Registry.PyPI.Document (simpleDocument, simpleEnvelope, simpleFiles)
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (..), GemRow (..))
import Ecluse.Core.Registry.RubyGems.SourceState (gemSourceDecodeCharge, gemSourceValue, parseGemSource)
import Ecluse.Core.Security
import Ecluse.Core.Server.Cache.VersionWeight (weighVersion)
import Ecluse.Core.Server.MemoryModel (bufferedChunkCharge, decodedDocumentCharge, expandWireBytes, saturatingCharge)
import Ecluse.Core.Text (textStorageBytes)
import Ecluse.Core.Version (mkVersion, renderVersion)

-- | Account for declared companion bytes as well as encoded source records before writing.
encodeProjectRecord :: Limits -> ProjectMirrorRecord -> Either MirrorStateFault ByteString
encodeProjectRecord limits record = do
    let remaining = toInteger (maxMetadataBytes limits) - companionBytes record
    when (remaining < 0) (Left (StateBound (BodyTooLarge (MetadataBodyLimit (maxMetadataBytes limits)))))
    let bytes = LBS.toStrict (LBS.take (fromInteger remaining + 1) (encode (recordValue record)))
    checkRecord limits (BS.length bytes) record
    void (boundedValue limits bytes)
    pure bytes

decodeProjectValue :: Limits -> ByteString -> Either MirrorStateFault Value
decodeProjectValue limits bytes = checkDimensions limits bytes >> boundedValue limits bytes

restoreProjectValue :: Limits -> StateTarget -> PackageName -> Int -> Value -> Either MirrorStateFault ProjectMirrorRecord
restoreProjectValue limits target name size value = do
    record <- first (StateInvalid . toText) (parseEither parseRecord value)
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
        traverse_ (checkArtifactCount limits) [a, f, s, c, h]
        pure updated

-- | Charge the durable graph separately from active-work reservations.
projectRecordWeight :: Limits -> ProjectMirrorRecord -> Either MirrorStateFault Int
projectRecordWeight limits record = checkRecord limits 0 record $> saturatingCharge (retainedRecordWeight record)

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
chargedDecodeProjectRecord :: StateCharge -> Limits -> StateTarget -> PackageName -> ByteString -> IO (Either MirrorStateFault ProjectMirrorRecord)
chargedDecodeProjectRecord charge limits target name bytes = do
    let treeCharge = toInteger stateExchangeCharge + toInteger (bufferedChunkCharge (BS.length bytes)) + toInteger (decodedDocumentCharge bytes)
    charge (saturatingCharge treeCharge)
    evaluate (decodeProjectValue limits bytes) >>= \case
        Left fault -> pure (Left fault)
        Right value -> do
            materialised <- evaluate (force value)
            charge (saturatingCharge (treeCharge + domainDecodeCharge materialised))
            evaluate (restoreProjectValue limits target name (BS.length bytes) materialised) >>= \case
                Left fault -> pure (Left fault)
                Right record -> case projectRecordWeight limits record of
                    Left fault -> pure (Left fault)
                    Right retained -> do
                        void (evaluate retained)
                        evaluate (forceRecordSources record)
                        pure (Right record)

domainDecodeCharge :: Value -> Integer
domainDecodeCharge value = 4096 + 8 * toInteger (estimateValueBytes value) + sumArray "versions" value versionCharge
  where
    -- Reuse VersionWeight's allowances at schema-owned coordinates, never at arbitrary publisher fields with the same name.
    versionCharge version = sumArray "fragments" version fragmentCharge
    fragmentCharge fragment = maybe 0 detailsCharge (fieldValue "details" fragment) + maybe 0 sourceCharge (fieldValue "source" fragment)
    detailsCharge details = 16 * 1024 + textCharge "version" details
    sourceCharge source = case fieldValue "kind" source of
        Just (String "rubygems") -> gemSourceDecodeCharge source
        _ -> sumArray "entries" source (maybe 0 (textCharge "filename") . fieldValue "value")
    textCharge key parent = case fieldValue key parent of
        Just (String text) -> 256 * toInteger (textStorageBytes text)
        _ -> 0
    sumArray key parent itemCharge = case fieldValue key parent of
        Just (Array values) -> foldl' (\total child -> total + itemCharge child) 0 values
        _ -> 0
    fieldValue key (Object fields) = KeyMap.lookup key fields
    fieldValue _ _ = Nothing

-- | Reserve retained input, encoded output and validation overlap before creating the encoding tree.
chargedEncodeProjectRecord :: StateCharge -> Limits -> ProjectMirrorRecord -> IO (Either MirrorStateFault ByteString)
chargedEncodeProjectRecord charge limits record = case projectRecordWorkWeight limits record of
    Left fault -> pure (Left fault)
    Right retained -> do
        charge (saturatingCharge (4 * toInteger retained + 32768))
        result <- evaluate (encodeProjectRecord limits record)
        traverse (\bytes -> evaluate (BS.length bytes) $> bytes) result

forceRecordSources :: ProjectMirrorRecord -> ()
forceRecordSources record = force (recordEnvelope record) `seq` foldl' forceVersion () (Map.elems (recordVersions record))
  where
    forceVersion () version = foldl' forceFragment () (versionFragments version)
    forceFragment () (StoredMirrorFragment _ source _) = case source of
        NpmMirrorRead fields -> force fields `seq` ()
        PyPIMirrorRead document -> force (simpleEnvelope document) `seq` foldl' forceEntry () (simpleFiles document)
        CargoMirrorRead document -> foldl' forceEntry () (cargoRecords document)
        RubyGemsMirrorRead document -> foldl' (\() row -> BS.length (grSource row) `seq` ()) () (gemRows document)
    forceEntry () (key, value) = forceKey key `seq` force value `seq` ()
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
    first StateBound (checkArtifactCount limits (sum (map countFiles (Map.elems (recordVersions record)))))
  where
    countFiles version = sum [length (pkgArtifacts details) | StoredMirrorFragment details _ _ <- toList (versionFragments version)]

companionBytes :: ProjectMirrorRecord -> Integer
companionBytes record =
    sum
        [toInteger (companionSize reference) | version <- Map.elems (recordVersions record), CompanionRetained reference <- Map.elems (versionCompanions version)]

recordValue :: ProjectMirrorRecord -> Value
recordValue record =
    object
        [ "schema" .= (2 :: Int)
        , "target" .= targetKey (recordTarget record)
        , "package" .= nameValue (recordPackage record)
        , "envelope" .= recordEnvelope record
        , "versions" .= [object ["version" .= key, "fragments" .= map fragmentValue (toList (versionFragments version)), "verified" .= [object ["filename" .= name, "digest" .= hashValueWire digest] | (name, digest) <- Map.toList (versionVerifiedIdentities version)], "companions" .= map companionValue (Map.toList (versionCompanions version))] | (key, version) <- Map.toList (recordVersions record)]
        ]

parseRecord :: Value -> Parser ProjectMirrorRecord
parseRecord = withObject "project mirror state" $ \value -> do
    schema <- value .: "schema"
    unless (schema == (2 :: Int)) (fail "unsupported mirror state schema")
    target <- StateTarget <$> value .: "target"
    name <- value .: "package" >>= parseName
    envelope <- value .: "envelope"
    versions <- value .: "versions" >>= traverse (parseVersion name)
    ensureDistinct (map fst versions)
    for_ (listToMaybe versions) $ \(_, version) -> do
        let StoredMirrorFragment details source _ :| _ = versionFragments version
        case source of
            PyPIMirrorRead document -> void (either (const (fail "invalid project envelope identity")) pure (restoreMirrorFragment details (PyPIMirrorRead (simpleDocument envelope (simpleFiles document)))))
            NpmMirrorRead _ -> unless (KeyMap.null envelope) (fail "npm record has an unexpected project envelope")
            CargoMirrorRead _ -> unless (KeyMap.null envelope) (fail "Cargo record has an unexpected project envelope")
            RubyGemsMirrorRead _ -> unless (KeyMap.null envelope) (fail "RubyGems record has an unexpected project envelope")
    pure (ProjectMirrorRecord target name envelope (Map.fromList versions))

parseVersion :: PackageName -> Value -> Parser (Text, MirrorVersion)
parseVersion name = withObject "mirror version" $ \value -> do
    key <- value .: "version"
    fragments <- value .: "fragments" >>= traverse (parseFragment name key) >>= nonempty "empty version fragments"
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
        required = Set.fromList [artFilename (expectedArtifact file) | StoredMirrorFragment _ source expected <- toList fragments, file <- toList expected, declaredSourceCompanion source (expectedArtifact file)]
    ensureDistinct (map artFilename files)
    unless (Set.fromList (map fst companions) == required) (fail "companion set does not match declared source companions")
    unless (Set.fromList (map fst verified) `Set.isSubsetOf` Set.fromList (map artFilename files)) (fail "verified digest has no expected file")
    pure (key, MirrorVersion fragments (Map.fromList verified) (Map.fromList companions))

fragmentValue :: StoredMirrorFragment -> Value
fragmentValue (StoredMirrorFragment details source _) = object ["details" .= detailsValue details, "source" .= sourceValue source]

parseFragment :: PackageName -> Text -> Value -> Parser StoredMirrorFragment
parseFragment name key = withObject "source fragment" $ \value -> do
    details <- value .: "details" >>= parseDetails
    unless (pkgName details == name && renderVersion (pkgVersion details) == key) (fail "fragment identity mismatch")
    source <- value .: "source" >>= parseSource
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
    object
        [ "package" .= nameValue (pkgName details)
        , "version" .= renderVersion (pkgVersion details)
        , "published" .= pkgPublishedAt details
        , "install" .= installValue (pkgInstallCode details)
        , "availability" .= availabilityValue (pkgAvailability details)
        , "artifacts" .= map artifactValue (toList (pkgArtifacts details))
        ]

parseDetails :: Value -> Parser PackageDetails
parseDetails = withObject "admission facts" $ \value -> do
    name <- value .: "package" >>= parseName
    version <- mkVersion (pkgEcosystem name) <$> value .: "version"
    PackageDetails name version
        <$> value .: "published"
        <*> (value .: "install" >>= parseInstall)
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

sourceValue :: MirrorReadData -> Value
sourceValue (NpmMirrorRead fields) = object ["kind" .= ("npm" :: Text), "fields" .= fields]
sourceValue (PyPIMirrorRead document) =
    object
        ["kind" .= ("pypi" :: Text), "envelope" .= simpleEnvelope document, "entries" .= [object ["key" .= keyValue key, "value" .= value] | (key, value) <- simpleFiles document]]
sourceValue (RubyGemsMirrorRead document) = gemSourceValue document
sourceValue (CargoMirrorRead document) =
    object ["kind" .= ("cargo" :: Text), "sourceIndex" .= cratesIoIndex, "entries" .= [object ["key" .= keyValue key, "value" .= value] | (key, value) <- cargoRecords document]]

parseSource :: Value -> Parser MirrorReadData
parseSource = withObject "read source" $ \value ->
    value .: "kind" >>= \case
        "npm" -> NpmMirrorRead <$> value .: "fields"
        "pypi" -> do
            envelope <- value .: "envelope"
            files <- value .: "entries" >>= traverse (withObject "file entry" (\entry -> (,) <$> (entry .: "key" >>= parseKey) <*> entry .: "value"))
            ensureDistinct (map fst files)
            pure (PyPIMirrorRead (simpleDocument envelope files))
        "rubygems" -> RubyGemsMirrorRead <$> parseGemSource (Object value)
        "cargo" -> do
            sourceIndex <- value .: "sourceIndex"
            unless (sourceIndex == cratesIoIndex) (fail "Cargo mirror source is not crates.io")
            entries <- value .: "entries" >>= traverse (withObject "Cargo entry" (\entry -> (,) <$> (entry .: "key" >>= parseKey) <*> entry .: "value"))
            ensureDistinct (map fst entries)
            pure (CargoMirrorRead (cargoDocument entries))
        (_ :: Text) -> fail "unknown source shape"

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
