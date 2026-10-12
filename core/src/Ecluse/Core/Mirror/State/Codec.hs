-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bounded durable records recheck stored source fragments rather than reconstructing policy facts.
module Ecluse.Core.Mirror.State.Codec (
    encodeProjectRecord,
    decodeProjectRecord,
    projectRecordWeight,
) where

import Control.Monad.State.Strict (evalState, state)
import Data.Aeson (Value (..), encode, object, (.:), (.=))
import Data.Aeson.Types (Parser, parseEither, withObject)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.JsonStream.Parser qualified as J
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Ecluse.Core.Ecosystem (ecosystemName, parseEcosystem)
import Ecluse.Core.Mirror.State.Internal
import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey (..))
import Ecluse.Core.Registry.JsonStream (StreamResult (..), readJsonStream, retainedValue)
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData (..), expectedArtifact)
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorFragment)
import Ecluse.Core.Registry.PyPI.Document (simpleDocument, simpleEnvelope, simpleFiles)
import Ecluse.Core.Security
import Ecluse.Core.Server.MemoryModel (expandWireBytes)
import Ecluse.Core.Version (mkVersion, versionRaw)

-- | Account for declared companion bytes as well as encoded source records before writing.
encodeProjectRecord :: Limits -> ProjectMirrorRecord -> Either MirrorStateFault ByteString
encodeProjectRecord limits record = do
    let remaining = toInteger (maxMetadataBytes limits) - companionBytes record
    when (remaining < 0) (Left (StateBound (BodyTooLarge (MetadataBodyLimit (maxMetadataBytes limits)))))
    let bytes = LBS.toStrict (LBS.take (fromInteger remaining + 1) (encode (recordValue record)))
    checkRecord limits (BS.length bytes) record
    void (boundedValue limits bytes)
    pure bytes

-- | Bytes and nesting are bounded before building the value tree. Identity and fragments are then checked.
decodeProjectRecord :: Limits -> StateTarget -> PackageName -> ByteString -> Either MirrorStateFault ProjectMirrorRecord
decodeProjectRecord limits target name bytes = do
    checkDimensions limits bytes
    value <- boundedValue limits bytes
    record <- first (StateInvalid . toText) (parseEither parseRecord value)
    unless (recordTarget record == target && recordPackage record == name) (Left StateIdentityMismatch)
    checkRecord limits (BS.length bytes) record
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

-- | The cache charges decoded expansion and retained companion bytes without storing authorisation.
projectRecordWeight :: Limits -> ProjectMirrorRecord -> Either MirrorStateFault Int
projectRecordWeight limits record = do
    bytes <- encodeProjectRecord limits record
    pure (fromInteger (min (toInteger (maxBound :: Int)) (toInteger (expandWireBytes (BS.length bytes)) + companionBytes record)))

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
        [ "schema" .= (1 :: Int)
        , "target" .= targetKey (recordTarget record)
        , "package" .= nameValue (recordPackage record)
        , "versions" .= [object ["version" .= key, "fragments" .= map fragmentValue (toList (versionFragments version)), "verified" .= [object ["filename" .= name, "digest" .= hashValueWire digest] | (name, digest) <- Map.toList (versionVerifiedIdentities version)], "companions" .= map companionValue (Map.toList (versionCompanions version))] | (key, version) <- Map.toList (recordVersions record)]
        ]

parseRecord :: Value -> Parser ProjectMirrorRecord
parseRecord = withObject "project mirror state" $ \value -> do
    schema <- value .: "schema"
    unless (schema == (1 :: Int)) (fail "unsupported mirror state schema")
    target <- StateTarget <$> value .: "target"
    name <- value .: "package" >>= parseName
    versions <- value .: "versions" >>= traverse (parseVersion name)
    ensureDistinct (map fst versions)
    pure (ProjectMirrorRecord target name (Map.fromList versions))

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
        required = Set.fromList [artFilename artifact | artifact <- files, declaredCompanion artifact]
    ensureDistinct (map artFilename files)
    unless (Set.fromList (map fst companions) == required) (fail "companion set does not match declared source companions")
    unless (Set.fromList (map fst verified) `Set.isSubsetOf` Set.fromList (map artFilename files)) (fail "verified digest has no expected file")
    pure (key, MirrorVersion fragments (Map.fromList verified) (Map.fromList companions))

fragmentValue :: StoredMirrorFragment -> Value
fragmentValue (StoredMirrorFragment details source _) = object ["details" .= detailsValue details, "source" .= sourceValue source]

parseFragment :: PackageName -> Text -> Value -> Parser StoredMirrorFragment
parseFragment name key = withObject "source fragment" $ \value -> do
    details <- value .: "details" >>= parseDetails
    unless (pkgName details == name && versionRaw (pkgVersion details) == key) (fail "fragment identity mismatch")
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
        , "version" .= versionRaw (pkgVersion details)
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
keyValue (ObjectEntry text) = object ["object" .= text]
keyValue SingletonEntry = Null

parseKey :: Value -> Parser EntryKey
parseKey Null = pure SingletonEntry
parseKey value =
    withObject
        "source coordinate"
        ( \fields ->
            ( do
                position <- fields .: "array"
                when (position < 0) (fail "negative source coordinate")
                pure (ArrayEntry position)
            )
                <|> (ObjectEntry <$> fields .: "object")
        )
        value

sourceValue :: MirrorReadData -> Value
sourceValue (NpmMirrorRead fields) = object ["kind" .= ("npm" :: Text), "fields" .= fields]
sourceValue (PyPIMirrorRead document) =
    object
        ["kind" .= ("pypi" :: Text), "envelope" .= simpleEnvelope document, "entries" .= [object ["key" .= keyValue key, "value" .= value] | (key, value) <- simpleFiles document]]

parseSource :: Value -> Parser MirrorReadData
parseSource = withObject "read source" $ \value ->
    value .: "kind" >>= \case
        "npm" -> NpmMirrorRead <$> value .: "fields"
        "pypi" -> do
            envelope <- value .: "envelope"
            files <- value .: "entries" >>= traverse (withObject "file entry" (\entry -> (,) <$> (entry .: "key" >>= parseKey) <*> entry .: "value"))
            ensureDistinct (map fst files)
            pure (PyPIMirrorRead (simpleDocument envelope files))
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
ensureDistinct values = unless (Set.size (Set.fromList values) == length values) (fail "duplicate state entry")

nonempty :: String -> [a] -> Parser (NonEmpty a)
nonempty message = maybe (fail message) pure . nonEmpty
