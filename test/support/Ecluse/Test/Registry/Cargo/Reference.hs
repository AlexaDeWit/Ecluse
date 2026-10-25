-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Independent native observations for Cargo fixtures, without production protocol code.
module Ecluse.Test.Registry.Cargo.Reference (
    -- * Registry bytes
    IndexLine (..),
    referenceIndex,
    referenceDiscovery,
    UploadFrame (..),
    referenceUpload,

    -- * Client observations
    CargoPackage (..),
    CargoDependency (..),
    CargoNode (..),
    CargoGraph (..),
    referenceGraph,
    CargoLocked (..),
    referenceLock,
) where

import Data.Aeson (FromJSON (parseJSON), Object, Value, eitherDecodeStrict, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither)
import Data.Bits (shiftL)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

-- | Physical zero-based line positions include blank and malformed records.
data IndexLine = IndexLine
    { indexPosition :: Int
    , indexBytes :: ByteString
    , indexValue :: Either Text Object
    }
    deriving stock (Eq, Show)

-- | Decode LF records independently, retaining failures and accepting a final unterminated line.
referenceIndex :: ByteString -> [IndexLine]
referenceIndex bytes =
    [ IndexLine position raw (first toText (eitherDecodeStrict raw >>= parseEither objectValue))
    | (position, raw) <- zip [0 ..] (BS.split 10 bytes)
    , not (BS.all (`elem` [9, 13, 32]) raw)
    ]
  where
    objectValue = withObject "Cargo index record" pure

-- | Require the native download location, retaining all additive discovery fields.
referenceDiscovery :: ByteString -> Either Text (Text, Maybe Text, Bool, Object)
referenceDiscovery bytes = first toText (eitherDecodeStrict bytes >>= parseEither discovery)
  where
    discovery = withObject "Cargo discovery" $ \fields ->
        (,,,) <$> fields .: "dl" <*> fields .:? "api" <*> fields .:? "auth-required" .!= False <*> pure fields

-- | Exact native upload metadata and archive bytes, after both uint32 frames.
data UploadFrame = UploadFrame
    { uploadMetadata :: Object
    , uploadArchive :: ByteString
    }
    deriving stock (Eq, Show)

-- | Reject short lengths, non-object metadata and trailing bytes independently of the server.
referenceUpload :: ByteString -> Either Text UploadFrame
referenceUpload bytes = do
    (metadataBytes, rest) <- framed bytes
    metadata <- first toText (eitherDecodeStrict metadataBytes >>= parseEither (withObject "Cargo upload" pure))
    (archive, trailing) <- framed rest
    unless (BS.null trailing) (Left "Cargo upload has trailing bytes")
    pure (UploadFrame metadata archive)
  where
    framed input = do
        let (prefix, payload) = BS.splitAt 4 input
        unless (BS.length prefix == 4) (Left "Cargo upload has a short uint32 length")
        let count = sum [toInteger byte `shiftL` (8 * offset) | (offset, byte) <- zip [0 ..] (BS.unpack prefix)]
        unless (count <= toInteger (BS.length payload)) (Left "Cargo upload has a short frame")
        pure (BS.splitAt (fromInteger count) payload)

-- | Native package declarations retain dependency aliases and target conditions.
data CargoPackage = CargoPackage
    { cargoPackageName :: Text
    , cargoPackageVersion :: Text
    , cargoPackageSource :: Maybe Text
    , cargoPackageDependencies :: [CargoDependency]
    , cargoPackageTargets :: [Value]
    , cargoPackageRustVersion :: Maybe Text
    , cargoPackageManifestPath :: FilePath
    }
    deriving stock (Eq, Show)

-- | Declaration names use Cargo's metadata convention, with the published name before the rename.
data CargoDependency = CargoDependency
    { cargoDependencyName :: Text
    , cargoDependencyRename :: Maybe Text
    , cargoDependencyRequirement :: Text
    , cargoDependencyKind :: Maybe Text
    , cargoDependencyTarget :: Maybe Text
    , cargoDependencyOptional :: Bool
    , cargoDependencyDefaults :: Bool
    , cargoDependencyFeatures :: [Text]
    , cargoDependencyRegistry :: Maybe Text
    }
    deriving stock (Eq, Show)

-- | Resolved edges refer to opaque package IDs within the same metadata document.
data CargoNode = CargoNode
    { cargoNodeFeatures :: [Text]
    , cargoNodeDependencies :: [(Text, Text, [Value])]
    }
    deriving stock (Eq, Show)

-- | Package IDs are map keys and are never split to infer native identities.
data CargoGraph = CargoGraph
    { cargoGraphPackages :: Map Text CargoPackage
    , cargoGraphNodes :: Map Text CargoNode
    , cargoGraphRoot :: Maybe Text
    }
    deriving stock (Eq, Show)

instance FromJSON CargoDependency where
    parseJSON = withObject "Cargo dependency" $ \fields ->
        CargoDependency
            <$> fields .: "name"
            <*> fields .:? "rename"
            <*> fields .: "req"
            <*> fields .:? "kind"
            <*> fields .:? "target"
            <*> fields .: "optional"
            <*> fields .: "uses_default_features"
            <*> fields .: "features"
            <*> fields .:? "registry"

-- | Decode the full resolved graph and reject duplicate or dangling package IDs.
referenceGraph :: ByteString -> Either Text CargoGraph
referenceGraph bytes = do
    (packages, nodes, root) <- first toText (eitherDecodeStrict bytes >>= parseEither metadata)
    packageMap <- unique "package" packages
    nodeMap <- unique "node" nodes
    forM_ (Map.toList nodeMap) $ \(identifier, resolvedNode) -> do
        require packageMap identifier
        forM_ (cargoNodeDependencies resolvedNode) $ \(_, dependency, _) -> require packageMap dependency
    forM_ root (require packageMap)
    pure (CargoGraph packageMap nodeMap root)
  where
    metadata = withObject "cargo metadata" $ \fields -> do
        packages <- fields .: "packages" >>= traverse package
        resolved <- fields .: "resolve"
        withObject "Cargo resolve" (\resolution -> (packages,,) <$> (resolution .: "nodes" >>= traverse node) <*> resolution .:? "root") resolved
    package = withObject "Cargo package" $ \fields ->
        (,) <$> fields .: "id" <*> (CargoPackage <$> fields .: "name" <*> fields .: "version" <*> fields .:? "source" <*> fields .: "dependencies" <*> fields .: "targets" <*> fields .:? "rust_version" <*> fields .: "manifest_path")
    node = withObject "Cargo node" $ \fields ->
        (,) <$> fields .: "id" <*> (CargoNode <$> fields .: "features" <*> (fields .: "deps" >>= traverse edge))
    edge = withObject "Cargo edge" $ \fields -> (,,) <$> fields .: "name" <*> fields .: "pkg" <*> fields .: "dep_kinds"
    unique label pairs = do
        let result = Map.fromList pairs
        unless (Map.size result == length pairs) (Left ("Cargo metadata repeats a " <> label <> " ID"))
        pure result
    require packages identifier = unless (Map.member identifier packages) (Left ("Cargo metadata refers to absent ID " <> identifier))

-- | Registry checksums and source identities observed in Cargo's generated lockfile.
data CargoLocked = CargoLocked
    { cargoLockedName :: Text
    , cargoLockedVersion :: Text
    , cargoLockedSource :: Maybe Text
    , cargoLockedChecksum :: Maybe Text
    }
    deriving stock (Eq, Ord, Show)

-- | Read Cargo-generated package sections only. This is not a general TOML parser.
referenceLock :: Text -> Either Text [CargoLocked]
referenceLock document = do
    packages <- traverse package (sections (T.lines document))
    let identities = map (\entry -> (cargoLockedName entry, cargoLockedVersion entry, cargoLockedSource entry)) packages
    unless (length identities == Map.size (Map.fromList [(packageIdentity, ()) | packageIdentity <- identities])) (Left "Cargo.lock repeats a package identity")
    pure packages
  where
    sections source = case dropWhile ((/= "[[package]]") . T.strip) source of
        [] -> []
        _ : rest -> let (current, remaining) = break (T.isPrefixOf "[" . T.strip) rest in current : sections remaining
    package source = do
        fields <- traverse field (filter interesting (map T.strip source))
        let values = Map.fromList fields
        unless (Map.size values == length fields) (Left "Cargo.lock repeats an observed field")
        CargoLocked <$> required "name" values <*> required "version" values <*> pure (Map.lookup "source" values) <*> pure (Map.lookup "checksum" values)
    interesting line = any (\key -> (key <> " ") `T.isPrefixOf` line || (key <> "=") `T.isPrefixOf` line) ["name", "version", "source", "checksum"]
    field line = do
        let (key, rest) = T.breakOn "=" line
        when (T.null rest) (Left "Cargo.lock has a malformed observed field")
        value <- first toText (eitherDecodeStrict (encodeUtf8 (T.strip (T.drop 1 rest))))
        pure (T.strip key, value)
    required key values = maybe (Left ("Cargo.lock lacks " <> key)) Right (Map.lookup key values)
