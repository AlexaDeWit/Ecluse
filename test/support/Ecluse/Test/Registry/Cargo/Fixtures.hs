-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Owned native crates and registry bytes for client and protocol contracts.
module Ecluse.Test.Registry.Cargo.Fixtures (
    -- * Owned graph
    CargoCrate (..),
    CargoRelease (..),
    minimalCargoRelease,
    cargoCrates,
    cargoRoot,
    cargoVersion,
    cargoExecutable,
    cargoExpectedOutput,
    loadCargoGraph,
    cargoLeafRelease,

    -- * Registry and publication bytes
    cargoCrateArchive,
    cargoShard,
    cargoArchivePath,
    cargoArchivePathFor,
    cargoViewDiscovery,
    cargoIndex,
    cargoIndexRecord,
    cargoUpload,
    cargoUploadWithMetadata,
    cargoUploadMetadata,
    cargoUploadFrame,
    cargoFixtureLock,
    writeCargoRegistry,
) where

import Codec.Compression.GZip qualified as GZip
import Data.Aeson (Value (Bool, Null, Object, String), encode, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString, toLazyByteString, word32LE)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAsciiUpper)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.Archive (tarBytes)

-- | Each directory contains source owned by this repository, never a downloaded package.
data CargoCrate = CargoCrate
    { crateName :: Text
    , crateVersion :: Text
    , crateDirectory :: FilePath
    , crateDependencies :: [Value]
    , crateFeatures :: Value
    , crateFeatures2 :: Value
    , crateFiles :: [FilePath]
    }
    deriving stock (Eq, Show)

-- | The checksum binds the exact compressed archive, including its native manifest.
data CargoRelease = CargoRelease
    { releaseCrate :: CargoCrate
    , releaseArchive :: ByteString
    , releaseChecksum :: Text
    , releaseReadme :: Text
    }
    deriving stock (Eq, Show)

-- | Metadata-only rows use opaque owned bytes. Archive inspection tests require 'loadCargoGraph'.
minimalCargoRelease :: Text -> Text -> CargoRelease
minimalCargoRelease name version =
    let spec = CargoCrate name version "unused" [] emptyFeatures emptyFeatures []
        bytes = "Owned Cargo metadata fixture bytes"
     in CargoRelease spec bytes (hexSha256Of bytes) "Owned fixture README"

-- | The native binary and dependency-graph root.
cargoRoot :: Text
cargoRoot = "ecluse-e2e-root"

-- | The root's exact release remains suitable for minimum-client observations.
cargoVersion :: Text
cargoVersion = "1.0.0"

-- | The root package provides the installed executable.
cargoExecutable :: Text
cargoExecutable = cargoRoot

-- | Literal expected execution output for the default or forwarded feature on Unix and Windows.
cargoExpectedOutput :: Bool -> Bool -> Text
cargoExpectedOutput windows forwarded =
    let leaf = if forwarded then "leaf-extra" else "leaf-base"
        platform = if windows then "windows" else "unix"
     in T.intercalate "|" [leaf, leaf, leaf <> "+leaf-v2", "optional-extra", platform, "owned-build", "owned-macro"]

-- | The graph exercises aliases, a diamond, two major versions, features, build tools and target declarations.
cargoCrates :: [CargoCrate]
cargoCrates =
    [ base "ecluse-e2e-leaf" "1.0.0" "leaf-v1" [] (object ["default" .= ["base" :: Text], "base" .= ([] :: [Text]), "extra" .= ([] :: [Text])]) emptyFeatures
    , base "ecluse-e2e-leaf" "2.0.0" "leaf-v2" [] emptyFeatures emptyFeatures
    , base "ecluse-e2e-left" cargoVersion "left" [dependency "ecluse-e2e-leaf" "^1.0"] emptyFeatures emptyFeatures
    , base "ecluse-e2e-right" cargoVersion "right" [dependency "ecluse-e2e-leaf" "^1.0", renamed "leaf_v2" "ecluse-e2e-leaf" "^2.0"] emptyFeatures emptyFeatures
    , base "ecluse-e2e-extra" cargoVersion "extra" [] (object ["default" .= ["default-marker" :: Text], "default-marker" .= ([] :: [Text]), "extra" .= ([] :: [Text])]) emptyFeatures
    , base "ecluse-e2e-unix" cargoVersion "unix" [] emptyFeatures emptyFeatures
    , base "ecluse-e2e-windows" cargoVersion "windows" [] emptyFeatures emptyFeatures
    , base "ecluse-e2e-build" cargoVersion "build" [] emptyFeatures emptyFeatures
    , base "ecluse-e2e-macro" cargoVersion "macro" [] emptyFeatures emptyFeatures
    , (base cargoRoot cargoVersion "root" rootDependencies (object ["default" .= ["optional" :: Text], "forwarded" .= ["renamed_leaf/extra" :: Text]]) (object ["optional" .= ["dep:ecluse-e2e-extra" :: Text, "ecluse-e2e-extra?/extra"]])){crateFiles = commonFiles <> ["src/main.rs", "build.rs"]}
    ]
  where
    base name version directory dependencies features features2 = CargoCrate name version directory dependencies features features2 commonFiles
    rootDependencies =
        [ setFields [("default_features", Bool False), ("features", array ["base"])] (renamed "renamed_leaf" "ecluse-e2e-leaf" "^1.0")
        , dependency "ecluse-e2e-left" "^1.0"
        , dependency "ecluse-e2e-right" "^1.0"
        , setFields [("optional", Bool True), ("default_features", Bool False)] (dependency "ecluse-e2e-extra" "^1.0")
        , dependency "ecluse-e2e-macro" "^1.0"
        , setFields [("kind", String "build")] (dependency "ecluse-e2e-build" "^1.0")
        , setFields [("target", String "cfg(all(unix, any(unix, windows)))")] (dependency "ecluse-e2e-unix" "^1.0")
        , setFields [("target", String "cfg(windows)")] (dependency "ecluse-e2e-windows" "^1.0")
        ]

commonFiles :: [FilePath]
commonFiles = ["Cargo.toml", "README.md", "src/lib.rs"]

emptyFeatures :: Value
emptyFeatures = object []

array :: [Text] -> Value
array = toJSON

dependency :: Text -> Text -> Value
dependency name requirement =
    object
        [ "name" .= name
        , "req" .= requirement
        , "features" .= ([] :: [Text])
        , "optional" .= False
        , "default_features" .= True
        , "target" .= Null
        , "kind" .= ("normal" :: Text)
        , "registry" .= Null
        , "package" .= Null
        ]

renamed :: Text -> Text -> Text -> Value
renamed alias name requirement = setFields [("package", String name)] (dependency alias requirement)

setFields :: [(Key, Value)] -> Value -> Value
setFields fields (Object original) = Object (foldl' (\held (key, value) -> KeyMap.insert key value held) original fields)
setFields _ original = original

-- | Package the root with a v3 lockfile bound to the exact owned dependency archives.
loadCargoGraph :: FilePath -> IO [CargoRelease]
loadCargoGraph fixtureRoot = do
    dependencies <- traverse (release []) [spec | spec <- cargoCrates, crateName spec /= cargoRoot]
    roots <- forM [spec | spec <- cargoCrates, crateName spec == cargoRoot] $ \spec ->
        release [("Cargo.lock", encodeUtf8 (fixtureLock cratesIoLockSource spec dependencies))] spec
    pure (dependencies <> roots)
  where
    release generated spec = do
        files <- forM (crateFiles spec) $ \path -> (path,) <$> readFileBS (fixtureRoot </> crateDirectory spec </> path)
        readme <- decodeUtf8 <$> readFileBS (fixtureRoot </> crateDirectory spec </> "README.md")
        packageCargoRelease spec (files <> generated) readme

-- | Retain the owned leaf source while varying the native name and version for isolated policy cases.
cargoLeafRelease :: Text -> Text -> IO CargoRelease
cargoLeafRelease name version = do
    let features = object ["default" .= ["base" :: Text], "base" .= ([] :: [Text]), "extra" .= ([] :: [Text])]
        spec = CargoCrate name version "leaf-v1" [] features emptyFeatures commonFiles
        source = "test/fixtures/cargo/graph/leaf-v1"
    files <- forM commonFiles $ \path -> do
        bytes <- readFileBS (source </> path)
        pure (path, if path == "Cargo.toml" then encodeUtf8 (T.replace "ecluse-e2e-leaf" name (T.replace "1.0.0" version (decodeUtf8 bytes))) else bytes)
    readme <- decodeUtf8 <$> readFileBS (source </> "README.md")
    packageCargoRelease spec files readme

packageCargoRelease :: CargoCrate -> [(FilePath, ByteString)] -> Text -> IO CargoRelease
packageCargoRelease spec files readme = do
    archive <- cargoCrateArchive (crateName spec) (crateVersion spec) files
    pure (CargoRelease spec archive (hexSha256Of archive) readme)

-- | Gzip a tar of the entries beneath the native @<name>-<version>/@ archive root.
cargoCrateArchive :: Text -> Text -> [(FilePath, ByteString)] -> IO ByteString
cargoCrateArchive name version entries = LBS.toStrict . GZip.compress <$> tarBytes [(toString (name <> "-" <> version) </> path, bytes) | (path, bytes) <- entries]

cratesIoLockSource :: Text
cratesIoLockSource = "registry+https://github.com/rust-lang/crates.io-index"

-- | Independent native shard spelling uses ASCII case folding without punctuation changes.
cargoShard :: Text -> Text
cargoShard name =
    let folded = T.map (\char -> if isAsciiUpper char then toEnum (fromEnum char + 32) else char) name
        prefix = case T.length folded of
            1 -> "1"
            2 -> "2"
            3 -> "3/" <> T.take 1 folded
            _ -> T.take 2 folded <> "/" <> T.take 2 (T.drop 2 folded)
     in prefix <> "/" <> folded

-- | The explicit captures avoid inferring identity from a hyphenated filename.
cargoArchivePath :: CargoCrate -> Text
cargoArchivePath spec = cargoArchivePathFor (crateName spec) (crateVersion spec)

-- | Render mount-relative fixture captures without changing native name or version spelling.
cargoArchivePathFor :: Text -> Text -> Text
cargoArchivePathFor name version = "crates/" <> name <> "/" <> version <> "/" <> name <> "-" <> version <> ".crate"

-- | Supply the proxy view's base so every advertised asset location traverses that view.
cargoViewDiscovery :: Text -> Maybe Text -> Bool -> ByteString
cargoViewDiscovery proxyBase api authenticated =
    LBS.toStrict $
        encode $
            object $
                [ "dl" .= (T.dropWhileEnd (== '/') proxyBase <> "/crates/{crate}/{version}/{crate}-{version}.crate")
                , "auth-required" .= authenticated
                ]
                    <> maybe [] (\url -> ["api" .= url]) api

-- | LF records retain literal native fields and admit independent yank/time mutations.
cargoIndex :: [CargoRelease] -> ByteString
cargoIndex = foldMap (\release -> LBS.toStrict (encode (cargoIndexRecord release)) <> "\n")

-- | Source metadata includes additive fields which must survive filtering and mirroring.
cargoIndexRecord :: CargoRelease -> Value
cargoIndexRecord release =
    object
        [ "name" .= crateName spec
        , "vers" .= crateVersion spec
        , "deps" .= crateDependencies spec
        , "cksum" .= releaseChecksum release
        , "features" .= crateFeatures spec
        , "features2" .= crateFeatures2 spec
        , "yanked" .= False
        , "links" .= Null
        , "v" .= (2 :: Int)
        , "rust_version" .= ("1.74" :: Text)
        , "pubtime" .= ("2020-01-01T00:00:00Z" :: Text)
        , "ecluse-fixture" .= object ["retained" .= True, "nullable" .= Null]
        ]
  where
    spec = releaseCrate release

-- | Native upload fields come from the owned fixture declarations, independently of production codecs.
cargoUploadMetadata :: CargoRelease -> Value
cargoUploadMetadata release =
    object
        [ "name" .= crateName spec
        , "vers" .= crateVersion spec
        , "deps" .= map uploadDependency (crateDependencies spec)
        , "features" .= mergedFeatures
        , "authors" .= ["Ecluse contributors" :: Text]
        , "description" .= ("Owned Cargo registry contract fixture" :: Text)
        , "documentation" .= Null
        , "homepage" .= Null
        , "readme" .= releaseReadme release
        , "readme_file" .= ("README.md" :: Text)
        , "keywords" .= ([] :: [Text])
        , "categories" .= ([] :: [Text])
        , "license" .= ("MIT" :: Text)
        , "license_file" .= Null
        , "repository" .= Null
        , "badges" .= object []
        , "links" .= Null
        , "rust_version" .= ("1.74" :: Text)
        ]
  where
    spec = releaseCrate release
    mergedFeatures = case (crateFeatures spec, crateFeatures2 spec) of
        (Object older, Object newer) -> Object (KeyMap.union newer older)
        _ -> crateFeatures spec
    uploadDependency (Object fields) =
        let named = case KeyMap.lookup "package" fields of
                Just (String name) -> String name
                _ -> fromMaybe Null (KeyMap.lookup "name" fields)
            alias = case KeyMap.lookup "package" fields of
                Just (String _) -> fromMaybe Null (KeyMap.lookup "name" fields)
                _ -> Null
            registry = case KeyMap.lookup "registry" fields of
                Just (String source) -> String source
                _ -> String "https://github.com/rust-lang/crates.io-index"
         in Object $ KeyMap.insert "registry" registry $ KeyMap.insert "explicit_name_in_toml" alias $ KeyMap.insert "name" named $ KeyMap.insert "version_req" (fromMaybe Null (KeyMap.lookup "req" fields)) $ KeyMap.delete "package" $ KeyMap.delete "req" fields
    uploadDependency other = other

-- | A checked uint32 length prevents fixture framing from wrapping before negative controls mutate it.
cargoUpload :: CargoRelease -> Either Text ByteString
cargoUpload release = cargoUploadWithMetadata (cargoUploadMetadata release) (releaseArchive release)

-- | Reuse native length framing when malformed metadata must reach the product unchanged.
cargoUploadWithMetadata :: Value -> ByteString -> Either Text ByteString
cargoUploadWithMetadata value archiveBytes = do
    let metadata = LBS.toStrict (encode value)
    for_ [metadata, archiveBytes] $ \bytes ->
        unless (toInteger (BS.length bytes) <= toInteger (maxBound :: Word32)) (Left "Cargo fixture exceeds a uint32 frame")
    pure (cargoUploadFrame metadata <> cargoUploadFrame archiveBytes)

-- | One native section: a uint32 little-endian length, then the bytes. Lengths past a uint32 wrap.
cargoUploadFrame :: ByteString -> ByteString
cargoUploadFrame bytes = LBS.toStrict (toLazyByteString (word32LE (fromIntegral (BS.length bytes)) <> byteString bytes))

-- | Seed the owned consumer's complete lock graph with exact source identities and checksums.
cargoFixtureLock :: Text -> [CargoRelease] -> Text
cargoFixtureLock source = fixtureLock source consumer
  where
    consumer = CargoCrate "ecluse-e2e-consumer" "0.0.0" "unused" [dependency cargoRoot ("=" <> cargoVersion)] emptyFeatures emptyFeatures []

fixtureLock :: Text -> CargoCrate -> [CargoRelease] -> Text
fixtureLock source rootSpec releases =
    "version = 3\n" <> foldMap package (sortOn (packageIdentity . fst) packages)
  where
    -- The packaged root has no registry checksum, which avoids an archive checksum cycle.
    packages = (rootSpec, Nothing) : [(releaseCrate release, Just (releaseChecksum release)) | release <- releases]
    packageIdentity spec = (crateName spec, crateVersion spec)
    quoted text = decodeUtf8 (LBS.toStrict (encode text))
    package (spec, checksum) =
        let dependencies = Map.keys (Map.fromList [(name, ()) | name <- mapMaybe dependencyVersion (crateDependencies spec)])
            dependencyLines = if null dependencies then "" else "dependencies = [\n" <> foldMap (\name -> " " <> quoted name <> ",\n") dependencies <> "]\n"
            registryLines = maybe "" (\digest -> "source = " <> quoted source <> "\nchecksum = " <> quoted digest <> "\n") checksum
         in "\n[[package]]\nname = " <> quoted (crateName spec) <> "\nversion = " <> quoted (crateVersion spec) <> "\n" <> registryLines <> dependencyLines
    dependencyVersion (Object fields) = do
        String alias <- KeyMap.lookup "name" fields
        let name = case KeyMap.lookup "package" fields of
                Just (String original) -> original
                _ -> alias
            version = case KeyMap.lookup "req" fields of
                Just (String "^2.0") -> "2.0.0"
                _ -> cargoVersion
            ambiguous = length (filter ((== name) . crateName . fst) packages) > 1
        pure (name <> if ambiguous then " " <> version else "")
    dependencyVersion _ = Nothing

-- | Write files under the shared registry fixture mount. The caller owns its Stub route and lifetime.
writeCargoRegistry :: FilePath -> Text -> Maybe Text -> Bool -> [CargoRelease] -> IO ()
writeCargoRegistry root proxyBase api authenticated releases = do
    write "config.json" (cargoViewDiscovery proxyBase api authenticated)
    forM_ (Map.toList grouped) $ \(name, versions) -> write (toString (cargoShard name)) (cargoIndex versions)
    forM_ releases $ \release -> write (toString (cargoArchivePath (releaseCrate release))) (releaseArchive release)
  where
    grouped = Map.fromListWith (flip (<>)) [(crateName (releaseCrate release), [release]) | release <- releases]
    write path bytes = do
        createDirectoryIfMissing True (takeDirectory (root </> path))
        writeFileBS (root </> path) bytes
