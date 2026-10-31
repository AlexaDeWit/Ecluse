-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native corpus framing and independent held-tree observations for complete sparse files.
module Ecluse.Test.Corpus.Cargo (
    benchCrate,
    syntheticIndexBytes,
    cargoCaptureDiscovery,
    cargoCaptureDiscoveryBytes,
    cargoServedDiscovery,
    cargoVersionKeys,
    decodeCargoValues,
    encodeCargoValues,
    projectCargoTree,
    projectCargoValues,
) where

import Data.Aeson (Value (..), encode, object, toJSON, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package (Artifact (..), Availability (Available, Yanked), CodeExecSignal (CodeExecUnknown), DependencySources (RegistryDependencies), HashAlg (SHA256), PackageDetails (..), PackageInfo (..), PackageName, mkHash, mkPackageName, renderPackageName)
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, cargoCached)
import Ecluse.Core.Registry.Cargo.Discovery (CargoDiscovery, renderCargoDiscovery)
import Ecluse.Core.Registry.Cargo.Document (cargoDocument)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataUndecodable))
import Ecluse.Core.Security (Limits, checkVersionCountOf)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Corpus (CaptureUpstream (..), cargoCaptureUpstream, syntheticProxyBase)
import Ecluse.Test.Registry.Cargo.Metadata (parseCargoDiscovery)
import Ecluse.Test.Registry.Cargo.Reference (IndexLine (..), referenceIndex)
import Ecluse.Test.Security.Limits (checkNestingDepth)

-- | Synthetic complexity inputs use a distinct native package identity.
benchCrate :: PackageName
benchCrate = mkPackageName Cargo Nothing "bench-crate"

-- | Bind the public capture's native download template without performing discovery I/O.
cargoCaptureDiscovery :: Either MetadataError CargoDiscovery
cargoCaptureDiscovery = parseCargoDiscovery (upstreamAuthorities cargoCaptureUpstream) (upstreamOrigin cargoCaptureUpstream) cargoCaptureDiscoveryBytes

-- | The public capture advertises its complete upstream download template.
cargoCaptureDiscoveryBytes :: ByteString
cargoCaptureDiscoveryBytes = renderCargoDiscovery "https://static.crates.io/crates/{crate}/{crate}-{version}.crate" Nothing False

-- | Use the local discovery response shape shared with the actual sparse route.
cargoServedDiscovery :: Either MetadataError CargoDiscovery
cargoServedDiscovery = parseCargoDiscovery (upstreamAuthorities cargoCaptureUpstream) syntheticProxyBase (renderCargoDiscovery (syntheticProxyBase <> "/crates/{crate}/{version}/{crate}-{version}.crate") Nothing False)

-- | Read raw release identifiers in physical source order, independently of production projection.
cargoVersionKeys :: ByteString -> Either Text [Text]
cargoVersionKeys = traverse version . referenceIndex
  where
    version line = indexValue line >>= first toText . parseEither (.: "vers")

-- | Decode each record independently into an internal array used only by corpus transforms.
decodeCargoValues :: ByteString -> Either Text Value
decodeCargoValues bytes = toJSON <$> traverse (fmap Object . indexValue) (referenceIndex bytes)

-- | Emit one original-shaped object per LF record, with no array envelope on the wire.
encodeCargoValues :: Value -> Either Text ByteString
encodeCargoValues = \case
    Array records -> foldMap (\value -> toStrict (encode value) <> "\n") <$> traverse requireObject (toList records)
    _ -> Left "Cargo corpus transform requires native records"
  where
    requireObject value@(Object _) = Right value
    requireObject _ = Left "Cargo corpus transform contains a non-object record"

-- | Independent Aeson rows retain source ordinals and all opaque native fields.
projectCargoTree :: Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, CachedDoc)
projectCargoTree limits name bytes = do
    referenceRows bytes >>= projectRows sourceUrl limits name

-- | Adapt an internal native array for the held-tree source comparison.
projectCargoValues :: Limits -> PackageName -> Value -> Either MetadataError (PackageInfo, Value)
projectCargoValues limits name value = case value of
    Array records -> (\(info, _) -> (info, value)) <$> projectRows sourceUrl limits name (zip (map RecordEntry [0 ..]) (toList records))
    _ -> Left MetadataUndecodable

projectRows :: (Text -> Text -> Text) -> Limits -> PackageName -> [(EntryKey, Value)] -> Either MetadataError (PackageInfo, CachedDoc)
projectRows location limits name rows = do
    first MetadataBoundExceeded (checkVersionCountOf limits (length rows))
    details <- traverse project rows
    let releases = Map.fromList [(raw, facts) | (raw, facts) <- details]
    unless (Map.size releases == length details) (Left MetadataUndecodable)
    pure (PackageInfo name releases Map.empty [], fst cargoCached (cargoDocument rows))
  where
    project (key, value) = do
        bounded <- first MetadataBoundExceeded (checkNestingDepth limits value)
        fields <- case bounded of
            Object members -> Right members
            _ -> Left MetadataUndecodable
        (reported, version, checksum, published, yanked) <- first (const MetadataUndecodable) (parseEither native fields)
        unless (reported == renderPackageName name) (Left MetadataUndecodable)
        digest <- first (const MetadataUndecodable) (mkHash SHA256 checksum)
        let url = location reported version
            artifact = Artifact key (reported <> "-" <> version <> ".crate") url [digest] Nothing Nothing
        -- crates.io refuses a dependency from another registry, so its captured rows resolve through the registry.
        pure (version, PackageDetails name (mkVersion Cargo version) published CodeExecUnknown RegistryDependencies (if yanked then Yanked else Available) (artifact :| []))
    native fields = (,,,,) <$> fields .: "name" <*> fields .: "vers" <*> fields .: "cksum" <*> fields .:? "pubtime" <*> fields .:? "yanked" .!= False

-- | Positive counts produce complete distinct releases with native dependencies and features.
syntheticIndexBytes :: Int -> ByteString
syntheticIndexBytes count = foldMap record [0 .. count - 1]
  where
    record index =
        toStrict
            ( encode
                ( object
                    [ "name" .= renderPackageName benchCrate
                    , "vers" .= ("1.0." <> show index :: Text)
                    , "deps" .= ([] :: [Value])
                    , "cksum" .= T.replicate 64 "0"
                    , "features" .= object []
                    , "yanked" .= False
                    , "pubtime" .= ("2020-01-01T00:00:00Z" :: Text)
                    ]
                )
            )
            <> "\n"

sourceUrl :: Text -> Text -> Text
sourceUrl reported version = "https://static.crates.io/crates/" <> reported <> "/" <> reported <> "-" <> version <> ".crate"

referenceRows :: ByteString -> Either MetadataError [(EntryKey, Value)]
referenceRows = traverse row . referenceIndex
  where
    row line = (RecordEntry (indexPosition line),) . Object <$> first (const MetadataUndecodable) (indexValue line)
