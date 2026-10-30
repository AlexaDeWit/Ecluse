-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Inspect verified Cargo archives without extraction or execution. Publication text uses one further local pass.
module Ecluse.Core.Registry.Cargo.Archive (
    CargoArchiveMetadata,
    inspectCargoArchive,
    cargoArchiveInventory,
    cargoArchiveInflatedBytes,
    cargoArchiveExecution,
    cargoPublicationMetadata,
    cargoPublicationAgreement,
    cargoUploadAgreement,
) where

import Data.Aeson (Value)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Text.Show (showString, shows, showsPrec)
import UnliftIO.Exception (evaluate)

import Ecluse.Core.Package.Hash (sha256Hex)
import Ecluse.Core.Registry (ParseError (..), parseRefusal)
import Ecluse.Core.Registry.Archive.Inflate (InflateFormat (GzipMembers), withInflatedChunks)
import Ecluse.Core.Registry.Archive.Tar (TarMember (..), TarScan, scanTar, sourceReader, tarContents, tarMembers)
import Ecluse.Core.Registry.CachedDocument (estimateValueBytes)
import Ecluse.Core.Registry.Cargo.Manifest (CargoManifest, CargoPublishMetadata, checkPublicationIndex, checkUploadAgreement, decodeManifest, manifestExecution, manifestTextPaths, publicationMetadata, publishMetadataValue)
import Ecluse.Core.Registry.Cargo.Project (cargoArchiveStem)
import Ecluse.Core.Registry.Cargo.Wire (cargoTreeCharge)
import Ecluse.Core.Security.Limits (Limits (..))
import Ecluse.Core.Server.Admission.Meter (newChargePair)
import Ecluse.Core.Server.MemoryModel (saturatingCharge)

-- | The inspected source bytes, checked against the admitted SHA-256 when one exists.
data CargoArchiveMetadata = CargoArchiveMetadata
    { caManifest :: CargoManifest
    , caInventory :: Set Text
    , caSource :: ByteString
    , caRoot :: Text
    , caRetainedBytes :: Int
    , caManifestWorkspace :: Int
    , caInflatedBytes :: Int
    }

instance Show CargoArchiveMetadata where
    showsPrec _ metadata = showString "CargoArchiveMetadata " . shows (caRoot metadata)

{- | Charge extra workspace as an absolute total. The caller owns the compressed-byte charge.
A first-party upload carries no checksum, so only an admitted SHA-256 is compared.
-}
inspectCargoArchive :: Limits -> (Int -> IO ()) -> Text -> Text -> Maybe Text -> ByteString -> IO (Either ParseError CargoArchiveMetadata)
inspectCargoArchive limits charge name version checksum bytes = do
    if maxMirrorArtifactBytes limits <= 0 || BS.length bytes > maxMirrorArtifactBytes limits
        then pure (parseRefusal "Cargo compressed archive exceeds its byte bound")
        else
            if any (\expected -> T.toLower expected /= sha256Hex bytes) checksum
                then pure (parseRefusal "Cargo compressed archive disagrees with its admitted SHA-256")
                else do
                    let root = cargoArchiveStem name version
                    result <- archivePass limits charge root (Set.singleton "Cargo.toml") bytes
                    case result >>= \(scan, inflated) -> (,inflated) <$> archiveFacts root scan of
                        Left fault -> pure (Left fault)
                        Right ((manifestBytes, inventory, retained), inflated) -> do
                            charge (retained + manifestMemory manifestBytes)
                            parsed <- evaluate (decodeManifest limits name version manifestBytes)
                            pure $ (\manifest -> CargoArchiveMetadata manifest inventory bytes root (retained + manifestMemory manifestBytes) (manifestMemory manifestBytes) inflated) <$> parsed

-- | Only regular-file paths appear, relative to the admitted single archive root.
cargoArchiveInventory :: CargoArchiveMetadata -> Set Text
cargoArchiveInventory = caInventory

-- | Complete first-pass decoder output, including tar headers, padding and trailers.
cargoArchiveInflatedBytes :: CargoArchiveMetadata -> Int
cargoArchiveInflatedBytes = caInflatedBytes

-- | An unsupported declaration remains an error. Source declarations alone do not establish harmlessness.
cargoArchiveExecution :: CargoArchiveMetadata -> Either ParseError Bool
cargoArchiveExecution metadata = manifestExecution (caManifest metadata) (cargoArchiveInventory metadata)

-- | Capture only the selected README and licence after the manifest pass, using the same verified compressed bytes.
cargoPublicationMetadata :: Limits -> (Int -> IO ()) -> CargoArchiveMetadata -> IO (Either ParseError CargoPublishMetadata)
cargoPublicationMetadata limits charge metadata =
    case manifestTextPaths (caManifest metadata) (caInventory metadata) of
        Left fault -> pure (Left fault)
        Right [] -> finish Map.empty
        Right paths -> do
            result <- archivePass limits (charge . (+ retained)) (caRoot metadata) (Set.fromList paths) (caSource metadata)
            case result of
                Left fault -> pure (Left fault)
                Right (scan, _) -> finish (Map.mapKeys dropArchiveRoot (tarContents scan))
  where
    retained = caRetainedBytes metadata
    finish content = do
        let textBytes = sum (map BS.length (Map.elems content))
        charge (retained + 65536 + 8 * textBytes + 512 * Map.size content)
        evaluate (publicationMetadata (caManifest metadata) content)

-- | Compare mirror source claims after reserving both resolver inputs.
cargoPublicationAgreement :: Limits -> (Int -> IO ()) -> CargoArchiveMetadata -> Value -> CargoPublishMetadata -> IO (Either ParseError CargoPublishMetadata)
cargoPublicationAgreement limits charge archive source metadata = do
    let inputs = [source, publishMetadataValue metadata]
        workspace = sum (map (toInteger . cargoTreeCharge . toInteger . estimateValueBytes) inputs)
        retained = toInteger (caRetainedBytes archive) + toInteger (caManifestWorkspace archive)
    charge (saturatingCharge (retained + workspace))
    evaluate (checkPublicationIndex limits metadata source)

-- | Reuse the retained effective manifest without a text extraction or another archive pass.
cargoUploadAgreement :: Limits -> (Int -> IO ()) -> Int -> Text -> Value -> CargoArchiveMetadata -> IO (Either ParseError ())
cargoUploadAgreement limits charge uploadWorkspace registry upload metadata = do
    charge (saturatingCharge (sum (map toInteger [caRetainedBytes metadata, caManifestWorkspace metadata, uploadWorkspace])))
    evaluate (checkUploadAgreement limits registry (caManifest metadata) upload)

archiveFacts :: Text -> TarScan -> Either ParseError (ByteString, Set Text, Int)
archiveFacts root scan = do
    case Map.lookup root (tarMembers scan) of
        Nothing -> Right ()
        Just Directory -> Right ()
        _ -> parseRefusal "Cargo archive root is not a directory"
    for_ (Map.elems (tarMembers scan)) $ \case
        Link _ -> parseRefusal "Cargo packaged archive contains an unflattened link"
        _ -> Right ()
    let prefix = root <> "/"
        files = Set.fromList [dropArchiveRoot path | (path, Regular) <- Map.toList (tarMembers scan)]
        retained = 65536 + sum [256 + 4 * BS.length (TE.encodeUtf8 path) | path <- Set.toList files]
    for_ (Set.toList files) $ \path -> do
        let prefixes = drop 1 (scanl (\held part -> if T.null held then part else held <> "/" <> part) "" (T.splitOn "/" path))
        when (any (`Set.member` files) (take (length prefixes - 1) prefixes)) (parseRefusal "Cargo archive file traverses another regular file")
    manifest <- maybe (parseRefusal "Cargo archive lacks its effective Cargo.toml") Right (Map.lookup (prefix <> "Cargo.toml") (tarContents scan))
    pure (manifest, files, retained)

dropArchiveRoot :: Text -> Text
dropArchiveRoot = T.drop 1 . snd . T.breakOn "/"

archivePass :: Limits -> (Int -> IO ()) -> Text -> Set Text -> ByteString -> IO (Either ParseError (TarScan, Int))
archivePass limits charge root selected bytes = do
    (decoderCharge, retainedCharge) <- newChargePair charge
    inflated <- newIORef (0 :: Integer)
    result <- scanTar limits retainedCharge select $ \consume ->
        withInflatedChunks (GzipMembers (maxArtifactCount limits)) decoderCharge bytes $ \pull ->
            consume . sourceReader $ do
                outcome <- pull
                for_ outcome $ \chunk -> modifyIORef' inflated (+ toInteger (BS.length chunk))
                pure outcome
    count <- readIORef inflated
    pure ((,fromInteger count) <$> result)
  where
    select parts = case parts of
        actualRoot : relative | actualRoot == root -> Right (Set.member (T.intercalate "/" relative) selected)
        _ -> parseRefusal "Cargo archive member disagrees with its admitted package root"

manifestMemory :: ByteString -> Int
manifestMemory = cargoTreeCharge . toInteger . BS.length
