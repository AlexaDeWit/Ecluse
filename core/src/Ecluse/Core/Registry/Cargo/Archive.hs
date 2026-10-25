-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Inspect verified Cargo archives without extraction or execution. Publication text uses one further local pass.
module Ecluse.Core.Registry.Cargo.Archive (
    CargoArchiveMetadata,
    inspectCargoArchive,
    cargoArchiveManifest,
    cargoArchiveInventory,
    cargoArchiveExecution,
    cargoPublicationMetadata,
) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import UnliftIO.Exception (evaluate)
import UnliftIO.Timeout (timeout)

import Ecluse.Core.Package.Hash (sha256Hex)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Archive.Inflate (InflateFormat (GzipMembers), splitArchiveCharge, withInflatedChunks)
import Ecluse.Core.Registry.Archive.Tar (TarMember (..), TarScan, scanTar, sourceReader, tarContents, tarMembers)
import Ecluse.Core.Registry.Cargo.Manifest (CargoManifest, CargoPublishMetadata, decodeManifest, manifestExecution, manifestTextPaths, publicationMetadata)
import Ecluse.Core.Security.Limits (Limits (..), requestTimeoutSeconds)

-- | The private source bytes are the same bytes checked against the admitted SHA-256.
data CargoArchiveMetadata = CargoArchiveMetadata
    { caManifest :: CargoManifest
    , caInventory :: Set Text
    , caSource :: ByteString
    , caRoot :: Text
    , caRetainedBytes :: Int
    }

instance Show CargoArchiveMetadata where
    show metadata = "CargoArchiveMetadata " <> show (caRoot metadata)

-- | Charge extra workspace as an absolute total. The caller separately owns the compressed-byte charge.
inspectCargoArchive :: Limits -> (Int -> IO ()) -> Text -> Text -> Text -> ByteString -> IO (Either ParseError CargoArchiveMetadata)
inspectCargoArchive limits charge name version checksum bytes = deadline $ do
    if maxMirrorArtifactBytes limits <= 0 || BS.length bytes > maxMirrorArtifactBytes limits
        then pure (bad "Cargo compressed archive exceeds its byte bound")
        else
            if T.toLower checksum /= sha256Hex bytes
                then pure (bad "Cargo compressed archive disagrees with its admitted SHA-256")
                else do
                    let root = name <> "-" <> version
                    result <- archivePass limits charge root (Set.singleton "Cargo.toml") bytes
                    case result >>= archiveFacts root of
                        Left fault -> pure (Left fault)
                        Right (manifestBytes, inventory, retained) -> do
                            charge (retained + manifestMemory manifestBytes)
                            parsed <- evaluate (decodeManifest limits name version manifestBytes)
                            pure $ (\manifest -> CargoArchiveMetadata manifest inventory bytes root (retained + manifestMemory manifestBytes)) <$> parsed

-- | The effective normalised manifest carries exact native package spelling and version.
cargoArchiveManifest :: CargoArchiveMetadata -> CargoManifest
cargoArchiveManifest = caManifest

-- | Only regular-file paths appear, relative to the admitted single archive root.
cargoArchiveInventory :: CargoArchiveMetadata -> Set Text
cargoArchiveInventory = caInventory

-- | An unsupported declaration remains an error. Source declarations alone do not establish harmlessness.
cargoArchiveExecution :: CargoArchiveMetadata -> Either ParseError Bool
cargoArchiveExecution metadata = manifestExecution (caManifest metadata) (caInventory metadata)

-- | Capture only the selected README and licence after the manifest pass, using the same verified compressed bytes.
cargoPublicationMetadata :: Limits -> (Int -> IO ()) -> CargoArchiveMetadata -> IO (Either ParseError CargoPublishMetadata)
cargoPublicationMetadata limits charge metadata = deadline $
    case manifestTextPaths (caManifest metadata) (caInventory metadata) of
        Left fault -> pure (Left fault)
        Right paths -> do
            let retained = caRetainedBytes metadata
            result <- archivePass limits (charge . (+ retained)) (caRoot metadata) (Set.fromList paths) (caSource metadata)
            case result of
                Left fault -> pure (Left fault)
                Right scan -> do
                    let content = Map.mapKeys (T.drop (T.length (caRoot metadata) + 1)) (tarContents scan)
                        textBytes = sum (map BS.length (Map.elems content))
                    charge (retained + 65536 + 8 * textBytes + 512 * Map.size content)
                    evaluate (publicationMetadata (caManifest metadata) content)

archiveFacts :: Text -> TarScan -> Either ParseError (ByteString, Set Text, Int)
archiveFacts root scan = do
    case Map.lookup root (tarMembers scan) of
        Nothing -> Right ()
        Just Directory -> Right ()
        _ -> bad "Cargo archive root is not a directory"
    for_ (Map.elems (tarMembers scan)) $ \case
        Link _ -> bad "Cargo packaged archive contains an unflattened link"
        _ -> Right ()
    let prefix = root <> "/"
        files = Set.fromList [T.drop (T.length prefix) path | (path, Regular) <- Map.toList (tarMembers scan)]
        retained = 65536 + sum [256 + 4 * BS.length (TE.encodeUtf8 path) | path <- Set.toList files]
    for_ (Set.toList files) $ \path -> do
        let prefixes = drop 1 (scanl (\held part -> if T.null held then part else held <> "/" <> part) "" (T.splitOn "/" path))
        when (any (`Set.member` files) (take (length prefixes - 1) prefixes)) (bad "Cargo archive file traverses another regular file")
    manifest <- maybe (bad "Cargo archive lacks its effective Cargo.toml") Right (Map.lookup (prefix <> "Cargo.toml") (tarContents scan))
    pure (manifest, files, retained)

archivePass :: Limits -> (Int -> IO ()) -> Text -> Set Text -> ByteString -> IO (Either ParseError TarScan)
archivePass limits charge root selected bytes = do
    (decoderCharge, retainedCharge) <- splitArchiveCharge charge
    scanTar limits retainedCharge select $ \consume ->
        withInflatedChunks (GzipMembers (maxArtifactCount limits)) decoderCharge bytes (consume . sourceReader)
  where
    select parts = case parts of
        actualRoot : relative | actualRoot == root -> Right (Set.member (T.intercalate "/" relative) selected)
        _ -> bad "Cargo archive member disagrees with its admitted package root"

manifestMemory :: ByteString -> Int
manifestMemory bytes = 65536 + 64 * BS.length bytes

deadline :: IO (Either ParseError a) -> IO (Either ParseError a)
deadline action = fromMaybe (bad "Cargo archive inspection exceeded its deadline") <$> timeout (requestTimeoutSeconds * 1000000) action

bad :: Text -> Either ParseError a
bad = Left . ParseError
