-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Complete native gem traversal binds metadata, member checksums and the original artifact bytes.
module Ecluse.Core.Registry.RubyGems.Archive (
    GemArchive,
    inspectGemArchive,
    gemArchiveBytes,
    gemArchiveName,
    gemArchiveSpecification,
    gemArchiveMetadata,
    gemArchiveInventory,
    gemArchiveSha256,
    checkGemArchive,
) where

import Conduit
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Text.Libyaml qualified as Y
import Text.Show (showString, shows, showsPrec)
import UnliftIO.Exception (evaluate)
import UnliftIO.Timeout (timeout)

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Package.Hash (HashAlg (SHA512), computeDigest, hexDigestText, sha256Hex)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Archive.Inflate (InflateFormat (GzipMembers), splitArchiveCharge, withInflatedChunks)
import Ecluse.Core.Registry.Archive.Tar (TarMember (..), scanTar, sourceReader, tarContents, tarMembers)
import Ecluse.Core.Registry.RubyGems.Archive.Metadata
import Ecluse.Core.Registry.RubyGems.Archive.Yaml
import Ecluse.Core.Registry.RubyGems.CompactSpec (GemSpecification (..), specificationCoordinate)
import Ecluse.Core.Registry.RubyGems.Project
import Ecluse.Core.Security.Limits (Limits (..), requestTimeoutSeconds)

-- | Private construction preserves the complete checked artifact and native metadata.
data GemArchive = GemArchive
    { gemArchiveBytes :: ByteString
    , gemArchiveMetadata :: GemMetadata
    , gemArchiveInventory :: Set Text
    , gemArchiveSha256 :: Text
    }
    deriving stock (Eq)

instance Show GemArchive where
    showsPrec _ archive = showString "GemArchive " . shows (gemArchiveName archive)

-- | Validate the full outer tar, every compressed member and the inner file inventory.
inspectGemArchive :: Limits -> (Int -> IO ()) -> ByteString -> IO (Either ParseError GemArchive)
inspectGemArchive limits charge bytes = do
    result <- timeout (requestTimeoutSeconds * 1000000) inspect
    pure (fromMaybe (bad "gem archive validation timed out") result)
  where
    inspect
        | BS.length bytes > maxMirrorArtifactBytes limits = pure (bad "gem archive exceeds its byte bound")
        | otherwise = do
            outer <- scanTar limits{maxMetadataBytes = maxMirrorArtifactBytes limits} charge (\parts -> Right (parts `elem` [["metadata.gz"], ["data.tar.gz"], ["checksums.yaml.gz"]])) (\consume -> consume (yield bytes))
            case outer >>= envelope of
                Left fault -> pure (Left fault)
                Right (metadataGzip, dataGzip, checksumsGzip) -> do
                    let retained = BS.length bytes + BS.length metadataGzip + BS.length dataGzip + BS.length checksumsGzip
                        pay extra = charge (retained + extra)
                    metadataBytes <- inflateMetadata limits pay metadataGzip
                    checksumBytes <- inflateMetadata limits (\extra -> pay (either (const 0) ((3 *) . BS.length) metadataBytes + extra)) checksumsGzip
                    case (metadataBytes, checksumBytes) of
                        (Right raw, Right sums) -> do
                            let nativePay extra = pay (128 * (BS.length raw + BS.length sums) + extra)
                            parsed <- decodeYaml limits nativePay raw
                            checksums <- decodeYaml limits nativePay sums
                            case (parsed >>= parseGemMetadata, checksums >>= verifyChecksums metadataGzip dataGzip) of
                                (Right metadata, Right ()) -> do
                                    (decoderCharge, treeCharge) <- splitArchiveCharge nativePay
                                    let innerLimits = limits{maxMirrorArtifactBytes = maxMirrorArtifactBytes limits - BS.length raw - BS.length sums}
                                    inner <- scanTar innerLimits treeCharge (const (Right False)) (\consume -> withInflatedChunks (GzipMembers 1) decoderCharge dataGzip (consume . sourceReader))
                                    case inner of
                                        Left fault -> pure (Left fault)
                                        Right scan -> do
                                            let inventory = Map.keysSet (Map.filter (/= Directory) (tarMembers scan))
                                            if inventory /= Set.fromList (metadataFiles metadata)
                                                then pure (bad "gem data inventory disagrees with metadata files")
                                                else do
                                                    digest <- evaluate (sha256Hex bytes)
                                                    Right <$> evaluate (GemArchive bytes metadata inventory digest)
                                (Left fault, _) -> pure (Left fault)
                                (_, Left fault) -> pure (Left fault)
                        (Left fault, _) -> pure (Left fault)
                        (_, Left fault) -> pure (Left fault)
    envelope scan = do
        let members = tarMembers scan
            mandatory = ["metadata.gz", "data.tar.gz", "checksums.yaml.gz"]
            allowed = mandatory <> map (<> ".sig") mandatory
        unless (all (`elem` allowed) (Map.keys members) && all (== Regular) (Map.elems members)) (bad "unsupported gem envelope member")
        traverse_ (\name -> unless (Map.member name members) (bad "missing required gem envelope member")) mandatory
        let content name = maybeToRight (ParseError "missing gem member payload") (Map.lookup name (tarContents scan))
        (,,) <$> content "metadata.gz" <*> content "data.tar.gz" <*> content "checksums.yaml.gz"

inflateMetadata :: Limits -> (Int -> IO ()) -> ByteString -> IO (Either ParseError ByteString)
inflateMetadata limits charge bytes = withInflatedChunks (GzipMembers 1) charge bytes (collect 0 [])
  where
    collect seen held pull =
        pull >>= \case
            Left fault -> pure (Left fault)
            Right chunk
                | BS.null chunk -> pure (Right (BS.concat (reverse held)))
                | BS.length chunk > maxMetadataBytes limits - seen -> pure (bad "gem compressed metadata exceeds its inflated byte bound")
                | otherwise -> do
                    let next = seen + BS.length chunk
                    charge (73728 + 3 * next)
                    collect next (chunk : held) pull

verifyChecksums :: ByteString -> ByteString -> YamlValue -> Either ParseError ()
verifyChecksums metadataBytes dataBytes value = do
    plainMapping value
    algorithms <- yamlMapping value
    unless (Map.member "SHA256" algorithms && all (`elem` ["SHA256", "SHA512"]) (Map.keys algorithms)) (bad "missing or unsupported gem checksum algorithm")
    for_ (Map.toList algorithms) $ \(algorithm, entries) -> do
        plainMapping entries
        digests <- yamlMapping entries
        unless (Map.keys digests == ["data.tar.gz", "metadata.gz"]) (bad "gem checksum member inventory disagrees")
        for_ [("metadata.gz", metadataBytes), ("data.tar.gz", dataBytes)] $ \(name, bytes) -> do
            expected <- yamlField name digests >>= yamlText
            digest <- maybeToRight (ParseError "SHA512 computation is unavailable") (computeDigest SHA512)
            let actual = if algorithm == "SHA256" then sha256Hex bytes else hexDigestText (digest (BSL.fromStrict bytes))
            unless (T.toLower expected == actual) (bad "gem member checksum disagrees")

plainMapping :: YamlValue -> Either ParseError ()
plainMapping value = unless (yamlTag value `elem` [Y.NoTag, Y.MapTag]) (bad "gem checksum mapping has an unsupported native tag")

-- | Return checked native specification facts, without deriving installation policy evidence.
gemArchiveSpecification :: GemArchive -> GemSpecification
gemArchiveSpecification = metadataSpecification . gemArchiveMetadata

-- | Return the exact package name inside the checked archive.
gemArchiveName :: GemArchive -> PackageName
gemArchiveName = specificationName . gemArchiveSpecification

-- | Bind the checked native archive identity and SHA-256 to an admitted coordinate.
checkGemArchive :: GemCoordinate -> Text -> GemArchive -> Either ParseError GemArchive
checkGemArchive coordinate checksum archive = do
    actual <- specificationCoordinate (gemArchiveSpecification archive)
    unless (actual == coordinate) (bad "gem archive identity disagrees with its admitted coordinate")
    unless (T.toLower checksum == gemArchiveSha256 archive) (bad "gem archive disagrees with its admitted SHA-256")
    pure archive

bad :: Text -> Either ParseError a
bad = Left . ParseError
