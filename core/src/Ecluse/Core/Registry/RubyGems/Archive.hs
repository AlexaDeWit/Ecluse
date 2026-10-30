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
    checkRawEnvelope,
) where

import Conduit
import Control.Monad.Except (throwError)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Text.Libyaml qualified as Y
import Text.Show (showString, shows, showsPrec)
import UnliftIO.Exception (evaluate)

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Package.Hash (HashAlg (SHA512), computeDigest, hexDigestText, sha256Hex)
import Ecluse.Core.Registry (ParseError (..), parseRefusal)
import Ecluse.Core.Registry.Archive.Inflate (InflateFormat (GzipMembers), inflateBounded, withInflatedChunks)
import Ecluse.Core.Registry.Archive.Tar (TarMember (..), TarScan, scanTar, sourceReader, tarContents, tarMembers, tarRawNames)
import Ecluse.Core.Registry.RubyGems.Archive.Metadata
import Ecluse.Core.Registry.RubyGems.Archive.Yaml
import Ecluse.Core.Registry.RubyGems.CompactSpec (GemSpecification (..), specificationCoordinate)
import Ecluse.Core.Registry.RubyGems.Project
import Ecluse.Core.Security.Limits (Limits (..))
import Ecluse.Core.Server.Admission.Meter (newChargePair)

-- | Private construction preserves the complete checked artifact and native metadata.
data GemArchive = GemArchive
    { gemArchiveBytes :: ByteString
    , gemArchiveMetadata :: GemMetadata
    , gemArchiveInventory :: Set Text
    , gemArchiveSha256 :: Text
    , gemArchiveRawNames :: !Bool
    }
    deriving stock (Eq)

instance Show GemArchive where
    showsPrec _ archive = showString "GemArchive " . shows (gemArchiveName archive)

-- | Validate the full outer tar, every compressed member and the inner file inventory.
inspectGemArchive :: Limits -> (Int -> IO ()) -> ByteString -> IO (Either ParseError GemArchive)
inspectGemArchive limits charge bytes
    | BS.length bytes > maxMirrorArtifactBytes limits = pure (parseRefusal "gem archive exceeds its byte bound")
    | otherwise = runExceptT $ do
        (!rawNames, (metadataGzip, dataGzip, checksumsGzip)) <- ExceptT (envelopeMembers limits charge bytes)
        let pay extra = charge (BS.length bytes + BS.length metadataGzip + BS.length dataGzip + BS.length checksumsGzip + extra)
        (raw, sums) <- ExceptT (inflateMembers limits pay metadataGzip checksumsGzip)
        let nativePay extra = pay (128 * (BS.length raw + BS.length sums) + extra)
        metadata <- ExceptT (decodeMembers limits nativePay metadataGzip dataGzip raw sums)
        inventory <- ExceptT (dataInventory limits nativePay (BS.length raw + BS.length sums) dataGzip)
        unless (inventory == Set.fromList (metadataFiles metadata)) (throwError (ParseError "gem data inventory disagrees with metadata files"))
        digest <- evaluate (sha256Hex bytes)
        evaluate (GemArchive bytes metadata inventory digest rawNames)

-- Whether every outer member kept its raw name, then the metadata, data and checksum members, each still compressed.
envelopeMembers :: Limits -> (Int -> IO ()) -> ByteString -> IO (Either ParseError (Bool, (ByteString, ByteString, ByteString)))
envelopeMembers limits charge bytes = (>>= \scan -> (tarRawNames scan,) <$> envelope scan) <$> scanTar limits{maxMetadataBytes = maxMirrorArtifactBytes limits} charge (\parts -> Right (parts `elem` [["metadata.gz"], ["data.tar.gz"], ["checksums.yaml.gz"]])) (\consume -> consume (yield bytes))

envelope :: TarScan -> Either ParseError (ByteString, ByteString, ByteString)
envelope scan = do
    let members = tarMembers scan
        mandatory = ["metadata.gz", "data.tar.gz", "checksums.yaml.gz"]
        allowed = mandatory <> map (<> ".sig") mandatory
    unless (all (`elem` allowed) (Map.keys members) && all (== Regular) (Map.elems members)) (parseRefusal "unsupported gem envelope member")
    traverse_ (\name -> unless (Map.member name members) (parseRefusal "missing required gem envelope member")) mandatory
    let content name = maybeToRight (ParseError "missing gem member payload") (Map.lookup name (tarContents scan))
    (,,) <$> content "metadata.gz" <*> content "data.tar.gz" <*> content "checksums.yaml.gz"

-- Inflate both small members, the checksums charged on top of the inflated metadata. Both always run.
inflateMembers :: Limits -> (Int -> IO ()) -> ByteString -> ByteString -> IO (Either ParseError (ByteString, ByteString))
inflateMembers limits pay metadataGzip checksumsGzip = do
    metadataBytes <- inflate pay metadataGzip
    checksumBytes <- inflate (\extra -> pay (either (const 0) ((3 *) . BS.length) metadataBytes + extra)) checksumsGzip
    pure ((,) <$> metadataBytes <*> checksumBytes)
  where
    inflate = inflateBounded (GzipMembers 1) "gem compressed metadata exceeds its inflated byte bound" (maxMetadataBytes limits)

-- Decode both YAML documents, then the metadata schema ahead of the member checksums.
decodeMembers :: Limits -> (Int -> IO ()) -> ByteString -> ByteString -> ByteString -> ByteString -> IO (Either ParseError GemMetadata)
decodeMembers limits nativePay metadataGzip dataGzip raw sums = do
    parsed <- decodeYaml limits nativePay raw
    checksums <- decodeYaml limits nativePay sums
    pure ((parsed >>= parseGemMetadata) <* (checksums >>= verifyChecksums metadataGzip dataGzip))

-- The inner data tar's non-directory members, within what the decoded metadata left of the artifact bound.
dataInventory :: Limits -> (Int -> IO ()) -> Int -> ByteString -> IO (Either ParseError (Set Text))
dataInventory limits nativePay decoded dataGzip = do
    (decoderCharge, treeCharge) <- newChargePair nativePay
    let innerLimits = limits{maxMirrorArtifactBytes = maxMirrorArtifactBytes limits - decoded}
    inner <- scanTar innerLimits treeCharge (const (Right False)) (\consume -> withInflatedChunks (GzipMembers 1) decoderCharge dataGzip (consume . sourceReader))
    pure (Map.keysSet . Map.filter (/= Directory) . tarMembers <$> inner)

verifyChecksums :: ByteString -> ByteString -> YamlValue -> Either ParseError ()
verifyChecksums metadataBytes dataBytes value = do
    plainMapping value
    algorithms <- yamlMapping value
    unless (Map.member "SHA256" algorithms && all (`elem` ["SHA256", "SHA512"]) (Map.keys algorithms)) (parseRefusal "missing or unsupported gem checksum algorithm")
    for_ (Map.toList algorithms) $ \(algorithm, entries) -> do
        plainMapping entries
        digests <- yamlMapping entries
        unless (Map.keys digests == ["data.tar.gz", "metadata.gz"]) (parseRefusal "gem checksum member inventory disagrees")
        for_ [("metadata.gz", metadataBytes), ("data.tar.gz", dataBytes)] $ \(name, bytes) -> do
            expected <- yamlField name digests >>= yamlText
            digest <- maybeToRight (ParseError "SHA512 computation is unavailable") (computeDigest SHA512)
            let actual = if algorithm == "SHA256" then sha256Hex bytes else hexDigestText (digest (BSL.fromStrict bytes))
            unless (T.toLower expected == actual) (parseRefusal "gem member checksum disagrees")

plainMapping :: YamlValue -> Either ParseError ()
plainMapping value = unless (yamlTag value `elem` [Y.NoTag, Y.MapTag]) (parseRefusal "gem checksum mapping has an unsupported native tag")

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
    unless (actual == coordinate) (parseRefusal "gem archive identity disagrees with its admitted coordinate")
    unless (T.toLower checksum == gemArchiveSha256 archive) (parseRefusal "gem archive disagrees with its admitted SHA-256")
    pure archive

{- | Refuse an outer tar that names any member through an extended header or a ustar prefix.
RubyGems' reader yields extended headers as ordinary members, so a Ruby store could register another gem.
-}
checkRawEnvelope :: GemArchive -> Either ParseError GemArchive
checkRawEnvelope archive = do
    unless (gemArchiveRawNames archive) (parseRefusal "gem envelope carries an extended or prefixed tar header")
    pure archive
