-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Bounded in-memory Core Metadata extraction from verified Python distributions.
module Ecluse.Core.Registry.PyPI.Archive (archiveMetadata) where

import Conduit
import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString, word64LE)
import Data.ByteString.Lazy qualified as BSL
import Data.Digest.CRC32 (crc32Update)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import UnliftIO.Exception (evaluate)

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError (..), parseRefusal)
import Ecluse.Core.Registry.Archive.Inflate (InflateFormat (..), withInflatedChunks)
import Ecluse.Core.Registry.Archive.Path (checkPath)
import Ecluse.Core.Registry.Archive.Tar (sourceReader)
import Ecluse.Core.Registry.PyPI.Archive.BZip (withBzipChunks)
import Ecluse.Core.Registry.PyPI.Archive.Lzma (LzmaFormat (..), withLzmaChunks)
import Ecluse.Core.Registry.PyPI.Archive.Path (metadataPath)
import Ecluse.Core.Registry.PyPI.Archive.Tar (extractTar)
import Ecluse.Core.Registry.PyPI.Project (DistributionKind (..), FileCoordinate (..), fileCoordinate)
import Ecluse.Core.Security.Limits (Limits (..))
import Ecluse.Core.Server.Admission.Meter (newChargePair)
import Ecluse.Core.Version (Version)

-- | Request absolute extra workspace before retention. The caller separately charges the verified source bytes.
archiveMetadata :: Limits -> (Int -> IO ()) -> PackageName -> Version -> Text -> ByteString -> IO (Either ParseError ByteString)
archiveMetadata limits charge package version filename bytes = case fileCoordinate package filename of
    Just coordinate
        | fcVersion coordinate == version
        , BS.length bytes <= maxMirrorArtifactBytes limits
        , maxMirrorArtifactBytes limits > 0 -> do
            (decoderCharge, contentCharge) <- newChargePair charge
            contentCharge archiveScratchBytes
            result <-
                if any (`T.isSuffixOf` filename) [".whl", ".zip"]
                    then extractZip limits decoderCharge contentCharge package version (fcKind coordinate) bytes
                    else case () of
                        _
                            | any (`T.isSuffixOf` filename) [".tar.gz", ".tgz"] ->
                                extractTar limits contentCharge package version $ \consume ->
                                    withInflatedChunks (GzipMembers (maxArtifactCount limits)) decoderCharge bytes (consume . sourceReader)
                            | ".tar.bz2" `T.isSuffixOf` filename ->
                                extractTar limits contentCharge package version (withBzipChunks (maxArtifactCount limits) decoderCharge bytes)
                            | ".tar.xz" `T.isSuffixOf` filename ->
                                extractTar limits contentCharge package version (withLzmaChunks XzArchive decoderCharge [bytes])
                            | otherwise -> pure (parseRefusal "unsupported distribution archive")
            traverse (\selected -> selected <$ evaluate (BS.length selected)) result
    _ -> pure (Left (ParseError "distribution filename disagrees with admitted identity"))

archiveScratchBytes :: Int
archiveScratchBytes = 65536

data Scan = Scan
    { scanCount :: !Int
    , scanPathBytes :: !Int
    , scanInflated :: !Integer
    , scanNames :: Set Text
    , scanMetadata :: Maybe ByteString
    }

emptyScan :: Scan
emptyScan = Scan 0 0 0 Set.empty Nothing

entryPathCost :: Limits -> Text -> Scan -> Either ParseError Int
entryPathCost limits path scan = do
    void (checkPath path)
    when (scanCount scan >= maxArtifactCount limits) (parseRefusal "archive entry count exceeds its bound")
    when (Set.member path (scanNames scan)) (parseRefusal "duplicate archive entry")
    let charge = BS.length (TE.encodeUtf8 path) + 128
    when (charge > maxMetadataBytes limits - scanPathBytes scan) (parseRefusal "archive path inventory exceeds its bound")
    pure charge

addInflated :: Limits -> Integer -> Scan -> Either ParseError Scan
addInflated limits size scan
    | size < 0 || scanInflated scan + size > toInteger (maxMirrorArtifactBytes limits) = parseRefusal "archive inflated bytes exceed their bound"
    | otherwise = Right scan{scanInflated = scanInflated scan + size}

keepMetadata :: Limits -> ByteString -> Scan -> Either ParseError Scan
keepMetadata limits bytes scan
    | BS.length bytes > maxMetadataBytes limits = parseRefusal "archive Core Metadata exceeds its bound"
    | isJust (scanMetadata scan) = parseRefusal "ambiguous archive Core Metadata"
    | otherwise = Right scan{scanMetadata = Just (BS.copy bytes)}

finishScan :: Scan -> Either ParseError ByteString
finishScan scan = maybe (parseRefusal "archive has no Core Metadata") Right (scanMetadata scan)

data ZipMember = ZipMember
    { zmName :: Text
    , zmStart :: Integer
    , zmEnd :: Integer
    , zmDataOffset :: Integer
    , zmCompressed :: Integer
    , zmInflated :: Integer
    , zmCrc :: Word32
    , zmMethod :: Integer
    , zmLzmaEndMarker :: Bool
    }

extractZip :: Limits -> (Int -> IO ()) -> (Int -> IO ()) -> PackageName -> Version -> DistributionKind -> ByteString -> IO (Either ParseError ByteString)
extractZip limits decoderCharge charge package version kind bytes = do
    directory <- zipDirectory limits charge bytes
    case directory of
        Left fault -> pure (Left fault)
        Right (members, directoryCharge) -> go directoryCharge emptyScan members
  where
    go _ scan [] = pure (finishScan scan)
    go directoryCharge scan (member : rest) = case prepareMember scan member of
        Left fault -> pure (Left fault)
        Right (pathCost, selected) -> do
            charge (workspace directoryCharge scan 0 0 + 2 * pathCost)
            let next = scan{scanCount = scanCount scan + 1, scanPathBytes = scanPathBytes scan + pathCost, scanNames = Set.insert (zmName member) (scanNames scan)}
                compressed = BS.take (fromInteger (zmCompressed member)) (BS.drop (fromInteger (zmDataOffset member)) bytes)
                consume source = runExceptT . runConduit $ source .| memberBytes directoryCharge next member selected 0 0 [] 0
            result <- case zmMethod member of
                0 -> consume (yield compressed)
                8 -> withInflatedChunks RawDeflate decoderCharge compressed (consume . sourceReader)
                12 -> withBzipChunks 1 decoderCharge compressed consume
                14 -> case zipLzmaParts member compressed of
                    Left fault -> pure (Left fault)
                    Right parts -> withLzmaChunks ZipLzmaMember decoderCharge parts consume
                _ -> pure (parseRefusal "unsupported ZIP compression method")
            decoderCharge 0
            case result of
                Left fault -> pure (Left fault)
                Right retained -> go directoryCharge retained rest
    prepareMember scan member = do
        pathCost <- entryPathCost limits (zmName member) scan
        selected <- checkPath (zmName member) >>= metadataPath package version kind
        void (addInflated limits (zmInflated member) scan)
        pure (pathCost, selected)
    memberBytes directoryCharge scan member selected seen crc chunks chunkCount =
        await >>= \case
            Nothing -> do
                unless (toInteger seen == zmInflated member && crc == zmCrc member) (lift (throwError (ParseError "ZIP entry integrity check failed")))
                charged <- either (lift . throwError) pure (addInflated limits (toInteger seen) scan)
                if selected then either (lift . throwError) pure (keepMetadata limits (BS.concat (reverse chunks)) charged) else pure charged
            Just chunk -> do
                let next = seen + BS.length chunk
                when (toInteger next > zmInflated member || toInteger next + scanInflated scan > toInteger (maxMirrorArtifactBytes limits)) (lift (throwError (ParseError "ZIP inflated bytes exceed their bound")))
                when (selected && next > maxMetadataBytes limits) (lift (throwError (ParseError "archive Core Metadata exceeds its bound")))
                when selected (liftIO (charge (workspace directoryCharge scan next (chunkCount + 1))))
                memberBytes directoryCharge scan member selected next (crc32Update crc chunk) (if selected then chunk : chunks else []) (chunkCount + 1)
    workspace directoryCharge scan held chunks = archiveScratchBytes + directoryCharge + 2 * scanPathBytes scan + 3 * held + 64 * chunks + maybe 0 BS.length (scanMetadata scan)

zipLzmaParts :: ZipMember -> ByteString -> Either ParseError [ByteString]
zipLzmaParts member bytes
    | BS.length bytes < 9 || little 2 2 bytes /= 5 = parseRefusal "malformed ZIP LZMA properties"
    | otherwise = Right [BS.take 5 (BS.drop 4 bytes), BSL.toStrict (toLazyByteString (word64LE (if zmLzmaEndMarker member then maxBound else fromInteger (zmInflated member)))), BS.drop 9 bytes]

zipDirectory :: Limits -> (Int -> IO ()) -> ByteString -> IO (Either ParseError ([ZipMember], Int))
zipDirectory limits charge bytes = runExceptT $ do
    end <- ExceptT (pure (findZipEnd bytes))
    (directoryEnd, count, size, offset) <- ExceptT (pure (directoryBounds bytes end))
    when (count > toInteger (maxArtifactCount limits) || size > toInteger (maxMetadataBytes limits)) (throwError (ParseError "ZIP directory exceeds its bound"))
    unless (offset + size == directoryEnd) (throwError (ParseError "ZIP directory offset is inconsistent"))
    entries <- readDirectory charge 0 bytes (fromInteger count) (BS.take (fromInteger size) (BS.drop (fromInteger offset) bytes))
    let ordered = sortOn zmStart entries
    finalOffset <- ExceptT (pure (foldM advance 0 ordered))
    unless (finalOffset == offset) (throwError (ParseError "ZIP local records do not fill the declared file area"))
    let weight = sum [512 + 4 * BS.length (TE.encodeUtf8 (zmName member)) | member <- ordered]
    liftIO (charge (archiveScratchBytes + weight))
    pure (ordered, weight)
  where
    advance expected member
        | zmStart member == expected && zmEnd member > zmStart member = Right (zmEnd member)
        | otherwise = parseRefusal "ZIP central offset does not identify the actual next local entry"

directoryBounds :: ByteString -> Int -> Either ParseError (Integer, Integer, Integer, Integer)
directoryBounds archive end = do
    let footer = BS.drop end archive
        count = little 10 2 footer
        size = little 12 4 footer
        offset = little 16 4 footer
        locator = BS.take 20 (BS.drop (end - 20) archive)
    unless (little 4 2 footer == 0 && little 6 2 footer == 0 && little 8 2 footer == count) (parseRefusal "multi-disk ZIP is unsupported")
    if end >= 20 && BS.isPrefixOf "PK\x06\x07" locator
        then do
            unless (little 4 4 locator == 0 && little 16 4 locator == 1) (parseRefusal "multi-disk ZIP64 is unsupported")
            let position = little 8 8 locator
            when (position > toInteger end) (parseRefusal "ZIP64 end offset is outside its archive")
            let record = BS.drop (fromInteger position) archive
                recordSize = little 4 8 record
                count64 = little 32 8 record
                size64 = little 40 8 record
                offset64 = little 48 8 record
            unless (BS.length record >= 56 && BS.isPrefixOf "PK\x06\x06" record && recordSize >= 44 && recordSize <= 16384 && position + 12 + recordSize == toInteger (end - 20)) (parseRefusal "malformed ZIP64 end record")
            unless (little 16 4 record == 0 && little 20 4 record == 0 && little 24 8 record == count64) (parseRefusal "inconsistent ZIP64 disk inventory")
            unless ((count == 65535 || count == count64) && (size == 4294967295 || size == size64) && (offset == 4294967295 || offset == offset64)) (parseRefusal "ZIP end records disagree")
            pure (position, count64, size64, offset64)
        else do
            when (count == 65535 || size == 4294967295 || offset == 4294967295) (parseRefusal "missing ZIP64 end record")
            pure (toInteger end, count, size, offset)

readDirectory :: (Int -> IO ()) -> Int -> ByteString -> Int -> ByteString -> ExceptT ParseError IO [ZipMember]
readDirectory charge held archive count bytes
    | count == 0 = if BS.null bytes then pure [] else throwError (ParseError "ZIP directory count is inconsistent")
    | BS.length bytes < 46 || not (BS.isPrefixOf "PK\x01\x02" bytes) = throwError (ParseError "truncated ZIP directory")
    | otherwise = do
        (member, total) <- ExceptT . pure $ do
            let nameSize = fromInteger (little 28 2 bytes)
                extraSize = fromInteger (little 30 2 bytes)
                commentSize = fromInteger (little 32 2 bytes)
                total = 46 + nameSize + extraSize + commentSize
                mode = little 38 4 bytes `shiftR` 16 .&. 61440
            when (total > BS.length bytes || nameSize > 4096 || extraSize > 16384 || commentSize > 16384) (parseRefusal "ZIP entry header exceeds its bound")
            when (mode /= 0 && mode /= 32768 && mode /= 16384) (parseRefusal "ZIP contains a link or special file")
            name <- zipName (little 8 2 bytes) (BS.take nameSize (BS.drop 46 bytes))
            void (checkPath name)
            extra <- zip64Extra (BS.take extraSize (BS.drop (46 + nameSize) bytes))
            (inflated, rest1) <- expanded 4294967295 (little 24 4 bytes) 8 extra
            (compressed, rest2) <- expanded 4294967295 (little 20 4 bytes) 8 rest1
            (offset, rest3) <- expanded 4294967295 (little 42 4 bytes) 8 rest2
            (disk, _) <- expanded 65535 (little 34 2 bytes) 4 rest3
            unless (disk == 0) (parseRefusal "multi-disk ZIP member is unsupported")
            (dataOffset, end) <- checkLocalHeader archive bytes name offset compressed inflated
            let method = little 10 2 bytes
            unless (method `elem` [0, 8, 12, 14]) (parseRefusal "unsupported ZIP compression method")
            pure (ZipMember name offset end dataOffset compressed inflated (fromInteger (little 16 4 bytes)) method (little 8 2 bytes .&. 2 /= 0), total)
        let wanted = held + 512 + 4 * BS.length (TE.encodeUtf8 (zmName member))
        liftIO (charge (archiveScratchBytes + wanted))
        remaining <- readDirectory charge wanted archive (count - 1) (BS.drop total bytes)
        pure (member : remaining)

zip64Extra :: ByteString -> Either ParseError ByteString
zip64Extra = go Nothing
  where
    go found bytes
        | BS.null bytes = Right (fromMaybe BS.empty found)
        | BS.length bytes < 4 = parseRefusal "truncated ZIP extra field"
        | otherwise = do
            let tag = little 0 2 bytes
                size = fromInteger (little 2 2 bytes)
                value = BS.take size (BS.drop 4 bytes)
            when (size > BS.length bytes - 4) (parseRefusal "truncated ZIP extra field")
            when (tag == 1 && isJust found) (parseRefusal "ambiguous ZIP64 extra field")
            go (if tag == 1 then Just value else found) (BS.drop (size + 4) bytes)

expanded :: Integer -> Integer -> Int -> ByteString -> Either ParseError (Integer, ByteString)
expanded sentinel original width bytes
    | original /= sentinel = Right (original, bytes)
    | BS.length bytes < width = parseRefusal "missing ZIP64 size or offset"
    | otherwise = Right (little 0 width bytes, BS.drop width bytes)

checkLocalHeader :: ByteString -> ByteString -> Text -> Integer -> Integer -> Integer -> Either ParseError (Integer, Integer)
checkLocalHeader archive central name offset compressed inflated = do
    when (offset > toInteger (BS.length archive)) (parseRefusal "ZIP directory points outside local entries")
    let localHeader = BS.drop (fromInteger offset) archive
        flags = little 8 2 central
        nameSize = fromInteger (little 26 2 localHeader)
        extraSize = fromInteger (little 28 2 localHeader)
        dataOffset = offset + 30 + toInteger nameSize + toInteger extraSize
        dataEnd = dataOffset + compressed
    when (BS.length localHeader < 30 || not (BS.isPrefixOf "PK\x03\x04" localHeader) || nameSize > 4096 || extraSize > 16384 || dataEnd > toInteger (BS.length archive)) (parseRefusal "invalid ZIP local entry extent")
    unless (flags .&. 0xf7f1 == 0 && little 4 2 localHeader <= 63) (parseRefusal "unsupported ZIP encryption flags or version")
    unless (little 6 2 localHeader == flags && little 8 2 localHeader == little 10 2 central) (parseRefusal "ZIP entry compression flags disagree")
    localName <- zipName flags (BS.take nameSize (BS.drop 30 localHeader))
    unless (localName == name) (parseRefusal "ZIP directory names another local entry")
    extra <- zip64Extra (BS.take extraSize (BS.drop (30 + nameSize) localHeader))
    if flags .&. 8 /= 0
        then do
            let width = if little 18 4 localHeader == 4294967295 || little 22 4 localHeader == 4294967295 || not (BS.null extra) then 8 else 4
            descriptorSize <- checkDescriptor (BS.drop (fromInteger dataEnd) archive) (little 16 4 central) compressed inflated width
            pure (dataOffset, dataEnd + descriptorSize)
        else do
            unless (little 14 4 localHeader == little 16 4 central) (parseRefusal "ZIP entry CRC declarations disagree")
            (localInflated, rest) <- expanded 4294967295 (little 22 4 localHeader) 8 extra
            (localCompressed, _) <- expanded 4294967295 (little 18 4 localHeader) 8 rest
            unless (localInflated == inflated && localCompressed == compressed) (parseRefusal "ZIP entry size declarations disagree")
            pure (dataOffset, dataEnd)

checkDescriptor :: ByteString -> Integer -> Integer -> Integer -> Int -> Either ParseError Integer
checkDescriptor raw crc compressed inflated width = case matching of
    [size] -> Right size
    _ -> parseRefusal "ZIP descriptor disagrees with central directory"
  where
    candidates = (0, raw) : [(4, BS.drop 4 raw) | BS.isPrefixOf "PK\x07\x08" raw]
    matching = [toInteger (prefix + 4 + 2 * width) | (prefix, descriptor) <- candidates, BS.length descriptor >= 4 + 2 * width, little 0 4 descriptor == crc, little 4 width descriptor == compressed, little (4 + width) width descriptor == inflated]

zipName :: Integer -> ByteString -> Either ParseError Text
zipName flags bytes
    | flags .&. 2048 /= 0 = utf8 bytes
    | otherwise = Right (TE.decodeLatin1 bytes)

findZipEnd :: ByteString -> Either ParseError Int
findZipEnd bytes = case candidates of
    [offset] -> Right offset
    _ -> parseRefusal "missing or ambiguous ZIP end record"
  where
    start = max 0 (BS.length bytes - 65557)
    candidates = search start (BS.drop start bytes)
    search offset remaining = case BS.breakSubstring "PK\x05\x06" remaining of
        (_, suffix) | BS.null suffix -> []
        (prefix, suffix) ->
            let found = offset + BS.length prefix
                valid = BS.length suffix >= 22 && little 20 2 suffix + 22 == toInteger (BS.length suffix)
             in [found | valid] <> search (found + 4) (BS.drop 4 suffix)

little :: Int -> Int -> ByteString -> Integer
little offset size = BS.foldr (\byte value -> fromIntegral byte + value * 256) 0 . BS.take size . BS.drop offset

utf8 :: ByteString -> Either ParseError Text
utf8 = first (const (ParseError "archive path is not UTF-8")) . TE.decodeUtf8'
