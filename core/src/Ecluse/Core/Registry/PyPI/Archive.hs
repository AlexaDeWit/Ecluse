-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Bounded in-memory Core Metadata extraction from verified Python distributions.
module Ecluse.Core.Registry.PyPI.Archive (archiveMetadata, archiveMemoryBytes) where

import Codec.Archive.Zip.Conduit.Types (ZipEntry (..))
import Codec.Archive.Zip.Conduit.UnZip (unZipStream)
import Codec.Compression.BZip qualified as BZip
import Codec.Compression.BZip.Internal qualified as BZipStream
import Codec.Compression.Lzma qualified as Lzma
import Codec.Compression.Zlib.Internal qualified as GZipStream
import Conduit
import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.PyPI.Archive.Path (checkPath, metadataPath)
import Ecluse.Core.Registry.PyPI.Archive.Tar (TarDecode, extractTar)
import Ecluse.Core.Registry.PyPI.Project (DistributionKind (..), FileCoordinate (..), fileCoordinate)
import Ecluse.Core.Security.Limits (Limits (..))
import Ecluse.Core.Version (Version)

-- | Traverse every entry before success. No archive content reaches the filesystem or executes.
archiveMetadata :: Limits -> PackageName -> Version -> Text -> ByteString -> IO (Either ParseError ByteString)
archiveMetadata limits package version filename bytes = case fileCoordinate package filename of
    Just coordinate
        | fcVersion coordinate == version
        , BS.length bytes <= maxMirrorArtifactBytes limits
        , maxMirrorArtifactBytes limits > 0 -> do
            outcome <- tryAny $ case archiveDecoder filename of
                Nothing -> pure (Left (ParseError "unsupported distribution archive"))
                Just Nothing -> extractZip limits package version (fcKind coordinate) bytes
                Just (Just inflate) -> extractTar limits package version (inflate bytes)
            pure $ case outcome of
                Left _ -> Left (ParseError "malformed or truncated distribution archive")
                Right result -> result
    _ -> pure (Left (ParseError "distribution filename disagrees with admitted identity"))

-- | Charge verified bytes, metadata copies, bounded path state, and the XZ dictionary in bytes.
archiveMemoryBytes :: Limits -> Integer
archiveMemoryBytes limits = toInteger (maxMirrorArtifactBytes limits) + 6 * toInteger (maxMetadataBytes limits) + 64 * 1024 * 1024 + 65536

archiveDecoder :: Text -> Maybe (Maybe (ByteString -> ConduitT () ByteString TarDecode ()))
archiveDecoder filename
    | any (`T.isSuffixOf` filename) [".whl", ".zip"] = Just Nothing
    | any (`T.isSuffixOf` filename) [".tar.gz", ".tgz"] = Just (Just gzipChunks)
    | ".tar.bz2" `T.isSuffixOf` filename = Just (Just bzipChunks)
    | ".tar.xz" `T.isSuffixOf` filename = Just (Just xzChunks)
    | otherwise = Nothing

xzChunks :: ByteString -> ConduitT () ByteString TarDecode ()
xzChunks bytes = liftIO (Lzma.decompressIO params) >>= go bytes
  where
    params = Lzma.defaultDecompressParams{Lzma.decompressMemLimit = 64 * 1024 * 1024, Lzma.decompressAutoDecoder = False}
    go input = \case
        Lzma.DecompressInputRequired supply -> liftIO (supply input) >>= go BS.empty
        Lzma.DecompressOutputAvailable chunk next -> yield chunk >> liftIO next >>= go input
        Lzma.DecompressStreamError _ -> lift (throwError (ParseError "malformed, truncated, or excessive-memory XZ archive"))
        Lzma.DecompressStreamEnd rest ->
            unless (BS.null rest && BS.null input) (lift (throwError (ParseError "XZ archive has trailing compressed bytes")))

gzipChunks :: ByteString -> ConduitT () ByteString TarDecode ()
gzipChunks bytes = go bytes (GZipStream.decompressIO GZipStream.gzipFormat GZipStream.defaultDecompressParams)
  where
    go input = \case
        GZipStream.DecompressInputRequired supply -> liftIO (supply input) >>= go BS.empty
        GZipStream.DecompressOutputAvailable chunk next -> yield chunk >> liftIO next >>= go input
        GZipStream.DecompressStreamError _ -> lift (throwError (ParseError "malformed or truncated gzip archive"))
        GZipStream.DecompressStreamEnd rest ->
            unless (BS.null rest && BS.null input) (lift (throwError (ParseError "gzip archive has trailing compressed bytes")))

bzipChunks :: ByteString -> ConduitT () ByteString TarDecode ()
bzipChunks bytes = go bytes (BZipStream.decompressIO BZip.defaultDecompressParams)
  where
    go input = \case
        BZipStream.DecompressInputRequired supply -> liftIO (supply input) >>= go BS.empty
        BZipStream.DecompressOutputAvailable chunk next -> yield chunk >> liftIO next >>= go input
        BZipStream.DecompressStreamError _ -> lift (throwError (ParseError "malformed or truncated bzip2 archive"))
        BZipStream.DecompressStreamEnd rest ->
            unless (BS.null rest && BS.null input) (go (rest <> input) (BZipStream.decompressIO BZip.defaultDecompressParams))

data Scan = Scan
    { scanCount :: !Int
    , scanPathBytes :: !Int
    , scanInflated :: !Integer
    , scanNames :: Set Text
    , scanMetadata :: Maybe ByteString
    }

emptyScan :: Scan
emptyScan = Scan 0 0 0 Set.empty Nothing

startEntry :: Limits -> Text -> Scan -> Either ParseError Scan
startEntry limits path scan = do
    void (checkPath path)
    when (scanCount scan >= maxArtifactCount limits) (bad "archive entry count exceeds its bound")
    when (Set.member path (scanNames scan)) (bad "duplicate archive entry")
    let charge = BS.length (TE.encodeUtf8 path) + 128
    when (charge > maxMetadataBytes limits - scanPathBytes scan) (bad "archive path inventory exceeds its bound")
    pure scan{scanCount = scanCount scan + 1, scanPathBytes = scanPathBytes scan + charge, scanNames = Set.insert path (scanNames scan)}

addInflated :: Limits -> Integer -> Scan -> Either ParseError Scan
addInflated limits size scan
    | size < 0 || scanInflated scan + size > toInteger (maxMirrorArtifactBytes limits) = bad "archive inflated bytes exceed their bound"
    | otherwise = Right scan{scanInflated = scanInflated scan + size}

keepMetadata :: Limits -> ByteString -> Scan -> Either ParseError Scan
keepMetadata limits bytes scan
    | BS.length bytes > maxMetadataBytes limits = bad "archive Core Metadata exceeds its bound"
    | isJust (scanMetadata scan) = bad "ambiguous archive Core Metadata"
    | otherwise = Right scan{scanMetadata = Just (BS.copy bytes)}

finishScan :: Scan -> Either ParseError ByteString
finishScan scan = maybe (bad "archive has no Core Metadata") Right (scanMetadata scan)

extractZip :: Limits -> PackageName -> Version -> DistributionKind -> ByteString -> IO (Either ParseError ByteString)
extractZip limits package version kind bytes = case zipDirectory limits bytes of
    Left fault -> pure (Left fault)
    Right names -> runConduit $ yield bytes .| void unZipStream .| walk names emptyScan Nothing [] 0
  where
    walk names scan selected chunks held =
        await >>= \case
            Nothing -> pure $ do
                retained <- flush scan selected chunks
                unless (null names) (bad "ZIP directory has missing entries")
                finishScan retained
            Just (Left entry) -> case flush scan selected chunks of
                Left fault -> pure (Left fault)
                Right retained -> case begin names retained entry of
                    Left fault -> pure (Left fault)
                    Right (remaining, next, isMetadata) -> walk remaining next (Just isMetadata) [] 0
            Just (Right chunk) -> case addInflated limits (toInteger (BS.length chunk)) scan of
                Left fault -> pure (Left fault)
                Right next
                    | selected == Just True && BS.length chunk > maxMetadataBytes limits - held -> pure (bad "archive Core Metadata exceeds its bound")
                    | selected == Just True -> walk names next selected (chunk : chunks) (held + BS.length chunk)
                    | otherwise -> walk names next selected [] 0
    flush scan (Just True) chunks = keepMetadata limits (BS.concat (reverse chunks)) scan
    flush scan _ _ = Right scan
    begin names scan entry = do
        name <- either Right (Right . TE.decodeLatin1) (zipEntryName entry)
        case names of
            expected : remaining | expected == name -> do
                next <- startEntry limits name scan
                selected <- checkPath name >>= metadataPath package version kind
                pure (remaining, next, selected)
            _ -> bad "ZIP local entries disagree with central directory"

zipDirectory :: Limits -> ByteString -> Either ParseError [Text]
zipDirectory limits bytes = do
    end <- findZipEnd bytes
    (directoryEnd, count, size, offset) <- directoryBounds bytes end
    when (count > toInteger (maxArtifactCount limits) || size > toInteger (maxMetadataBytes limits)) (bad "ZIP directory exceeds its bound")
    unless (offset + size == directoryEnd) (bad "ZIP directory offset is inconsistent")
    entries <- readDirectory bytes (fromInteger count) (BS.take (fromInteger size) (BS.drop (fromInteger offset) bytes))
    let ordered = sortOn (\(start, _, _) -> start) entries
    finalOffset <- foldM advance 0 ordered
    unless (finalOffset == offset) (bad "ZIP local records do not fill the declared file area")
    pure [name | (_, _, name) <- ordered]
  where
    advance expected (start, end, _)
        | start == expected && end > start = Right end
        | otherwise = bad "ZIP central offset does not identify the actual next local entry"

directoryBounds :: ByteString -> Int -> Either ParseError (Integer, Integer, Integer, Integer)
directoryBounds archive end = do
    let footer = BS.drop end archive
        count = little 10 2 footer
        size = little 12 4 footer
        offset = little 16 4 footer
        locator = BS.take 20 (BS.drop (end - 20) archive)
    unless (little 4 2 footer == 0 && little 6 2 footer == 0 && little 8 2 footer == count) (bad "multi-disk ZIP is unsupported")
    if end >= 20 && BS.isPrefixOf "PK\x06\x07" locator
        then do
            unless (little 4 4 locator == 0 && little 16 4 locator == 1) (bad "multi-disk ZIP64 is unsupported")
            let position = little 8 8 locator
            when (position > toInteger end) (bad "ZIP64 end offset is outside its archive")
            let record = BS.drop (fromInteger position) archive
                recordSize = little 4 8 record
                count64 = little 32 8 record
                size64 = little 40 8 record
                offset64 = little 48 8 record
            unless (BS.length record >= 56 && BS.isPrefixOf "PK\x06\x06" record && recordSize >= 44 && recordSize <= 16384 && position + 12 + recordSize == toInteger (end - 20)) (bad "malformed ZIP64 end record")
            unless (little 16 4 record == 0 && little 20 4 record == 0 && little 24 8 record == count64) (bad "inconsistent ZIP64 disk inventory")
            unless ((count == 65535 || count == count64) && (size == 4294967295 || size == size64) && (offset == 4294967295 || offset == offset64)) (bad "ZIP end records disagree")
            pure (position, count64, size64, offset64)
        else do
            when (count == 65535 || size == 4294967295 || offset == 4294967295) (bad "missing ZIP64 end record")
            pure (toInteger end, count, size, offset)

readDirectory :: ByteString -> Int -> ByteString -> Either ParseError [(Integer, Integer, Text)]
readDirectory archive count bytes
    | count == 0 = if BS.null bytes then Right [] else bad "ZIP directory count is inconsistent"
    | BS.length bytes < 46 || not (BS.isPrefixOf "PK\x01\x02" bytes) = bad "truncated ZIP directory"
    | otherwise = do
        let nameSize = fromInteger (little 28 2 bytes)
            extraSize = fromInteger (little 30 2 bytes)
            commentSize = fromInteger (little 32 2 bytes)
            total = 46 + nameSize + extraSize + commentSize
            mode = little 38 4 bytes `shiftR` 16 .&. 61440
        when (total > BS.length bytes || nameSize > 4096 || extraSize > 16384 || commentSize > 16384) (bad "ZIP entry header exceeds its bound")
        when (mode /= 0 && mode /= 32768 && mode /= 16384) (bad "ZIP contains a link or special file")
        name <- zipName (little 8 2 bytes) (BS.take nameSize (BS.drop 46 bytes))
        void (checkPath name)
        extra <- zip64Extra (BS.take extraSize (BS.drop (46 + nameSize) bytes))
        (inflated, rest1) <- expanded 4294967295 (little 24 4 bytes) 8 extra
        (compressed, rest2) <- expanded 4294967295 (little 20 4 bytes) 8 rest1
        (offset, rest3) <- expanded 4294967295 (little 42 4 bytes) 8 rest2
        (disk, _) <- expanded 65535 (little 34 2 bytes) 4 rest3
        unless (disk == 0) (bad "multi-disk ZIP member is unsupported")
        end <- checkLocalHeader archive bytes name offset compressed inflated
        remaining <- readDirectory archive (count - 1) (BS.drop total bytes)
        pure ((offset, end, name) : remaining)

zip64Extra :: ByteString -> Either ParseError ByteString
zip64Extra = go Nothing
  where
    go found bytes
        | BS.null bytes = Right (fromMaybe BS.empty found)
        | BS.length bytes < 4 = bad "truncated ZIP extra field"
        | otherwise = do
            let tag = little 0 2 bytes
                size = fromInteger (little 2 2 bytes)
                value = BS.take size (BS.drop 4 bytes)
            when (size > BS.length bytes - 4) (bad "truncated ZIP extra field")
            when (tag == 1 && isJust found) (bad "ambiguous ZIP64 extra field")
            go (if tag == 1 then Just value else found) (BS.drop (size + 4) bytes)

expanded :: Integer -> Integer -> Int -> ByteString -> Either ParseError (Integer, ByteString)
expanded sentinel original width bytes
    | original /= sentinel = Right (original, bytes)
    | BS.length bytes < width = bad "missing ZIP64 size or offset"
    | otherwise = Right (little 0 width bytes, BS.drop width bytes)

checkLocalHeader :: ByteString -> ByteString -> Text -> Integer -> Integer -> Integer -> Either ParseError Integer
checkLocalHeader archive central name offset compressed inflated = do
    when (offset > toInteger (BS.length archive)) (bad "ZIP directory points outside local entries")
    let local = BS.drop (fromInteger offset) archive
        flags = little 8 2 central
        nameSize = fromInteger (little 26 2 local)
        extraSize = fromInteger (little 28 2 local)
        dataEnd = offset + 30 + toInteger nameSize + toInteger extraSize + compressed
    when (BS.length local < 30 || not (BS.isPrefixOf "PK\x03\x04" local) || nameSize > 4096 || extraSize > 16384 || dataEnd > toInteger (BS.length archive)) (bad "invalid ZIP local entry extent")
    unless (little 6 2 local == flags && little 8 2 local == little 10 2 central) (bad "ZIP entry compression flags disagree")
    localName <- zipName flags (BS.take nameSize (BS.drop 30 local))
    unless (localName == name) (bad "ZIP directory names another local entry")
    extra <- zip64Extra (BS.take extraSize (BS.drop (30 + nameSize) local))
    if flags .&. 8 /= 0
        then do
            let width = if little 18 4 local == 4294967295 || little 22 4 local == 4294967295 || not (BS.null extra) then 8 else 4
            descriptorSize <- checkDescriptor (BS.drop (fromInteger dataEnd) archive) (little 16 4 central) compressed inflated width
            pure (dataEnd + descriptorSize)
        else do
            unless (little 14 4 local == little 16 4 central) (bad "ZIP entry CRC declarations disagree")
            (localInflated, rest) <- expanded 4294967295 (little 22 4 local) 8 extra
            (localCompressed, _) <- expanded 4294967295 (little 18 4 local) 8 rest
            unless (localInflated == inflated && localCompressed == compressed) (bad "ZIP entry size declarations disagree")
            pure dataEnd

checkDescriptor :: ByteString -> Integer -> Integer -> Integer -> Int -> Either ParseError Integer
checkDescriptor raw crc compressed inflated width = case matching of
    [size] -> Right size
    _ -> bad "ZIP descriptor disagrees with central directory"
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
    _ -> bad "missing or ambiguous ZIP end record"
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

bad :: Text -> Either ParseError a
bad = Left . ParseError
