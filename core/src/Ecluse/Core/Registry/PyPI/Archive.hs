-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

-- | Bounded in-memory Core Metadata extraction from verified Python distributions.
module Ecluse.Core.Registry.PyPI.Archive (archiveMetadata, archiveMemoryBytes) where

import Codec.Archive.Zip.Conduit.Types (ZipEntry (..))
import Codec.Archive.Zip.Conduit.UnZip (unZipStream)
import Codec.Compression.BZip qualified as BZip
import Codec.Compression.BZip.Internal qualified as BZipStream
import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Lzma qualified as Lzma
import Conduit
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as SBS
import Data.Conduit.Tar qualified as Tar
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError (..))
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
    | any (`T.isSuffixOf` filename) [".tar.gz", ".tgz"] = Just (Just (lazyChunks GZip.decompress))
    | ".tar.bz2" `T.isSuffixOf` filename = Just (Just bzipChunks)
    | ".tar.xz" `T.isSuffixOf` filename = Just (Just (lazyChunks (Lzma.decompressWith xzParams)))
    | otherwise = Nothing
  where
    -- A hostile XZ dictionary must not allocate outside the archive traversal budget.
    xzParams = Lzma.defaultDecompressParams{Lzma.decompressMemLimit = 64 * 1024 * 1024, Lzma.decompressAutoDecoder = False}

lazyChunks :: (BSL.ByteString -> BSL.ByteString) -> ByteString -> ConduitT () ByteString TarDecode ()
lazyChunks decoder = yieldMany . BSL.toChunks . decoder . BSL.fromStrict

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

checkPath :: Text -> Either ParseError [Text]
checkPath path = do
    let parts = T.splitOn "/" (fromMaybe path (T.stripSuffix "/" path))
    when (T.length path > 4096 || T.any (\c -> c < ' ' || c == '\\' || c == ':' || c == '\DEL') path) (bad "unsafe archive path")
    when (any (`elem` ["", ".", ".."]) parts) (bad "unsafe archive path")
    pure parts

metadataPath :: PackageName -> Version -> DistributionKind -> [Text] -> Either ParseError Bool
metadataPath package version kind parts = case (kind, parts) of
    (Wheel, [root, "METADATA"]) | Just stem <- T.stripSuffix ".dist-info" root -> identity stem
    (Sdist, [root, "PKG-INFO"]) -> identity root
    _ -> Right False
  where
    identity stem = case fileCoordinate package (stem <> ".tar.gz") of
        Just coordinate | fcVersion coordinate == version -> Right True
        _ -> bad "archive metadata directory disagrees with admitted identity"

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

type TarDecode = ExceptT ParseError IO

data TarSelection = TarDiscard | TarMetadata | TarPax | TarLongName

extractTar :: Limits -> PackageName -> Version -> ConduitT () ByteString TarDecode () -> IO (Either ParseError ByteString)
extractTar limits package version inflated = do
    total <- newIORef 0
    result <-
        runExceptT . runConduit $
            inflated .| boundInflated limits total .| (Tar.untarChunksRaw >> checkTarTail) .| walk emptyScan Nothing TarDiscard [] 0 0
    consumed <- readIORef total
    pure $ do
        (bytes, minimumSize) <- result
        unless (consumed >= minimumSize) (bad "tar archive has no complete trailer")
        pure bytes
  where
    walk scan pending selected chunks retained lastEnd =
        await >>= \case
            Nothing -> do
                (finished, unfinished) <- liftEitherTar (flushTar limits scan pending selected chunks)
                when (isJust unfinished) (lift (throwError (ParseError "orphan extended tar header")))
                bytes <- liftEitherTar (finishScan finished)
                pure (bytes, lastEnd + 1024)
            Just (Tar.ChunkException _) -> lift (throwError (ParseError "malformed or truncated tar archive"))
            Just (Tar.ChunkHeader header) -> do
                (finished, override) <- liftEitherTar (flushTar limits scan pending selected chunks)
                (next, selection) <- liftEitherTar (beginTar limits package version finished override header)
                let size = toInteger (Tar.headerPayloadSize header)
                    end = toInteger (Tar.headerPayloadOffset header) + size + (512 - size `mod` 512) `mod` 512
                    pending' = case selection of TarPax -> override; TarLongName -> override; _ -> Nothing
                walk next pending' selection [] 0 end
            Just (Tar.ChunkPayload _ chunk) -> case selected of
                TarDiscard -> walk scan pending selected [] 0 lastEnd
                _ -> do
                    let cap = case selected of TarMetadata -> maxMetadataBytes limits; _ -> 16384
                    when (BS.length chunk > cap - retained) (lift (throwError (ParseError "tar metadata or extended header exceeds its bound")))
                    walk scan pending selected (chunk : chunks) (retained + BS.length chunk) lastEnd

liftEitherTar :: Either ParseError a -> ConduitT i o TarDecode a
liftEitherTar = either (lift . throwError) pure

boundInflated :: Limits -> IORef Integer -> ConduitT ByteString ByteString TarDecode ()
boundInflated limits total = awaitForever $ \chunk -> do
    seen <- liftIO (readIORef total)
    let next = seen + toInteger (BS.length chunk)
    when (next > toInteger (maxMirrorArtifactBytes limits)) (lift (throwError (ParseError "archive inflated bytes exceed their bound")))
    liftIO (writeIORef total next)
    yield chunk

checkTarTail :: ConduitT ByteString Tar.TarChunk TarDecode ()
checkTarTail = awaitForever $ \chunk ->
    unless (BS.all (== 0) chunk) (lift (throwError (ParseError "tar archive has trailing nonzero bytes")))

beginTar :: Limits -> PackageName -> Version -> Scan -> Maybe Text -> Tar.Header -> Either ParseError (Scan, TarSelection)
beginTar limits package version scan pending header = do
    let kind = Tar.headerFileType header
        extended = case kind of Tar.FTOther code -> code == 120 || code == 76; _ -> False
        size = toInteger (Tar.headerPayloadSize header)
    unless (SBS.fromShort (Tar.headerMagicVersion header) `elem` [BS.replicate 8 0, "ustar\NUL00", "ustar  \NUL"]) (bad "unsupported tar header format")
    when (size < 0 || size > toInteger (maxMirrorArtifactBytes limits)) (bad "tar entry size exceeds its bound")
    when (scanCount scan >= maxArtifactCount limits) (bad "archive entry count exceeds its bound")
    if extended
        then do
            when (isJust pending) (bad "ambiguous extended tar headers")
            when (size > 16384) (bad "extended tar header exceeds its bound")
            pure (scan{scanCount = scanCount scan + 1}, if Tar.headerLinkIndicator header == 120 then TarPax else TarLongName)
        else do
            base <- utf8 (tarPath header)
            let path = fromMaybe base pending
            next <- startEntry limits path scan
            selected <- checkPath path >>= metadataPath package version Sdist
            case kind of
                Tar.FTNormal -> pure (next, if selected then TarMetadata else TarDiscard)
                Tar.FTDirectory | size == 0 -> pure (next, TarDiscard)
                _ -> bad "unsupported tar entry type or link"

flushTar :: Limits -> Scan -> Maybe Text -> TarSelection -> [ByteString] -> Either ParseError (Scan, Maybe Text)
flushTar limits scan pending selection chunks = case selection of
    TarDiscard -> Right (scan, pending)
    TarMetadata -> (,pending) <$> keepMetadata limits bytes scan
    TarPax -> (scan,) <$> paxPath bytes
    TarLongName -> do
        path <- utf8 (BS.takeWhile (/= 0) bytes)
        void (checkPath path)
        pure (scan, Just path)
  where
    bytes = BS.concat (reverse chunks)

tarPath :: Tar.Header -> ByteString
tarPath header
    | BS.null prefix = suffix
    | otherwise = prefix <> "/" <> suffix
  where
    prefix = SBS.fromShort (Tar.headerFileNamePrefix header)
    suffix = SBS.fromShort (Tar.headerFileNameSuffix header)

paxPath :: ByteString -> Either ParseError (Maybe Text)
paxPath = go Map.empty
  where
    go fields bytes
        | BS.null bytes = traverse utf8 (Map.lookup "path" fields)
        | otherwise = do
            let (digits, separated) = BS.break (== 32) bytes
            when (BS.null digits || BS.length digits > 6 || not (BS.all (\c -> c >= 48 && c <= 57) digits)) (bad "malformed pax record length")
            let size = BS.foldl' (\n c -> n * 10 + fromIntegral c - 48) 0 digits
            when (size <= BS.length digits + 3 || size > BS.length bytes || BS.null separated) (bad "truncated pax record")
            let record = BS.take (size - BS.length digits - 1) (BS.drop 1 separated)
                (key, value) = BS.break (== 61) record
            unless (BS.isSuffixOf "\n" record && not (BS.null value)) (bad "malformed pax record")
            when (Map.member key fields) (bad "duplicate pax attribute")
            unless (key `elem` ["path", "mtime", "atime", "ctime", "uid", "gid", "uname", "gname", "comment", "charset"]) (bad "unsupported pax attribute")
            go (Map.insert key (BS.take (BS.length value - 2) (BS.drop 1 value)) fields) (BS.drop size bytes)

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
    let footer = BS.drop end bytes
        count = little 10 2 footer
        size = little 12 4 footer
        offset = little 16 4 footer
    unless (little 4 2 footer == 0 && little 6 2 footer == 0 && little 8 2 footer == count) (bad "multi-disk ZIP is unsupported")
    when (count >= 65535 || count > toInteger (maxArtifactCount limits) || size > toInteger (maxMetadataBytes limits)) (bad "ZIP directory exceeds its bound")
    unless (offset + size == toInteger end) (bad "ZIP directory offset is inconsistent")
    entries <- readDirectory bytes (fromInteger count) (BS.take (fromInteger size) (BS.drop (fromInteger offset) bytes))
    pure (map snd (sortOn fst entries))

readDirectory :: ByteString -> Int -> ByteString -> Either ParseError [(Integer, Text)]
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
        checkLocalHeader archive bytes name
        remaining <- readDirectory archive (count - 1) (BS.drop total bytes)
        pure ((little 42 4 bytes, name) : remaining)

checkLocalHeader :: ByteString -> ByteString -> Text -> Either ParseError ()
checkLocalHeader archive central name = do
    let offset = little 42 4 central
        local = BS.drop (fromInteger offset) archive
        flags = little 8 2 central
        nameSize = fromInteger (little 26 2 local)
    when (offset > toInteger (BS.length archive) || BS.length local < 30 || not (BS.isPrefixOf "PK\x03\x04" local)) (bad "ZIP directory points outside local entries")
    unless (little 6 2 local == flags && little 8 2 local == little 10 2 central) (bad "ZIP entry compression flags disagree")
    localName <- zipName flags (BS.take nameSize (BS.drop 30 local))
    unless (localName == name) (bad "ZIP directory names another local entry")
    when (flags .&. 8 /= 0) (checkDescriptor archive central local)
    when (flags .&. 8 == 0) $ do
        unless (little 14 4 local == little 16 4 central) (bad "ZIP entry CRC declarations disagree")
        for_ [(18, 20), (22, 24)] $ \(localOffset, centralOffset) ->
            unless (little localOffset 4 local == 4294967295 || little localOffset 4 local == little centralOffset 4 central) (bad "ZIP entry size declarations disagree")

checkDescriptor :: ByteString -> ByteString -> ByteString -> Either ParseError ()
checkDescriptor archive central local = do
    let compressed = little 20 4 central
        inflated = little 24 4 central
        offset = little 42 4 central + 30 + little 26 2 local + little 28 2 local + compressed
        raw = BS.drop (fromInteger offset) archive
        descriptor = fromMaybe raw (BS.stripPrefix "PK\x07\x08" raw)
        width = if little 18 4 local == 4294967295 || little 22 4 local == 4294967295 then 8 else 4
    when (compressed == 4294967295 || inflated == 4294967295) (bad "ZIP64 central entries are unsupported")
    unless (BS.length descriptor >= 4 + 2 * width && little 0 4 descriptor == little 16 4 central && little 4 width descriptor == compressed && little (4 + width) width descriptor == inflated) (bad "ZIP descriptor disagrees with central directory")

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
