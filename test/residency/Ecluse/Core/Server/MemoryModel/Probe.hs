-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Isolated retained-heap probes for the shipping metadata representation.
Preparation forces derived renderings, so its allocation and high-water counters include that work.
-}
module Ecluse.Core.Server.MemoryModel.Probe (
    Shape (..),
    Measurement (..),
    packages,
    probe,
    SourceMode (..),
    probeSource,
) where

import Data.Aeson (FromJSON, ToJSON, Value, eitherDecodeStrict, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (allocated_bytes, gc, max_live_bytes), getRTSStats, getRTSStatsEnabled)
import System.IO (withBinaryFile)
import System.Mem (performMajorGC)
import UnliftIO.Exception (bracket, evaluate)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName, pkgEcosystem)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, estimateValueBytes, npmCached, pypiSimpleCached, weighCachedDoc)

import Ecluse.Core.Registry.JsonStream (StreamResult (..), readJsonStream)
import Ecluse.Core.Registry.Metadata (VersionDoc (..), VersionRead (vrBodyBytes, vrVersion))
import Ecluse.Core.Registry.Npm.Metadata (projectNpmStream, selectNpmRead)
import Ecluse.Core.Registry.Npm.Project (versionListParser)
import Ecluse.Core.Registry.Npm.Streaming (NpmRead (..), npmFields)
import Ecluse.Core.Registry.Npm.StreamingProjection (collectField, emptyProjection)
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIStream)
import Ecluse.Core.Registry.PyPI.Streaming qualified as PyPIStream
import Ecluse.Core.Registry.PyPI.StreamingProjection qualified as PyPIProjection
import Ecluse.Core.Registry.VersionList (collectVersionList, emptyVersionList, finishVersionList)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits, boundedRead, defaultLimits, maxMetadataBytes, maxNestingDepth)
import Ecluse.Core.Server.Cache (CacheEntry (..))
import Ecluse.Core.Server.Cache.VersionWeight (weighVersion)
import Ecluse.Core.Snapshot (ContentDigest, digestOf)
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath), corpusPackages, pypiCorpusPackages)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks)
import Ecluse.Test.Registry.Metadata.Projection (projectMetadata)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Test.Registry.Npm.Project (parsePackageInfoFromValue)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)
import Ecluse.Test.Registry.PyPI.Project (projectSimpleIndexFromValue)
import Ecluse.Test.Server.Cache (diagnosticDocumentValue, weighCacheEntry)
import Ecluse.Test.Snapshot (untaggedRead)

-- | Each shape gets a fresh process and an independently loaded capture.
data Shape
    = -- | Original strict input and its backing storage.
      Wire
    | -- | The decoded JSON tree without a typed projection.
      Raw
    | -- | The typed projection and any raw data it still reaches.
      Typed
    | -- | The cache entry, preserving sharing between both views.
      Shared
    deriving stock (Eq, Show, Read, Enum, Bounded)

-- | Absolute GC samples and preparation counters. Memory quantities use bytes.
data Measurement = Measurement
    { wireBytes :: Int
    , compactBytes :: Int64
    , cacheWeight :: Int
    , versions :: Int
    , baselineLive :: Word64
    , heldLive :: Word64
    , releasedLive :: Word64
    , preparationAllocated :: Word64
    , preparationMaxLive :: Word64
    }
    deriving stock (Show, Generic)

instance ToJSON Measurement
instance FromJSON Measurement

data Held = HeldWire ByteString | HeldRaw Value | HeldTyped PackageInfo | HeldShared CacheEntry | HeldSelected VersionRead | HeldVersions [Version] | HeldLegacy PackageInfo Value

-- | Use the same complete package catalogue as the performance harnesses.
packages :: [CorpusPackage]
packages = corpusPackages <> pypiCorpusPackages

-- | Root only the selected representation across collections, then verify its release separately.
probe :: Shape -> CorpusPackage -> IO Measurement
probe shape package = do
    enabled <- getRTSStatsEnabled
    unless enabled (fail "metadata residency requires RTS -T")
    bracket (prepare shape package) (freeStablePtr . fst) (observe . fst)
    before <- sample
    (bytes, compact, weight, count, held, prepared) <-
        bracket (prepare shape package) (freeStablePtr . fst) $ \(root, (bytes, compact, weight, count)) -> do
            retained <- sample
            observe root
            pure (bytes, compact, weight, count, retained, allocated_bytes retained)
    released <- sample
    pure
        Measurement
            { wireBytes = bytes
            , compactBytes = compact
            , cacheWeight = weight
            , versions = count
            , baselineLive = live before
            , heldLive = live held
            , releasedLive = live released
            , preparationAllocated = prepared - allocated_bytes before
            , preparationMaxLive = max_live_bytes held
            }

sample :: IO RTSStats
sample = performMajorGC >> getRTSStats

-- Dereferencing after GC makes the root's continued reachability observable.
observe :: StablePtr Held -> IO ()
observe root = do
    observed <- deRefStablePtr root >>= evaluate . heldSize
    when (observed <= 0) (fail "retained metadata root is empty")

live :: RTSStats -> Word64
live = gcdetails_live_bytes . gc

-- The caller retains only a StablePtr and scalars, never the preparation closure's input graph.
{-# NOINLINE prepare #-}
prepare :: Shape -> CorpusPackage -> IO (StablePtr Held, (Int, Int64, Int, Int))
prepare shape package = do
    bytes <- BS.readFile (cpPath package)
    (held, compact, weight, count) <- case shape of
        Wire -> pure (HeldWire bytes, 0, 0, 0)
        Raw -> do
            raw <- either fail pure (eitherDecodeStrict bytes)
            _ <- forceShown raw
            compact <- evaluate (LBS.length (encode raw))
            pure (HeldRaw raw, compact, 0, 0)
        Typed -> do
            (info, _) <- project package bytes
            _ <- forceShown info
            pure (HeldTyped info, 0, 0, Map.size (infoVersions info))
        Shared -> do
            (info, document) <- project package bytes
            let entry = CacheEntry info document (BS.length bytes) (digestOf bytes)
            _ <- forceShown entry
            compact <- evaluate (LBS.length (encode (diagnosticDocumentValue document)))
            weight <- evaluate (weighCacheEntry entry)
            pure (HeldShared entry, compact, weight, Map.size (infoVersions info))
    size <- evaluate (BS.length bytes)
    compactSize <- evaluate compact
    charged <- evaluate weight
    versionCount <- evaluate count
    root <- evaluate held >>= newStablePtr
    pure (root, (size, compactSize, charged, versionCount))

project :: CorpusPackage -> ByteString -> IO (PackageInfo, CachedDoc)
project package bytes = case pkgEcosystem name of
    Npm -> either (fail . show) (pure . second (fst npmCached)) (projectNpmManifest defaultLimits name bytes)
    PyPI -> either (fail . show) (pure . second (fst pypiSimpleCached)) (projectPyPIIndex defaultLimits name bytes)
    RubyGems -> fail "no RubyGems metadata residency corpus"
  where
    name = cpPackage package

forceShown :: (Show a) => a -> IO Int
forceShown value = evaluate (length (show value :: String))

heldSize :: Held -> Int
heldSize = \case
    HeldWire bytes -> BS.length bytes
    HeldRaw value -> fromIntegral (LBS.length (encode value))
    HeldTyped info -> Map.size (infoVersions info)
    HeldShared entry -> weighCacheEntry entry
    HeldSelected selected -> weighVersion selected
    HeldVersions selected -> sum (map (T.length . renderVersion) selected)
    HeldLegacy info raw -> fromIntegral (estimateValueBytes raw) + Map.size (infoVersions info)

-- | Source-reading comparisons. BufferedLegacy keeps the former complete Aeson representation.
data SourceMode = BufferedLegacy | BufferedCompact | StreamedFull | StreamedSelected | StreamedVersions
    deriving stock (Eq, Show, Read)

-- | Measure one read without Show or serialisation. External process sampling includes native parser buffers.
probeSource :: SourceMode -> Limits -> PackageName -> Version -> FilePath -> IO (Either Text (Measurement, ContentDigest, Int64, Word64))
probeSource mode limits name version path = do
    enabled <- getRTSStatsEnabled
    unless enabled (fail "metadata residency requires RTS -T")
    before <- sample
    started <- getMonotonicTimeNSec
    prepareSource mode limits name version path >>= \case
        Left fault -> pure (Left fault)
        Right (root, (bytes, compact, count, digest)) -> do
            finished <- getMonotonicTimeNSec
            held <- bracket (pure root) freeStablePtr $ \pointer -> do
                retained <- sample
                void (deRefStablePtr pointer >>= evaluate . sourceSize)
                pure retained
            released <- sample
            pure
                ( Right
                    ( Measurement
                        { wireBytes = bytes
                        , compactBytes = 0
                        , cacheWeight = 0
                        , versions = count
                        , baselineLive = live before
                        , heldLive = live held
                        , releasedLive = live released
                        , preparationAllocated = allocated_bytes held - allocated_bytes before
                        , preparationMaxLive = max_live_bytes held
                        }
                    , digest
                    , compact
                    , finished - started
                    )
                )

{-# NOINLINE prepareSource #-}
prepareSource :: SourceMode -> Limits -> PackageName -> Version -> FilePath -> IO (Either Text (StablePtr Held, (Int, Int64, Int, ContentDigest)))
prepareSource mode limits name version path =
    withBinaryFile path ReadMode $ \handle ->
        readSource mode limits name version (BS.hGetSome handle 32768) >>= \case
            Left fault -> pure (Left fault)
            Right (held, bytes, digest) -> do
                void (evaluate (sourceSize held))
                let (compact, count) = sourceSummary held
                compactBytes' <- evaluate compact
                count' <- evaluate count
                bytes' <- evaluate bytes
                digest' <- evaluate digest
                root <- newStablePtr held
                pure (Right (root, (bytes', compactBytes', count', digest')))

readSource :: SourceMode -> Limits -> PackageName -> Version -> IO ByteString -> IO (Either Text (Held, Int, ContentDigest))
readSource mode limits name version next = case pkgEcosystem name of
    Npm -> readNpmSource mode limits name version next
    PyPI -> readPyPISource mode limits name version next
    RubyGems -> pure (Left "no RubyGems source-read measurement")

readNpmSource :: SourceMode -> Limits -> PackageName -> Version -> IO ByteString -> IO (Either Text (Held, Int, ContentDigest))
readNpmSource mode limits name version next = case mode of
    BufferedLegacy -> readLegacySource limits name next
    BufferedCompact -> buffered $ \_ body ->
        first show (parseJsonChunks bound parser step emptyProjection [body]) >>= fullResult
    StreamedFull -> fmap (first show) (readJsonStream bound parser step emptyProjection next) <&> (>>= fullResult)
    StreamedSelected -> fmap (first show) (readJsonStream bound (npmFields (maxNestingDepth limits) (SelectedRead (renderVersion version))) step emptyProjection next) <&> (>>= selectedResult)
    StreamedVersions -> fmap (first show) (readJsonStream bound (versionListParser limits) (collectVersionList limits) emptyVersionList next) <&> (>>= versionsResult)
  where
    bound = MetadataBodyLimit (maxMetadataBytes limits)
    parser = npmFields (maxNestingDepth limits) FullRead
    step = collectField limits name
    buffered projectBody = boundedRead bound next <&> (first show >=> uncurry projectBody)
    fullResult streamed = do
        (info, raw) <- first show (projectNpmStream limits name "https://registry.npmjs.org" streamed)
        pure (HeldShared (CacheEntry info (fst npmCached raw) (streamBytes streamed) (streamDigest streamed)), streamBytes streamed, streamDigest streamed)
    selectedResult streamed = do
        projected <- first show (projectNpmStream limits name "https://registry.npmjs.org" streamed)
        let selected = selectNpmRead version (streamBytes streamed) projected
        pure (HeldSelected selected, streamBytes streamed, streamDigest streamed)
    versionsResult streamed = do
        selected <- first show (streamValue streamed >>= finishVersionList)
        pure (HeldVersions selected, streamBytes streamed, streamDigest streamed)

readPyPISource :: SourceMode -> Limits -> PackageName -> Version -> IO ByteString -> IO (Either Text (Held, Int, ContentDigest))
readPyPISource mode limits name version next = case mode of
    BufferedLegacy -> readLegacySource limits name next
    BufferedCompact -> boundedRead bound next <&> (first show >=> \(_, body) -> first show (parseJsonChunks bound parser step PyPIProjection.emptyProjection [body]) >>= fullResult)
    StreamedFull -> fmap (first show) (readJsonStream bound parser step PyPIProjection.emptyProjection next) <&> (>>= fullResult)
    StreamedSelected ->
        let selectedMode = PyPIStream.SelectedRead name (renderVersion version)
         in fmap (first show) (readJsonStream bound (PyPIStream.pypiFields (maxNestingDepth limits) selectedMode) (PyPIProjection.collectField limits name selectedMode) PyPIProjection.emptyProjection next) <&> (>>= selectedResult)
    StreamedVersions -> pure (Left "PyPI exposes no version-list-only read")
  where
    bound = MetadataBodyLimit (maxMetadataBytes limits)
    parser = PyPIStream.pypiFields (maxNestingDepth limits) PyPIStream.FullRead
    step = PyPIProjection.collectField limits name PyPIStream.FullRead
    fullResult streamed = do
        (info, document) <- first show (projectPyPIStream limits name streamed)
        pure (HeldShared (CacheEntry info (fst pypiSimpleCached document) (streamBytes streamed) (streamDigest streamed)), streamBytes streamed, streamDigest streamed)
    selectedResult streamed = do
        (info, _) <- first show (projectPyPIStream limits name streamed)
        let selected = (untaggedRead (Map.lookup (renderVersion version) (infoVersions info))){vrBodyBytes = streamBytes streamed}
        pure (HeldSelected selected, streamBytes streamed, streamDigest streamed)

readLegacySource :: Limits -> PackageName -> IO ByteString -> IO (Either Text (Held, Int, ContentDigest))
readLegacySource limits name next = boundedRead (MetadataBodyLimit (maxMetadataBytes limits)) next <&> (first show >=> projectBody)
  where
    projectBody (size, body) = do
        (info, raw) <- first show (projectMetadata reference limits body)
        let digest = digestOf body
            held = case pkgEcosystem name of
                PyPI -> HeldLegacy info raw
                _ -> HeldShared (CacheEntry info (fst npmCached raw) size digest)
        pure (held, size, digest)
    reference = case pkgEcosystem name of
        PyPI -> projectSimpleIndexFromValue name
        _ -> parsePackageInfoFromValue name

sourceSize :: Held -> Int
sourceSize = \case
    HeldShared entry -> fromIntegral (weighCachedDoc (entryRaw entry)) + infoSize (entryInfo entry)
    HeldTyped info -> infoSize info
    HeldSelected selected -> weighVersion selected
    HeldVersions selected -> sum (map (T.length . renderVersion) selected)
    HeldRaw raw -> fromIntegral (estimateValueBytes raw)
    HeldLegacy info raw -> fromIntegral (estimateValueBytes raw) + infoSize info
    HeldWire bytes -> BS.length bytes
  where
    infoSize = Map.foldl' (\total details -> total + weighVersion (untaggedRead (Just details))) 0 . infoVersions

sourceSummary :: Held -> (Int64, Int)
sourceSummary = \case
    HeldShared entry -> (weighCachedDoc (entryRaw entry), Map.size (infoVersions (entryInfo entry)))
    HeldTyped info -> (0, Map.size (infoVersions info))
    HeldSelected selected -> (maybe 0 (maybe 0 weighCachedDoc . vdRaw) (vrVersion selected), maybe 0 (const 1) (vrVersion selected))
    HeldVersions selected -> (0, length selected)
    HeldRaw _ -> (0, 0)
    HeldWire _ -> (0, 0)
    HeldLegacy info raw -> (estimateValueBytes raw, Map.size (infoVersions info))
