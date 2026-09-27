-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Isolated retained-heap probes for the shipping metadata representation, each run in a fresh
child process of the residency executable. A forced preparation renders its shape, so its
allocation and high-water counters include that work.
-}
module Ecluse.Core.Server.MemoryModel.Probe (
    Shape (..),
    Measurement (..),
    packages,
    probe,
    Evaluated (..),
    probeEvaluation,
    project,
    measureInChild,
    childMain,
    evaluationMain,
    SelectedShape (..),
    probeSelected,
    SourceMode (..),
    probeSource,
    Held (..),
    readSource,
    sourceSize,
    sourceSummary,
) where

import Data.Aeson (FromJSON, ToJSON, Value, eitherDecodeStrict, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (allocated_bytes, gc, max_live_bytes), getRTSStats, getRTSStatsEnabled)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (withBinaryFile)
import System.Mem (performMajorGC)
import System.Process (readProcessWithExitCode)
import UnliftIO.Exception (bracket, evaluate)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName, pkgEcosystem)
import Ecluse.Core.Package.Filter (enforceArtifactLocations)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, estimateValueBytes, npmCached, pypiSimpleCached, weighCachedDoc)

import Ecluse.Core.Registry.Exchange (digestingRead)
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
import Ecluse.Core.Snapshot (ContentDigest)
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Test.Corpus (CaptureUpstream (..), CorpusPackage (cpPackage, cpPath), corpusPackages, npmCaptureUpstream, pypiCaptureUpstream, pypiCorpusPackages)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks)
import Ecluse.Test.Registry.Metadata.Projection (projectMetadata)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Test.Registry.Npm.Project (parsePackageInfoFromValue)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)
import Ecluse.Test.Registry.PyPI.Project (projectSimpleIndexFromValue)
import Ecluse.Test.Server.Cache (diagnosticDocumentValue, weighCacheEntry)
import Ecluse.Test.Snapshot (digestOf, untaggedRead)

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

-- | A rooted representation shared by isolated retention and materialisation probes.
data Held = HeldWire ByteString | HeldRaw Value | HeldTyped PackageInfo | HeldShared CacheEntry | HeldSelected VersionRead | HeldVersions [Version] | HeldLegacy PackageInfo Value

-- | Compare a selected value with the same read whose value is discarded before collection.
data SelectedShape
    = -- | Keep the projected release reachable through the collection.
      SelectedValue
    | -- | Discard the release and retain only a warmed constant marker.
      SelectedControl
    deriving stock (Eq, Show)

-- | Live bytes before one read result, with it rooted as production holds it, and once forced in place.
data Evaluated = Evaluated
    { evaluatedVersions :: Int
    , evaluatedBaseline :: Word64
    , evaluatedWeakHead :: Word64
    , evaluatedForced :: Word64
    }
    deriving stock (Show, Generic)

instance ToJSON Evaluated
instance FromJSON Evaluated

data HeapSample = HeapSample
    { live :: !Word64
    , sampleAllocated :: !Word64
    , samplePeakLive :: !Word64
    }

-- | Use the same complete package catalogue as the performance harnesses.
packages :: [CorpusPackage]
packages = corpusPackages <> pypiCorpusPackages

-- | Measure one capture in a fresh process of this executable, on one capability with RTS statistics.
measureInChild :: (FromJSON a) => [String] -> CorpusPackage -> IO (Either String a)
measureInChild mode package = do
    executable <- getExecutablePath
    (status, output, errors) <- readProcessWithExitCode executable (mode <> [cpPath package, "+RTS", "-T", "-N1", "-RTS"]) ""
    pure $
        if status == ExitSuccess
            then eitherDecodeStrict (encodeUtf8 (toText output))
            else Left (show status <> ": " <> errors)

-- | Dispatch a fresh process without entering Hspec or loading any other capture.
childMain :: (Read shape) => (shape -> CorpusPackage -> IO Measurement) -> String -> FilePath -> IO ()
childMain measure rawShape path = do
    shape <- maybe (fail "unknown metadata residency shape") pure (readMaybe rawShape)
    package <- corpusPackageAt path
    measure shape package >>= LBS.putStr . encode

-- | 'childMain' for 'probeEvaluation', which has one mode.
evaluationMain :: FilePath -> IO ()
evaluationMain path = corpusPackageAt path >>= probeEvaluation >>= LBS.putStr . encode

corpusPackageAt :: FilePath -> IO CorpusPackage
corpusPackageAt path = maybe (fail "unknown metadata residency corpus path") pure (find ((== path) . cpPath) packages)

-- | Root only the selected representation across collections, then verify its release separately.
probe :: Shape -> CorpusPackage -> IO Measurement
probe shape package = measureRetained (prepare shape package)

{- | Sample live bytes with the read result rooted as production holds it, then again after forcing it
where it is rooted. Both samples share one heap layout, so only deferred work separates them.
-}
probeEvaluation :: CorpusPackage -> IO Evaluated
probeEvaluation package = do
    enabled <- getRTSStatsEnabled
    unless enabled (fail "metadata residency requires RTS -T")
    bracket (prepareEntry package) (freeStablePtr . fst) (forceEntry . fst)
    before <- sample
    bracket (prepareEntry package) (freeStablePtr . fst) $ \(root, count) -> do
        -- Forcing renders names into pinned memory, which retires the pinned block holding the digest.
        -- 128 strings of 16 bytes, 32 each with their header, fill a 4 KiB block first, so both samples count it.
        forM_ [1 .. 128 :: Int] $ \i -> evaluate (BS.replicate 16 (fromIntegral i))
        weakHead <- live <$> sample
        forceEntry root
        forced <- live <$> sample
        pure (Evaluated count (live before) weakHead forced)

{-# NOINLINE prepareEntry #-}
prepareEntry :: CorpusPackage -> IO (StablePtr CacheEntry, Int)
prepareEntry package = do
    bytes <- BS.readFile (cpPath package)
    (info, document) <- project package bytes
    entry <- evaluate (CacheEntry info document (BS.length bytes) (digestOf bytes))
    count <- evaluate (Map.size (infoVersions info))
    root <- newStablePtr entry
    pure (root, count)

forceEntry :: StablePtr CacheEntry -> IO ()
forceEntry root = deRefStablePtr root >>= void . forceShown

-- | Use matched selected-value and discard controls to resolve retention above harness overhead.
probeSelected :: SelectedShape -> Limits -> PackageName -> Version -> FilePath -> IO Measurement
probeSelected shape limits name version path = measureRetained (prepareSelected shape limits name version path)

{-# NOINLINE prepareSelected #-}
prepareSelected :: SelectedShape -> Limits -> PackageName -> Version -> FilePath -> IO (StablePtr Held, (Int, Int64, Int, Int))
prepareSelected shape limits name version path = do
    (root, (bytes, _, count, _)) <- prepareSource StreamedSelected limits name version path >>= either (fail . toString) pure
    held <- deRefStablePtr root
    weight <- evaluate (sourceSize held)
    compact <- case held of
        HeldSelected selected ->
            evaluate (maybe 0 (maybe 0 (LBS.length . encode . diagnosticDocumentValue) . vdRaw) (vrVersion selected))
        _ -> fail "selected retention probe retained a different representation"
    retainedRoot <- case shape of
        SelectedValue -> pure root
        SelectedControl -> freeStablePtr root >> newStablePtr controlRoot
    pure (retainedRoot, (bytes, compact, weight, count))

controlRoot :: Held
controlRoot = HeldWire "control"

measureRetained :: IO (StablePtr Held, (Int, Int64, Int, Int)) -> IO Measurement
measureRetained prepareShape = do
    enabled <- getRTSStatsEnabled
    unless enabled (fail "metadata residency requires RTS -T")
    bracket prepareShape (freeStablePtr . fst) (observe . fst)
    before <- sample
    (bytes, compact, weight, count, held, prepared) <-
        bracket prepareShape (freeStablePtr . fst) $ \(root, (bytes, compact, weight, count)) -> do
            retained <- sample
            observe root
            pure (bytes, compact, weight, count, retained, sampleAllocated retained)
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
            , preparationAllocated = prepared - sampleAllocated before
            , preparationMaxLive = samplePeakLive held
            }

-- Full RTSStats records must die before the next collection measures a small retained root.
{-# NOINLINE sample #-}
sample :: IO HeapSample
sample = do
    performMajorGC
    stats <- getRTSStats
    evaluate (HeapSample (gcdetails_live_bytes (gc stats)) (allocated_bytes stats) (max_live_bytes stats))

-- Dereferencing after GC makes the root's continued reachability observable.
observe :: StablePtr Held -> IO ()
observe root = do
    observed <- deRefStablePtr root >>= evaluate . heldSize
    when (observed <= 0) (fail "retained metadata root is empty")

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

-- | The full read as production returns it, with artifact locations enforced against the capture's registry.
project :: CorpusPackage -> ByteString -> IO (PackageInfo, CachedDoc)
project package bytes = case pkgEcosystem name of
    Npm -> either (fail . show) (pure . located npmCaptureUpstream (fst npmCached)) (projectNpmManifest defaultLimits name bytes)
    PyPI -> either (fail . show) (pure . located pypiCaptureUpstream (fst pypiSimpleCached)) (projectPyPIIndex defaultLimits name bytes)
    RubyGems -> fail "no RubyGems metadata residency corpus"
  where
    name = cpPackage package
    located upstream cached (info, document) = (enforceArtifactLocations (upstreamAuthorities upstream) (upstreamOrigin upstream) info, cached document)

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
                        , preparationAllocated = sampleAllocated held - sampleAllocated before
                        , preparationMaxLive = samplePeakLive held
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

-- | Read one explicit ecosystem representation through the production streaming projection.
readSource :: SourceMode -> Limits -> PackageName -> Version -> IO ByteString -> IO (Either Text (Held, Int, ContentDigest))
readSource mode limits name version next = case pkgEcosystem name of
    Npm -> readNpmSource mode limits name version next
    PyPI -> readPyPISource mode limits name version next
    RubyGems -> pure (Left "no RubyGems source-read measurement")

readNpmSource :: SourceMode -> Limits -> PackageName -> Version -> IO ByteString -> IO (Either Text (Held, Int, ContentDigest))
readNpmSource mode limits name version next = case mode of
    BufferedLegacy -> readLegacySource limits name next
    BufferedCompact -> buffered $ \_ body ->
        first show (parseJsonChunks bound parser step emptyProjection [body]) >>= fullResult . (,digestOf body)
    StreamedFull -> digested (readJsonStream bound parser step emptyProjection) fullResult
    StreamedSelected -> digested (readJsonStream bound (npmFields (maxNestingDepth limits) (SelectedRead (renderVersion version))) step emptyProjection) selectedResult
    StreamedVersions -> digested (readJsonStream bound (versionListParser limits) (collectVersionList limits) emptyVersionList) versionsResult
  where
    bound = MetadataBodyLimit (maxMetadataBytes limits)
    parser = npmFields (maxNestingDepth limits) FullRead
    step = collectField limits name
    buffered projectBody = boundedRead bound next <&> (first show >=> uncurry projectBody)
    digested consume result = digestingRead consume next <&> (first show >=> result)
    fullResult (streamed, digest) = do
        (info, raw) <- first show (projectNpmStream limits name "https://registry.npmjs.org" streamed)
        pure (HeldShared (CacheEntry info (fst npmCached raw) (streamBytes streamed) digest), streamBytes streamed, digest)
    selectedResult (streamed, digest) = do
        projected <- first show (projectNpmStream limits name "https://registry.npmjs.org" streamed)
        let selected = selectNpmRead version (streamBytes streamed) projected
        pure (HeldSelected selected, streamBytes streamed, digest)
    versionsResult (streamed, digest) = do
        selected <- first show (streamValue streamed >>= finishVersionList)
        pure (HeldVersions selected, streamBytes streamed, digest)

readPyPISource :: SourceMode -> Limits -> PackageName -> Version -> IO ByteString -> IO (Either Text (Held, Int, ContentDigest))
readPyPISource mode limits name version next = case mode of
    BufferedLegacy -> readLegacySource limits name next
    BufferedCompact -> boundedRead bound next <&> (first show >=> \(_, body) -> first show (parseJsonChunks bound parser step (PyPIProjection.emptyProjection name) [body]) >>= fullResult . (,digestOf body))
    StreamedFull -> digested (readJsonStream bound parser step (PyPIProjection.emptyProjection name)) fullResult
    StreamedSelected ->
        let selectedMode = PyPIStream.SelectedRead name (renderVersion version)
         in digested (readJsonStream bound (PyPIStream.pypiFields (maxNestingDepth limits) selectedMode) (PyPIProjection.collectField limits selectedMode) (PyPIProjection.emptyProjection name)) selectedResult
    StreamedVersions -> pure (Left "PyPI exposes no version-list-only read")
  where
    bound = MetadataBodyLimit (maxMetadataBytes limits)
    parser = PyPIStream.pypiFields (maxNestingDepth limits) PyPIStream.FullRead
    step = PyPIProjection.collectField limits PyPIStream.FullRead
    digested consume result = digestingRead consume next <&> (first show >=> result)
    fullResult (streamed, digest) = do
        (info, document) <- first show (projectPyPIStream limits name streamed)
        pure (HeldShared (CacheEntry info (fst pypiSimpleCached document) (streamBytes streamed) digest), streamBytes streamed, digest)
    selectedResult (streamed, digest) = do
        (info, _) <- first show (projectPyPIStream limits name streamed)
        let selected = (untaggedRead (Map.lookup (renderVersion version) (infoVersions info))){vrBodyBytes = streamBytes streamed}
        pure (HeldSelected selected, streamBytes streamed, digest)

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

-- | Force retained fields through their accounting traversal without rendering them.
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

-- | Return the compact structural charge and retained version count.
sourceSummary :: Held -> (Int64, Int)
sourceSummary = \case
    HeldShared entry -> (weighCachedDoc (entryRaw entry), Map.size (infoVersions (entryInfo entry)))
    HeldTyped info -> (0, Map.size (infoVersions info))
    HeldSelected selected -> (maybe 0 (maybe 0 weighCachedDoc . vdRaw) (vrVersion selected), maybe 0 (const 1) (vrVersion selected))
    HeldVersions selected -> (0, length selected)
    HeldRaw _ -> (0, 0)
    HeldWire _ -> (0, 0)
    HeldLegacy info raw -> (estimateValueBytes raw, Map.size (infoVersions info))
