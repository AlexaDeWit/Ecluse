-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Isolated native heap probes use the residency executable's existing child-process sampler.
module Ecluse.Core.Server.MemoryModel.Probe (
    HeapSample (..),
    sample,
    forceShown,
    Shape (..),
    Measurement (..),
    packages,
    Residency (..),
    residencyFor,
    requireResidency,
    requireFigures,
    retainedFigure,
    probe,
    Evaluated (..),
    probeEvaluation,
    project,
    ListingPeaks (..),
    SingleListing (..),
    probeListing,
    writeMergeDocuments,
    probeMerge,
    measureInChild,
    childMain,
    packageMain,
    SelectedShape (..),
    probeSelected,
    SourceMode (..),
    probeSource,
) where

import Prelude hiding (universe)

import Data.Aeson (FromJSON, ToJSON, Value, eitherDecodeStrict, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Universe.Class (universe)
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (getRTSStatsEnabled)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (withBinaryFile)
import System.Process (readProcessWithExitCode)
import UnliftIO.Exception (bracket, evaluate, throwIO)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems), ecosystemName)
import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName, pkgEcosystem)
import Ecluse.Core.Package.Merge (Provenance (GatedSource, TrustedSource), mergePackuments)
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMetadata))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, estimateValueBytes, npmCached, weighCachedDoc)
import Ecluse.Core.Registry.Cargo.Project qualified as CargoProject
import Ecluse.Core.Registry.Cargo.Serving (assembleCargoDocument, serialiseCargoDocument)
import Ecluse.Core.Registry.Json.Walk (readJsonWalk)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError (MetadataUndecodable), VersionDoc (..), VersionRead (vrBodyBytes, vrVersion))
import Ecluse.Core.Registry.Metadata.Fetch (readManifest, readVersion)
import Ecluse.Core.Registry.Metadata.Fetch.Types (Body, EcosystemRead, ReadTerms (..))
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.Npm.Metadata (npmRead)
import Ecluse.Core.Registry.Npm.Project qualified as Npm
import Ecluse.Core.Registry.Npm.VersionListReader (npmVersionListWalk)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Registry.PyPI.Metadata (pypiRead)
import Ecluse.Core.Registry.PyPI.Project qualified as PyPI
import Ecluse.Core.Registry.RubyGems.Metadata (gemReadWith)
import Ecluse.Core.Registry.RubyGems.Project qualified as RubyGemsProject
import Ecluse.Core.Registry.RubyGems.Serving (assembleRubyGemsDocument, serialiseRubyGemsDocument)
import Ecluse.Core.Registry.VersionList (collectVersionList, emptyVersionList, finishVersionList)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits, boundedRead, defaultLimits, maxMetadataBytes)
import Ecluse.Core.Server.Cache (CacheEntry (..))
import Ecluse.Core.Server.Cache.VersionWeight (weighVersion)
import Ecluse.Core.Server.MemoryModel (expandWireBytes)
import Ecluse.Core.Server.MemoryModel.Calibration (Calibration, Figures (..), cargoCalibration, cargoFigures, npmCalibration, npmFigures, pypiCalibration, pypiFigures, rubyGemsCalibration, rubyGemsFigures)
import Ecluse.Core.Server.MemoryModel.HeapSample (HeapSample (..), sample)
import Ecluse.Core.Server.MemoryModel.ProbeMode (ProbeMode, probeArguments)
import Ecluse.Core.Server.Pipeline.Packument.Representation (Contribution (..), outputBasisBytes)
import Ecluse.Core.Snapshot (ContentDigest, Snapshot (Snapshot))
import Ecluse.Core.Version (Version, canonicalPep440, mkVersion, renderVersion)
import Ecluse.Test.Corpus (CaptureUpstream (..), CorpusPackage (cpPackage, cpPath), cargoCaptureUpstream, cargoCorpusPackages, corpusPackages, cpName, npmCaptureUpstream, pypiCaptureUpstream, pypiCorpusPackages, rubyGemsCaptureUpstream, rubyGemsCorpusPackages, syntheticProxyBase)
import Ecluse.Test.Corpus qualified as Corpus
import Ecluse.Test.Corpus.Cargo qualified as Cargo
import Ecluse.Test.Corpus.Merge (MergeDocument (Captured, Rewritten, RewrittenNative), MergeShape, captureDocuments)
import Ecluse.Test.Corpus.RubyGems qualified as RubyGems
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Cargo.Metadata (cargoRead)
import Ecluse.Test.Registry.Metadata.Fetch (captureManifest, heldBody, sourceBody)
import Ecluse.Test.Registry.Metadata.Projection (projectMetadata)
import Ecluse.Test.Registry.Metadata.Rendering (MetadataRendering (..), renderingOf)
import Ecluse.Test.Registry.Npm.Project (parsePackageInfoFromValue)
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

-- A rooted representation the isolated retention and source probes hold through a collection.
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

-- | Absolute live bytes around one listing's full reads and its render. Memory quantities use bytes.
data ListingPeaks = ListingPeaks
    { listingSourceBytes :: Int
    -- ^ Every source the listing reads.
    , listingBasisBytes :: Int
    -- ^ The source bytes the output charge scales.
    , listingServedBytes :: Int
    , listingBaseline :: Word64
    , listingReadPeak :: Word64
    -- ^ The high-water through the full reads.
    , listingEntryLive :: Word64
    -- ^ Live bytes holding the reads' sources.
    , listingPeak :: Word64
    -- ^ The high-water through the reads and the render of the served body.
    }
    deriving stock (Show, Generic)

instance ToJSON ListingPeaks
instance FromJSON ListingPeaks

-- | A single-source listing's peaks, and what its cache entry holds for its served document.
data SingleListing = SingleListing
    { singlePeaks :: ListingPeaks
    , singleDocumentLive :: Integer
    -- ^ The live bytes another read's entry frees when it drops its served document.
    , singleDocumentCharge :: Int
    -- ^ The heap bytes the served document's weight stands for, as a cache expands it.
    }
    deriving stock (Show, Generic)

instance ToJSON SingleListing
instance FromJSON SingleListing

-- | Use the same complete package catalogue as the performance harnesses.
packages :: [CorpusPackage]
packages = concatMap residencyPackages (mapMaybe residencyFor universe)

-- | All mandatory measurements and probe hooks for one registered ecosystem.
data Residency = Residency
    { residencyPackages :: [CorpusPackage]
    , residencyRead :: EcosystemRead
    , residencyMetadata :: MetadataRendering
    , residencyUpstream :: CaptureUpstream
    , residencyCalibration :: Maybe Calibration
    , residencyFigures :: Map Text Figures
    , residencyEntryBelowSource :: Bool
    , residencyLegacyRead :: Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, Value)
    , residencyLegacyHeld :: PackageInfo -> Value -> Int -> ContentDigest -> Held
    , residencyIdentity :: String -> String -> IO (PackageName, Version)
    , residencyVersionList :: Limits -> IO ByteString -> IO (Either Text (Held, Int, Maybe ContentDigest))
    }

-- | Native providers expose raw observations before calibration or adapter activation.
residencyFor :: Ecosystem -> Maybe Residency
residencyFor = \case
    Npm ->
        Just
            Residency
                { residencyPackages = corpusPackages
                , residencyRead = npmRead
                , residencyMetadata = renderingOf (adapterMetadata npmAdapter)
                , residencyUpstream = npmCaptureUpstream
                , residencyCalibration = Just npmCalibration
                , residencyFigures = npmFigures
                , residencyEntryBelowSource = True
                , residencyLegacyRead = \limits name -> projectMetadata (parsePackageInfoFromValue name) limits
                , residencyLegacyHeld = \info raw size digest -> HeldShared (CacheEntry info (fst npmCached raw) size digest)
                , residencyIdentity = \name version -> (,mkVersion Npm (toText version)) <$> either (fail . show) pure (Npm.projectName (toText name))
                , residencyVersionList = readVersionList
                }
    PyPI ->
        Just
            Residency
                { residencyPackages = pypiCorpusPackages
                , residencyRead = pypiRead
                , residencyMetadata = renderingOf (adapterMetadata pypiAdapter)
                , residencyUpstream = pypiCaptureUpstream
                , residencyCalibration = Just pypiCalibration
                , residencyFigures = pypiFigures
                , residencyEntryBelowSource = False
                , residencyLegacyRead = \limits name -> projectMetadata (projectSimpleIndexFromValue name) limits
                , residencyLegacyHeld = \info raw _ _ -> HeldLegacy info raw
                , residencyIdentity = \name version -> (,) <$> either (fail . show) pure (PyPI.projectName (toText name)) <*> maybe (fail "invalid PEP 440 probe version") pure (canonicalPep440 (toText version))
                , residencyVersionList = \_ _ -> pure (Left "PyPI exposes no version-list-only read")
                }
    Cargo -> do
        discovery <- rightToMaybe Cargo.cargoCaptureDiscovery
        pure
            Residency
                { residencyPackages = cargoCorpusPackages
                , residencyRead = cargoRead discovery
                , residencyMetadata = MetadataRendering assembleCargoDocument serialiseCargoDocument
                , residencyUpstream = cargoCaptureUpstream
                , residencyCalibration = Just cargoCalibration
                , residencyFigures = cargoFigures
                , residencyEntryBelowSource = False
                , residencyLegacyRead = \limits name bytes -> first (const MetadataUndecodable) (Cargo.decodeCargoValues bytes) >>= Cargo.projectCargoValues limits name
                , residencyLegacyHeld = \info raw _ _ -> HeldLegacy info raw
                , residencyIdentity = \name version -> (,mkVersion Cargo (toText version)) <$> either (fail . show) pure (CargoProject.projectName (toText name))
                , residencyVersionList = \_ _ -> pure (Left "Cargo exposes no version-list-only read")
                }
    RubyGems ->
        Just
            Residency
                { residencyPackages = rubyGemsCorpusPackages
                , residencyRead = gemReadWith (const pass) (const pass) "https://rubygems.org"
                , residencyMetadata = MetadataRendering assembleRubyGemsDocument serialiseRubyGemsDocument
                , residencyUpstream = rubyGemsCaptureUpstream
                , residencyCalibration = Just rubyGemsCalibration
                , residencyFigures = rubyGemsFigures
                , residencyEntryBelowSource = False
                , residencyLegacyRead = \limits name bytes -> first (const MetadataUndecodable) (RubyGems.decodeRubyGemsValues bytes) >>= RubyGems.projectRubyGemsValues limits name
                , residencyLegacyHeld = \info raw _ _ -> HeldLegacy info raw
                , residencyIdentity = \name version -> (,mkVersion RubyGems (toText version)) <$> either (fail . show) pure (RubyGemsProject.projectName (toText name))
                , residencyVersionList = \_ _ -> pure (Left "RubyGems exposes no version-list-only read")
                }

-- | Missing residency wiring fails a probe instead of skipping its checks.
requireResidency :: Ecosystem -> IO Residency
requireResidency ecosystem = maybe (fail ("no metadata residency record for " <> toString (ecosystemName ecosystem))) pure (residencyFor ecosystem)

-- | A capture without measured figures fails its probe instead of skipping its limits.
requireFigures :: CorpusPackage -> IO Figures
requireFigures package = do
    record <- requireResidency (pkgEcosystem (cpPackage package))
    maybe (fail ("no residency figures for " <> toString (cpName package))) pure (Map.lookup (cpName package) (residencyFigures record))

-- | The measured figure that a retained shape's limit derives from.
retainedFigure :: Shape -> Figures -> Rational
retainedFigure = \case
    Wire -> retainedWire
    Raw -> retainedRaw
    Typed -> retainedTyped
    Shared -> retainedShared

-- | Measure one capture in a fresh process of this executable, on one capability with RTS statistics.
measureInChild :: (FromJSON a) => ProbeMode -> CorpusPackage -> IO (Either String a)
measureInChild mode package = do
    executable <- getExecutablePath
    (status, output, errors) <- readProcessWithExitCode executable (probeArguments mode (cpPath package)) ""
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

-- | 'childMain' for a probe with one mode, such as 'probeEvaluation' or 'probeListing'.
packageMain :: (ToJSON a) => (CorpusPackage -> IO a) -> FilePath -> IO ()
packageMain measure path = corpusPackageAt path >>= measure >>= LBS.putStr . encode

corpusPackageAt :: FilePath -> IO CorpusPackage
corpusPackageAt path = maybe (fail "unknown metadata residency corpus path") pure (find ((== path) . cpPath) packages)

-- | Root only the selected representation across collections, then verify its release separately.
probe :: Shape -> CorpusPackage -> IO Measurement
probe shape package = measureRetained (prepare shape package)

-- | Compare the rooted read result before and after forcing in the same heap layout.
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
        Evaluated count (live before) weakHead . live <$> sample

{-# NOINLINE prepareEntry #-}
prepareEntry :: CorpusPackage -> IO (StablePtr CacheEntry, Int)
prepareEntry package = do
    entry <- BS.readFile (cpPath package) >>= readCapture package >>= evaluate . manifestEntry
    count <- evaluate (Map.size (infoVersions (entryInfo entry)))
    root <- newStablePtr entry
    pure (root, count)

forceEntry :: StablePtr CacheEntry -> IO ()
forceEntry root = deRefStablePtr root >>= void . forceShown

-- | Measure a public-only listing where most collections are major.
probeListing :: CorpusPackage -> IO SingleListing
probeListing package = do
    peaks <- probeReads [(GatedSource, cpPath package)] package
    (document, charged) <- documentLive package
    pure SingleListing{singlePeaks = peaks, singleDocumentLive = document, singleDocumentCharge = charged}

-- | 'probeListing' for a listing that merges a trusted private document with a public one.
probeMerge :: FilePath -> FilePath -> CorpusPackage -> IO ListingPeaks
probeMerge private public = probeReads [(TrustedSource, private), (GatedSource, public)]

probeReads :: [(Provenance, FilePath)] -> CorpusPackage -> IO ListingPeaks
probeReads documents package = do
    enabled <- getRTSStatsEnabled
    unless enabled (fail "metadata residency requires RTS -T")
    -- A first read settles the read's one-off state, so the baseline holds it.
    bracket (prepareListingReads (cpPackage package) (take 1 documents)) freeStablePtr (void . deRefStablePtr)
    before <- sample
    bracket (prepareListingReads (cpPackage package) documents) freeStablePtr $ \sourcesRoot -> do
        held <- sample
        sources <- deRefStablePtr sourcesRoot
        bracket (prepareListingRender (pkgEcosystem (cpPackage package)) sources) (freeStablePtr . fst) $ \(servedRoot, basis) -> do
            rendered <- sample
            served <- deRefStablePtr servedRoot
            -- Evaluated here, so nothing sampled later holds the sources or the served body.
            evaluate
                ListingPeaks
                    { listingSourceBytes = sum (map srcBodyBytes sources)
                    , listingBasisBytes = basis
                    , listingServedBytes = BS.length served
                    , listingBaseline = live before
                    , listingReadPeak = samplePeakLive held
                    , listingEntryLive = live held
                    , listingPeak = samplePeakLive rendered
                    }

-- Both samples stay in this function so each counts only the entry fields still reachable.
{-# NOINLINE documentLive #-}
documentLive :: CorpusPackage -> IO (Integer, Int)
documentLive package = do
    (whole, others, charged) <- bracket (prepareListingRead package) freeStablePtr $ \root -> do
        whole <- sample
        entry <- deRefStablePtr root
        charged <- evaluate (expandWireBytes (fromIntegral (weighCachedDoc (entryRaw entry))))
        -- Evaluated here, so the fields kept do not hold the entry.
        let !info = entryInfo entry
            !digest = entryDigest entry
        pure (whole, (info, digest), charged)
    dropped <- bracket (newStablePtr others) freeStablePtr (const sample)
    pure (toInteger (live whole) - toInteger (live dropped), charged)

-- The production full read of the file in 32 KiB chunks, with the entry forced.
readListingEntry :: PackageName -> FilePath -> IO CacheEntry
readListingEntry name path = do
    entry <- withBinaryFile path ReadMode (readFull defaultLimits name . sourceBody . (`BS.hGetSome` 32768)) >>= either (fail . toString) pure
    entry <$ evaluate (sourceSize (HeldShared entry))

{-# NOINLINE prepareListingRead #-}
prepareListingRead :: CorpusPackage -> IO (StablePtr CacheEntry)
prepareListingRead package = readListingEntry (cpPackage package) (cpPath package) >>= newStablePtr

-- One production full read for each document, held as the merge's sources.
{-# NOINLINE prepareListingReads #-}
prepareListingReads :: PackageName -> [(Provenance, FilePath)] -> IO (StablePtr [Contribution])
prepareListingReads name documents = do
    sources <- forM documents $ \(provenance, path) -> do
        entry <- readListingEntry name path
        pure (Contribution provenance (entryInfo entry) (entryRaw entry) (entryDigest entry) (entryBodyBytes entry))
    newStablePtr sources

-- The strict served body of the sources' merge in which every version survives, and its output basis.
{-# NOINLINE prepareListingRender #-}
prepareListingRender :: Ecosystem -> [Contribution] -> IO (StablePtr ByteString, Int)
prepareListingRender ecosystem sources = do
    metadata <- residencyMetadata <$> requireResidency ecosystem
    plan <- maybe (fail "capture has no merge plan") pure (mergePackuments [(srcProvenance s, Snapshot (srcDigest s) (srcInfo s)) | s <- sources])
    basis <- evaluate (outputBasisBytes plan sources)
    served <- either throwIO (evaluate . LBS.toStrict) (metadataSerialise metadata (metadataAssemble metadata syntheticProxyBase bySource plan base))
    root <- newStablePtr served
    pure (root, basis)
  where
    bySource = Map.fromList (zip [0 ..] [Snapshot (srcDigest source) (srcValue source) | source <- sources])
    base = srcValue <$> (find ((== TrustedSource) . srcProvenance) sources <|> listToMaybe sources)

-- | Write a merge's rewritten documents under the directory, and return both documents' paths.
writeMergeDocuments :: MergeShape -> FilePath -> CorpusPackage -> IO (FilePath, FilePath)
writeMergeDocuments shape directory package = do
    (private, public) <- Corpus.readCapture package >>= either fail pure . captureDocuments shape package
    (,) <$> place "private.json" private <*> place "public.json" public
  where
    place file = \case
        Captured -> pure (cpPath package)
        Rewritten document -> (directory <> "/" <> file) <$ LBS.writeFile (directory <> "/" <> file) (encode document)
        RewrittenNative bytes -> (directory <> "/" <> file) <$ BS.writeFile (directory <> "/" <> file) bytes

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
            raw <- either fail pure $ case pkgEcosystem (cpPackage package) of
                Cargo -> first toString (Cargo.decodeCargoValues bytes)
                RubyGems -> first toString (RubyGems.decodeRubyGemsValues bytes)
                _ -> eitherDecodeStrict bytes
            _ <- forceShown raw
            compact <- evaluate (LBS.length (encode raw))
            pure (HeldRaw raw, compact, 0, 0)
        Typed -> do
            (info, _) <- project package bytes
            _ <- forceShown info
            pure (HeldTyped info, 0, 0, Map.size (infoVersions info))
        Shared -> do
            (info, document) <- project package bytes
            -- The probe's own size and digest: the read's digest keeps a 4 KiB pinned block the gate omits.
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

-- | The typed view and the served document of 'readCapture'.
project :: CorpusPackage -> ByteString -> IO (PackageInfo, CachedDoc)
project package bytes = (\manifest -> (manifestInfo manifest, manifestRaw manifest)) <$> readCapture package bytes

readCapture :: CorpusPackage -> ByteString -> IO Manifest
readCapture package bytes = do
    (nativeRead, upstream) <- maybe noResidencyCorpus pure (captureSource (pkgEcosystem (cpPackage package)))
    captureManifest nativeRead upstream (cpPackage package) [bytes] >>= either (fail . show) pure

captureSource :: Ecosystem -> Maybe (EcosystemRead, CaptureUpstream)
captureSource = fmap (\record -> (residencyRead record, residencyUpstream record)) . residencyFor

noResidencyCorpus :: IO a
noResidencyCorpus = fail "no metadata residency record"

-- | Consume the complete derived rendering, including opaque fields, without retaining its output.
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
        Right (root, (bytes, compact, count, readDigest)) -> do
            finished <- getMonotonicTimeNSec
            held <- bracket (pure root) freeStablePtr $ \pointer -> do
                retained <- sample
                void (deRefStablePtr pointer >>= evaluate . sourceSize)
                pure retained
            released <- sample
            -- A read that takes no digest reports the file's, hashed here after every sample.
            digest <- maybe (digestOf <$> BS.readFile path) pure readDigest
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
prepareSource :: SourceMode -> Limits -> PackageName -> Version -> FilePath -> IO (Either Text (StablePtr Held, (Int, Int64, Int, Maybe ContentDigest)))
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
                digest' <- traverse evaluate digest
                root <- newStablePtr held
                pure (Right (root, (bytes', compactBytes', count', digest')))

readSource :: SourceMode -> Limits -> PackageName -> Version -> IO ByteString -> IO (Either Text (Held, Int, Maybe ContentDigest))
readSource mode limits name version next = do
    record <- requireResidency (pkgEcosystem name)
    case mode of
        BufferedLegacy -> readLegacySource record limits name next
        BufferedCompact -> boundedRead bound next >>= either (pure . Left . show) (fmap (fmap heldEntry) . readFull limits name . heldBody . one . snd)
        StreamedFull -> fmap heldEntry <$> readFull limits name (sourceBody next)
        StreamedSelected -> streamSelected limits name version next
        StreamedVersions -> residencyVersionList record limits next
  where
    bound = MetadataBodyLimit (maxMetadataBytes limits)

readVersionList :: Limits -> IO ByteString -> IO (Either Text (Held, Int, Maybe ContentDigest))
readVersionList limits next = readJsonWalk (MetadataBodyLimit (maxMetadataBytes limits)) (npmVersionListWalk limits (collectVersionList limits) emptyVersionList) next <&> (first show >=> versionsResult)
  where
    versionsResult streamed = do
        selected <- first show (streamValue streamed >>= finishVersionList)
        pure (HeldVersions selected, streamBytes streamed, Nothing)

readFull :: Limits -> PackageName -> Body -> IO (Either Text CacheEntry)
readFull limits name body = case captureSource (pkgEcosystem name) of
    Nothing -> pure (Left "no metadata residency record")
    Just (nativeRead, upstream) -> bimap show manifestEntry <$> readManifest nativeRead passthroughTracingPort (sourceTerms limits upstream) name body

streamSelected :: Limits -> PackageName -> Version -> IO ByteString -> IO (Either Text (Held, Int, Maybe ContentDigest))
streamSelected limits name version next = case captureSource (pkgEcosystem name) of
    Nothing -> pure (Left "no metadata residency record")
    Just (nativeRead, upstream) ->
        bimap show (\selected -> (HeldSelected selected, vrBodyBytes selected, Nothing))
            <$> readVersion nativeRead passthroughTracingPort (sourceTerms limits upstream) name version (sourceBody next)

-- A read from the capture's registry under the probe's own limits, outside the memory gate.
sourceTerms :: Limits -> CaptureUpstream -> ReadTerms
sourceTerms limits upstream = ReadTerms{rtLimits = limits, rtBaseUrl = upstreamOrigin upstream, rtChargeFullRead = const pass}

manifestEntry :: Manifest -> CacheEntry
manifestEntry manifest = CacheEntry (manifestInfo manifest) (manifestRaw manifest) (manifestBodyBytes manifest) (manifestDigest manifest)

heldEntry :: CacheEntry -> (Held, Int, Maybe ContentDigest)
heldEntry entry = (HeldShared entry, entryBodyBytes entry, Just (entryDigest entry))

readLegacySource :: Residency -> Limits -> PackageName -> IO ByteString -> IO (Either Text (Held, Int, Maybe ContentDigest))
readLegacySource record limits name next = boundedRead (MetadataBodyLimit (maxMetadataBytes limits)) next <&> (first show >=> projectBody)
  where
    projectBody (size, body) = do
        (info, raw) <- first show (residencyLegacyRead record limits name body)
        let digest = digestOf body
        pure (residencyLegacyHeld record info raw size digest, size, Just digest)

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
