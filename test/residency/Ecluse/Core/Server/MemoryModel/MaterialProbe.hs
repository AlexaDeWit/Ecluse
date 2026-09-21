-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Explicit metadata stages for static admission calibration.
Source roots survive policy and output work. Process RSS includes native allocations and
runtime overhead. These sequential probes do not establish a concurrent request heap bound.
-}
module Ecluse.Core.Server.MemoryModel.MaterialProbe (materialMain) where

import Data.Aeson (ToJSON, Value, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (allocated_bytes, gc, max_live_bytes, max_mem_in_use_bytes), getRTSStats, getRTSStatsEnabled)
import System.IO (withBinaryFile)
import System.Mem (performMajorGC)
import UnliftIO.Exception (bracket, evaluate)

import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName, pkgEcosystem)
import Ecluse.Core.Package.Filter (filterPlanFromDecisions, fpSurvivors, restrictToSurvivors)
import Ecluse.Core.Package.Merge (MergePlan, Provenance (GatedSource, TrustedSource), mergePackuments)
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMetadata), adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataAssemble, metadataSerialise))
import Ecluse.Core.Registry.Metadata (VersionDoc (vdDetails), VersionRead (vrVersion))
import Ecluse.Core.Rules (evalRules, prepare)
import Ecluse.Core.Rules.Types (EvalContext (EvalContext), completeEvidence)
import Ecluse.Core.Security (Limits (maxMetadataBytes))
import Ecluse.Core.Server.Cache (CacheEntry (..))
import Ecluse.Core.Server.MemoryModel.Probe (Held (..), SourceMode (StreamedFull, StreamedSelected), readSource, sourceSize, sourceSummary)
import Ecluse.Core.Server.MemoryModelResidencySpec (probeIdentity, probeLimits)
import Ecluse.Core.Snapshot (ContentDigest, Snapshot (Snapshot), digestBytes)
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Test.Corpus (permissiveAgeRules, syntheticProxyBase)
import Ecluse.Test.Rules (inertRuleDeps)

data Mode = ColdSelected | RetainedSelected | FullOrigin | ListingOneOrigin | ListingTwoOrigins
    deriving stock (Eq, Show, Read)

data Sample = Sample
    { stage :: Text
    , monotonicNs :: Word64
    , allocatedBytes :: Word64
    , lastGcLiveBytes :: Word64
    , peakGcLiveBytes :: Word64
    , peakRtsCommittedBytes :: Word64
    , processRssBytes :: Integer
    , processPeakRssBytes :: Integer
    }
    deriving stock (Generic)

instance ToJSON Sample

data Source = Source
    { sourceRoot :: StablePtr Held
    , sourceBytes :: Int
    , sourceDigest :: ContentDigest
    , sourceCompact :: Int64
    , sourceVersions :: Int
    , sourceCharge :: Int
    }

data Input = Input
    { inputName :: PackageName
    , inputVersion :: Version
    , inputLimits :: Limits
    , inputPath :: FilePath
    }

-- | Run one Linux process with explicit source identity and report each materialisation stage.
materialMain :: String -> String -> String -> String -> String -> FilePath -> IO ()
materialMain ecosystem rawMode name version rawLimit path = do
    enabled <- getRTSStatsEnabled
    unless enabled (fail "material calibration requires RTS -T")
    mode <- maybe (fail "unknown material probe mode") pure (readMaybe rawMode)
    (package, selected) <- probeIdentity ecosystem name version
    limits <- probeLimits rawLimit
    let limit = maxMetadataBytes limits
        input = Input package selected limits path
    before <- sample "runtime_baseline" True
    result <- measure mode input
    released <- sample "released" True
    LBS.putStr $
        encode $
            object
                [ "mode" .= rawMode
                , "ecosystem" .= ecosystem
                , "package" .= name
                , "selected_version" .= version
                , "lookup_version" .= renderVersion selected
                , "path" .= path
                , "body_limit" .= limit
                , "chunk_bytes" .= (32768 :: Int)
                , "baseline" .= before
                , "result" .= result
                , "released" .= released
                , "scope" .= ("Sequential file reads, permissive age policy, production merge and output. No HTTP, advisory database, concurrency or install. The retained-selected mode collects at the reuse baseline." :: Text)
                ]

{-# NOINLINE measure #-}
measure :: Mode -> Input -> IO Value
measure mode input =
    withSources count sourceMode input $ \sources -> do
        ready <- sample "sources_ready" (mode == RetainedSelected)
        held <- traverse (deRefStablePtr . sourceRoot) sources
        let identities = map sourceIdentity sources
        (outputSize, decisions, finished, retained) <- case mode of
            ColdSelected -> selectedWork held
            RetainedSelected -> selectedWork held
            FullOrigin -> observeOutput BS.empty 0
            ListingOneOrigin -> listingWork input held
            ListingTwoOrigins -> listingWork input held
        traverse_ (deRefStablePtr . sourceRoot >=> evaluate . sourceSize) sources
        pure $
            object
                [ "sources" .= identities
                , "ready" .= ready
                , "after_material" .= finished
                , "held" .= retained
                , "output_bytes" .= outputSize
                , "policy_admitted_versions" .= decisions
                ]
  where
    count = if mode == ListingTwoOrigins then 2 else 1
    sourceMode = if mode `elem` [ColdSelected, RetainedSelected] then StreamedSelected else StreamedFull

withSources :: Int -> SourceMode -> Input -> ([Source] -> IO a) -> IO a
withSources count mode input use
    | count <= 0 = use []
    | otherwise = bracket (readOne mode input) (freeStablePtr . sourceRoot) $ \source ->
        withSources (count - 1) mode input (use . (source :))

readOne :: SourceMode -> Input -> IO Source
readOne mode input = withBinaryFile (inputPath input) ReadMode $ \handle -> do
    (held, bytes, digest) <- readSource mode (inputLimits input) (inputName input) (inputVersion input) (BS.hGetSome handle 32768) >>= either (fail . toString) pure
    charge <- evaluate (sourceSize held)
    let (compact, versions) = sourceSummary held
    compact' <- evaluate compact
    versions' <- evaluate versions
    root <- newStablePtr held
    pure (Source root bytes digest compact' versions' charge)

sourceIdentity :: Source -> Value
sourceIdentity source =
    object
        [ "source_bytes" .= sourceBytes source
        , "digest_bytes" .= BS.unpack (digestBytes (sourceDigest source))
        , "versions" .= sourceVersions source
        , "compact_byte_estimate" .= sourceCompact source
        , "accounting_charge" .= sourceCharge source
        ]

selectedWork :: [Held] -> IO (Int, Int, Sample, Sample)
selectedWork held = do
    rules <- prepare inertRuleDeps permissiveAgeRules
    decisions <- forM held $ \case
        HeldSelected selected -> traverse (evalRules calibrationClock rules . completeEvidence . vdDetails) (vrVersion selected)
        _ -> fail "selected probe retained a different representation"
    checksum <- evaluate (sum [Set.size (fpSurvivors (filterPlanFromDecisions (Map.singleton "selected" decision))) | Just decision <- decisions])
    observeOutput BS.empty checksum

listingWork :: Input -> [Held] -> IO (Int, Int, Sample, Sample)
listingWork input held = do
    entries <- traverse fullEntry held
    rules <- prepare inertRuleDeps permissiveAgeRules
    let origins = if length entries == 2 then [TrustedSource, GatedSource] else [GatedSource]
    admitted <- forM (zip origins entries) $ \(origin, entry) -> case origin of
        TrustedSource -> pure (entryInfo entry, 0)
        GatedSource -> do
            decisions <- traverse (evalRules calibrationClock rules . completeEvidence) (infoVersions (entryInfo entry))
            let survivors = fpSurvivors (filterPlanFromDecisions decisions)
            pure (restrictToSurvivors survivors (entryInfo entry), Set.size survivors)
    let (infos, counts) = unzip admitted
        contributions = zipWith (\origin (entry, info) -> (origin, Snapshot (entryDigest entry) info)) origins (zip entries infos)
    plan <- maybe (fail "calibration listing has no surviving versions") pure (mergePackuments contributions)
    metadata <- maybe (fail "no metadata adapter for probe ecosystem") (pure . adapterMetadata) (adapterFor (pkgEcosystem (inputName input)))
    output <- evaluate (renderOutput metadata entries plan)
    observeOutput output (sum counts)
  where
    fullEntry = \case
        HeldShared entry -> pure entry
        _ -> fail "listing probe retained a different representation"

renderOutput :: AdapterMetadata -> [CacheEntry] -> MergePlan -> ByteString
renderOutput metadata entries plan =
    LBS.toStrict $
        metadataSerialise metadata $
            metadataAssemble metadata syntheticProxyBase documents plan (entryRaw <$> listToMaybe entries)
  where
    documents = Map.fromList (zip [0 ..] [Snapshot (entryDigest entry) (entryRaw entry) | entry <- entries])

observeOutput :: ByteString -> Int -> IO (Int, Int, Sample, Sample)
observeOutput output decisions = bracket (newStablePtr output) freeStablePtr $ \root -> do
    size <- evaluate (BS.length output)
    checksum <- evaluate decisions
    finished <- sample "after_material" False
    retained <- sample "held" True
    void (deRefStablePtr root >>= evaluate . BS.length)
    pure (size, checksum, finished, retained)

calibrationClock :: EvalContext
calibrationClock = EvalContext (UTCTime (fromGregorian 2026 9 22) 0) Nothing

sample :: Text -> Bool -> IO Sample
sample name collect = do
    when collect performMajorGC
    stats <- getRTSStats
    elapsed <- getMonotonicTimeNSec
    status <- readFileBS "/proc/self/status"
    rss <- statusBytes "VmRSS:" status
    peak <- statusBytes "VmHWM:" status
    pure (Sample name elapsed (allocated_bytes stats) (gcdetails_live_bytes (gc stats)) (max_live_bytes stats) (max_mem_in_use_bytes stats) rss peak)

statusBytes :: Text -> ByteString -> IO Integer
statusBytes field bytes =
    case [amount | line <- T.lines (decodeUtf8 bytes), [key, amount, "kB"] <- [T.words line], key == field] of
        [amount] -> maybe (fail "invalid process memory counter") (pure . (* 1024)) (readMaybe (toString amount))
        _ -> fail "Linux process memory counter is missing"
