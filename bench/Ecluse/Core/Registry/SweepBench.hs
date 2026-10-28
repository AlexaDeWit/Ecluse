-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure complete Dredger cycles over fixed-page CodeArtifact inventories.
Paced and unpaced rows retain the same native calls and independently checked inventory counts.
-}
module Ecluse.Core.Registry.SweepBench (withBenchmarks) where

import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import GHC.Stats (GCDetails (gcdetails_gen, gcdetails_live_bytes), RTSStats (allocated_bytes, cumulative_live_bytes, gc, major_gcs, max_live_bytes), getRTSStats, getRTSStatsEnabled)
import System.CPUTime (getCPUTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performGC)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCaseInfo, (@?=))
import UnliftIO.Async (withAsync)

import Ecluse.Core.Clock (monoSecondsBetween, monotonicNow, waitSeconds)
import Ecluse.Core.Cve (CveDb (cveDbLookup), CveLookup (cveCoveredNames))
import Ecluse.Core.Cve.Types (DbEtag)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems), ecosystemName)
import Ecluse.Core.Package (infoVersions, mkPackageName, renderPackageName)
import Ecluse.Core.Package.Hash (sha256Hex)
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMaintenance, adapterProjectName), adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMaintenance (maintenanceAlphabet))
import Ecluse.Core.Registry.Cargo.Project (cargoNameChars)
import Ecluse.Core.Registry.Maintenance (StoreFacts (factBackend, factBudget), StoreMaintenance (storeFacts), StoreManifestRead, meteredMaintenance, storeFaultOfMetadata)
import Ecluse.Core.Registry.Maintenance.Budget (BudgetPort (budgetClose), CycleCost (ccRequests), QuotaScope, RequestGate, RequestKind (..), StoreBudget (bgScope), newBudgetMeter, tallyCounts)
import Ecluse.Core.Registry.Maintenance.NameSpace (NameAlphabet, initialBuckets, noNameAlphabet, renderNamePrefix)
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo))
import Ecluse.Core.Registry.Npm.Project (npmNameLeadChars)
import Ecluse.Core.Registry.PyPI.Project (pypiNameLeadChars)
import Ecluse.Core.Registry.Sweep (paceAtCeiling, sweepCycle)
import Ecluse.Core.Registry.Sweep.Outcome (CycleOutcome (outcomeTally), SweepTally (..), outcomeComplete)
import Ecluse.Core.Registry.Sweep.Pacing (defaultCycleWindow)
import Ecluse.Core.Registry.Sweep.Types (SweepMount (..), SweepPacing (..), SweepPorts (..), SweepShape (SweepCandidates, SweepEverything), deletingCache, pairedStore)
import Ecluse.Core.Rules (AdvisoryDatabase (AdvisoryDatabase, NoAdvisoryDatabase), AdvisoryGeneration (agLookup), PreparedRule (prepEval, prepName), RuleDeps (rdAdvisoryDatabase, rdCurrentAdvisoryEtag), RuleEval (PerInstallEvidence, PerPackage, PerVersion), prepare)
import Ecluse.Core.Rules.Install (InstallEvidenceBudget (InstallEvidenceBudget))
import Ecluse.Core.Rules.Types (Rule (DenyByIdentity, DenyIfCve), RuleEvidence (evName, evVersion), ruleName)
import Ecluse.Core.Security (Limits (maxVersionCount), defaultLimits)
import Ecluse.Core.Server.Metadata (selectVersion)
import Ecluse.Core.Telemetry.Record (DredgerMetricsPort (DredgerMetricsPort))
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (CodeArtifactStore (..), codeArtifactFormat, listPageLimit)
import Ecluse.Runtime.Test.PackageStore.FixedPages (FixedPlane (..), FixedRepository (..), NativeCall (..), fixedCache, fixedMirror, fixedVersion, newFixedPlane)
import Ecluse.Test.Corpus.Advisories (SyntheticTarget (..), compileAdvisoryInputs, fillerTargets, suggestedDenyIfCve, syntheticAdvisories)
import Ecluse.Test.Cve (countingCoveredNames)
import Ecluse.Test.EcosystemBench (EcosystemBench (ebEcosystem, ebRead, ebSynthetic, ebSyntheticName))
import Ecluse.Test.OsvDb (withServedArtifact)
import Ecluse.Test.Registry.Metadata.Fetch (captureChunks)
import Ecluse.Test.Rules (atDefaultPrecedence)
import Ecluse.Test.Sweep (RecordedSweep (recErrors, recPorts, recWarnings), deletingReport, recordingPortsUnder, testPacing)

-- | Compile each ecosystem's two advisory catalogues, then hand over one test per row.
withBenchmarks :: [EcosystemBench] -> (TestTree -> IO a) -> IO a
withBenchmarks ecosystems use = do
    getRTSStatsEnabled >>= assertBool "the Dredger rows need +RTS -T"
    output <- rowOutput
    prepared <- traverse (maybe (assertFailure "missing native Dredger measurement provider") pure . scaleOf) ecosystems
    withSystemTempDirectory "ecluse-dredger-scale" $ \directory ->
        withScales directory prepared $ \scales ->
            use (testGroup "dredger scale" (map (scaleRow output) (scaleRows scales)))

-- One ecosystem's inputs: its reader and documents, its adapter, its buckets, and both catalogues.
data Scale = Scale
    { scBench :: EcosystemBench
    , scAdapter :: RegistryAdapter
    , scLeads :: [Char]
    , scFull :: RuleDeps
    , scDefault :: RuleDeps
    }

scaleOf :: EcosystemBench -> Maybe (EcosystemBench, RegistryAdapter, [Char])
scaleOf bench = do
    adapter <- adapterFor eco
    let leads = case eco of
            Npm -> npmNameLeadChars
            PyPI -> pypiNameLeadChars
            Cargo -> cargoNameChars
            RubyGems -> concatMap (T.unpack . renderNamePrefix) (toList (initialBuckets (maintenanceAlphabet (adapterMaintenance adapter))))
    pure (bench, adapter, leads)
  where
    eco = ebEcosystem bench

withScales :: FilePath -> [(EcosystemBench, RegistryAdapter, [Char])] -> ([Scale] -> IO a) -> IO a
withScales _ [] use = use []
withScales directory ((bench, adapter, leads) : rest) use =
    withCatalogue directory eco "full" (map quiet [0 .. 9999] <> fillerTargets 10000) $ \full ->
        withCatalogue directory eco "default" (map quiet coveredIndices) $ \candidates -> do
            checkDocuments bench
            withScales directory rest (use . (Scale bench adapter leads full candidates :))
  where
    eco = ebEcosystem bench
    -- Each range ends before every version the stores hold, so the rules read and keep them all.
    quiet k = SyntheticTarget (scaleName k) ["0.0.1", "0.0.2"] 1

withCatalogue :: FilePath -> Ecosystem -> Text -> [SyntheticTarget] -> (RuleDeps -> IO a) -> IO a
withCatalogue directory eco label targets use = do
    path <- syntheticAdvisories eco targets >>= compileAdvisoryInputs eco (directory </> toString (ecosystemName eco <> "-" <> label))
    withServedArtifact eco path $ \deps db -> do
        covered <- cveCoveredNames (cveDbLookup db)
        sort covered @?= sort (map stPackage targets)
        use deps

-- A held package: its name, how many versions it holds, and whether the cache holds a copy.
data Held = Held
    { heldName :: Text
    , heldVersions :: Int
    , heldCached :: Bool
    }

-- Ten populated buckets. One name in a hundred holds 1,000 versions, nine hold 100, the rest 10.
-- The cache holds the first hundred of every 500 names: a fifth of each stratum and bucket.
scaleInventory :: Int -> [Held]
scaleInventory size = [Held (scaleName k) (versionsOf k) (k `mod` 500 < 100) | k <- [0 .. size - 1]]
  where
    versionsOf k
        | k `mod` 100 == 0 = 1000
        | k `mod` 100 < 10 = 100
        | otherwise = 10

scaleName :: Int -> Text
scaleName k = T.take 1 (T.drop (k `mod` 10) "acfhkmpsux") <> "scale" <> T.justifyRight 5 '0' (show k)

{- The default catalogue's 100 names: ten per populated bucket, 90 small, nine medium and one large,
of which the cache holds 18 small and two medium. -}
coveredIndices :: [Int]
coveredIndices = [1, 2] <> [10 .. 27] <> [100] <> [103 .. 109] <> [128 .. 199]

-- One row: an inventory walked in one shape under one catalogue, for a number of cycles.
data Row = Row
    { rowScale :: Scale
    , rowHeld :: [Held]
    , rowShape :: SweepShape
    , rowAlphabet :: NameAlphabet
    , rowBuckets :: [Text]
    , rowCatalogue :: RuleDeps
    , rowCovered :: Set Text
    , rowCatalogueNames :: Int
    , rowCycles :: Int
    , rowPace :: Pace
    }

-- An unpaced row serves no wait and no fixture delay, so a full walk's native calls count in seconds.
data Pace = Paced | Unpaced
    deriving stock (Eq)

-- Keep the long paced npm walk last so interrupted runs retain the other measurements.
scaleRows :: [Scale] -> [Row]
scaleRows scales =
    map smallControl scales
        <> [walk scale size SweepCandidates | size <- [1000, 10000], scale <- scales]
        <> [(walk scale size SweepEverything){rowPace = Unpaced} | size <- [1000, 10000], scale <- scales]
        <> [walk scale 1000 SweepEverything | scale <- scales]
        <> [walk scale 10000 SweepEverything | scale <- scales, ebEcosystem (scBench scale) == Npm]

walk :: Scale -> Int -> SweepShape -> Row
walk scale size shape =
    Row
        { rowScale = scale
        , rowHeld = scaleInventory size
        , rowShape = shape
        , rowAlphabet = maintenanceAlphabet (adapterMaintenance (scAdapter scale))
        , rowBuckets = map T.singleton (scLeads scale)
        , rowCatalogue = if full then scFull scale else scDefault scale
        , rowCovered = Set.fromList (map scaleName (if full then [0 .. 9999] else coveredIndices))
        , rowCatalogueNames = if full then 20000 else 100
        , rowCycles = if full then 1 else 2
        , rowPace = Paced
        }
  where
    full = shape == SweepEverything

-- The default cycle #1807 slowed: three small covered packages in one bucket.
smallControl :: Scale -> Row
smallControl scale =
    (walk scale 0 SweepCandidates)
        { rowHeld = [Held (scaleName k) 2 False | k <- [10, 11, 12]]
        , rowAlphabet = noNameAlphabet
        , rowBuckets = [""]
        }

rowLabel :: Row -> String
rowLabel row =
    toString $
        ecosystemName (ebEcosystem (scBench (rowScale row)))
            <> ", "
            <> show (length (rowHeld row))
            <> " names, "
            <> shapeName (rowShape row)
            <> " over "
            <> show (length (rowBuckets row))
            <> " buckets, "
            <> show (rowCatalogueNames row)
            <> "-name catalogue"
            <> (if rowPace row == Unpaced then ", unpaced" else "")

shapeName :: SweepShape -> Text
shapeName = \case
    SweepEverything -> "full walk"
    SweepCandidates -> "default cycle"

-- Run each cycle, check it, write its line, and require the same decisions from every cycle.
scaleRow :: FilePath -> Row -> TestTree
scaleRow output row = testCaseInfo (rowLabel row) $ do
    let wanted = expectedOf (fromIntegral listPageLimit) row
    checkDesign row
    reports <- forM [1 .. rowCycles row] $ \cycleNumber -> do
        measured <- runCycle row
        checkCycle row wanted measured
        let (digest, report) = reportOf row cycleNumber measured
        LBS.appendFile output (encode report <> "\n")
        pure (digest, summaryOf digest measured)
    let digests = map fst reports
    assertBool "the cycles decided differently" (and (zipWith (==) digests (drop 1 digests)))
    pure (maybe "" snd (listToMaybe (reverse reports)))

-- What a cycle must show, derived from the inventory and the shipped quota model alone.
data Expected = Expected
    { exNative :: Map Text (Map NativeCall Int)
    , exMetered :: Map RequestKind Int
    , exQuotaSeconds :: Rational
    , exChunkPauses :: Int
    , exExamined :: Int
    , exIdentities :: Identities
    , exCursorWrites :: [Text]
    }

expectedOf :: Int -> Row -> Expected
expectedOf pageSize row =
    Expected
        { exNative = Map.fromList [("mirror", mirrorCalls), ("cache", cacheCalls)]
        , exMetered = Map.filter (> 0) (Map.fromList metered)
        , exQuotaSeconds = sum [fromIntegral count * requestSeconds kind | (kind, count) <- metered]
        , exChunkPauses = max 0 ((length evaluated - 1) `div` 50)
        , exExamined = sum (map heldVersions copies)
        , exIdentities = foldl' (\held (name, version) -> noted name version held) (Identities 0 0) [(heldName h, fixedVersion i) | h <- copies, i <- [0 .. heldVersions h - 1]]
        , exCursorWrites = if full then rowBuckets row else []
        }
  where
    full = rowShape row == SweepEverything
    evaluated = filter (\h -> full || Set.member (heldName h) (rowCovered row)) (rowHeld row)
    cached = filter heldCached
    copies = evaluated <> cached evaluated
    buckets = length (rowBuckets row)
    permissions = [(DescribeRepository, 2), (ListTagsForResource, 1)]
    mirrorCalls = storeCalls (rowHeld row) evaluated (permissions <> if full then [(DescribeRepository, buckets + 2), (ListTagsForResource, 1), (TagResource, buckets), (UntagResource, 1)] else [])
    cacheCalls = storeCalls (cached (rowHeld row)) (cached evaluated) permissions
    storeCalls held enumerated extra =
        Map.fromListWith (+) $
            [(if n == 0 then ListPackagesEmpty else ListPackages, max 1 (pages n)) | bucket <- rowBuckets row, let n = length (filter ((bucket `T.isPrefixOf`) . heldName) held)]
                <> [(ListPackageVersions, max 1 (pages (heldVersions h))) | h <- enumerated]
                <> [(ReadManifest, length enumerated) | not (null enumerated)]
                <> extra
    pages n = (n + pageSize - 1) `div` pageSize
    listings = sum [count | calls <- [mirrorCalls, cacheCalls], kind <- [ListPackages, ListPackagesEmpty], count <- maybeToList (Map.lookup kind calls)]
    metered =
        [ (ListingPage, listings)
        , (VersionPage, length copies)
        , (ManifestRead, length copies)
        , (PermissionRead, 4)
        , (CursorRead, if full then 1 else 0)
        , (CursorWrite, if full then buckets + 1 else 0)
        ]

-- The seconds one request waits under the shipped CodeArtifact quotas at the computed one-quarter share.
requestSeconds :: RequestKind -> Rational
requestSeconds = \case
    ListingPage -> 1 / 50
    VersionPage -> 1 / 50
    ManifestRead -> 1 / 200
    PermissionRead -> 1 / 200
    CursorRead -> 1 / 200
    CursorWrite -> 1 / 25
    DeleteBatch -> 1 / 25

-- The npm native totals the scale design calculated at its page size, which tie this inventory to that design.
checkDesign :: Row -> IO ()
checkDesign row = for_ (lookup key designTotals) $ \total ->
    sum (concatMap Map.elems (Map.elems (exNative (expectedOf designPageSize row)))) @?= total
  where
    key = (ebEcosystem (scBench (rowScale row)), rowShape row == SweepEverything, length (rowHeld row))
    designTotals = [((Npm, True, 1000), 2790), ((Npm, True, 10000), 25462), ((Npm, False, 1000), 391), ((Npm, False, 10000), 491)]

-- Each synthetic document, renamed, holds one entry for every version the fixture lists.
checkDocuments :: EcosystemBench -> IO ()
checkDocuments bench = for_ [2, 10, 100, 1000] $ \count -> do
    let eco = ebEcosystem bench
    manifest <- ebRead bench (mkPackageName eco Nothing (scaleName 0)) (documentOf bench (scaleName 0) count) >>= either (assertFailure . show) pure
    let info = manifestInfo manifest
    Map.size (infoVersions info) @?= count
    assertBool "a listed version has no metadata entry" (all (\i -> isJust (selectVersion (mkVersion eco (fixedVersion i)) info)) [0 .. count - 1])

-- The generator's document for one version count, under another package's name.
documentOf :: EcosystemBench -> Text -> Int -> [ByteString]
documentOf bench name count =
    captureChunks (encodeUtf8 (T.replace originalName name (T.replace filenameName name (decodeUtf8 (ebSynthetic bench count)))))
  where
    originalName = renderPackageName (ebSyntheticName bench)
    filenameName = T.replace "-" "_" originalName

-- What the instrumented catalogue and rules saw during one cycle.
data Probe = Probe
    { prIdentities :: IORef Identities
    , prAcquired :: IORef Int
    , prServing :: IORef Int
    , prCoveredQueries :: IORef Int
    }

-- An order-free checksum of the identities a rule decided: their count and their summed FNV-1a hashes.
data Identities = Identities Int Word64
    deriving stock (Eq, Show)

noted :: Text -> Text -> Identities -> Identities
noted name version (Identities count total) = Identities (count + 1) (total + BS.foldl' step 14695981039346656037 (encodeUtf8 (name <> "@" <> version)))
  where
    step hash byte = (hash `xor` fromIntegral byte) * 1099511628211

-- Count each generation acquisition and covered-name query, keeping the borrow and its lifetime.
probedDeps :: Probe -> Maybe DbEtag -> RuleDeps -> RuleDeps
probedDeps probe serving deps =
    deps
        { rdAdvisoryDatabase = case rdAdvisoryDatabase deps of
            NoAdvisoryDatabase -> NoAdvisoryDatabase
            AdvisoryDatabase borrow -> AdvisoryDatabase $ \use -> borrow $ \generation ->
                use generation{agLookup = \readLookup -> agLookup generation (\held -> acquired held >> readLookup (second (countingCoveredNames (prCoveredQueries probe)) <$> held))}
        }
  where
    acquired held = do
        modifyIORef' (prAcquired probe) (+ 1)
        when (fmap fst held == serving) (modifyIORef' (prServing probe) (+ 1))

-- An identity deny that names nothing the stores hold. 'tracedRules' records each identity it decides.
identityDeny :: Rule
identityDeny = DenyByIdentity "zulu@1.0.0"

configuredRules :: [Rule]
configuredRules = [DenyIfCve suggestedDenyIfCve, identityDeny]

tracedRules :: Probe -> [PreparedRule] -> [PreparedRule]
tracedRules probe = map traced
  where
    traced rule = case prepEval rule of
        PerVersion verdict
            | prepName rule == ruleName identityDeny ->
                rule{prepEval = PerVersion (\ctx evidence -> note evidence >> verdict ctx evidence)}
            | otherwise -> rule
        PerInstallEvidence _ -> rule
        PerPackage _ -> rule
    note evidence = modifyIORef' (prIdentities probe) (noted (renderPackageName (evName evidence)) (renderVersion (evVersion evidence)))

-- Waits a cycle served: how many, the seconds asked for, and the seconds they took.
data Waits = Waits Int Rational Double

recordWait :: Pace -> IORef Waits -> NominalDiffTime -> IO ()
recordWait pace held duration = do
    started <- monotonicNow
    when (pace == Paced) (waitSeconds duration)
    ended <- monotonicNow
    modifyIORef' held (\(Waits count asked served) -> Waits (count + 1) (asked + toRational duration) (served + monoSecondsBetween started ended))

data CycleTiming = CycleTiming
    { ctWall :: Double
    , ctCpu :: Double
    , ctAllocated :: Word64
    , ctLiveBefore :: Word64
    , ctLiveAfter :: Word64
    , ctMajorGcs :: Word32
    , ctMajorLiveMean :: Maybe Word64
    , ctMajorLivePeak :: Maybe Word64
    , ctProcessMaxLive :: Word64
    }

-- What one cycle did, as the fixture, the meter, and the probe recorded it.
data Measured = Measured
    { msOutcome :: CycleOutcome
    , msMetered :: Map RequestKind Int
    , msNative :: Map Text (Map NativeCall Int)
    , msFixtureSeconds :: Double
    , msQuota :: Waits
    , msChunks :: Waits
    , msIdentities :: Identities
    , msGenerations :: (Int, Int)
    , msCoveredQueries :: Int
    , msCursorWrites :: [Text]
    , msCursorLeft :: Maybe Text
    , msAudit :: ([Text], [Text])
    , msTiming :: CycleTiming
    }

runCycle :: Row -> IO Measured
runCycle row = do
    probe <- Probe <$> newIORef (Identities 0 0) <*> newIORef 0 <*> newIORef 0 <*> newIORef 0
    quota <- newIORef (Waits 0 0 0)
    chunks <- newIORef (Waits 0 0 0)
    closed <- newIORef Nothing
    serving <- rdCurrentAdvisoryEtag (rowCatalogue row)
    let deps = probedDeps probe serving (rowCatalogue row)
    store <- maybe (assertFailure "the ecosystem has no CodeArtifact format") pure (scaleStore (ebEcosystem (scBench (rowScale row))))
    mirror <- newFixedPlane (repositoryOf (rowPace row) store "mirror" (rowHeld row))
    cache <- newFixedPlane (repositoryOf (rowPace row) store "cache" (filter heldCached (rowHeld row)))
    rules <- tracedRules probe <$> prepare deps (map atDefaultPrecedence configuredRules)
    recorded <- recordingPortsUnder deletingReport serving
    (budget, gateFor) <- newBudgetMeter (recordWait (rowPace row) quota)
    let ports =
            (recPorts recorded)
                { sweepBudget = budget{budgetClose = budgetClose budget >>= \cost -> cost <$ writeIORef closed (Just cost)}
                , sweepDelay = recordWait (rowPace row) chunks
                , sweepMetrics = DredgerMetricsPort (\_ _ -> pass)
                }
        mount = (scaleMount row gateFor mirror cache){smRuleDeps = deps, smRules = rules}
    paceAtCeiling (pacingOf row) ports [mount]
    (outcome, timing) <- timeCycle (sweepCycle (pacingOf row) ports [mount])
    cost <- readIORef closed >>= maybe (assertFailure "the cycle did not close its budget") pure
    mirrorCalls <- fpCalls mirror
    cacheCalls <- fpCalls cache
    delays <- (+) <$> fpDelayed mirror <*> fpDelayed cache
    Measured outcome (Map.fromListWith (+) [pair | tally <- Map.elems (ccRequests cost), pair <- tallyCounts tally]) (Map.fromList [("mirror", mirrorCalls), ("cache", cacheCalls)]) delays
        <$> readIORef quota
        <*> readIORef chunks
        <*> readIORef (prIdentities probe)
        <*> ((,) <$> readIORef (prAcquired probe) <*> readIORef (prServing probe))
        <*> readIORef (prCoveredQueries probe)
        <*> fpCursorWrites mirror
        <*> fpCursor mirror
        <*> ((,) <$> recErrors recorded <*> recWarnings recorded)
        <*> pure timing

-- The mount a deleting run builds: each store metered against its pool and labelled by its role.
scaleMount :: Row -> (QuotaScope -> RequestGate) -> FixedPlane -> FixedPlane -> SweepMount
scaleMount row gateFor mirror cache =
    SweepMount
        { smEcosystem = ebEcosystem bench
        , smStore = pairedStore limit (deletingCache (metered "mirror" fixedMirror mirror)) (deletingCache (metered "cache" fixedCache cache))
        , smInstallEvidence = Nothing
        , smInstallBudget = InstallEvidenceBudget (\action -> Just <$> action (const pass) (const pass))
        , smInstallLimits = defaultLimits
        , smRules = []
        , smConfigured = configuredRules
        , smRuleDeps = rowCatalogue row
        , smProjectName = adapterProjectName (scAdapter (rowScale row))
        , smFirstParty = const False
        }
  where
    bench = scBench (rowScale row)
    limit = maxVersionCount defaultLimits
    metered label build plane =
        let built = build limit (rowAlphabet row) (manifestsOf bench plane) plane
            facts = storeFacts built
         in (meteredMaintenance (gateFor (bgScope (factBudget facts))) built){storeFacts = facts{factBackend = label}}

-- The shipped chunk size and pause, and the allowance three cycle pauses give a cycle.
pacingOf :: Row -> SweepPacing
pacingOf row = testPacing{swpChunkSize = 50, swpChunkPause = 2, swpCyclePause = 3600, swpCycleWindow = defaultCycleWindow 3600, swpShape = rowShape row}

-- Both repositories sit in one account and Region, so they share one quota pool.
scaleStore :: Ecosystem -> Maybe CodeArtifactStore
scaleStore eco = store <$> codeArtifactFormat eco
  where
    store format = CodeArtifactStore{casDomain = "scale", casDomainOwner = "123456789012", casRegion = "us-east-1", casRepository = "", casFormat = format}

repositoryOf :: Pace -> CodeArtifactStore -> Text -> [Held] -> FixedRepository
repositoryOf pace store name held =
    FixedRepository
        { frStore = store{casRepository = name}
        , frPackages = Map.fromList [(heldName h, heldVersions h) | h <- held]
        , frDefaultPageSize = designPageSize
        , frDelay = if pace == Paced then 0.02 else 0
        }

-- The page size the scale design declared as a fixture input, not an AWS default.
designPageSize :: Int
designPageSize = 100

-- The production full read of each package's synthetic document.
manifestsOf :: EcosystemBench -> FixedPlane -> StoreManifestRead
manifestsOf bench plane name =
    first storeFaultOfMetadata <$> ebRead bench name (documentOf bench key (Map.findWithDefault 0 key (frPackages (fpRepository plane))))
  where
    key = renderPackageName name

{- Setup and collection stay outside the interval. Only a major GC measures the live heap, so the
cycle reports the exact mean over its own major GCs and the largest a sampler saw, if any ran. -}
timeCycle :: IO a -> IO (a, CycleTiming)
timeCycle action = do
    performGC
    before <- getRTSStats
    peak <- newIORef Nothing
    cpuBefore <- getCPUTime
    started <- monotonicNow
    result <- withAsync (sampleMajorLive (major_gcs before) peak) (const action)
    ended <- monotonicNow
    cpuAfter <- getCPUTime
    atEnd <- getRTSStats
    performGC
    after <- getRTSStats
    sampled <- readIORef peak
    let majors = major_gcs atEnd - major_gcs before
    pure
        ( result
        , CycleTiming
            { ctWall = monoSecondsBetween started ended
            , ctCpu = fromIntegral (cpuAfter - cpuBefore) / 1e12
            , ctAllocated = allocated_bytes after - allocated_bytes before
            , ctLiveBefore = gcdetails_live_bytes (gc before)
            , ctLiveAfter = gcdetails_live_bytes (gc after)
            , ctMajorGcs = majors
            , ctMajorLiveMean = (cumulative_live_bytes atEnd - cumulative_live_bytes before) `div` fromIntegral majors <$ guard (majors > 0)
            , ctMajorLivePeak = sampled
            , ctProcessMaxLive = max_live_bytes after
            }
        )

sampleMajorLive :: Word32 -> IORef (Maybe Word64) -> IO ()
sampleMajorLive earlier peak = forever $ do
    stats <- getRTSStats
    when (major_gcs stats > earlier && gcdetails_gen (gc stats) > 0) $
        modifyIORef' peak (Just . maybe (gcdetails_live_bytes (gc stats)) (max (gcdetails_live_bytes (gc stats))))
    waitSeconds 0.01

checkCycle :: Row -> Expected -> Measured -> IO ()
checkCycle row wanted measured = do
    assertBool "the cycle halted or missed evidence" (outcomeComplete (msOutcome measured))
    outcomeTally (msOutcome measured) @?= SweepTally (exExamined wanted) 0 (exExamined wanted) 0
    msIdentities measured @?= exIdentities wanted
    msNative measured @?= exNative wanted
    msMetered measured @?= exMetered wanted
    waitsAsked (msQuota measured) @?= (sum (exMetered wanted), exQuotaSeconds wanted)
    waitsAsked (msChunks measured) @?= (exChunkPauses wanted, 2 * fromIntegral (exChunkPauses wanted))
    msCursorWrites measured @?= exCursorWrites wanted
    msCursorLeft measured @?= Nothing
    msAudit measured @?= ([], [])
    let (acquired, serving) = msGenerations measured
    assertBool "a read borrowed a generation other than the served one" (acquired > 0 && acquired == serving)
    assertBool "more covered-name queries than buckets" (msCoveredQueries measured <= length (rowBuckets row))

waitsAsked :: Waits -> (Int, Rational)
waitsAsked (Waits count asked _) = (count, asked)

waitsServed :: Waits -> Double
waitsServed (Waits _ _ served) = served

-- The decision digest covers what must not change between cycles or builds, and no time.
reportOf :: Row -> Int -> Measured -> (Text, Value)
reportOf row cycleNumber measured =
    ( digest
    , object
        [ "ecosystem" .= ecosystemName (ebEcosystem (scBench (rowScale row)))
        , "names" .= length (rowHeld row)
        , "shape" .= shapeName (rowShape row)
        , "buckets" .= length (rowBuckets row)
        , "catalogue_names" .= rowCatalogueNames row
        , "paced" .= (rowPace row == Paced)
        , "cycle" .= cycleNumber
        , "wall_seconds" .= ctWall timing
        , "cpu_seconds" .= ctCpu timing
        , "active_seconds" .= (ctWall timing - waitsServed (msQuota measured) - waitsServed (msChunks measured) - msFixtureSeconds measured)
        , "quota_wait_seconds" .= waitsServed (msQuota measured)
        , "chunk_wait_seconds" .= waitsServed (msChunks measured)
        , "fixture_delay_seconds" .= msFixtureSeconds measured
        , "allocated_bytes" .= ctAllocated timing
        , "live_bytes_before" .= ctLiveBefore timing
        , "live_bytes_after" .= ctLiveAfter timing
        , "major_gcs" .= ctMajorGcs timing
        , "major_gc_live_bytes_mean" .= ctMajorLiveMean timing
        , "major_gc_live_bytes_peak_sampled" .= ctMajorLivePeak timing
        , "process_max_live_bytes" .= ctProcessMaxLive timing
        , "versions_per_hour" .= (fromIntegral examined * 3600 / ctWall timing :: Double)
        , "native_requests_per_version" .= Map.fromList [(show kind :: Text, fromIntegral count / fromIntegral (max 1 examined) :: Double) | (kind, count) <- Map.toList nativeTotals]
        , "native_requests_total" .= sum nativeTotals
        , "metered_requests_total" .= sum (msMetered measured)
        , "covered_name_queries" .= msCoveredQueries measured
        , "decisions" .= decisions
        , "decision_digest" .= digest
        ]
    )
  where
    timing = msTiming measured
    tally = outcomeTally (msOutcome measured)
    examined = tallyExamined tally
    nativeTotals = nativeTotalsOf measured
    Identities identities checksum = msIdentities measured
    decisions =
        object
            [ "examined_versions" .= examined
            , "kept_versions" .= tallyKept tally
            , "deleted_versions" .= tallyDeleted tally
            , "guard_skipped_versions" .= tallyGuardSkipped tally
            , "decided_identities" .= object ["count" .= identities, "checksum" .= checksum]
            , "served_generation_only" .= uncurry (==) (msGenerations measured)
            , "cursor_writes" .= msCursorWrites measured
            , "native_requests" .= Map.map (Map.mapKeys (show :: NativeCall -> Text)) (msNative measured)
            , "metered_requests" .= Map.mapKeys (show :: RequestKind -> Text) (msMetered measured)
            , "quota_waits" .= object ["count" .= fst (waitsAsked (msQuota measured)), "requested_seconds" .= (fromRational (snd (waitsAsked (msQuota measured))) :: Double)]
            , "chunk_pauses" .= fst (waitsAsked (msChunks measured))
            ]
    digest = sha256Hex (LBS.toStrict (encode decisions))

summaryOf :: Text -> Measured -> String
summaryOf digest measured =
    toString $
        "wall "
            <> tenths (ctWall (msTiming measured))
            <> " s, "
            <> show (tallyExamined (outcomeTally (msOutcome measured)))
            <> " versions, "
            <> show (sum native)
            <> " native requests ("
            <> T.intercalate ", " [show call <> " " <> show (Map.findWithDefault 0 call native) | call <- walkCalls]
            <> "), decisions "
            <> T.take 12 digest
  where
    native = nativeTotalsOf measured
    walkCalls = [ListPackages, ListPackagesEmpty, ListPackageVersions, DescribeRepository, TagResource]
    tenths seconds = show (fromIntegral (round (seconds * 10) :: Int) / 10 :: Double)

nativeTotalsOf :: Measured -> Map NativeCall Int
nativeTotalsOf = Map.unionsWith (+) . Map.elems . msNative

-- The rows' lines go beside the run's other benchmark files, which CI uploads.
rowOutput :: IO FilePath
rowOutput = do
    directory <- fromMaybe ("scratchpad" </> "performance") <$> lookupEnv "PERF_ARTIFACT_DIR"
    createDirectoryIfMissing True directory
    let path = directory </> "dredger-scale.jsonl"
    LBS.writeFile path ""
    pure path
