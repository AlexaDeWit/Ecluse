-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Complete preview cycles over real SQLite advisories and deterministic store inventories.
module Ecluse.Core.Registry.SweepBench (measure) where

import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import GHC.Stats (RTSStats (allocated_bytes, max_live_bytes), getRTSStats, getRTSStatsEnabled)
import System.CPUTime (getCPUTime)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performGC)
import Test.Tasty.HUnit (assertBool, (@?=))

import Ecluse.Core.Clock (monoSecondsBetween, monotonicNow, waitSeconds)
import Ecluse.Core.Cve (CveDb (cveDbLookup), CveLookup (cveCoveredNames))
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry.Maintenance (
    StoreCursor (writeCursor),
    StoreFacts (factBackend, factBudget, factNameAlphabet),
    StoreMaintenance (storeCursor),
    StoreObservation (..),
    meteredObservation,
 )
import Ecluse.Core.Registry.Maintenance.Budget (
    BudgetPort (budgetClose),
    CycleCost (..),
    QuotaDimension (StoreRequests),
    QuotaOrigin (QuotaDeclared),
    RequestKind (ListingPage, ManifestRead, PermissionRead, VersionPage),
    StoreBudget (..),
    mkQuotaScope,
    newBudgetMeter,
    requestKinds,
    tallyCounts,
 )
import Ecluse.Core.Registry.Maintenance.NameSpace (NamePrefix, mkNameAlphabet, parseNamePrefix, renderNamePrefix)
import Ecluse.Core.Registry.Npm.Project (npmNameLeadChars)
import Ecluse.Core.Registry.Sweep (paceAtCeiling, sweepCycle)
import Ecluse.Core.Registry.Sweep.Outcome (CycleOutcome (..), SweepTally (..), outcomeComplete, prerequisitesMet)
import Ecluse.Core.Registry.Sweep.Types (
    SweepMount (smRuleDeps),
    SweepPacing (..),
    SweepPorts (sweepAdvisoryEtag, sweepBudget, sweepDelay),
    SweepShape (SweepCandidates, SweepEverything),
    previewCache,
 )
import Ecluse.Core.Rules (
    AdvisoryDatabase (AdvisoryDatabase, NoAdvisoryDatabase),
    AdvisoryGeneration (agLookup),
    RuleDeps (rdAdvisoryDatabase, rdCurrentAdvisoryEtag),
    prepare,
 )
import Ecluse.Core.Rules.Types (DenyIfCveParams (DenyIfCveParams), FailureAlignment (FailDeny), Rule (DenyByIdentity, DenyIfCve))
import Ecluse.Core.Version (Version)
import Ecluse.Test.Corpus.Advisories (SyntheticTarget (..), compileAdvisoryInputs, fillerTargets, syntheticAdvisories)
import Ecluse.Test.Maintenance (FakeStore (..), FakeStoreConfig (..), newFakeStore, seededStoreConfig)
import Ecluse.Test.OsvDb (withServedArtifact)
import Ecluse.Test.Package (npmVersion, unscopedNpm)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Sweep (RecordedSweep (..), previewMount, previewingReport, recordingPortsUnder, testPacing, withPrivateCache)

-- | Compile outside measurement, then emit one JSON object for each qualified complete cycle.
measure :: Int -> IO ()
measure samples = do
    enabled <- getRTSStatsEnabled
    assertBool "RTS statistics require +RTS -T" enabled
    withSystemTempDirectory "ecluse-dredger-bench" $ \directory -> do
        for_ [100, 20000] $ \size -> do
            let targets = SyntheticTarget "left-pad" ["1.0.0", "2.0.0"] 1 : fillerTargets (size - 1)
            database <- syntheticAdvisories Npm targets >>= compileAdvisoryInputs Npm (directory </> show size)
            withServedArtifact Npm database $ \deps db -> do
                names <- cveCoveredNames (cveDbLookup db)
                sort names @?= sort (map stPackage targets)
                runCases samples size deps
        runCases samples 0 inertRuleDeps

runCases :: Int -> Int -> RuleDeps -> IO ()
runCases samples catalogue deps =
    for_ [("one", ""), ("alphabet", npmNameLeadChars)] $ \(label, alphabet) ->
        for_ [SweepEverything, SweepCandidates] $ \shape -> do
            let scenario = Scenario catalogue label alphabet shape
            mirror <- makeStore scenario "mirror" inventory
            cache <- makeStore scenario "cache" []
            marker <- maybe (fail "invalid fixture cursor") pure (parseNamePrefix (mkNameAlphabet alphabet) (if null alphabet then "" else "l"))
            for_ [mirror, cache] $ \store ->
                traverse_ (\cursor -> writeCursor cursor marker >>= (@?= Right ())) (storeCursor (fakeMaintenance store))
            -- A complete untimed cycle checks the oracle before any sample is reported.
            void (runSample scenario deps mirror cache marker 0)
            for_ [1 .. samples] (runSample scenario deps mirror cache marker >=> LBS.putStrLn . encode)

data Scenario = Scenario
    { scCatalogue :: Int
    , scBuckets :: Text
    , scAlphabet :: String
    , scShape :: SweepShape
    }

data CycleTiming = CycleTiming
    { ctWall :: Double
    , ctCpu :: Double
    , ctAllocated :: Word64
    , ctProcessPeak :: Word64
    }

inventory :: [(PackageName, [Version])]
inventory = [(unscopedNpm name, map npmVersion ["1.0.0", "2.0.0"]) | name <- ["alpine", "left-pad", "zulu"]]

makeStore :: Scenario -> Text -> [(PackageName, [Version])] -> IO FakeStore
makeStore scenario label held =
    newFakeStore config{fakeFacts = (fakeFacts config){factBackend = label, factNameAlphabet = mkNameAlphabet (scAlphabet scenario), factBudget = declaredBudget}}
  where
    config = seededStoreConfig held

declaredBudget :: StoreBudget
declaredBudget =
    StoreBudget
        { bgScope = mkQuotaScope "dredger-preview"
        , bgQuotas = Map.singleton StoreRequests 1000
        , bgOrigin = QuotaDeclared
        , bgCosts = Map.fromList [(kind, Map.singleton StoreRequests 1) | kind <- requestKinds]
        }

pacingFor :: Scenario -> SweepPacing
pacingFor scenario = testPacing{swpShape = scShape scenario, swpChunkSize = 2, swpChunkPause = 2, swpBudgetFraction = Just (1 / 2)}

-- Setup, GC and assertions stay outside the interval around the actual cycle.
runSample :: Scenario -> RuleDeps -> FakeStore -> FakeStore -> NamePrefix -> Int -> IO Value
runSample scenario source mirror cache marker sample = do
    namesRead <- newIORef (0 :: Int)
    calls <- newIORef []
    quotaWaits <- newIORef []
    chunkWaits <- newIORef []
    closed <- newIORef Nothing
    let deps = countCoveredNames namesRead source
        configured = [DenyByIdentity "zulu@1.0.0"] <> [DenyIfCve (DenyIfCveParams 8 FailDeny) | scCatalogue scenario > 0]
    rules <- prepare deps (map atDefaultPrecedence configured)
    generation <- rdCurrentAdvisoryEtag deps
    recorded <- recordingPortsUnder previewingReport generation
    (budget, gateFor) <- newBudgetMeter (recordWait quotaWaits)
    let observed label store = meteredObservation (gateFor (bgScope declaredBudget)) (recordObservation calls label (fakeObservation store))
        mount =
            (withPrivateCache (previewCache (observed "cache" cache)) (previewMount (observed "mirror" mirror) rules configured))
                { smRuleDeps = deps
                }
        ports =
            (recPorts recorded)
                { sweepBudget = budget{budgetClose = budgetClose budget >>= \cost -> cost <$ writeIORef closed (Just cost)}
                , sweepDelay = recordWait chunkWaits
                , sweepAdvisoryEtag = const (rdCurrentAdvisoryEtag deps)
                }
        pacing = pacingFor scenario
    paceAtCeiling pacing ports [mount]
    (outcome, timing) <- timeCycle (sweepCycle pacing ports [mount])
    cost <- readIORef closed >>= maybe (fail "cycle did not close its budget") pure
    requests <- reverse <$> readIORef calls
    covered <- readIORef namesRead
    quota <- readIORef quotaWaits
    chunks <- readIORef chunkWaits
    qualify scenario mirror cache marker outcome requests covered cost
    selections <- recInfo recorded >>= qualifySelections scenario generation
    map fst quota @?= replicate (sum [count | tally <- Map.elems (ccRequests cost), (_, count) <- tallyCounts tally]) 0.002
    map fst chunks @?= [2 | scShape scenario == SweepEverything]
    recErrors recorded >>= (@?= [])
    pure (reportSample scenario sample outcome requests covered cost quota chunks timing selections)

timeCycle :: IO CycleOutcome -> IO (CycleOutcome, CycleTiming)
timeCycle action = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    started <- monotonicNow
    outcome <- action
    ended <- monotonicNow
    afterCpu <- getCPUTime
    performGC
    afterStats <- getRTSStats
    pure
        ( outcome
        , CycleTiming
            { ctWall = monoSecondsBetween started ended
            , ctCpu = fromIntegral (afterCpu - beforeCpu) / 1e12
            , ctAllocated = allocated_bytes afterStats - allocated_bytes beforeStats
            , ctProcessPeak = max_live_bytes afterStats
            }
        )

-- Count the real query while preserving generation acquisition and lookup lifetime.
countCoveredNames :: IORef Int -> RuleDeps -> RuleDeps
countCoveredNames counter deps =
    deps
        { rdAdvisoryDatabase = case rdAdvisoryDatabase deps of
            NoAdvisoryDatabase -> NoAdvisoryDatabase
            AdvisoryDatabase borrow -> AdvisoryDatabase $ \use -> borrow $ \generation ->
                use generation{agLookup = \readLookup -> agLookup generation (readLookup . fmap (second countLookup))}
        }
  where
    countLookup cve = cve{cveCoveredNames = modifyIORef' counter (+ 1) >> cveCoveredNames cve}

recordWait :: IORef [(Double, Double)] -> NominalDiffTime -> IO ()
recordWait recorded duration = do
    started <- monotonicNow
    waitSeconds duration
    ended <- monotonicNow
    modifyIORef' recorded ((realToFrac duration, monoSecondsBetween started ended) :)

recordObservation :: IORef [Text] -> Text -> StoreObservation -> StoreObservation
recordObservation calls label observed =
    observed
        { obListPackagesIn = \prefix -> lift (record ("list:" <> renderNamePrefix prefix)) >> obListPackagesIn observed prefix
        , obEnumerateVersions = \name -> record ("versions:" <> renderPackageName name) >> obEnumerateVersions observed name
        , obReadManifest = \name -> record ("manifest:" <> renderPackageName name) >> obReadManifest observed name
        , obVerifyConsent = record "consent" >> obVerifyConsent observed
        , obClassifyStore = record "classify" >> obClassifyStore observed
        }
  where
    record operation = modifyIORef' calls ((label <> ":" <> operation) :)

-- The oracle spells selected names independently of the sweep's candidate helper.
qualify :: Scenario -> FakeStore -> FakeStore -> NamePrefix -> CycleOutcome -> [Text] -> Int -> CycleCost -> IO ()
qualify scenario mirror cache marker outcome calls covered cost = do
    assertBool "incomplete preview cycle" (outcomeComplete outcome)
    assertBool "unmet preview prerequisite" (all prerequisitesMet (outcomePrerequisites outcome))
    outcomeTally outcome @?= SweepTally (2 * length selected) removed (2 * length selected - removed) 0
    calls @?= expectedCalls scenario selected
    readFakeContents mirror >>= (@?= fakeContents (seededStoreConfig inventory))
    readFakeContents cache >>= (@?= Map.empty)
    for_ [mirror, cache] (readFakeCursor >=> (@?= Just marker))
    assertBool "unexpected covered-name query count" (covered `elem` permittedCounts)
    fmap tallyCounts (ccRequests cost)
        @?= Map.singleton
            (bgScope declaredBudget)
            ( sort
                [(ListingPage, if null (scAlphabet scenario) then 2 else 3), (PermissionRead, 4), (VersionPage, length selected), (ManifestRead, length selected)]
            )
  where
    selected = selectedNames scenario
    removed = if scCatalogue scenario == 0 then 1 else 2
    bucketCount = max 1 (length (scAlphabet scenario))
    permittedCounts
        | scCatalogue scenario == 0 = [0]
        | scShape scenario == SweepEverything = [0, bucketCount]
        | otherwise = [bucketCount]

selectedNames :: Scenario -> [Text]
selectedNames scenario
    | scShape scenario == SweepEverything = ["alpine", "left-pad", "zulu"]
    | scCatalogue scenario == 0 = ["zulu"]
    | otherwise = ["left-pad", "zulu"]

-- Exact audit lines pin identities, named denials and the generation before any path is omitted.
qualifySelections :: Scenario -> Maybe DbEtag -> [Text] -> IO Value
qualifySelections scenario generation messages = do
    isJust generation @?= (scCatalogue scenario > 0)
    proposed @?= expectedProposed
    retained @?= expectedRetained
    pure
        ( object
            [ "proposed_identities" .= map identity proposed
            , "retained_identities" .= retained
            , "denial_evidence" .= map (fst . T.breakOn "; advisory generation ") proposed
            , "serving_generation_identities" .= [identity line | line <- proposed, not ("; advisory generation none" `T.isSuffixOf` line)]
            ]
        )
  where
    proposed = mapMaybe (T.stripPrefix "dry run, would delete ") messages
    retained = mapMaybe (T.stripPrefix "dry run, keeping ") messages
    identity = fst . T.breakOn ": blocked by "
    serving = maybe "none" (\(DbEtag etag) -> etag) generation
    expectedProposed =
        [ "left-pad@1.0.0: blocked by DenyIfCve (affected by ECLUSE-BENCH-0 (CVSS >= 8.0)); advisory generation " <> serving
        | scCatalogue scenario > 0
        ]
            <> ["zulu@1.0.0: blocked by DenyByIdentity (identity zulu@1.0.0 is revoked by operator); advisory generation none"]
    expectedRetained =
        ["alpine@" <> version | scShape scenario == SweepEverything, version <- ["1.0.0", "2.0.0"]]
            <> ["left-pad@1.0.0" | scCatalogue scenario == 0 && scShape scenario == SweepEverything]
            <> ["left-pad@2.0.0" | scCatalogue scenario > 0 || scShape scenario == SweepEverything]
            <> ["zulu@2.0.0"]

expectedCalls :: Scenario -> [Text] -> [Text]
expectedCalls scenario selected =
    ["mirror:consent", "mirror:classify", "cache:consent", "cache:classify"]
        <> concatMap bucketCalls buckets
  where
    buckets = if null (scAlphabet scenario) then [""] else map T.singleton (scAlphabet scenario)
    bucketCalls prefix =
        ["mirror:list:" <> prefix, "cache:list:" <> prefix]
            <> concat [["mirror:versions:" <> name, "mirror:manifest:" <> name] | name <- selected, prefix == "" || prefix == T.take 1 name]

reportSample :: Scenario -> Int -> CycleOutcome -> [Text] -> Int -> CycleCost -> [(Double, Double)] -> [(Double, Double)] -> CycleTiming -> Value -> Value
reportSample scenario sample outcome calls covered cost quota chunks timing selections =
    object
        [ "sample" .= sample
        , "catalogue_names" .= scCatalogue scenario
        , "buckets" .= scBuckets scenario
        , "mode" .= (if scShape scenario == SweepEverything then "full" else "default" :: Text)
        , "preview" .= True
        , "wall_seconds" .= ctWall timing
        , "cpu_seconds" .= ctCpu timing
        , "meter_work_seconds" .= (fromRational (ccWorkSeconds cost) :: Double)
        , "active_seconds" .= max 0 (ctWall timing - sum (map snd quota) - sum (map snd chunks))
        , "quota_waits" .= reverse quota
        , "chunk_waits" .= reverse chunks
        , "allocated_bytes_with_instrumentation" .= ctAllocated timing
        , "process_peak_live_bytes_including_setup" .= ctProcessPeak timing
        , "covered_name_calls" .= covered
        , "examined_versions" .= tallyExamined tally
        , "would_delete_versions" .= tallyDeleted tally
        , "kept_versions" .= tallyKept tally
        , "selections" .= selections
        , "store_calls" .= calls
        , "request_counts" .= [(show kind :: Text, count) | tally <- Map.elems (ccRequests cost), (kind, count) <- tallyCounts tally]
        , "configured_requests_per_second" .= (1000 :: Int)
        , "configured_budget_fraction" .= (0.5 :: Double)
        , "chunk_size" .= swpChunkSize (pacingFor scenario)
        , "chunk_pause_seconds" .= (realToFrac (swpChunkPause (pacingFor scenario)) :: Double)
        ]
  where
    tally = outcomeTally outcome
