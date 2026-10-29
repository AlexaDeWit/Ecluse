-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Render the child reports into the Markdown the run summary and the uploaded artifact carry.
Successes lead every table, each beside the floor it is held to. Memory, collector, admission, and
cache figures describe the proxy process, and the verdict section lists every broken invariant.
-}
module Ecluse.BenchLoad.Report (
    Section (..),
    renderReports,
    renderServiceTime,
    renderLoadSaturation,
    renderThrash,
    renderFloors,
    renderVerdict,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Numeric (showFFloat)

import Ecluse.BenchLoad.BootLines (BootLimits (..), LoggedRule (..), bootLimits, loggedRules, metadataCompression, renderMetadataCompression, ruleBootOrders)
import Ecluse.BenchLoad.CpuUse (
    CpuWindow (..),
    cpuBusyShare,
    foreignCpuSeconds,
    generatorSaturated,
    harnessCpuRate,
    harnessCpuShare,
    headroomLimit,
    ohaCpu,
    proxyCpuRate,
    proxyCpuShare,
 )
import Ecluse.BenchLoad.Exposition (CacheOutcomes (..), GaugeSummary (..))
import Ecluse.BenchLoad.Floors (Calibration (calConditions), Enforcement, FloorCheck (..), Pass (ConcurrencyOne, Loaded), PassSelection (LoadedPassOnly), describeEnforcement, describeRetainedConditions, floorCheck, floorsPath)
import Ecluse.BenchLoad.Harness (LoadKnobs (..), LoadSummary (..), ProxyFigures (..), ScenarioReport (..), hasDiagnosticBurst, windowAttempts, windowRefusals, windowSuccesses)
import Ecluse.BenchLoad.Latency (Percentiles (..))
import Ecluse.BenchLoad.Normalise (
    BaselineSource,
    NormalisedRow (NormalisedRow),
    SaturationInput (SaturationInput),
    deriveSaturation,
    queuingDominanceThreshold,
    renderNormalised,
    renderSaturation,
 )
import Ecluse.BenchLoad.PatternReport (renderReplayTotals)
import Ecluse.BenchLoad.Pod (BurstMemory (..), CgroupReading (..), PodShape (Unlimited), ProxyTuning (..), counter, noTuning, proxyCgroupFiles, renderCpuList, renderPodShape)
import Ecluse.BenchLoad.ReportLabels (admissionRow, generatorHeadroomRow, headroomHeld, headroomSaturated, loadCell, loadRow, memoryEventsRow, memoryPeakColumn, postureRow)
import Ecluse.BenchLoad.RtsWindow (RtsSnapshot (..), RtsWindow (..), compactionThresholdCrossed, gcCpuShare, meanLiveAtMajors, perSuccess)
import Ecluse.BenchLoad.Selection (measurementConditionVersion)
import Ecluse.BenchLoad.Verdict (ProxyEnding (..), describeEnding)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)

-- | What one ecosystem's section of the report is rendered from.
data Section = Section
    { sectionKnobs :: LoadKnobs
    -- ^ The knobs of the loaded pass, with the latency its fixture injects.
    , sectionCapabilities :: Int
    , sectionProcessors :: Int
    , sectionShape :: PodShape
    , sectionEcosystem :: Ecosystem
    , sectionEnforcement :: Enforcement
    , sectionConcurrencyOne :: [ScenarioReport]
    , sectionLoaded :: [ScenarioReport]
    }

-- | Render loaded outcomes and costs beside the corresponding concurrency-one measurements.
renderReports :: Section -> Text
renderReports section =
    T.unlines $
        [ "## Load test: throughput and latency over " <> ecosystemName (sectionEcosystem section)
        , ""
        , "_Successful responses per window are the primary figure. Reading notes are at the end of the report._"
        , ""
        ]
            <> operatingPointTable (sectionKnobs section) (sectionCapabilities section) (sectionProcessors section) (sectionShape section) (fromMaybe [] firstBoot)
            <> runtimeLines
            <> rulePolicyLines c1Reports reports
            <> [ "### At a glance"
               , ""
               , "| scenario | description | connections | successes | floor | refusals | transport failures | successful req/s | success p50 | success p99 | alloc / success (upper bound) | GC share | " <> memoryPeakColumn <> " | ending |"
               , "| --- | --- | --: | --: | --: | --: | --: | --: | --: | --: | --: | --: | --: | --- |"
               ]
            <> map (glanceRow (floorOf Loaded)) reports
            <> [""]
            <> costTable (floorOf ConcurrencyOne) c1Reports reports
            <> concatMap (renderScenario (lkProxyTuning (sectionKnobs section))) reports
            <> readingNotes
  where
    reports = sectionLoaded section
    c1Reports = sectionConcurrencyOne section
    floorOf whichPass name = floorCheck (sectionEnforcement section) (name, whichPass)
    firstBoot = listToMaybe [pfBootLines p | Just p <- map srProxy reports, not (null (pfBootLines p))]
    runtimeLines = case firstBoot of
        Nothing -> []
        Just lines' ->
            ["**Runtime posture and memory plan, as the first scenario's proxy logged them**", ""]
                <> map ("- " <>) lines'
                <> [""]

operatingPointTable :: LoadKnobs -> Int -> Int -> PodShape -> [Text] -> [Text]
operatingPointTable knobs capabilities processors shape boot =
    [ "**Operating point**"
    , ""
    , "| knob | value |"
    , "| --- | --- |"
    , row "pod shape" (shapeNote shape capabilities (lkProxyTuning knobs))
    , row "runner logical processors" (show processors <> " (oha is pinned to logical CPU 0 when isolated)")
    , row "harness GHC capabilities (-N)" (show capabilities)
    , row loadRow (loadCell (lkConcurrency knobs) (lkDurationSeconds knobs))
    , row "measurement conditions" measurementConditionVersion
    , row "injected upstream latency" (fmt1 (fromIntegral (lkUpstreamLatencyMicros knobs) / 1_000) <> " ms")
    , row "serve admission (requests)" (maybe "n/a" show (blCpuAdmission limits) <> origin (lkServeMaxInFlight knobs))
    , row "memory admission budget at boot" (maybe "n/a" bytesCell (blMemoryBudgetBytes limits))
    , row "native metadata gzip" (renderMetadataCompression (metadataCompression boot))
    , row "private pool" (maybe "computed by the proxy from its fd limit" (\n -> show n <> " (explicit)") (lkPrivateConnectionsPerHost knobs))
    , row "public pool" (maybe "computed by the proxy from its fd limit" (\n -> show n <> " (explicit)") (lkPublicConnectionsPerHost knobs))
    , row "cache-eviction entries" (show (lkCacheMaxEntries knobs))
    , row "working-set cap" (show (lkWorkingSet knobs) <> " projects")
    , row "worker artifact" ("~" <> kib (fromIntegral (lkPayloadBytes knobs)))
    ]
        <> tuningRows knobs
        <> [""]
  where
    limits = bootLimits boot
    origin = maybe " (the proxy's computed default)" (const " (explicit)")

-- The rows a run adds when it pins proxy settings or runs the loaded pass alone.
tuningRows :: LoadKnobs -> [Text]
tuningRows knobs =
    [row "proxy CPU placement (cpuset.cpus)" (renderCpuList cpus) | Just cpus <- [ptCpus tuning]]
        <> [row "proxy runtime settings pinned" (T.intercalate ", " pinned) | not (null pinned)]
        <> [row "passes" "loaded pass only" | lkPasses knobs == LoadedPassOnly]
  where
    tuning = lkProxyTuning knobs
    pinned =
        ["runtime.cores " <> show n | Just n <- [ptCores tuning]]
            <> ["runtime.maxHeapBytes " <> show n | Just n <- [ptMaxHeapBytes tuning]]
            <> ["-A" <> show n <> "m" | Just n <- [ptAllocAreaMib tuning]]

-- The first scenario's rule policy, and the proxies of either pass that logged a different one.
rulePolicyLines :: [ScenarioReport] -> [ScenarioReport] -> [Text]
rulePolicyLines c1Reports reports = case policies reports "" <> policies c1Reports " (concurrency one)" of
    [] -> []
    (_, logged) : rest ->
        ["**Rule policy, as the first scenario's proxy logged it**", ""]
            <> ruleTable (loggedRules logged)
            <> map orderLine (ruleBootOrders logged)
            <> ["- Scenarios whose proxy logged a different rule policy: " <> T.intercalate ", " differing | let differing = [name | (name, other) <- rest, other /= logged], not (null differing)]
            <> [""]
  where
    policies rs label = [(srName r <> label, pfRuleLines p) | r <- rs, Just p <- [srProxy r]]
    orderLine (mount, rules) = "- Mount " <> mount <> " evaluates, highest precedence first: " <> bool (T.intercalate ", then " rules) "no rule" (null rules)

ruleTable :: [LoggedRule] -> [Text]
ruleTable = \case
    [] -> ["The proxy logged no rule configuration.", ""]
    rules ->
        ["| rule | type | other keys | set by |", "| --- | --- | --- | --- |"]
            <> map ruleRow rules
            <> [""]
  where
    ruleRow r = "| " <> T.intercalate " | " [lrName r <> maybe "" (\why -> " (inactive: " <> why <> ")") (lrInactive r), fromMaybe "n/a" (lrType r), settings r, T.intercalate ", " (lrLayers r)] <> " |"
    settings r = bool (T.intercalate ", " [key <> " " <> value | (key, value) <- lrSettings r]) "none" (null (lrSettings r))

shapeNote :: PodShape -> Int -> ProxyTuning -> Text
shapeNote shape capabilities tuning
    | shape == Unlimited && tuning == noTuning = "unlimited: no harness-imposed cgroup limit, runtime.cores " <> show capabilities <> " (GHC capabilities)"
    | otherwise = renderPodShape shape <> ": the proxy's own cgroup, " <> T.intercalate ", " (quota <> memory)
  where
    written = map fst (proxyCgroupFiles shape tuning)
    quota = [bool "no CPU-time quota" "CPU-time quota via cpu.max" ("cpu.max" `elem` written)]
    memory
        | "memory.max" `elem` written = ["memory.max set, swap off"]
        | "memory.high" `elem` written = ["no memory.max, memory.high backstop"]
        | otherwise = ["no memory.max"]

glanceRow :: (Text -> FloorCheck) -> ScenarioReport -> Text
glanceRow floorOf r =
    "| ["
        <> srName r
        <> "](#"
        <> srName r
        <> ") | "
        <> T.intercalate
            " | "
            [ srDescription r
            , show (lsConnections load)
            , show (lsSuccesses load) <> (if null (srSteps r) then "" else " (last step)")
            , floorCell r (floorOf (srName r))
            , show (lsRefusals load)
            , show (lsTransportFailures load)
            , fmt1 (throughput load)
            , msCell (pP50Ms (lsLatency load))
            , msCell (pP99Ms (lsLatency load))
            , maybe "n/a" kib (allocPerSuccess r)
            , maybe "n/a" pct (gcCpuShare =<< srRtsWindow r)
            , memoryCell r
            , maybe "in harness" (endingCell . pfEnding) (srProxy r)
            ]
        <> " |"
  where
    load = srLoad r

floorCell :: ScenarioReport -> FloorCheck -> Text
floorCell r = \case
    Unchecked -> "not held"
    NoFloor _ -> "**none**"
    AtLeast least
        | not (null (srSteps r)) -> show least <> " (every step)"
        | hasDiagnosticBurst r -> show least <> " (background only)"
        | isJust (srCompanion r) -> show least <> " (both loads)"
        | otherwise -> show least

-- Each scenario's loaded cost beside its concurrency-one cost and floor, its refusals' cost, and cache sharing.
costTable :: (Text -> FloorCheck) -> [ScenarioReport] -> [ScenarioReport] -> [Text]
costTable floorAtOne c1Reports reports =
    [ "### Loaded cost against concurrency one"
    , ""
    , "| scenario | alloc / success (upper bound) | at base concurrency one | success floor at base concurrency one | alloc / refusal (upper bound) | success p50 | at base concurrency one | missed lookups | collapsed lookups |"
    , "| --- | --: | --: | --: | --: | --: | --: | --: | --: |"
    ]
        <> map costRow (withConcurrencyOne c1Reports reports)
        <> [""]
  where
    costRow (r, c1) =
        "| "
            <> T.intercalate
                " | "
                [ srName r
                , countedKib (allocPerSuccess r) (windowSuccesses r) "successes"
                , maybe "n/a" (\atOne -> countedKib (allocPerSuccess atOne) (windowSuccesses atOne) "successes") c1
                , maybe "n/a" (\atOne -> floorCell atOne (floorAtOne (srName atOne))) c1
                , countedKib (allocPerRefusal r) (windowRefusals r) "refusals"
                , msCell (pP50Ms (lsLatency (srLoad r)))
                , msCell (pP50Ms . lsLatency . srLoad =<< c1)
                , lookups coMisses r
                , lookups coCollapsed r
                ]
            <> " |"
    lookups field r = maybe "n/a" (show . sum . map (field . snd)) (pfCacheWindow =<< srProxy r)

-- Each loaded report with its concurrency-one counterpart, when that pass ran the scenario.
withConcurrencyOne :: [ScenarioReport] -> [ScenarioReport] -> [(ScenarioReport, Maybe ScenarioReport)]
withConcurrencyOne c1Reports reports = [(r, Map.lookup (srName r) byName) | r <- reports]
  where
    byName = Map.fromList [(srName r, r) | r <- c1Reports]

renderScenario :: ProxyTuning -> ScenarioReport -> [Text]
renderScenario tuning r =
    [ "### " <> srName r
    , ""
    , srDescription r
    , ""
    , "| metric | value |"
    , "| --- | --- |"
    ]
        <> loadRows (srLoad r)
        <> maybe [] companionRows (srCompanion r)
        <> rtsRows r
        <> maybe [] proxyRows (srProxy r)
        <> [row "memory.peak less RTS max memory in use" (maybe "n/a" signedMib (offHeapGap r)) | isJust (srProxy r)]
        <> [row "serve slot held per success: in-flight mean x window / successes" (msCell (slotPerSuccessMs r)) | isJust (srProxy r)]
        <> maybe [] (cpuRows (isJust (ptCpus tuning)) r) (pfCpu =<< srProxy r)
        <> [""]
        <> stepsTable (srSteps r)
        <> [maybe "" renderReplayTotals (srReplayTotals r), srEvidence r]
        <> maybe [] proxyDetail (srProxy r)
        <> [""]

loadRows :: LoadSummary -> [Text]
loadRows l =
    [ row "connections held open" (show (lsConnections l))
    , row "successes" (show (lsSuccesses l) <> " in " <> fmt1 (lsElapsedSeconds l) <> " s (" <> fmt1 (throughput l) <> " req/s)")
    , row "completed responses / refusals (429, 503) / other statuses" (show (lsCompleted l) <> " / " <> show (lsRefusals l) <> " / " <> show (lsOtherStatuses l))
    , row "transferred response body bytes / bytes per second (all statuses)" (maybe "unavailable" (\bytes -> show bytes <> " / " <> fmt1 (fromIntegral bytes / max 1e-9 (lsElapsedSeconds l))) (lsTransferredBodyBytes l))
    , row "transport failures / unfinished at the deadline" (show (lsTransportFailures l) <> " / " <> show (lsDeadlineAborts l))
    , row "success latency p50 / p90 / p99 / p99.9" (T.intercalate " / " (map msCell [pP50Ms lat, pP90Ms lat, pP99Ms lat, pP999Ms lat]))
    , row "success latency p95" (msCell (pP95Ms lat))
    , row "first body byte p50 / p95 / p99 (all completed responses, including refusals)" (maybe "unavailable" (\observed -> T.intercalate " / " (map msCell [pP50Ms observed, pP95Ms observed, pP99Ms observed])) (lsFirstByteLatency l))
    , row "distribution" (lsNote l)
    ]
  where
    lat = lsLatency l

companionRows :: LoadSummary -> [Text]
companionRows l =
    [ row (lsLabel l <> ": successes / refusals / transport failures") (show (lsSuccesses l) <> " / " <> show (lsRefusals l) <> " / " <> show (lsTransportFailures l))
    , row (lsLabel l <> ": success latency p50 / p99") (msCell (pP50Ms (lsLatency l)) <> " / " <> msCell (pP99Ms (lsLatency l)))
    , row (lsLabel l <> ": distribution") (lsNote l)
    ]

rtsRows :: ScenarioReport -> [Text]
rtsRows r =
    [ row "RTS figures from" (srRtsSource r)
    , row "allocation / successful request" (maybe "n/a" kib (allocPerSuccess r) <> " (" <> show (windowSuccesses r) <> " successes, " <> show (windowAttempts r) <> " attempts in the window)")
    , row "allocation / attempt" (maybe "n/a" kib (perAttempt r))
    , row "allocation / refusal" (countedKib (allocPerRefusal r) (windowRefusals r) "refusals in the window")
    , row "GCs (total / major) / GC share of CPU / GC wall" (maybe "n/a" gcCell (srRtsWindow r))
    , row "mean live data after the window's major collections" (maybe "n/a" (mib . round) (meanLiveAtMajors =<< srRtsWindow r))
    , row "RTS max live / max memory in use" (maybe "n/a" (\s -> mib (rsMaxLiveBytes s) <> " / " <> mib (rsMaxMemInUseBytes s)) (srRtsEnd r))
    , row postureRow (maybe "n/a" postureCell (posture r))
    , row "processors the RTS sees" (maybe "n/a" (show . rsProcessors) (posture r))
    , row "compaction threshold crossed (inferred from the maxima)" (maybe "n/a" compactionCell (srRtsEnd r))
    , row "retained after the run" (maybe "n/a" mib (srRetainedBytes r))
    ]
  where
    compactionCell s = maybe "n/a (no heap ceiling)" (bool "no" "yes") (compactionThresholdCrossed s) <> " (-c " <> fmt1 (rsCompactThresholdPercent s) <> "% of -M)"
    gcCell w = show (rwGcs w) <> " / " <> show (rwMajorGcs w) <> " / " <> maybe "n/a" pct (gcCpuShare w) <> " / " <> fmt1 (fromIntegral (rwGcElapsedNs w) / 1_000_000) <> " ms"
    postureCell s = maybe "none" (mib . fromIntegral) (rsMaxHeapBytes s) <> " / " <> show (rsCapabilities s) <> " / " <> mib (fromIntegral (rsAllocAreaBytes s))

proxyRows :: ProxyFigures -> [Text]
proxyRows p =
    [ row "idle floor: live after a major GC / RTS memory in use / cgroup memory.current" idleCell
    , row "cgroup memory.peak / memory.max" (maybe "no cgroup" (\c -> maybe "n/a" (mib . fromIntegral) (crMemoryPeak c) <> " / " <> maybe "max" (mib . fromIntegral) (crMemoryMax c)) cgroup)
    , row "memory.stat at the window's end: anon / file / kernel / sock" (maybe "no cgroup" statCell (pfWindowCgroup p))
    , row memoryEventsRow (maybe "no cgroup" eventsCell cgroup)
    , row "CPU throttled during the window" (maybe "n/a" (\us -> fmt1 (fromIntegral us / 1_000) <> " ms") (pfWindowThrottledUsec p))
    , row "proxy ending" (endingCell (pfEnding p) <> if pfExitedEarly p then ", before the harness stopped it" else "")
    , row "boot attempts" (show (length (pfBootRetries p) + 1))
    , row admissionRow (maybe "n/a" show (blCpuAdmission limits) <> " / " <> maybe "n/a" bytesCell (blMemoryBudgetBytes limits))
    , row "admission in-flight gauge: max / mean / last (samples, missed)" (gaugeCell (pfInFlight p))
    , row "proxy threads (pids.current): max / mean / last (samples, missed)" (gaugeCell (pfTasks p))
    , row "metadata cache hit / miss / collapsed in the window" (maybe "n/a (a scrape failed)" cacheCell (pfCacheWindow p))
    ]
        <> maybe [] burstRows (pfBurstMemory p)
  where
    cgroup = pfCgroup p
    limits = bootLimits (pfBootLines p)
    idleCell =
        maybe "n/a" (mib . rsLiveBytes) (pfIdleRts p)
            <> " / "
            <> maybe "n/a" (mib . rsMemInUseBytes) (pfIdleRts p)
            <> " / "
            <> maybe "n/a" (mib . fromIntegral) (pfIdleCgroupBytes p)
    cacheCell stores = T.intercalate ", " [store <> " " <> show (coHits o) <> " / " <> show (coMisses o) <> " / " <> show (coCollapsed o) | (store, o) <- stores]
    eventsCell c = T.intercalate " / " [show (counter key (crMemoryEvents c)) | key <- ["oom_kill", "oom", "max", "high"]]
    gaugeCell g =
        T.intercalate " / " (map (maybe "n/a" fmt1) [gsMax g, gsMean g, gsLast g])
            <> " ("
            <> show (gsSamples g)
            <> ", "
            <> show (gsMissed g)
            <> ")"

-- The cgroup's memory through a burst: the reading with the most charged, and the one with the most in socket buffers.
-- Every burst reads its cgroup at least once, so a burst without a reading had no cgroup to read.
burstRows :: BurstMemory -> [Text]
burstRows b
    | bmSamples b == 0 = [row "cgroup readings through the burst" "no cgroup"]
    | otherwise =
        [ row ("cgroup readings through the burst, every " <> show (bmIntervalMs b) <> " ms") (show (bmSamples b))
        , row "highest memory.current in the burst, then memory.stat anon / file / kernel / sock" (maybe "n/a" (\c -> currentCell c <> ", then " <> statCell c) (bmHighestCurrent b))
        , row "highest memory.stat sock in the burst, then memory.current" (maybe "n/a" (\c -> mib (fromIntegral (counter "sock" (crMemoryStat c))) <> ", then " <> currentCell c) (bmHighestSock b))
        ]
  where
    currentCell = maybe "n/a" (mib . fromIntegral) . crMemoryCurrent

statCell :: CgroupReading -> Text
statCell c = T.intercalate " / " [mib (fromIntegral (counter key (crMemoryStat c))) | key <- ["anon", "file", "kernel", "sock"]]

-- The CPU time of the window: the proxy against its CPUs, and the load generator's headroom.
cpuRows :: Bool -> ScenarioReport -> CpuWindow -> [Text]
cpuRows placed r w =
    [ row "proxy CPU-seconds per second: cgroup / RTS (share of the cgroup's CPUs)" (rate (proxyCpuRate w) <> " / " <> rate rtsRate <> share (proxyCpuShare w) (cwProxyCpus w))
    , row "harness cgroup CPU-seconds per second: load generator, stubs, and harness (share of its CPUs)" (rate (harnessCpuRate w) <> share (harnessCpuShare w) (cwHarnessCpus w))
    , row ("CPU " <> show ohaCpu <> " busy, where oha runs") (maybe "n/a" pct (cpuBusyShare ohaCpu w))
    , row generatorHeadroomRow (maybe "n/a" (bool headroomHeld ("**" <> headroomSaturated <> "**")) (generatorSaturated w))
    ]
        -- An unplaced proxy shares its CPUs with the harness, so their busy time is not foreign to it.
        <> [row "busy time on the proxy's CPUs that the proxy did not spend" (maybe "n/a" (\seconds -> fmt2 seconds <> " s in " <> fmt1 (cwSeconds w) <> " s") (foreignCpuSeconds w)) | placed]
  where
    rtsRate = do
        window <- srRtsWindow r
        guard (cwSeconds w > 0)
        pure (fromIntegral (rwCpuNs window) / 1e9 / cwSeconds w)
    rate = maybe "n/a" fmt2
    share value cpus = maybe "" (\v -> " (" <> pct v <> " of CPUs " <> maybe "?" renderCpuList cpus <> ")") value

proxyDetail :: ProxyFigures -> [Text]
proxyDetail p =
    section
        "Serve memory charges in the window"
        [ "all serve memory charge bytes: " <> maybe "unavailable" (\observed -> "sampled max " <> maybe "unavailable" show (gsMax observed) <> ", last " <> maybe "unavailable" show (gsLast observed)) (pfMemoryChargeBytes p)
        ]
        <> section "Admission series at the end of the window" (pfAdmissionSeries p)
        <> section "Retried proxy boots" (concatMap lines (pfBootRetries p))
        <> section "Proxy stderr" (if T.null (pfStderrTail p) then [] else lines (pfStderrTail p))
  where
    section _ [] = []
    section title body = ["<details><summary>" <> title <> "</summary>", "", "```text"] <> body <> ["```", "", "</details>", ""]

stepsTable :: [LoadSummary] -> [Text]
stepsTable [] = []
stepsTable steps =
    [ "| ramp step | successes | successful req/s | refusals | transport failures | success p50 | success p99 |"
    , "| --- | --: | --: | --: | --: | --: | --: |"
    ]
        <> map step steps
        <> [""]
  where
    step l =
        "| "
            <> T.intercalate
                " | "
                [ lsLabel l
                , show (lsSuccesses l)
                , fmt1 (throughput l)
                , show (lsRefusals l)
                , show (lsTransportFailures l)
                , msCell (pP50Ms (lsLatency l))
                , msCell (pP99Ms (lsLatency l))
                ]
            <> " |"

readingNotes :: [Text]
readingNotes =
    [ "### Reading the numbers"
    , ""
    , "- **Successes are the primary figure.** A 2xx or 3xx response is a success. A `503` shed or a `429` is a refusal: a client retries it at once, so refusal counts measure retry speed, not demand."
    , "- **The run fails** when a required load or ramp step has no successful response or misses its floor. A scheduled run also fails when its configuration keeps it off the floors. Proxy OOM kills, heap overflows, abnormal shutdowns, and early exits fail the run. Latency and memory figures have no threshold."
    , "- **A success floor** is the minimum count of successes for a scenario and pass under this pod shape, from `" <> toText floorsPath <> "`. The at-a-glance table shows the loaded floor. The cost table shows the concurrency-one floor. A ramp holds every step to its floor. A paired scenario holds both loads to its floor. The combined `herd-under-heavy-private` probes hold only the concurrent background load to a floor. Their burst successes and refusals remain diagnostic, including zero successes. A held run also fails on a missing floor or a floor that names no scenario."
    , "- **A run is held to the floors** only on the runner they were calibrated on, at the calibrated settings, under a pod shape the floors hold, and while each ecosystem fixture stays below its own latency ceiling and measurement conditions match. The Success floors section says whether this run is held, and names every reason when it is not. A run that is not held shows `not held` for each floor."
    , "- **Allocation and collector figures describe the proxy process alone** over the measured window, and divide by every successful request in it: both generators of a paired scenario, every step of a ramp. The stub upstreams and the load generator run outside it."
    , "- **The proxy's cgroup is not charged for its logs.** They go through pipes the harness drains, and only unread pipe buffers, at most 64 KiB per pipe, count against the limit. On CI the build step has just built or restored the executable, so its text pages are already in the page cache when the proxy starts and are not charged to it either. The harness does not enforce that."
    , "- **Each scenario boots its own proxy** from its own cgroup, so the runtime posture and the admission budgets are the ones that pod shape resolves. A boot retried after the runtime could not start a thread gets a fresh cgroup, so its readings exclude the failed attempt."
    , "- **Native metadata gzip is off** in every proxy. The harness sets `ECLUSE_SERVER__METADATA_GZIP=false` and refuses a proxy that logs another posture, so these figures measure identity listings, not gzip-enabled throughput. A report from a build before the setting shows it as unreported."
    , "- **memory.peak** is the kernel's high-water mark for the proxy's cgroup. **RTS max memory in use** is what the heap held, the figure `-M` is compared with. The gap is off-heap and kernel memory."
    , "- **CPU figures** cover the window. The proxy's come from its cgroup's `cpu.stat` and from the RTS. The harness cgroup holds the load generator, the stubs, and the harness. The generator keeps its headroom while that cgroup and the CPU `oha` runs on each stay below " <> show (round (headroomLimit * 100) :: Int) <> "% busy. For a proxy placed on its own CPUs, busy time on them that the proxy did not spend comes from `/proc/stat`: runner services and the kernel."
    , "- **The in-flight gauge** is `ecluse.serve.admission.in_flight`, sampled each second. Requests that hold admission without finishing show as a flat, nonzero gauge beside zero successes. The serve slot held per success multiplies the gauge's mean by the window and divides by the successes, so it estimates how long one success kept its CPU slot."
    , "- **The rule policy** is what the first scenario's proxy logged at boot: each rule its configuration names, with its type and keys, and the order each mount evaluates the rules in. A proxy of either pass that logged a different policy is named."
    , "- **Allocation per refusal** divides the same window's allocation by its refusals instead of its successes. Each figure charges the whole window to one kind of response, so each is an upper bound on that kind's cost, and a small count makes it large."
    , "- **Concurrency-one** figures come from the concurrency-one pass, which runs the same scenario on a fresh proxy after the same warm-up, with exactly one connection regardless of loaded-pass scaling. Scenarios outside that pass show n/a. A loaded allocation per success far above its concurrency-one figure points at contention, or at work the concurrent requests did not share."
    , "- **Missed and collapsed lookups** are the metadata cache's request outcomes in the window, summed over the full, version, and assembled stores. One request can look up more than one store, so the sums count lookups, not requests. Each scenario lists them per store. A collapsed lookup waited for another request's fetch instead of making its own, so few collapsed lookups beside many missed ones show concurrent requests that did not share work."
    ]

-- | Attribute concurrency-one service time against the named upstream baseline.
renderServiceTime :: BaselineSource -> [ScenarioReport] -> Text
renderServiceTime source reports =
    renderNormalised source [NormalisedRow (srName r) (pP50Ms (lat r)) (pP99Ms (lat r)) (srPublicLegUnavailableReason r) | r <- reports]
  where
    lat = lsLatency . srLoad

-- | Pair loaded reports with their concurrency-one counterparts to describe saturation.
renderLoadSaturation :: [ScenarioReport] -> [ScenarioReport] -> Text
renderLoadSaturation c1Reports loadedReports =
    renderSaturation queuingDominanceThreshold (map (deriveSaturation queuingDominanceThreshold . toInput) (withConcurrencyOne c1Reports loadedReports))
  where
    toInput (loaded, c1) =
        SaturationInput
            (srName loaded)
            (throughput (srLoad loaded))
            (lsDeadlineAborts (srLoad loaded))
            (pP50Ms . lsLatency . srLoad =<< c1)
            (pP50Ms (lsLatency (srLoad loaded)))

-- | Render the GC-thrash probe by memory limit. GHC.Stats lacks the data for reclaim per major GC.
renderThrash :: Text -> [(Text, Either Text ScenarioReport)] -> Text
renderThrash scenarioKey steps =
    T.unlines $
        [ "## GC-thrash probe: " <> scenarioKey
        , ""
        , "The load stays fixed while the memory limit steps down. An OOM kill or a heap overflow here is the probe's reading, not a failed run. The probe is not held to the success floors."
        , ""
        , "| pod shape | successes | success p99 | GC share | major GCs | mean live after majors | RTS max live | heap ceiling | compaction crossed | memory peak / max | peak less RTS in use | oom_kill | ending |"
        , "| --- | --: | --: | --: | --: | --: | --: | --: | --- | --: | --: | --: | --- |"
        ]
            <> map step steps
  where
    step (shape, Left failure) = "| " <> shape <> " | the scenario did not run: " <> T.replace "\n" " " failure <> " | | | | | | | | | | | |"
    step (shape, Right r) =
        "| "
            <> T.intercalate
                " | "
                [ shape
                , show (lsSuccesses (srLoad r))
                , msCell (pP99Ms (lsLatency (srLoad r)))
                , maybe "n/a" pct (gcCpuShare =<< srRtsWindow r)
                , maybe "n/a" (show . rwMajorGcs) (srRtsWindow r)
                , maybe "n/a" (mib . round) (meanLiveAtMajors =<< srRtsWindow r)
                , maybe "n/a" (mib . rsMaxLiveBytes) (srRtsEnd r)
                , maybe "n/a" (maybe "none" (mib . fromIntegral) . rsMaxHeapBytes) (posture r)
                , maybe "n/a" (maybe "n/a" (bool "no" "yes") . compactionThresholdCrossed) (srRtsEnd r)
                , memoryCell r
                , maybe "n/a" signedMib (offHeapGap r)
                , maybe "n/a" (show . counter "oom_kill" . crMemoryEvents) (pfCgroup =<< srProxy r)
                , maybe "n/a" (endingCell . pfEnding) (srProxy r)
                ]
            <> " |"

-- | Whether the run is held to the success floors, with their calibration or every reason it is not.
renderFloors :: Calibration -> Enforcement -> Text
renderFloors calibration enforcement =
    T.unlines
        [ "## Success floors"
        , ""
        , describeEnforcement calibration enforcement
        , ""
        , "Floor measurement conditions: `" <> calConditions calibration <> "`."
        , describeRetainedConditions calibration
        ]

-- | The run's verdict: every broken invariant, or a line saying none broke.
renderVerdict :: [Text] -> Text
renderVerdict violations =
    T.unlines $
        ["## Verdict", ""]
            <> case violations of
                [] -> ["Every required load had successful responses and reached any applicable floor. No proxy was OOM-killed or exited on heap overflow."]
                _ -> "**The run fails:**" : "" : map ("- " <>) violations

-- memory.peak less the RTS's own high-water mark: off-heap and kernel memory, and the overshoot
-- between collections.
offHeapGap :: ScenarioReport -> Maybe Int
offHeapGap r = do
    peak <- crMemoryPeak =<< pfCgroup =<< srProxy r
    inUse <- rsMaxMemInUseBytes <$> srRtsEnd r
    pure (peak - fromIntegral inUse)

-- Little's law over the sampled gauge: slot-milliseconds in the window shared out over its successes.
slotPerSuccessMs :: ScenarioReport -> Maybe Double
slotPerSuccessMs r = do
    held <- gsMean . pfInFlight =<< srProxy r
    guard (windowSuccesses r > 0)
    pure (held * windowSeconds * 1_000 / fromIntegral (windowSuccesses r))
  where
    windowSeconds = case srSteps r of
        [] -> lsElapsedSeconds (srLoad r)
        steps -> sum (map lsElapsedSeconds steps)

-- The posture at the window's end, or at the idle floor when the proxy died before the window closed.
posture :: ScenarioReport -> Maybe RtsSnapshot
posture r = srRtsEnd r <|> (pfIdleRts =<< srProxy r)

memoryCell :: ScenarioReport -> Text
memoryCell r = case pfCgroup =<< srProxy r of
    Nothing -> "n/a"
    Just c -> maybe "n/a" (mib . fromIntegral) (crMemoryPeak c) <> " / " <> maybe "max" (mib . fromIntegral) (crMemoryMax c)

endingCell :: ProxyEnding -> Text
endingCell ending
    | ending == CleanShutdown = describeEnding ending
    | otherwise = "**" <> describeEnding ending <> "**"

allocPerSuccess, perAttempt, allocPerRefusal :: ScenarioReport -> Maybe Double
allocPerSuccess = allocationPer windowSuccesses
perAttempt = allocationPer windowAttempts
allocPerRefusal = allocationPer windowRefusals

-- The window's allocation divided by a count of its responses, 'Nothing' when the count is zero.
allocationPer :: (ScenarioReport -> Int) -> ScenarioReport -> Maybe Double
allocationPer count r = do
    w <- srRtsWindow r
    perSuccess (fromIntegral (rwAllocatedBytes w)) (count r)

throughput :: LoadSummary -> Double
throughput l = if lsElapsedSeconds l > 0 then fromIntegral (lsSuccesses l) / lsElapsedSeconds l else 0

row :: Text -> Text -> Text
row k v = "| " <> k <> " | " <> v <> " |"

msCell :: Maybe Double -> Text
msCell = maybe "n/a" (\v -> fmt2 v <> " ms")

bytesCell :: Int -> Text
bytesCell bytes = show bytes <> " B (" <> mib (fromIntegral bytes) <> ")"

pct :: Double -> Text
pct x = fmt1 (x * 100) <> "%"

kib :: Double -> Text
kib bytes = fmt1 (bytes / 1024) <> " KiB"

-- A per-response figure with the count it divides by.
countedKib :: Maybe Double -> Int -> Text -> Text
countedKib perResponse count noun = maybe "n/a" kib perResponse <> " (" <> show count <> " " <> noun <> ")"

mib :: Word64 -> Text
mib = signedMib . fromIntegral

signedMib :: Int -> Text
signedMib bytes = fmt1 (fromIntegral bytes / (1024 * 1024)) <> " MiB"

fmt1, fmt2 :: Double -> Text
fmt1 x = toText (showFFloat (Just 1) x "")
fmt2 x = toText (showFFloat (Just 2) x "")
