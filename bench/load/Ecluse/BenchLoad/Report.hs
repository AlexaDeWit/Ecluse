-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Render the child reports into the Markdown the run summary and the uploaded artifact carry.
Successes lead every table. Memory, collector, and admission figures describe the proxy process,
and the verdict section lists every broken invariant.
-}
module Ecluse.BenchLoad.Report (
    renderReports,
    renderServiceTime,
    renderLoadSaturation,
    renderThrash,
    renderVerdict,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Numeric (showFFloat)

import Ecluse.BenchLoad.BootLines (BootLimits (..), admittedListings, bootLimits)
import Ecluse.BenchLoad.Exposition (GaugeSummary (..))
import Ecluse.BenchLoad.Harness (LoadKnobs (..), LoadSummary (..), ProxyFigures (..), ScenarioReport (..), windowAttempts, windowSuccesses)
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
import Ecluse.BenchLoad.Pod (CgroupReading (..), counter)
import Ecluse.BenchLoad.RtsWindow (RtsSnapshot (..), RtsWindow (..), compactionThresholdCrossed, gcCpuShare, meanLiveAtMajors, perSuccess)
import Ecluse.BenchLoad.Verdict (ProxyEnding (..), describeEnding)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)

-- | One ecosystem's loaded pass: the operating point, the at-a-glance table, and each scenario.
renderReports :: LoadKnobs -> Int -> Int -> Text -> Ecosystem -> [ScenarioReport] -> Text
renderReports knobs capabilities processors shape ecosystem reports =
    T.unlines $
        [ "## Load test: throughput and latency over " <> ecosystemName ecosystem
        , ""
        , "_Successful responses per window are the primary figure. Reading notes are at the end of the report._"
        , ""
        , "**Operating point**"
        , ""
        , "| knob | value |"
        , "| --- | --- |"
        , row "pod shape" (shapeNote shape capabilities)
        , row "runner processors" (show processors <> " (oha is pinned to core 0 when isolated)")
        , row "load" (show (lkConcurrency knobs) <> " connections x " <> show (lkDurationSeconds knobs) <> " s (a scenario may scale its own connections)")
        , row "injected upstream latency" (fmt1 (fromIntegral (lkUpstreamLatencyMicros knobs) / 1_000) <> " ms")
        , row "CPU admission" (maybe "n/a" show (blCpuAdmission limits) <> origin (lkServeMaxInFlight knobs))
        , row "memory admission budget" (maybe "n/a" bytesCell (blMaterialBudgetBytes limits))
        , row "cold two-origin listings admitted at once" (maybe "n/a" show (admittedListings limits))
        , row "private pool" (maybe "computed by the proxy from its fd limit" (\n -> show n <> " (explicit)") (lkPrivateConnectionsPerHost knobs))
        , row "public pool" (maybe "computed by the proxy from its fd limit" (\n -> show n <> " (explicit)") (lkPublicConnectionsPerHost knobs))
        , row "cache-eviction entries" (show (lkCacheMaxEntries knobs))
        , row "working-set cap" (show (lkWorkingSet knobs) <> " projects")
        , row "worker artifact" ("~" <> kib (fromIntegral (lkPayloadBytes knobs)))
        , ""
        ]
            <> runtimeLines
            <> [ "### At a glance"
               , ""
               , "| scenario | connections | successes | refusals | transport failures | successful req/s | success p50 | success p99 | alloc / success | GC share | memory peak / max | ending |"
               , "| --- | --: | --: | --: | --: | --: | --: | --: | --: | --: | --: | --- |"
               ]
            <> map glanceRow reports
            <> [""]
            <> concatMap renderScenario reports
            <> readingNotes
  where
    firstBoot = listToMaybe [pfBootLines p | Just p <- map srProxy reports, not (null (pfBootLines p))]
    limits = bootLimits (fromMaybe [] firstBoot)
    origin = maybe " (the proxy's computed default)" (const " (explicit)")
    runtimeLines = case firstBoot of
        Nothing -> []
        Just lines' ->
            ["**Runtime posture and memory plan, as the first scenario's proxy logged them**", ""]
                <> map ("- " <>) lines'
                <> [""]

shapeNote :: Text -> Int -> Text
shapeNote shape capabilities
    | shape == "unlimited" = "unlimited: no cgroup limit, runtime.cores " <> show capabilities
    | otherwise = shape <> ": the proxy's own cgroup, memory.max and cpu.max set, swap off"

glanceRow :: ScenarioReport -> Text
glanceRow r =
    "| ["
        <> srName r
        <> "](#"
        <> srName r
        <> ") | "
        <> T.intercalate
            " | "
            [ show (lsConnections load)
            , show (lsSuccesses load) <> (if null (srSteps r) then "" else " (last step)")
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

renderScenario :: ScenarioReport -> [Text]
renderScenario r =
    [ "### " <> srName r
    , ""
    , "| metric | value |"
    , "| --- | --- |"
    ]
        <> loadRows (srLoad r)
        <> maybe [] companionRows (srCompanion r)
        <> rtsRows r
        <> maybe [] proxyRows (srProxy r)
        <> [row "memory.peak less RTS max memory in use" (maybe "n/a" signedMib (offHeapGap r)) | isJust (srProxy r)]
        <> [""]
        <> stepsTable (srSteps r)
        <> [maybe "" renderReplayTotals (srReplayTotals r), srEvidence r]
        <> maybe [] proxyDetail (srProxy r)
        <> ["> " <> srDescription r, ""]

loadRows :: LoadSummary -> [Text]
loadRows l =
    [ row "connections held open" (show (lsConnections l))
    , row "successes" (show (lsSuccesses l) <> " in " <> fmt1 (lsElapsedSeconds l) <> " s (" <> fmt1 (throughput l) <> " req/s)")
    , row "completed responses / refusals (429, 503) / other statuses" (show (lsCompleted l) <> " / " <> show (lsRefusals l) <> " / " <> show (lsOtherStatuses l))
    , row "transport failures / unfinished at the deadline" (show (lsTransportFailures l) <> " / " <> show (lsDeadlineAborts l))
    , row "success latency p50 / p90 / p99 / p99.9" (T.intercalate " / " (map msCell [pP50Ms lat, pP90Ms lat, pP99Ms lat, pP999Ms lat]))
    , row "distribution" (lsNote l)
    ]
  where
    lat = lsLatency l

companionRows :: LoadSummary -> [Text]
companionRows l =
    [ row "concurrent load: successes / refusals / transport failures" (show (lsSuccesses l) <> " / " <> show (lsRefusals l) <> " / " <> show (lsTransportFailures l))
    , row "concurrent load: success latency p50 / p99" (msCell (pP50Ms (lsLatency l)) <> " / " <> msCell (pP99Ms (lsLatency l)))
    , row "concurrent load: distribution" (lsNote l)
    ]

rtsRows :: ScenarioReport -> [Text]
rtsRows r =
    [ row "RTS figures from" (srRtsSource r)
    , row "allocation / successful request" (maybe "n/a" kib (allocPerSuccess r) <> " (" <> show (windowSuccesses r) <> " successes, " <> show (windowAttempts r) <> " attempts in the window)")
    , row "allocation / attempt" (maybe "n/a" kib (perAttempt r))
    , row "GCs (total / major) / GC share of CPU / GC wall" (maybe "n/a" gcCell (srRtsWindow r))
    , row "mean live data after the window's major collections" (maybe "n/a" (mib . round) (meanLiveAtMajors =<< srRtsWindow r))
    , row "RTS max live / max memory in use" (maybe "n/a" (\s -> mib (rsMaxLiveBytes s) <> " / " <> mib (rsMaxMemInUseBytes s)) (srRtsEnd r))
    , row "heap ceiling (-M) / capabilities / allocation area" (maybe "n/a" postureCell (posture r))
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
    , row "memory.events oom_kill / oom / max / high" (maybe "no cgroup" eventsCell cgroup)
    , row "CPU throttled during the window" (maybe "n/a" (\us -> fmt1 (fromIntegral us / 1_000) <> " ms") (pfWindowThrottledUsec p))
    , row "proxy ending" (endingCell (pfEnding p) <> if pfExitedEarly p then ", before the harness stopped it" else "")
    , row "CPU admission / memory admission budget / cold listings at once" (maybe "n/a" show (blCpuAdmission limits) <> " / " <> maybe "n/a" bytesCell (blMaterialBudgetBytes limits) <> " / " <> maybe "n/a" show (admittedListings limits))
    , row "admission in-flight gauge: max / mean / last (samples, missed)" inFlightCell
    ]
  where
    cgroup = pfCgroup p
    limits = bootLimits (pfBootLines p)
    idleCell =
        maybe "n/a" (mib . rsLiveBytes) (pfIdleRts p)
            <> " / "
            <> maybe "n/a" (mib . rsMemInUseBytes) (pfIdleRts p)
            <> " / "
            <> maybe "n/a" (mib . fromIntegral) (pfIdleCgroupBytes p)
    eventsCell c = T.intercalate " / " [show (counter key (crMemoryEvents c)) | key <- ["oom_kill", "oom", "max", "high"]]
    statCell c = T.intercalate " / " [mib (fromIntegral (counter key (crMemoryStat c))) | key <- ["anon", "file", "kernel", "sock"]]
    g = pfInFlight p
    inFlightCell =
        T.intercalate " / " (map (maybe "n/a" fmt1) [gsMax g, gsMean g, gsLast g])
            <> " ("
            <> show (gsSamples g)
            <> ", "
            <> show (gsMissed g)
            <> ")"

proxyDetail :: ProxyFigures -> [Text]
proxyDetail p =
    section "Admission series at the end of the window" (pfAdmissionSeries p)
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
    , "- **The run fails** when a scenario or a ramp step has no successful response, when the kernel OOM-kills a proxy, when a proxy exits on heap overflow or ends any other way than the clean shutdown the harness asks for, or when it exits early. Throughput, latency, and memory have no threshold and never fail it."
    , "- **Allocation and collector figures describe the proxy process alone** over the measured window, and divide by every successful request in it: both generators of a paired scenario, every step of a ramp. The stub upstreams and the load generator run outside it."
    , "- **The proxy's cgroup is not charged for its logs.** They go through pipes the harness drains, and only unread pipe buffers, at most 64 KiB per pipe, count against the limit. On CI the build step has just built or restored the executable, so its text pages are already in the page cache when the proxy starts and are not charged to it either. The harness does not enforce that."
    , "- **Each scenario boots its own proxy** from its own cgroup, so the runtime posture and the admission budgets are the ones that pod shape resolves."
    , "- **memory.peak** is the kernel's high-water mark for the proxy's cgroup. **RTS max memory in use** is what the heap held, the figure `-M` is compared with. The gap is off-heap and kernel memory."
    , "- **The in-flight gauge** is `ecluse.serve.admission.in_flight`, sampled each second. Requests that hold admission without finishing show as a flat, nonzero gauge beside zero successes."
    ]

-- | Attribute concurrency-one service time against the named upstream baseline.
renderServiceTime :: BaselineSource -> [ScenarioReport] -> Text
renderServiceTime source reports =
    renderNormalised source [NormalisedRow (srName r) (pP50Ms (lat r)) (pP99Ms (lat r)) | r <- reports]
  where
    lat = lsLatency . srLoad

-- | Pair loaded reports with their concurrency-one counterparts to describe saturation.
renderLoadSaturation :: [ScenarioReport] -> [ScenarioReport] -> Text
renderLoadSaturation c1Reports loadedReports =
    renderSaturation queuingDominanceThreshold (map (deriveSaturation queuingDominanceThreshold . toInput) loadedReports)
  where
    c1ByName = Map.fromList [(srName r, r) | r <- c1Reports]
    toInput loaded =
        SaturationInput
            (srName loaded)
            (throughput (srLoad loaded))
            (lsDeadlineAborts (srLoad loaded))
            (pP50Ms . lsLatency . srLoad =<< Map.lookup (srName loaded) c1ByName)
            (pP50Ms (lsLatency (srLoad loaded)))

{- | The GC-thrash probe: one scenario at each memory limit, highest first. Reclaim per major
collection is absent: GHC.Stats records no promotion or pre-collection size to derive it from.
-}
renderThrash :: Text -> [(Text, Either Text ScenarioReport)] -> Text
renderThrash scenarioKey steps =
    T.unlines $
        [ "## GC-thrash probe: " <> scenarioKey
        , ""
        , "The load stays fixed while the memory limit steps down. An OOM kill or a heap overflow here is the probe's reading, not a failed run."
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

-- | The run's verdict: every broken invariant, or a line saying none broke.
renderVerdict :: [Text] -> Text
renderVerdict violations =
    T.unlines $
        ["## Verdict", ""]
            <> case violations of
                [] -> ["Every scenario had successful responses, and no proxy was OOM-killed or exited on heap overflow."]
                _ -> "**The run fails:**" : "" : map ("- " <>) violations

-- memory.peak less the RTS's own high-water mark: off-heap and kernel memory, and the overshoot
-- between collections.
offHeapGap :: ScenarioReport -> Maybe Int
offHeapGap r = do
    peak <- crMemoryPeak =<< pfCgroup =<< srProxy r
    inUse <- rsMaxMemInUseBytes <$> srRtsEnd r
    pure (peak - fromIntegral inUse)

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

allocPerSuccess :: ScenarioReport -> Maybe Double
allocPerSuccess r = do
    w <- srRtsWindow r
    perSuccess (fromIntegral (rwAllocatedBytes w)) (windowSuccesses r)

perAttempt :: ScenarioReport -> Maybe Double
perAttempt r = do
    w <- srRtsWindow r
    perSuccess (fromIntegral (rwAllocatedBytes w)) (windowAttempts r)

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

mib :: Word64 -> Text
mib = signedMib . fromIntegral

signedMib :: Int -> Text
signedMib bytes = fmt1 (fromIntegral bytes / (1024 * 1024)) <> " MiB"

fmt1, fmt2 :: Double -> Text
fmt1 x = toText (showFFloat (Just 1) x "")
fmt2 x = toText (showFFloat (Just 2) x "")
