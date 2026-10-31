-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Exact load-report labels and cells shared by the report the harness renders and the comparisons that read it back.
module Ecluse.BenchLoad.ReportLabels (
    loadRow,
    loadCell,
    parseLoadCell,
    latencyRow,
    latencyCell,
    parseLatencyCell,
    memoryPeakColumn,
    postureRow,
    admissionRow,
    memoryEventsRow,
    generatorHeadroomRow,
    headroomHeld,
    headroomSaturated,
) where

import Ecluse.BenchLoad.CpuUse (headroomLimit)

-- | The operating point row with the connections and the window length.
loadRow :: Text
loadRow = "load"

-- | The load row's value for these connections and this window length in seconds.
loadCell :: Int -> Int -> Text
loadCell connections seconds = show connections <> " connections x " <> show seconds <> " s (a scenario may scale its own connections)"

-- | The connections and the window length in seconds a load row's value states.
parseLoadCell :: Text -> Maybe (Int, Int)
parseLoadCell cell = case words cell of
    connections : "connections" : "x" : seconds : "s" : _ -> (,) <$> readMaybe (toString connections) <*> readMaybe (toString seconds)
    _ -> Nothing

-- | The operating point row with the latency every stub upstream injects.
latencyRow :: Text
latencyRow = "injected upstream latency"

-- | The latency row's value for this injected latency in microseconds.
latencyCell :: Int -> Text
latencyCell micros = show (micros `div` 1_000) <> "." <> show ((micros `mod` 1_000) `div` 100) <> " ms"

-- | The injected latency in whole milliseconds a latency row's value states.
parseLatencyCell :: Text -> Maybe Int
parseLatencyCell cell = case words cell of
    [value, "ms"] -> readMaybe (takeWhile (/= '.') (toString value))
    _ -> Nothing

-- | The at-a-glance column with the cgroup's @memory.peak@ and @memory.max@.
memoryPeakColumn :: Text
memoryPeakColumn = "memory peak / max"

-- | The scenario row with the RTS's heap ceiling, capabilities, and allocation area per capability.
postureRow :: Text
postureRow = "heap ceiling (-M) / proxy GHC capabilities / allocation area per capability"

-- | The scenario row with the serve admission and the memory admission budget the proxy logged at boot.
admissionRow :: Text
admissionRow = "serve admission (requests) / memory admission budget at boot"

-- | The scenario row with the cgroup's @memory.events@ counters.
memoryEventsRow :: Text
memoryEventsRow = "memory.events oom_kill / oom / max / high"

-- | The scenario row that says whether the load generator kept its CPU headroom.
generatorHeadroomRow :: Text
generatorHeadroomRow = "load generator headroom (harness cgroup and oha's CPU below " <> show (round (headroomLimit * 100) :: Int) <> "% busy)"

-- | The headroom row's value while the load generator kept its headroom.
headroomHeld :: Text
headroomHeld = "held"

-- | The headroom row's value, shown in bold, once the load generator reached the limit.
headroomSaturated :: Text
headroomSaturated = "saturated"
