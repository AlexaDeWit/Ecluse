-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Exact load-report labels shared by the report the harness renders and the comparisons that read it back.
module Ecluse.BenchLoad.ReportLabels (
    memoryPeakColumn,
    postureRow,
    admissionRow,
    memoryEventsRow,
    generatorHeadroomRow,
    headroomHeld,
    headroomSaturated,
) where

import Ecluse.BenchLoad.CpuUse (headroomLimit)

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

-- | The headroom row's value once the load generator reached the limit.
headroomSaturated :: Text
headroomSaturated = "**saturated**"
