-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Reads of the OTLP collector's debug-exporter output, which prints every signal it receives at
detailed verbosity. A case that needs one span's own attributes reads the output as spans, because
an attribute is evidence only beside the name and the other attributes of the span that carries it.
-}
module Ecluse.E2E.Harness.Collector (
    awaitCollectorLog,
    awaitCollectorSpans,

    -- * Printed spans
    module Ecluse.Test.Collector,
) where

import Ecluse.E2E.Harness.Docker (awaitContainerLog, containerLogs)
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Collector
import Ecluse.Test.Poll (pollUntil)

{- | Poll the OTLP collector's debug-exporter output until the predicate holds. It fails loudly when
the environment booted without a collector, which only @ecCollector = True@ provides.
-}
awaitCollectorLog :: E2E -> (Text -> Bool) -> Int -> IO Bool
awaitCollectorLog e2e matches attempts = do
    collector <- collectorContainer e2e
    awaitContainerLog collector matches attempts

{- | Poll the collector's printed spans until the predicate holds, at the pace of
'awaitCollectorLog'. It yields the spans it last read, so a failure shows what the collector held.
-}
awaitCollectorSpans :: E2E -> ([ExportedSpan] -> Bool) -> Int -> IO [ExportedSpan]
awaitCollectorSpans e2e matches attempts = do
    collector <- collectorContainer e2e
    pollUntil attempts 250000 matches (exportedSpans <$> containerLogs collector)

collectorContainer :: E2E -> IO String
collectorContainer =
    maybe (fail "this environment was booted without a collector") pure . e2eCollectorContainer
