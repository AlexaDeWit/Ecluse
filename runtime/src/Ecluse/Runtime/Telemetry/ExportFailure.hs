-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Route OpenTelemetry export failures into @katip@ through one shared throttle, so a
persistently unreachable collector is one visible warning and a periodic heartbeat rather than a
per-flush flood. The span exporter, the metric exporter, and the SDK's own diagnostic stream all
coalesce through the same sink. None of it reaches the request path: the SDK's batch exporter runs
asynchronously, so a failed export never touches a served request. The exporter wrappers in
"Ecluse.Runtime.Telemetry" feed the sink through 'observeExportResult'.
"Ecluse.Runtime.Telemetry.ExportFailure.Internal" implements it.
-}
module Ecluse.Runtime.Telemetry.ExportFailure (
    ExportFailureSink,
    exportFailureSink,
    observeExportResult,
    installExportErrorHandler,
) where

import Ecluse.Runtime.Telemetry.ExportFailure.Internal (
    ExportFailureSink,
    exportFailureSink,
    installExportErrorHandler,
    observeExportResult,
 )
