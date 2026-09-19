-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The log-to-trace correlation glue: read the active OpenTelemetry span off the ambient
context and stamp its ids onto the @dd@ log object ("Ecluse.Runtime.Log"), so a reader joins a
JSONL line to the trace it was emitted within. This is the IO half "Ecluse.Runtime.Log" defers,
which is why that module needs no OpenTelemetry dependency. The ids take
@hs-opentelemetry-propagator-datadog@'s form: the unsigned decimal of the low 64 bits,
big-endian. No span in scope, or one whose context is not valid, contributes no ids, so a line
never carries a meaningless all-zero trace id. The identity still stamps it. "Ecluse.Runtime.Telemetry.Correlation.Internal" implements it.
-}
module Ecluse.Runtime.Telemetry.Correlation (
    -- * Identity
    ddIdentity,
    ddIdentityFromEnvironment,

    -- * Active-span correlation
    ddPayloadNow,
) where

import Ecluse.Runtime.Telemetry.Correlation.Internal (
    ddIdentity,
    ddIdentityFromEnvironment,
    ddPayloadNow,
 )
