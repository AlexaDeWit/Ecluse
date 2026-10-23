-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Synchronous metric records batched per capability, so a request thread does not queue on the
SDK's single storage cell. A flush gives the SDK one summed delta per counter series, the newest
value per gauge series, and every histogram sample in recording order, each under the span it was
recorded in, so exemplars keep their trace. Flushes run every second and before every collection,
the reader's last at shutdown included, so an export sees every record made before it. Observable
and Double instruments pass straight through, and the SDK stays the aggregator and exporter.
-}
module Ecluse.Runtime.Telemetry.Batched (
    batchMeterProvider,
) where

import Ecluse.Runtime.Telemetry.Batched.Internal (batchMeterProvider)
