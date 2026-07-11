-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The mirror worker: the supervised consume loop that turns enqueued jobs into
mirrored packages.

The worker is the consumer end of the demand-driven mirror queue (see
"Ecluse.Core.Queue"). The consume loop long-polls the queue, and for each received job:

1. __probes__ the mirror target for the job's version, acking a confirmed-present
   duplicate outright (demand-driven enqueue means a fleet-wide install of a novel
   version enqueues many jobs for it; only the first has work to do),
2. __re-evaluates current policy__ for the version through the same rules and
   single-version fetch the serve path gates with, so a version denied since its
   serve-time admit is dropped rather than mirrored,
3. fetches the artifact bytes from the public upstream named on the job,
4. __verifies__ those bytes against the integrity digests of the artifact the
   re-evaluation re-admitted (the floor-checked, current-metadata set; the queue
   payload contributes no digest to the gate),
5. assembles the npm publish document and publishes it to the mirror target (the
   publish-side registry handle on the 'WorkerRuntime', resolved at the composition
   root with the bearer from the "Ecluse.Core.Credential" provider), and
6. acknowledges the job.

See individual modules for detailed behaviour:
* "Ecluse.Core.Worker.Integrity" for the security gate on artifact digests.
* "Ecluse.Core.Worker.Loop" for supervision and graceful shutdown.
* "Ecluse.Core.Worker.Job" for ack semantics within the visibility budget.

See @docs\/architecture\/cloud-backends.md@ → "Mirror Queue" and "Process model".
-}
module Ecluse.Core.Worker (
    -- * Worker runtime
    WorkerRuntime (..),

    -- * Per-ecosystem ingest re-evaluation
    WorkerPolicy (..),
    WorkerPolicies,

    -- * The worker monad
    WorkerM,
    runWorkerM,

    -- * Loop and job processing (exposed for direct testing)
    workerLoop,
    processBatch,
    processJob,
    JobOutcome (..),

    -- * Liveness
    WorkerHeartbeat,
    newWorkerHeartbeat,
    recordPoll,
    lastPoll,
    workerHeartbeatStaleAfter,
    heartbeatHealthy,
    heartbeatHealthyNow,

    -- * Integrity verification
    IntegrityResult (..),
    verifyIntegrity,
) where

import Ecluse.Core.Worker.Integrity
import Ecluse.Core.Worker.Job
import Ecluse.Core.Worker.Liveness
import Ecluse.Core.Worker.Loop
import Ecluse.Core.Worker.Types
