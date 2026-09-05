-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The mirror worker: the supervised consume loop that turns enqueued jobs into
mirrored packages.

The worker is the consumer end of the demand-driven mirror queue (see
"Ecluse.Core.Queue"). The consume loop long-polls the queue and resolves each received
job's __ecosystem bundle__ ('WorkerPolicy', keyed by the job's own ecosystem). A job
whose ecosystem carries no bundle is fail-closed, and so is one for a name the deployment
owns ('wpFirstParty'): the queue outlives a namespace declaration, so the privilege is read
here before any public request. Otherwise, through that bundle the loop:

1. __probes__ the mirror target for the job's version and acks a confirmed-present
   duplicate outright. Demand-driven enqueue means a fleet-wide install of a novel
   version enqueues many jobs for it, and only the first has work to do.
2. __re-evaluates current policy__ for the version through the same rules and
   single-version fetch the serve path gates with. It drops a version denied since
   its serve-time admit rather than mirroring it.
3. fetches the artifact bytes from the public upstream named on the job.
4. __verifies__ those bytes against the integrity digests of the artifact the
   re-evaluation re-admitted. That is the floor-checked, current-metadata set, since
   the queue payload carries no digest at all.
5. assembles the ecosystem's publish document from the re-admitted artifact's
   descriptor and publishes it to the mirror target. That is the bundle's married
   publish capability, resolved at the composition root with the bearer from the
   "Ecluse.Core.Credential" provider.
6. acknowledges the job.

See individual modules for detailed behaviour:
* "Ecluse.Core.Worker.Integrity" for the security gate on artifact digests.
* "Ecluse.Core.Worker.Loop" for supervision and graceful shutdown.
* "Ecluse.Core.Worker.Job" for the per-job decision within the visibility budget.
* "Ecluse.Core.Worker.Realise" for realising each verdict at the queue handle.

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
    workerPublishVisibilityBudget,

    -- * Liveness
    WorkerHeartbeat,
    newWorkerHeartbeat,
    recordPoll,
    lastPoll,
    workerHeartbeatStaleAfter,
    heartbeatHealthy,
    Liveness (..),
    alwaysLive,
    heartbeatLivenessNow,

    -- * Integrity verification
    IntegrityResult (..),
    verifyIntegrity,
) where

import Ecluse.Core.Worker.Integrity
import Ecluse.Core.Worker.Job
import Ecluse.Core.Worker.Liveness
import Ecluse.Core.Worker.Loop
import Ecluse.Core.Worker.Realise
import Ecluse.Core.Worker.Types
