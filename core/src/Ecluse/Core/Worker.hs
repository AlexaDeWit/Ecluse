-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The mirror worker's public surface over the supervised loop that turns enqueued jobs
into mirrored packages.

The loop long-polls the demand-driven mirror queue ("Ecluse.Core.Queue") and resolves each
job's ecosystem bundle ('WorkerPolicy'). A job is fail-closed when its ecosystem carries no
bundle, and when its name is one the deployment owns: the queue outlives a namespace
declaration, so that privilege is read here before any public request. The per-job decision,
the digest gate, the receipt lease, verdict realisation and supervision live in the child
modules re-exported below. See @docs\/architecture\/cloud-backends.md@.
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
    RetryLeg (..),

    -- * Liveness
    WorkerHeartbeat,
    newWorkerHeartbeat,
    recordPoll,
    lastPoll,
    workerJobStepAllowance,
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
