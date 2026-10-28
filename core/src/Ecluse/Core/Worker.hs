-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Mirror worker entry points and supervision.
Queued jobs receive current policy and first-party checks before any public fetch.
The queue can outlive the deployment's first-party declarations.
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

import Ecluse.Core.Package.Integrity (IntegrityResult (..), verifyIntegrity)
import Ecluse.Core.Worker.Job
import Ecluse.Core.Worker.Liveness
import Ecluse.Core.Worker.Loop
import Ecluse.Core.Worker.Realise
import Ecluse.Core.Worker.Types
