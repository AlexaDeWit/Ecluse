-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The mirror worker heartbeat behind @\/livez@, shared by embedded and dedicated workers.
Startup and later stalls use the same allowance. Roles without a worker use 'alwaysLive'.
-}
module Ecluse.Core.Worker.Liveness (
    WorkerHeartbeat,
    newWorkerHeartbeat,
    newWorkerHeartbeatWithClock,
    recordPoll,
    lastPoll,
    workerHeartbeatStaleAfter,
    heartbeatHealthy,
    Liveness (..),
    alwaysLive,
    heartbeatLivenessNow,
) where

import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

-- | Worker progress and its startup allowance, separate from HTTP readiness.
data WorkerHeartbeat = WorkerHeartbeat
    { whStartedAt :: UTCTime
    , whNow :: IO UTCTime
    , whLastPoll :: TVar (Maybe UTCTime)
    }

{- | Build a fresh 'WorkerHeartbeat' with no poll yet recorded ('lastPoll' is
'Nothing' until the worker's first successful @receive@).
-}
newWorkerHeartbeat :: IO WorkerHeartbeat
newWorkerHeartbeat = newWorkerHeartbeatWithClock getCurrentTime

-- | Start the allowance at the supplied clock, which also drives liveness probes.
newWorkerHeartbeatWithClock :: IO UTCTime -> IO WorkerHeartbeat
newWorkerHeartbeatWithClock clock = do
    startedAt <- clock
    var <- newTVarIO Nothing
    pure WorkerHeartbeat{whStartedAt = startedAt, whNow = clock, whLastPoll = var}

{- | Stamp the heartbeat with the given instant, recording a unit of worker progress.
The worker calls it through 'Ecluse.Core.Worker.Types.recordWorkerProgress'.
-}
recordPoll :: WorkerHeartbeat -> UTCTime -> IO ()
recordPoll heartbeat now = atomically (writeTVar (whLastPoll heartbeat) (Just now))

{- | The instant of the worker's last recorded progress, a successful poll or a completed
job, or 'Nothing' before its first.
-}
lastPoll :: WorkerHeartbeat -> IO (Maybe UTCTime)
lastPoll = readTVarIO . whLastPoll

{- | Startup and progress allowance, exceeding two 'Ecluse.Core.Worker.Job.workerPublishVisibilityBudget'
spans so a fetch followed by a publish does not trigger a restart.
-}
workerHeartbeatStaleAfter :: NominalDiffTime
workerHeartbeatStaleAfter = 660

-- | Judge progress at @now@, using startup time only until the first successful progress.
heartbeatHealthy :: UTCTime -> UTCTime -> Maybe UTCTime -> Bool
heartbeatHealthy now startedAt polledAt =
    diffUTCTime now (fromMaybe startedAt polledAt) <= workerHeartbeatStaleAfter

{- | What @\/livez@ answers from: the health verdict, plus the instant the checked loop last
recorded progress so an orchestrator can judge staleness rather than only pass or fail.
-}
data Liveness = Liveness
    { liveHealthy :: Bool
    , liveLastPoll :: Maybe UTCTime
    -- ^ 'Nothing' before the loop's first poll, and for a role that runs no such loop.
    }
    deriving stock (Eq, Show)

-- | The verdict of a role with no background loop to stall: live, with nothing to report.
alwaysLive :: Liveness
alwaysLive = Liveness{liveHealthy = True, liveLastPoll = Nothing}

{- | Read the worker heartbeat and judge it against the current wall clock, keeping the
instant judged. Both the embedded and the dedicated worker answer @\/livez@ through this.
-}
heartbeatLivenessNow :: WorkerHeartbeat -> IO Liveness
heartbeatLivenessNow heartbeat = do
    now <- whNow heartbeat
    polledAt <- lastPoll heartbeat
    pure Liveness{liveHealthy = heartbeatHealthy now (whStartedAt heartbeat) polledAt, liveLastPoll = polledAt}
