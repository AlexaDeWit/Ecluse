-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Worker.Liveness (
    WorkerHeartbeat,
    newWorkerHeartbeat,
    recordPoll,
    lastPoll,
    workerHeartbeatStaleAfter,
    heartbeatHealthy,
    heartbeatHealthyNow,
) where

import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

{- | The mirror worker's consume-loop heartbeat: the wall-clock instant of its last
recorded progress, a successful poll or a completed job. It is the worker's own liveness
signal, kept apart from the server's HTTP readiness.
-}
newtype WorkerHeartbeat = WorkerHeartbeat (TVar (Maybe UTCTime))

{- | Build a fresh 'WorkerHeartbeat' with no poll yet recorded ('lastPoll' is
'Nothing' until the worker's first successful @receive@).
-}
newWorkerHeartbeat :: IO WorkerHeartbeat
newWorkerHeartbeat = WorkerHeartbeat <$> newTVarIO Nothing

{- | Stamp the heartbeat with the given instant, recording a unit of worker progress.
The worker calls it through 'Ecluse.Core.Worker.Types.recordWorkerProgress'.
-}
recordPoll :: WorkerHeartbeat -> UTCTime -> IO ()
recordPoll (WorkerHeartbeat var) now = atomically (writeTVar var (Just now))

{- | The instant of the worker's last recorded progress, a successful poll or a completed
job, or 'Nothing' before its first.
-}
lastPoll :: WorkerHeartbeat -> IO (Maybe UTCTime)
lastPoll (WorkerHeartbeat var) = readTVarIO var

{- | How long the worker's last recorded progress may be stale before the liveness probe
counts the loop as stalled.

The bound clears two 'Ecluse.Core.Worker.Job.workerPublishVisibilityBudget' spans (~300s
each, the fetch and the publish legs of one 512 MiB job) with headroom. A tighter bound
would kill a healthy pod mid-publish, and the redelivered jobs would stall the same way:
a self-inflicted restart loop. @Ecluse.Worker.LivenessSpec@ pins the two together.
-}
workerHeartbeatStaleAfter :: NominalDiffTime
workerHeartbeatStaleAfter = 660

{- | Whether the worker's consume loop is healthy as of @now@, given its last recorded
progress. The single-process @\/livez@ probe folds this in (see "Ecluse.Server"), apart
from HTTP readiness.

'Nothing' (no poll yet) is __healthy__: the worker is still starting, not stalled.

>>> import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
>>> let t0 = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)
>>> heartbeatHealthy t0 Nothing
True

>>> let now = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 10)
>>> heartbeatHealthy now (Just t0)
True

>>> let later = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 700)
>>> heartbeatHealthy later (Just t0)
False
-}
heartbeatHealthy :: UTCTime -> Maybe UTCTime -> Bool
heartbeatHealthy _ Nothing = True
heartbeatHealthy now (Just polledAt) = diffUTCTime now polledAt <= workerHeartbeatStaleAfter

{- | Read the worker heartbeat and decide liveness against the current wall clock: the
@IO@ wrapper the liveness probe calls.
-}
heartbeatHealthyNow :: WorkerHeartbeat -> IO Bool
heartbeatHealthyNow heartbeat = heartbeatHealthy <$> getCurrentTime <*> lastPoll heartbeat
