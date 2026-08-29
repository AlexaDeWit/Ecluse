-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The liveness vocabulary behind @\/livez@: the mirror worker's consume-loop heartbeat, the
staleness rule read against it, and the 'Liveness' verdict a probe renders.

A process runs the worker or it does not ("Ecluse.Composition.MirrorRole" decides), so the
verdict has two sources: 'heartbeatLivenessNow' where a loop runs, 'alwaysLive' where none
does. Both carry the last recorded progress, so a probe can report staleness as well as
pass or fail.
-}
module Ecluse.Core.Worker.Liveness (
    WorkerHeartbeat,
    newWorkerHeartbeat,
    recordPoll,
    lastPoll,
    workerHeartbeatStaleAfter,
    heartbeatHealthy,
    Liveness (..),
    alwaysLive,
    heartbeatLivenessNow,
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

{- | How long the worker's last recorded progress may be stale before the liveness probe counts
the loop as stalled. It clears two 'Ecluse.Core.Worker.Job.workerPublishVisibilityBudget'
spans with headroom, because a tighter bound would kill a healthy pod mid-publish and the
redelivered jobs would stall the same way.
-}
workerHeartbeatStaleAfter :: NominalDiffTime
workerHeartbeatStaleAfter = 660

{- | Whether the worker's consume loop is healthy as of @now@, given its last recorded
progress. The @\/livez@ probe of a role that runs the worker folds this in (see
"Ecluse.Runtime.Server"), apart from HTTP readiness.

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
    now <- getCurrentTime
    polledAt <- lastPoll heartbeat
    pure Liveness{liveHealthy = heartbeatHealthy now polledAt, liveLastPoll = polledAt}
