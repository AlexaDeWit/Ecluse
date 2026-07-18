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

{- | The mirror worker's consume-loop heartbeat: the wall-clock time of the
worker's __last recorded progress__ -- a successful poll of the queue, or a
completed job.

It is the worker's own liveness signal, kept apart from the server's HTTP
readiness so single-process health reflects a stalled worker today and a future
standalone worker binary keeps the same probe. The worker advances it (via
'Ecluse.Core.Worker.Types.recordWorkerProgress') after each successful @receive@
(whether or not the batch was empty -- an empty long-poll is a healthy idle, not a
stall) and after each completed job, so a long batch of large artifacts cannot
starve it; a liveness probe reads 'lastPoll' and compares it against the wall clock
to decide whether the loop has gone quiet for too long.
-}
newtype WorkerHeartbeat = WorkerHeartbeat (TVar (Maybe UTCTime))

{- | Build a fresh 'WorkerHeartbeat' with no poll yet recorded ('lastPoll' is
'Nothing' until the worker's first successful @receive@).
-}
newWorkerHeartbeat :: IO WorkerHeartbeat
newWorkerHeartbeat = WorkerHeartbeat <$> newTVarIO Nothing

{- | Stamp the heartbeat with the given instant, recording a unit of worker
progress. The worker advances it (via 'Ecluse.Core.Worker.Types.recordWorkerProgress')
after each successful @receive@ -- the loop is alive even on an empty batch -- and
after each completed job, so a long batch of large artifacts cannot starve the signal.
-}
recordPoll :: WorkerHeartbeat -> UTCTime -> IO ()
recordPoll (WorkerHeartbeat var) now = atomically (writeTVar var (Just now))

{- | The instant of the worker's last recorded progress (a successful poll or a
completed job), or 'Nothing' before its first. A liveness probe reads this and
compares it against the wall clock.
-}
lastPoll :: WorkerHeartbeat -> IO (Maybe UTCTime)
lastPoll (WorkerHeartbeat var) = readTVarIO var

{- | How long the worker's last recorded progress may be stale before the loop is
considered stalled -- the staleness threshold the liveness probe applies.

The worker records progress on two events (see
'Ecluse.Core.Worker.Types.recordWorkerProgress'): each successful poll and each
__completed job__. The threshold must clear the larger of the two gaps. The idle
gap is small -- a healthy idle worker completes a poll at least every SQS long-poll
window (@sqsWaitSeconds@, ≤ 20s by default). The busy gap is the binding one: a
single job can legitimately run a fetch and then a publish of the maximum 512 MiB
artifact (the @workerArtifactLimits@ fetch cap), and each transfer is budgeted at the
publish-visibility floor
('Ecluse.Core.Worker.Job.workerPublishVisibilityBudget', ~300s for 512 MiB over a
conservative ~2 MiB/s link). One healthy job therefore runs for up to about two such
budgets before its heartbeat next advances.

Set above that two-budget sum (with headroom for the bounded probe, metadata
re-fetch, and integrity hashing between the legs) so a healthy worker mid-large-publish
is never mistaken for a stalled one. Advancing the heartbeat only once per batch under
a 120s bound previously flagged such a worker dead, so an orchestrator liveness probe
killed the pod mid-publish and the un-acked jobs redelivered into the identical stall:
a self-inflicted restart loop. @Ecluse.Worker.LivenessSpec@ pins the relationship to
'Ecluse.Core.Worker.Job.workerPublishVisibilityBudget' so the two budgets cannot drift.
-}
workerHeartbeatStaleAfter :: NominalDiffTime
workerHeartbeatStaleAfter = 660

{- | Whether the worker's consume loop is healthy as of @now@, given its last
successful poll. This is the liveness signal the single-process @\/livez@ probe
folds in (see "Ecluse.Server"), distinct from HTTP readiness.

* 'Nothing' (no poll yet) is __healthy__: the worker is still starting, not stalled.
* A poll within 'workerHeartbeatStaleAfter' is healthy.
* A poll older than that is __unhealthy__: the loop has gone quiet for too long.

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

{- | Read the worker heartbeat and decide liveness against the current wall clock --
the @IO@ wrapper the liveness probe calls. 'True' while the consume loop is alive
(or still starting); 'False' once the last successful poll is staler than
'workerHeartbeatStaleAfter'.
-}
heartbeatHealthyNow :: WorkerHeartbeat -> IO Bool
heartbeatHealthyNow heartbeat = heartbeatHealthy <$> getCurrentTime <*> lastPoll heartbeat
