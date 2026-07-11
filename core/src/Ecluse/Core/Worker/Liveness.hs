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
worker's __last successful poll__ of the queue.

It is the worker's own liveness signal, kept apart from the server's HTTP
readiness so single-process health reflects a stalled worker today and a future
standalone worker binary keeps the same probe. The worker 'recordPoll's after each
successful @receive@ (whether or not the batch was empty -- an empty long-poll is a
healthy idle, not a stall); a liveness probe reads 'lastPoll' and compares it
against the wall clock to decide whether the loop has gone quiet for too long.
-}
newtype WorkerHeartbeat = WorkerHeartbeat (TVar (Maybe UTCTime))

{- | Build a fresh 'WorkerHeartbeat' with no poll yet recorded ('lastPoll' is
'Nothing' until the worker's first successful @receive@).
-}
newWorkerHeartbeat :: IO WorkerHeartbeat
newWorkerHeartbeat = WorkerHeartbeat <$> newTVarIO Nothing

{- | Record the time of a successful queue poll, advancing the heartbeat. Called
by the worker after each @receive@ returns (the loop is alive even on an empty
batch).
-}
recordPoll :: WorkerHeartbeat -> UTCTime -> IO ()
recordPoll (WorkerHeartbeat var) now = atomically (writeTVar var (Just now))

{- | The time of the worker's last successful poll, or 'Nothing' before its first.
A liveness probe reads this and compares it against the wall clock.
-}
lastPoll :: WorkerHeartbeat -> IO (Maybe UTCTime)
lastPoll (WorkerHeartbeat var) = readTVarIO var

{- | How long the worker's last successful poll may be stale before the loop is
considered stalled -- the staleness threshold the liveness probe applies.

It is a generous multiple of the long-poll cadence: a healthy idle worker still
completes a poll at least every SQS long-poll window (@sqsWaitSeconds@, ≤ 20s by
default), so a gap several times that is a genuine stall, not an idle queue. Set
well above one poll window so liveness never flaps on normal scheduling jitter.
-}
workerHeartbeatStaleAfter :: NominalDiffTime
workerHeartbeatStaleAfter = 120

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

>>> let later = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 300)
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
