{- | Loop robustness and supervision for the worker.

The loop cannot be killed by a single bad iteration: a failed @receive@ arrives as
the queue handle's typed fault value and is logged and backed off, and any residue
an iteration still throws is caught, logged, and backed off the same way, so the
loop always continues. (Job-level "retry is don't ack" is a
separate concern -- it governs whether one message redelivers; it does not protect
the loop, since an escaping exception would still tear the thread down.) The
composition root holds the worker under @concurrently_@ alongside the server, so a
genuinely fatal error propagates and takes the process down (fail-stop), while
transient faults self-recover here. A successful poll advances the 'WorkerHeartbeat',
so a stalled loop is visible to the liveness probe.

Shutdown tears the loop down cleanly: the composition root runs it under
@concurrently_@ within its resource bracket, so process teardown cancels the loop
thread and an in-flight, un-acked message simply redelivers -- safe, because
publishing is idempotent (a version already present is success).
-}
module Ecluse.Core.Worker.Loop (
    workerLoop,
) where

import Data.Time (getCurrentTime)
import Katip (Severity (DebugS, ErrorS), logFM, ls)
import UnliftIO (tryAny)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Queue (MirrorQueue (receive), qfDetail)
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Core.Worker.Job (processBatch)
import Ecluse.Core.Worker.Liveness (recordPoll)
import Ecluse.Core.Worker.Types

{- | The continuous consume loop: long-poll for a batch, process it, repeat.

A failed poll arrives as the handle's typed 'Ecluse.Core.Queue.QueueFault' value:
it is logged and the loop backs off and polls again, so a queue outage cannot kill
the worker thread. Each iteration is additionally wrapped so residue -- an
exception escaping a dependency's typed contract mid-batch -- is caught and logged
with the same backoff, rather than tearing the thread down. A successful poll
advances the heartbeat (whether or not the batch was empty), so a liveness probe
sees the loop is alive; an idle queue is a healthy empty poll, not a stall. The
heartbeat advances only on a successful @receive@, so a worker that cannot poll at
all (a persistently faulting @receive@) keeps retrying but never advances it: the
heartbeat goes stale and @\/livez@ fails, surfacing a fully-dead worker for the
orchestrator to restart.
-}
workerLoop :: WorkerM ()
workerLoop = forever $ do
    outcome <- tryAny pollAndProcess
    whenLeft_ outcome $ \err -> do
        logFM ErrorS (ls ("worker iteration failed, backing off: " <> displayExceptionT err))
        backoff
  where
    pollAndProcess :: WorkerM ()
    pollAndProcess = do
        queue <- asks wrQueue
        liftIO (receive queue) >>= \case
            Left fault -> do
                -- A failed poll: no heartbeat advance (the loop is retrying, not
                -- healthy-idle), log the typed fault, and back off before the next
                -- poll so a dead backend is retried at a bounded rate.
                logFM ErrorS (ls ("worker receive failed, backing off: " <> qfDetail fault))
                backoff
            Right messages -> do
                case messages of
                    [] -> pass
                    _ -> logFM DebugS (ls ("worker received " <> show (length messages) <> " messages" :: Text))
                -- Heartbeat on every successful poll -- an empty long-poll is a healthy idle.
                heartbeat <- asks wrHeartbeat
                now <- liftIO getCurrentTime
                liftIO (recordPoll heartbeat now)
                processBatch messages

-- The fixed pause after a failed iteration, so a persistently failing dependency
-- (queue, upstream) is retried at a bounded rate rather than hot-looping.
backoff :: WorkerM ()
backoff = threadDelay 1_000_000
