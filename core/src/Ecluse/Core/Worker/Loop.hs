{- | Loop robustness and supervision for the worker.

The loop cannot be killed by a single bad iteration. A failed @receive@ arrives
as the queue handle's typed fault value: the step logs it and backs off (its own
fixed pacing over the typed channel). Residue -- an exception escaping a
dependency's typed contract mid-iteration -- is the supervision combinator's
concern ('Ecluse.Core.Supervision.superviseLoop' wraps the step under the
caller-supplied policy), classified per that policy: transient residue is logged
and retried with bounded exponential backoff, while a wiring fault the policy
names 'Ecluse.Core.Supervision.Permanent' fails up through the composition
root's race and takes the process down (fail-stop). A successful poll advances
the 'WorkerHeartbeat', so a stalled loop is visible to the liveness probe.

Shutdown tears the loop down cleanly: the composition root runs it raced against
the server within its resource bracket, so process teardown cancels the loop
thread (the combinator never catches cancellation) and an in-flight, un-acked
message simply redelivers -- safe, because publishing is idempotent (a version
already present is success).
-}
module Ecluse.Core.Worker.Loop (
    workerLoop,
) where

import Data.Time (getCurrentTime)
import Katip (Severity (DebugS, ErrorS), logFM, ls)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Queue (MirrorQueue (receive), qfDetail)
import Ecluse.Core.Supervision (SupervisionPolicy, superviseLoop)
import Ecluse.Core.Worker.Job (processBatch)
import Ecluse.Core.Worker.Liveness (recordPoll)
import Ecluse.Core.Worker.Types

{- | The continuous consume loop: long-poll for a batch, process it, repeat,
supervised under the given policy (the composition root names the wiring faults
that must fail up rather than retry; tests inject their own).

A failed poll arrives as the handle's typed 'Ecluse.Core.Queue.QueueFault' value:
it is logged and the step backs off and polls again, so a queue outage cannot
kill the worker thread. A successful poll advances the heartbeat (whether or not
the batch was empty), so a liveness probe sees the loop is alive; an idle queue
is a healthy empty poll, not a stall. The heartbeat advances only on a successful
@receive@, so a worker that cannot poll at all (a persistently faulting
@receive@) keeps retrying but never advances it: the heartbeat goes stale and
@\/livez@ fails, surfacing a fully-dead worker for the orchestrator to restart.
-}
workerLoop :: SupervisionPolicy -> WorkerM Void
workerLoop policy = superviseLoop policy pollAndProcess
  where
    pollAndProcess :: WorkerM ()
    pollAndProcess = do
        queue <- asks wrQueue
        liftIO (receive queue) >>= \case
            Left fault -> do
                -- A failed poll: no heartbeat advance (the loop is retrying, not
                -- healthy-idle), log the typed fault, and back off before the next
                -- poll so a dead backend is retried at a bounded rate. This is the
                -- step's own pacing over the typed channel; the supervisor's
                -- exponential backoff paces only residue.
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

-- The fixed pause after a faulted poll, so a persistently failing queue backend
-- is retried at a bounded rate rather than hot-looping.
backoff :: WorkerM ()
backoff = threadDelay 1_000_000
