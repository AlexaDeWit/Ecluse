-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Failure handling and supervision for the worker's consume loop.

A single bad iteration cannot kill the loop. A failed @receive@ arrives as the queue
handle's typed fault value. The step logs it and backs off, its own fixed pacing over
the typed channel. Residue, an exception escaping a dependency's typed contract
mid-iteration, is the supervision combinator's concern.
'Ecluse.Core.Supervision.superviseLoop' wraps the step under the caller-supplied
policy and classifies residue by that policy. The combinator logs transient residue
and retries it with bounded exponential backoff. A wiring fault the policy names
'Ecluse.Core.Supervision.Permanent' fails up through the composition root's race and
takes the process down (fail-stop). Each successful poll and each completed job
advances the 'WorkerHeartbeat', so the liveness probe can see a stalled loop.

Shutdown tears the loop down cleanly. The composition root runs it raced against the
server within its resource bracket. Process teardown therefore cancels the loop
thread, and the combinator never catches cancellation. An in-flight, un-acked message
simply redelivers, which is safe because publishing is idempotent: a version already
present is success.
-}
module Ecluse.Core.Worker.Loop (
    workerLoop,
) where

import Katip (Severity (DebugS, ErrorS), logFM, ls)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Queue (MirrorQueue (receive), qfDetail)
import Ecluse.Core.Supervision (SupervisionPolicy, superviseLoop)
import Ecluse.Core.Worker.Job (processBatch)
import Ecluse.Core.Worker.Types

{- | The continuous consume loop: long-poll for a batch, process it, repeat, under the
given supervision policy. The composition root names the wiring faults that must fail
up rather than retry, and a test injects its own.

A failed poll arrives as the handle's typed 'Ecluse.Core.Queue.QueueFault' value. The
step logs it, backs off, and polls again, so a queue outage cannot kill the worker
thread. A successful poll advances the heartbeat, whether or not the batch was empty,
and 'processBatch' advances it again after each completed job. A liveness probe
therefore sees the loop alive even while a healthy worker grinds through a long batch
of large artifacts. An idle queue reads as a healthy empty poll, not a stall.

The heartbeat advances only on demonstrated progress, a successful @receive@ or a
completed job. A worker that cannot poll at all, on a persistently faulting @receive@,
keeps retrying but never advances the heartbeat. The heartbeat then goes stale and
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
                -- poll. The backoff retries a dead backend at a bounded rate. This is
                -- the step's own pacing over the typed channel. The supervisor's
                -- exponential backoff paces only residue.
                logFM ErrorS (ls ("worker receive failed, backing off: " <> qfDetail fault))
                backoff
            Right messages -> do
                case messages of
                    [] -> pass
                    _ -> logFM DebugS (ls ("worker received " <> show (length messages) <> " messages" :: Text))
                -- Beat on every successful poll: an empty long-poll is a healthy idle.
                -- 'processBatch' beats again after each job, so a long batch cannot starve it.
                recordWorkerProgress
                processBatch messages

-- The fixed pause after a faulted poll, so the loop retries a persistently failing
-- queue backend at a bounded rate rather than hot-looping.
backoff :: WorkerM ()
backoff = threadDelay 1_000_000
