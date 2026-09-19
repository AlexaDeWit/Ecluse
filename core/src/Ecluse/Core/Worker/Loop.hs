-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Supervision for the worker's consume loop. A failed @receive@ arrives as the queue
handle's typed fault value, which the step logs and backs off from at its own pacing. Residue,
an exception escaping a dependency's typed contract, is 'superviseLoop''s concern under the
caller's policy.

Shutdown cancels the loop thread, and an un-acked in-flight message simply redelivers, which
is safe because publishing is idempotent.
-}
module Ecluse.Core.Worker.Loop (
    workerLoop,
) where

import Katip (Severity (DebugS, WarningS), logFM, ls)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Fault (TransportFault, tfDetail)
import Ecluse.Core.Queue (MirrorQueue (receive), QueueMessage)
import Ecluse.Core.Supervision (SupervisionPolicy, superviseLoop)
import Ecluse.Core.Worker.Realise (processBatch)
import Ecluse.Core.Worker.Types

{- | The continuous consume loop: long-poll, process, repeat, under the supervision policy. The
heartbeat advances only on progress, so a persistently faulting @receive@ goes stale on @\/livez@.
-}
workerLoop :: SupervisionPolicy -> WorkerM Void
workerLoop policy = superviseLoop policy pollAndProcess

pollAndProcess :: WorkerM ()
pollAndProcess = do
    queue <- asks wrQueue
    liftIO (receive queue) >>= either backOffFrom processPolled

-- No heartbeat advance: the loop is retrying, not healthy-idle, so a persistent fault
-- escalates on @\/livez@ rather than here.
backOffFrom :: TransportFault -> WorkerM ()
backOffFrom fault = do
    logFM WarningS (ls ("worker receive failed, backing off: " <> tfDetail fault))
    backoff

-- Beat on every successful poll: an empty long-poll is a healthy idle. 'processBatch' beats
-- again after each job, so a long batch cannot starve it.
processPolled :: [QueueMessage] -> WorkerM ()
processPolled messages = do
    unless (null messages) $
        logFM DebugS (ls ("worker received " <> show (length messages) <> " messages" :: Text))
    recordWorkerProgress
    processBatch messages

-- The fixed pause after a faulted poll, so the loop retries a persistently failing
-- queue backend at a bounded rate rather than hot-looping.
backoff :: WorkerM ()
backoff = threadDelay 1_000_000
