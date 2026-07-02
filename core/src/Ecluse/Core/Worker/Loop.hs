{- | Loop robustness and supervision for the worker.

The loop is wrapped so a single bad iteration cannot kill the worker thread: a
transient @receive@ / fetch / publish error, or an undecodable body, is caught,
logged, and the loop backs off and continues. (Job-level "retry is don't ack" is a
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

import Control.Monad (forever)
import Control.Monad.Reader (asks)
import Data.List (foldl')
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Katip (Severity (DebugS, ErrorS), logFM, ls)
import UnliftIO (tryAny)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Queue (MirrorQueue (receive))
import Ecluse.Core.Worker.Job (displayExceptionT, processBatch)
import Ecluse.Core.Worker.Liveness (recordPoll)
import Ecluse.Core.Worker.Types

{- | The continuous consume loop: long-poll for a batch, process it, repeat.

Each iteration is wrapped so a single failure -- a @receive@ that throws, a fetch or
publish error, an undecodable body -- is caught and logged, then the loop backs off
briefly and continues, so one bad iteration cannot kill the worker thread. A
successful poll advances the heartbeat (whether or not the batch was empty), so a
liveness probe sees the loop is alive; an idle queue is a healthy empty poll, not a
stall.
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
        messages <- liftIO (receive queue)
        case messages of
            [] -> pass
            _ -> logFM DebugS (ls ("worker received " <> show (foldl' (\n _ -> n + 1) (0 :: Int) messages) <> " messages" :: Text))
        -- Heartbeat on every successful poll -- an empty long-poll is a healthy idle.
        heartbeat <- asks wrHeartbeat
        now <- liftIO getCurrentTime
        liftIO (recordPoll heartbeat now)
        processBatch messages

-- The fixed pause after a failed iteration, so a persistently failing dependency
-- (queue, upstream) is retried at a bounded rate rather than hot-looping.
backoff :: WorkerM ()
backoff = threadDelay 1_000_000
