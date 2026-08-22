-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The STM-backed in-memory 'MirrorQueue': the bounded, best-effort production
backend mirroring rolls over to when no @ECLUSE_QUEUE__URL@ is set.

It honours the handle's contract (see "Ecluse.Core.Queue" for the @enqueue@ \/
don't-@ack@-to-retry \/ no-@nack@ conventions) and uses the contract module's backend
building blocks. See 'newBoundedInMemoryQueue' for why it is correctness-safe (the
next demand re-enqueues a dropped job) and why it deliberately does not redeliver.
-}
module Ecluse.Core.Queue.Memory (
    -- * Bounded in-memory production backend
    MemoryQueueConfig (..),
    defaultMemoryQueueConfig,
    newBoundedInMemoryQueue,
    memoryQueueBatchSize,
    memoryQueueDropReportInterval,
) where

import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, readTBQueue, tryReadTBQueue)
import System.Timeout (timeout)

import Ecluse.Core.Queue (
    DeadLetterTerminus (TerminusAbsent),
    MirrorJob,
    MirrorQueue (..),
    QueueMessage (..),
    defaultDeliveryBudget,
    mkReceiptHandle,
    reportWorthy,
    writeOrDrop,
 )

{- | What the bounded in-memory backend needs: its depth cap and its idle-poll
window. A record (like the SQS backend's @SqsConfig@) so each knob has a name rather
than being a bare 'Int'. Build it with 'defaultMemoryQueueConfig' for the production
poll window.
-}
data MemoryQueueConfig = MemoryQueueConfig
    { memQueueMaxDepth :: Int
    {- ^ The maximum number of jobs the queue holds. A fresh 'enqueue' past this cap
    is dropped-newest, which rejects that enqueue. A dropped job is safe, since the
    next demand re-enqueues it. Must be positive (the config layer enforces it).
    -}
    , memQueuePollWaitMicros :: Int
    {- ^ The idle long-poll window in microseconds: how long a 'receive' waits for a
    job before returning @[]@ (an empty, healthy poll). Bounds the idle wait so the
    worker's liveness heartbeat keeps advancing. See 'newBoundedInMemoryQueue'.
    -}
    }
    deriving stock (Eq, Show)

{- | A 'MemoryQueueConfig' for a given depth cap, with the idle-poll window at its
production default of @20s@. That mirrors the SQS long-poll cadence (the SQS backend's
@defaultSqsConfig@) and sits comfortably under the worker's heartbeat-staleness budget
('Ecluse.Core.Worker.workerHeartbeatStaleAfter'). An idle 'receive' therefore returns
a healthy empty poll long before @\/livez@ would flag the loop stalled. The depth cap
stays the operator-tunable knob. The poll window is a fixed cadence, on the record
only so a test can shorten it.
-}
defaultMemoryQueueConfig :: Int -> MemoryQueueConfig
defaultMemoryQueueConfig maxDepth =
    MemoryQueueConfig
        { memQueueMaxDepth = maxDepth
        , memQueuePollWaitMicros = 20_000_000
        }

{- | The most jobs one 'receive' delivers from the bounded in-memory backend. Held at
the SQS batch cap, so the worker sees one bounded batch shape whatever the backend,
not a whole cold-cache burst in one poll. The worker processes a batch sequentially
and advances its liveness heartbeat after each completed job, not once per poll. This
cap therefore bounds per-poll work and memory, and is not the heartbeat's protection
against a long batch.
-}
memoryQueueBatchSize :: Int
memoryQueueBatchSize = 10

{- | How many cap-overflow drops the bounded in-memory backend absorbs between
warning reports. It always reports the first drop, then every multiple of this. A
sustained flood therefore logs at most about one line per this many drops, rather
than one per dropped job.
-}
memoryQueueDropReportInterval :: Int
memoryQueueDropReportInterval = 1000

{- | Build a bounded, best-effort in-memory 'MirrorQueue': the production backend
mirroring runs on when no @ECLUSE_QUEUE__URL@ is set. It is a 'TBQueue' shared between
the serve path's 'enqueue' and the worker's 'receive'.

It is correctness-safe despite being lossy. Mirroring is a demand-driven optimisation
over the always-available public upstream. A job lost to the cap or to process
teardown just means the package is served from public again and re-enqueued on the
next pull. That is a deferred performance win, never a correctness loss. It admits two
deliberate departures from the cloud backends' contract:

* Bounded, drop-newest on overflow. The queue holds at most 'memQueueMaxDepth' jobs.
  An 'enqueue' past the cap is rejected: it drops the newest job rather than growing
  memory without bound. That is the load-bearing constraint, since a cold-cache
  @npm ci@ enqueues thousands of jobs at once. 'enqueue' never throws, because it runs
  on the serve hot path. Each report-worthy drop invokes the injected drop callback
  with the running drop count, rate-limited by 'memoryQueueDropReportInterval' so a
  flood does not spam.
* No redelivery, and 'ack' \/ 'extendVisibility' \/ 'deadLetter' are no-ops. Unlike the
  cloud backends, there is no visibility-timeout in-flight tracking: a 'receive'
  removes a job for good. A job whose processing fails is therefore not redelivered.
  The next demand simply re-enqueues it. This bounds memory hardest, since nothing is
  retained after delivery, and it is admissible precisely because a lost job is safe.
  A terminal fault ('deadLetter') is the same drop, since this backend has no
  dead-letter queue to route to. Its observability is the worker's error log and
  metric, not a retained message. This backend reports that absent terminus honestly
  ('Ecluse.Core.Queue.deadLetterTerminus'). The worker's redelivery budget backstops a
  queue that captures no poison message. The budget never bites here, because this
  backend does not redeliver at all: every delivery it makes is a first delivery.

'receive' is a bounded long-poll, the in-process analogue of the cloud long-poll. It
waits up to 'memQueuePollWaitMicros' for a job, then drains up to
'memoryQueueBatchSize' without blocking, or returns @[]@ when the window lapses. The
bound is load-bearing. On an idle queue the worker advances its liveness heartbeat
only when 'receive' returns, and an empty poll is a healthy idle. A busy worker also
beats after each completed job. An idle 'receive' that blocked forever would let the
heartbeat go stale and @\/livez@ flag the loop stalled. The wait uses the
@timeout@-over-@atomically@ idiom rather than @registerDelay@, so it works on the
non-threaded RTS too. An interrupted poll aborts the STM transaction and consumes
nothing.
-}
newBoundedInMemoryQueue ::
    -- | The depth cap and the idle-poll window.
    MemoryQueueConfig ->
    {- | Invoked on each report-worthy cap-overflow drop with the running total
    drops, so the composition root can log it.
    -}
    (Int -> IO ()) ->
    IO MirrorQueue
newBoundedInMemoryQueue cfg onDrop = do
    -- A capacity of at least one. The config layer enforces a positive cap, but guard
    -- anyway so a directly-constructed queue can never be the degenerate always-full
    -- zero.
    queue <- newTBQueueIO (fromIntegral (max 1 (memQueueMaxDepth cfg)))
    dropCount <- newTVarIO (0 :: Int)
    nextReceipt <- newTVarIO (0 :: Word64)
    pure
        MirrorQueue
            { enqueue = \job -> do
                dropped <- atomically (writeOrDrop queue dropCount job)
                whenJust dropped (\n -> when (shouldReportDrop n) (onDrop n))
                -- A cap overflow is the documented drop-newest shed (reported through
                -- the callback), not a backend fault: the enqueue itself worked.
                pure (Right ())
            , -- A bounded long-poll: wait up to the poll window for a batch, else return
              -- [] so the worker's heartbeat keeps advancing on an idle queue. The
              -- timeout aborts the blocked STM transaction, so no job is consumed.
              receive = Right . fromMaybe [] <$> timeout (memQueuePollWaitMicros cfg) (atomically (receiveBatch queue nextReceipt))
            , -- A delivered job is already gone from the queue, so there is nothing to
              -- retire and a failed job redelivers via the next demand, not here.
              ack = const (pure (Right ()))
            , extendVisibility = \_ _ -> pure (Right ())
            , -- The in-memory backend's only terminus is the drop a delivered job
              -- already is. There is no dead-letter queue to route to, so a terminal
              -- fault just discards the delivery (its observability is the worker's
              -- error log and metric). A future demand re-enqueues, which re-fails and
              -- re-alarms. That is accepted: a durable dead-letter needs a durable backend.
              deadLetter = const (pure (Right ()))
            , -- Nothing here captures a poison message, and nothing redelivers one, so
              -- the backend holds the budget inert at the shipped default.
              deliveryBudget = defaultDeliveryBudget
            , deadLetterTerminus = Right TerminusAbsent
            }

-- Report the first drop, then every interval-th, so the first shed is always
-- visible while a sustained flood is rate-limited.
shouldReportDrop :: Int -> Bool
shouldReportDrop n = reportWorthy n memoryQueueDropReportInterval

{- Take a bounded batch within one STM transaction: block (retry) until at least one
job is available, then drain up to 'memoryQueueBatchSize' total without blocking. The
caller bounds the initial block with a timeout, so an idle queue yields @[]@ rather
than hanging the worker. A fired timeout aborts this transaction, which then consumes
nothing. Each delivery takes a fresh receipt from a monotonic counter so messages stay
distinct, even though 'ack' on this backend is a no-op. -}
receiveBatch :: TBQueue MirrorJob -> TVar Word64 -> STM [QueueMessage]
receiveBatch queue nextReceipt = do
    headJob <- readTBQueue queue
    rest <- drainUpTo (memoryQueueBatchSize - 1)
    traverse assignReceipt (headJob : rest)
  where
    drainUpTo :: Int -> STM [MirrorJob]
    drainUpTo budget
        | budget <= 0 = pure []
        | otherwise =
            tryReadTBQueue queue >>= \case
                Nothing -> pure []
                Just job -> (job :) <$> drainUpTo (budget - 1)

    assignReceipt :: MirrorJob -> STM QueueMessage
    assignReceipt job = do
        n <- readTVar nextReceipt
        writeTVar nextReceipt (n + 1)
        -- Every delivery is a first delivery: a received job leaves the queue for
        -- good, so this backend never redelivers one.
        pure QueueMessage{msgJob = job, msgReceipt = mkReceiptHandle (show n), msgReceiveCount = 1}
