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

{- | The bounded in-memory backend's depth cap and idle-poll window. Build it with
'defaultMemoryQueueConfig' for the production poll window.
-}
data MemoryQueueConfig = MemoryQueueConfig
    { memQueueMaxDepth :: Int
    {- ^ The maximum number of jobs the queue holds. The config layer enforces a positive cap. An
    'enqueue' past it drops the newest job, a safe loss because the next demand re-enqueues.
    -}
    , memQueuePollWaitMicros :: Int
    {- ^ The idle long-poll window in microseconds: how long a 'receive' waits for a job before
    returning @[]@. The bound keeps the worker's liveness heartbeat advancing on an idle queue.
    -}
    }
    deriving stock (Eq, Show)

{- | A 'MemoryQueueConfig' for a given depth cap, with the idle-poll window at its production
default of @20s@. That sits under the worker's heartbeat-staleness budget
('Ecluse.Core.Worker.workerHeartbeatStaleAfter'), so an idle 'receive' returns before
@\/livez@ flags the loop stalled.
-}
defaultMemoryQueueConfig :: Int -> MemoryQueueConfig
defaultMemoryQueueConfig maxDepth =
    MemoryQueueConfig
        { memQueueMaxDepth = maxDepth
        , memQueuePollWaitMicros = 20_000_000
        }

{- The most jobs one 'receive' delivers from the bounded in-memory backend. Held at the SQS
batch cap, so the worker sees one bounded batch shape whatever the backend, and per-poll work
and memory stay bounded.
-}
memoryQueueBatchSize :: Int
memoryQueueBatchSize = 10

{- | How many cap-overflow drops the bounded in-memory backend absorbs between warning reports.
It reports the first drop, then every multiple of this, so a sustained flood cannot spam.
-}
memoryQueueDropReportInterval :: Int
memoryQueueDropReportInterval = 1000

{- | Build the bounded, best-effort in-memory 'MirrorQueue': the backend mirroring runs on when
no @ECLUSE_QUEUE__URL@ is set. Loss is safe, because mirroring is a demand-driven optimisation
over the always-available public upstream and the next pull re-enqueues a lost job. Two
departures from the cloud backends' contract follow.

* Bounded, drop-newest past 'memQueueMaxDepth', since a cold-cache @npm ci@ enqueues thousands
of jobs at once. 'enqueue' never throws, because it runs on the serve hot path, and it
reports drops through the injected callback at 'memoryQueueDropReportInterval'.
* No redelivery. A 'receive' removes a job for good, so 'ack', 'extendVisibility' and
'deadLetter' are no-ops and the backend reports its terminus as absent.

'receive' waits up to 'memQueuePollWaitMicros' for a job, drains up to 'memoryQueueBatchSize'
without blocking, then returns. The bound is load-bearing: an idle 'receive' that blocked
forever would let the worker's heartbeat go stale and @\/livez@ flag the loop stalled. The wait
uses @timeout@ over @atomically@ rather than @registerDelay@, so it works on the non-threaded
RTS, and an interrupted poll consumes nothing.
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
    -- A capacity of at least one. The config layer enforces a positive cap, but a
    -- directly-constructed queue must never be the degenerate always-full zero.
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
              -- already is. A terminal fault discards the delivery, and its observability is the
              -- worker's error log and metric. A durable dead-letter needs a durable backend.
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

{- Take a bounded batch in one transaction: block until a job is available, then drain up to
'memoryQueueBatchSize' more without blocking. The caller bounds the initial block with a
timeout, and a fired timeout aborts this transaction, which then consumes nothing. -}
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
