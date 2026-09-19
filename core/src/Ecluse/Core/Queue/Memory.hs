-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The STM-backed in-memory 'MirrorQueue': the bounded, best-effort production backend
mirroring rolls over to when no @ECLUSE_QUEUE__URL@ is set.

It honours the handle's contract ("Ecluse.Core.Queue") with two departures. 'enqueue' drops the
newest job past the depth cap, and a 'receive' is final, so nothing redelivers and no delivery
carries a lease. Both are safe because the next demand re-enqueues the job.
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

{- | A 'MemoryQueueConfig' at the production @20s@ idle-poll window, which sits under
'Ecluse.Core.Worker.workerHeartbeatStaleAfter' so an idle poll cannot stall @\/livez@.
-}
defaultMemoryQueueConfig :: Int -> MemoryQueueConfig
defaultMemoryQueueConfig maxDepth =
    MemoryQueueConfig
        { memQueueMaxDepth = maxDepth
        , memQueuePollWaitMicros = 20_000_000
        }

-- Held at the SQS batch cap, so the worker sees one bounded batch shape whatever the backend.
memoryQueueBatchSize :: Int
memoryQueueBatchSize = 10

{- | How many cap-overflow drops the bounded in-memory backend absorbs between warning reports.
It reports the first drop, then every multiple of this, so a sustained flood cannot spam.
-}
memoryQueueDropReportInterval :: Int
memoryQueueDropReportInterval = 1000

{- | Build the bounded, best-effort in-memory 'MirrorQueue'. A cold-cache @npm ci@ enqueues
thousands of jobs at once, so 'enqueue' sheds past 'memQueueMaxDepth' rather than throwing.
-}
newBoundedInMemoryQueue ::
    -- | The depth cap and the idle-poll window.
    MemoryQueueConfig ->
    -- | Invoked on each report-worthy cap-overflow drop with the running total, for the log.
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
                whenJust dropped (\n -> when (reportWorthy n memoryQueueDropReportInterval) (onDrop n))
                -- A cap overflow is the documented drop-newest shed (reported through
                -- the callback), not a backend fault: the enqueue itself worked.
                pure (Right ())
            , -- @timeout@ over @atomically@, not @registerDelay@, so the poll bound holds on the
              -- non-threaded RTS too. A fired timeout aborts the transaction, consuming nothing.
              receive = Right . fromMaybe [] <$> timeout (memQueuePollWaitMicros cfg) (atomically (receiveBatch queue nextReceipt))
            , -- A delivered job is already gone from the queue, so there is nothing to
              -- retire and a failed job redelivers via the next demand, not here.
              ack = const (pure (Right ()))
            , extendVisibility = \_ _ -> pure (Right ())
            , -- A delivered job is already dropped, so a terminal fault has nowhere further to
              -- go. Its observability is the worker's error log and metric.
              deadLetter = const (pure (Right ()))
            , -- Nothing here captures a poison message, and nothing redelivers one, so
              -- the backend holds the budget inert at the shipped default.
              deliveryBudget = defaultDeliveryBudget
            , deadLetterTerminus = Right TerminusAbsent
            }

-- One transaction, so a timeout fired by the caller during the initial block aborts the whole
-- batch and consumes nothing.
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
        -- Every delivery is a first delivery and none expires: a received job leaves the
        -- queue for good, so this backend never redelivers one and grants no lease.
        pure QueueMessage{msgJob = job, msgReceipt = mkReceiptHandle (show n), msgReceiveCount = 1, msgLease = Nothing}
