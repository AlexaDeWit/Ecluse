-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The STM-backed in-memory 'MirrorQueue': the __bounded, best-effort
production backend__ mirroring rolls over to when no @ECLUSE_QUEUE_URL@ is set.

It honours the handle's contract (see "Ecluse.Core.Queue" for the @enqueue@ \/
don't-@ack@-to-retry \/ no-@nack@ conventions) and is built from the contract
module's backend building blocks. See 'newBoundedInMemoryQueue' for why it is
correctness-safe (a dropped job is re-enqueued on the next demand) and why it
deliberately does __not__ redeliver.
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
    MirrorJob,
    MirrorQueue (..),
    QueueMessage (..),
    mkReceiptHandle,
    reportWorthy,
    writeOrDrop,
 )

{- | What the bounded in-memory backend needs: its depth cap and its idle-poll
window. A record (like the SQS backend's @SqsConfig@) so each knob is named rather
than a bare 'Int'; build it with 'defaultMemoryQueueConfig' for the production poll
window.
-}
data MemoryQueueConfig = MemoryQueueConfig
    { memQueueMaxDepth :: Int
    {- ^ The maximum number of jobs the queue holds. A fresh 'enqueue' past this cap
    is __dropped-newest__ (the enqueue is rejected); a dropped job is safe, as it is
    re-enqueued on the next demand. Must be positive (the config layer enforces it).
    -}
    , memQueuePollWaitMicros :: Int
    {- ^ The idle long-poll window in microseconds: how long a 'receive' waits for a
    job before returning @[]@ (an empty, healthy poll). Bounds the idle wait so the
    worker's liveness heartbeat keeps advancing -- see 'newBoundedInMemoryQueue'.
    -}
    }
    deriving stock (Eq, Show)

{- | A 'MemoryQueueConfig' for a given depth cap with the idle-poll window at its
production default -- @20s@, mirroring the SQS long-poll cadence
(the SQS backend's @defaultSqsConfig@) and comfortably under the worker's @120s@
heartbeat-staleness budget ('Ecluse.Core.Worker.workerHeartbeatStaleAfter'), so an idle
'receive' returns a healthy empty poll long before @\/livez@ would flag the loop
stalled. The depth cap stays the operator-tunable knob; the poll window is a fixed
cadence, exposed on the record only so a test can shorten it.
-}
defaultMemoryQueueConfig :: Int -> MemoryQueueConfig
defaultMemoryQueueConfig maxDepth =
    MemoryQueueConfig
        { memQueueMaxDepth = maxDepth
        , memQueuePollWaitMicros = 20_000_000
        }

{- | The most jobs one 'receive' delivers from the bounded in-memory backend. Held
at the SQS batch cap so the worker -- which processes a batch __sequentially__ and
advances its liveness heartbeat once per poll -- sees the same bounded batch shape
regardless of backend, rather than one poll returning a whole cold-cache burst and
starving the heartbeat past its staleness window.
-}
memoryQueueBatchSize :: Int
memoryQueueBatchSize = 10

{- | How many cap-overflow drops the bounded in-memory backend absorbs between
warning reports. The first drop is always reported, then every multiple of this, so
a sustained flood logs at most about one line per this many drops rather than one
per dropped job.
-}
memoryQueueDropReportInterval :: Int
memoryQueueDropReportInterval = 1000

{- | Build a bounded, best-effort in-memory 'MirrorQueue' -- the production backend
mirroring runs on when no @ECLUSE_QUEUE_URL@ is set, a 'TBQueue' shared between the
serve path's 'enqueue' and the worker's 'receive'.

It is __correctness-safe despite being lossy__: mirroring is a demand-driven
optimization over the always-available public upstream, so a job lost to the cap or
to process teardown just means the package is served from public again and
re-enqueued on the next pull -- a deferred performance win, never a correctness loss.
That admits two deliberate departures from the cloud backends' contract:

* __Bounded, drop-newest on overflow.__ The queue holds at most 'memQueueMaxDepth'
  jobs; an 'enqueue' that would exceed the cap is rejected (the newest job is
  dropped) rather than growing memory without bound -- the load-bearing constraint,
  since a cold-cache @npm ci@ enqueues thousands of jobs at once. 'enqueue' never
  throws (it runs on the serve hot path), and each report-worthy drop invokes the
  injected drop callback with the running drop count, rate-limited by
  'memoryQueueDropReportInterval' so a flood does not spam.
* __No redelivery; 'ack' \/ 'extendVisibility' are no-ops.__ Unlike the cloud
  backends, there is no visibility-timeout in-flight
  tracking: a 'receive' removes a job for good. A job whose processing fails is
  therefore __not__ redelivered -- it is simply re-enqueued on the next demand. This
  bounds memory hardest (nothing is retained after delivery) and is admissible
  precisely because a lost job is safe.

'receive' is a __bounded long-poll__: it waits up to 'memQueuePollWaitMicros' for a
job, then drains up to 'memoryQueueBatchSize' without blocking, or returns @[]@ when
the window lapses -- the in-process analogue of the cloud long-poll. The bound is
load-bearing: the worker advances its liveness heartbeat only when 'receive' returns
(an empty poll is a healthy idle), so an idle 'receive' that blocked forever would
let the heartbeat go stale and @\/livez@ flag the loop stalled. The wait is the
@timeout@-over-@atomically@ idiom rather than @registerDelay@ so it works on the
non-threaded RTS too; an interrupted poll aborts the STM transaction, consuming
nothing.
-}
newBoundedInMemoryQueue ::
    -- | The depth cap (and any future knobs).
    MemoryQueueConfig ->
    {- | Invoked on each report-worthy cap-overflow drop with the running total drops,
    so the composition root can log it (and, once the @ecluse.mirror.*@ metric
    catalogue lands, increment a drop counter alongside).
    -}
    (Int -> IO ()) ->
    IO MirrorQueue
newBoundedInMemoryQueue cfg onDrop = do
    -- A capacity of at least one: the config layer enforces a positive cap, but guard
    -- so a directly-constructed queue can never be the degenerate always-full zero.
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
            }

-- Report the first drop, then every interval-th, so the first shed is always
-- visible while a sustained flood is rate-limited.
shouldReportDrop :: Int -> Bool
shouldReportDrop n = reportWorthy n memoryQueueDropReportInterval

{- Take a bounded batch within one STM transaction: block (retry) until at least one
job is available, then drain up to 'memoryQueueBatchSize' total without blocking. The
caller bounds the initial block with a timeout (so an idle queue yields @[]@ rather
than hanging the worker); if that timeout fires, this transaction is aborted and
consumes nothing. Each delivery is assigned a fresh receipt from a monotonic counter
so messages stay distinct, even though 'ack' on this backend is a no-op. -}
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
        pure QueueMessage{msgJob = job, msgReceipt = mkReceiptHandle (show n)}
