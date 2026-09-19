-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A bounded drop-newest hand-off in front of a 'MirrorQueue'.

The serve path pays an STM write instead of the backend's producer call, which on SQS is an
HTTP round trip. At the cap the newest job is dropped, which is safe because mirroring is
demand-driven and the next demand re-enqueues it. The drain loop never returns: race it.
-}
module Ecluse.Core.Queue.Buffer (
    -- * Buffered producer hand-off
    newEnqueueBuffer,

    -- * Backend building blocks
    writeOrDrop,
    reportWorthy,
) where

import Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Fault (tfDetail)
import Ecluse.Core.Queue (MirrorJob, MirrorQueue (enqueue))
import Ecluse.Core.Supervision (BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros), backoffMicros)

{- | Hand a job to a bounded queue inside the caller's transaction. At the cap it drops the newest
job and returns the running drop total, a safe loss because the next demand re-enqueues it.
-}
writeOrDrop :: TBQueue MirrorJob -> TVar Int -> MirrorJob -> STM (Maybe Int)
writeOrDrop queue dropCount job = do
    full <- isFullTBQueue queue
    if full
        then Just <$> bumpCount dropCount
        else writeTBQueue queue job $> Nothing

bumpCount :: TVar Int -> STM Int
bumpCount counter = do
    n <- (+ 1) <$> readTVar counter
    writeTVar counter n
    pure n

{- | Whether the caller should report the @n@-th event in a rate-limited series: the first, then
every @interval@-th.
-}
reportWorthy :: Int -> Int -> Bool
reportWorthy n interval = n == 1 || n `mod` interval == 0

{- | Wrap a bounded drop-newest hand-off in front of a queue, so the serve path pays an STM write
and not the backend producer call, an HTTP round trip on SQS. The drain loop never returns, race it.
-}
newEnqueueBuffer ::
    -- | Buffer depth: undelivered jobs the hand-off retains before it drops the newest.
    Int ->
    -- | Invoked on every drop with the running total. A drop is safe: the next demand re-enqueues.
    (Int -> IO ()) ->
    -- | Invoked on every backend delivery failure, with the running total and the detail.
    (Int -> Text -> IO ()) ->
    -- | The backend whose 'enqueue' the buffer decouples from its callers.
    MirrorQueue ->
    IO (MirrorQueue, IO ())
newEnqueueBuffer depth onDrop onDeliveryFailure backend = do
    -- At least one slot, so a degenerate depth cannot make the hand-off an always-full drop.
    buffer <- newTBQueueIO (fromIntegral (max 1 depth))
    dropCount <- newTVarIO (0 :: Int)
    failureCount <- newTVarIO (0 :: Int)
    let
        handOff job = do
            dropped <- atomically (writeOrDrop buffer dropCount job)
            -- 'onDrop' is a best-effort observer on the serve hot path. Guard it so a throwing
            -- observer cannot turn a safe drop into an exception on the client response.
            whenJust dropped (void . tryAny . onDrop)
            pure (Right ())
    pure (backend{enqueue = handOff}, drainLoop buffer failureCount onDeliveryFailure backend)

-- Deliver buffered jobs forever. A failed delivery backs off, so a dead backend is retried at a
-- bounded rate rather than hot-looped through the buffer. The failed job is not redelivered here.
drainLoop :: TBQueue MirrorJob -> TVar Int -> (Int -> Text -> IO ()) -> MirrorQueue -> IO ()
drainLoop buffer failureCount onDeliveryFailure backend = go 0
  where
    go consecutiveFailures = do
        job <- atomically (readTBQueue buffer)
        -- Delivery failures arrive as 'TransportFault' values, so this match is total. An
        -- exception escaping here is an invariant break, left to the loop's supervisor.
        enqueue backend job >>= \case
            Right () -> go 0
            Left fault -> do
                n <- atomically (bumpCount failureCount)
                -- 'onDeliveryFailure' is a best-effort observer. Guard it so a throwing
                -- observer can never escape the loop and tear down the composition root.
                void (tryAny (onDeliveryFailure n (tfDetail fault)))
                threadDelay (backoffMicros drainBackoff consecutiveFailures)
                go (consecutiveFailures + 1)

-- The pacing between failed deliveries: from 200ms towards a 30s cap as consecutive failures
-- mount, so the loop retries a dead backend at most once per cap interval.
drainBackoff :: BackoffSchedule
drainBackoff = BackoffSchedule{bsBaseMicros = 200_000, bsCapMicros = 30_000_000}
