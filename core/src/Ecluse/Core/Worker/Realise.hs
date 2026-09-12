-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Realising a job's verdict at the queue handle: ack, dead-letter, or release for retry.

This half runs a batch and turns each 'JobOutcome' the decision half
("Ecluse.Core.Worker.Job") reached into a queue operation. Every receipt in the batch is
leased for the batch's whole run ("Ecluse.Core.Worker.Lease"), so a job never races the
backend's visibility window and its disposition is the one thing that ends the lease. Jobs
still run __sequentially__, one artifact task at a time. A delivery that already spent the
queue's redelivery budget is retired before its job runs, so a message nothing else captures
stops cycling instead of re-fetching its artifact on every redelivery.
-}
module Ecluse.Core.Worker.Realise (
    processBatch,
) where

import Katip (Severity (ErrorS, WarningS), logFM, ls)

import Ecluse.Core.Fault (tfDetail)
import Ecluse.Core.Queue (
    DeliveryBudget,
    MirrorQueue (ack, deadLetter, deliveryBudget, extendVisibility),
    QueueMessage (msgJob, msgReceipt, msgReceiveCount),
    ReceiptHandle,
    Seconds (Seconds),
    deliveryBudgetSpent,
    retiringDelivery,
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort (..))
import Ecluse.Core.Worker.Job (JobOutcome (DeadLettered, Dropped, Retried, Succeeded), processJob)
import Ecluse.Core.Worker.Lease (LeasedReceipt, disposing, leasedMessage, queueLeaseOps, whileLeased, withLeasedBatch)
import Ecluse.Core.Worker.Types

-- What the worker leaves at the queue handle once a message is decided. It is realised
-- under the receipt's own lease, so no renewal can follow it.
data Disposition
    = DisposeAck
    | DisposeDeadLetter
    | DisposeRelease

{- | Process one batch sequentially under a lease on every receipt in it. The heartbeat advances
per job, so 'Ecluse.Core.Worker.Liveness.workerHeartbeatStaleAfter' covers one job.
-}
processBatch :: [QueueMessage] -> WorkerM ()
processBatch messages = do
    queue <- asks wrQueue
    withLeasedBatch (queueLeaseOps queue) messages (traverse_ processLeased)

{- Decide one leased message and realise it, then beat. A receipt whose lease was dropped is
left unacknowledged, so the backend redelivers it once its window lapses. -}
processLeased :: LeasedReceipt -> WorkerM ()
processLeased leased = do
    decided <- whileLeased leased (decideMessage message)
    whenJust decided (disposing leased . realiseDisposition (msgReceipt message))
    recordWorkerProgress
  where
    message = leasedMessage leased

{- Check the queue's delivery budget before running the job, so a poison message retires without
re-fetching its artifact, even on a queue with no dead-letter terminus. -}
decideMessage :: QueueMessage -> WorkerM Disposition
decideMessage message = do
    budget <- asks (deliveryBudget . wrQueue)
    if deliveryBudgetSpent budget message
        then do
            metrics <- asks wrMetrics
            liftIO (wmpMirrorJobProcessed metrics Metric.Discarded)
            -- On a queue with no dead-letter terminus this line is the only record it leaves.
            DisposeAck <$ logFM ErrorS (ls (budgetSpentReason budget message))
        else decideDelivery message

-- Run the job and read its outcome as the disposition for a delivery still within the budget.
decideDelivery :: QueueMessage -> WorkerM Disposition
decideDelivery message = do
    metrics <- asks wrMetrics
    outcome <- processJob (msgJob message)
    liftIO (wmpMirrorJobProcessed metrics (jobResultMetric outcome))
    case outcome of
        Succeeded -> pure DisposeAck
        Dropped reason ->
            -- Non-retryable, and not worth a dead-letter forensic trail, so retire it instead.
            DisposeAck <$ logFM ErrorS (ls ("dropping unrecoverable mirror job: " <> reason))
        DeadLettered reason ->
            -- Alarm first: on the in-memory backend the log and metric are the only record.
            DisposeDeadLetter <$ logFM ErrorS (ls ("dead-lettering unmirrorable mirror job (rides the backend's dead-letter terminus): " <> reason))
        Retried reason ->
            DisposeRelease <$ logFM WarningS (ls ("leaving mirror job un-acked for retry (redelivered by a durable queue, re-mirrored on next demand by the in-memory one): " <> reason))

realiseDisposition :: ReceiptHandle -> Disposition -> WorkerM ()
realiseDisposition receipt = \case
    DisposeAck -> ackMessage receipt
    DisposeDeadLetter -> deadLetterMessage receipt
    DisposeRelease -> releaseForRetry receipt

budgetSpentReason :: DeliveryBudget -> QueueMessage -> Text
budgetSpentReason budget message =
    "discarding a mirror job after "
        <> show (msgReceiveCount message)
        <> " deliveries (this queue retires one on delivery "
        <> show (retiringDelivery budget)
        <> "): "
        <> renderJob (msgJob message)
        <> ". No dead-letter queue captured it, so it is retired here rather than left to"
        <> " cycle until the queue's retention window drops it unseen. Attach a redrive"
        <> " policy to retain it for inspection."

-- Classify a job outcome for the @ecluse.mirror.jobs.processed@ metric. 'Metric.Discarded' is
-- absent here on purpose: the worker counts a budget-spent delivery at its retirement.
jobResultMetric :: JobOutcome -> Metric.MirrorResult
jobResultMetric = \case
    Succeeded -> Metric.Published
    Dropped _ -> Metric.Failed
    DeadLettered _ -> Metric.Failed
    Retried _ -> Metric.Failed

ackMessage :: ReceiptHandle -> WorkerM ()
ackMessage receipt =
    queueOp (`ack` receipt) $ \fault ->
        logFM WarningS (ls ("ack failed; the processed message will redeliver (harmless, publishing is idempotent): " <> tfDetail fault))

-- Hand the message to the queue's dead-letter terminus, never a plain delete, which would
-- silently discard it on a durable queue.
deadLetterMessage :: ReceiptHandle -> WorkerM ()
deadLetterMessage receipt =
    queueOp (`deadLetter` receipt) $ \fault ->
        logFM WarningS (ls ("dead-letter realisation failed; the message redelivers and re-fails terminally (harmless): " <> tfDetail fault))

-- Reset the message to visible, so a failed job redelivers at once instead of waiting out the
-- lease the worker held. Best effort: a missed reset only delays the redelivery.
releaseForRetry :: ReceiptHandle -> WorkerM ()
releaseForRetry receipt =
    queueOp (\queue -> extendVisibility queue receipt (Seconds 0)) (const pass)
