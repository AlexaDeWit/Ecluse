-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Realising a job's verdict at the queue handle: ack, dead-letter, or leave it to
redeliver.

This half runs a batch and turns each 'JobOutcome' the decision half
("Ecluse.Core.Worker.Job") reached into a queue operation. A batch is processed
__sequentially__, so each job holds the full visibility budget rather than competing
with its batch-mates. A delivery that already spent the queue's redelivery budget is
retired before the job runs, so a message nothing else captures stops cycling instead
of re-fetching its artifact on every redelivery.
-}
module Ecluse.Core.Worker.Realise (
    processBatch,
) where

import Katip (Severity (ErrorS, WarningS), logFM, ls)

import Ecluse.Core.Fault (tfDetail)
import Ecluse.Core.Queue (
    DeliveryBudget,
    MirrorQueue (ack, deadLetter, deliveryBudget),
    QueueMessage (msgJob, msgReceipt, msgReceiveCount),
    ReceiptHandle,
    deliveryBudgetSpent,
    retiringDelivery,
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort (..))
import Ecluse.Core.Worker.Job (JobOutcome (DeadLettered, Dropped, Retried, Succeeded), processJob)
import Ecluse.Core.Worker.Types

{- | Process one batch sequentially, so each job gets the full visibility budget. The heartbeat
advances per job, so 'Ecluse.Core.Worker.Liveness.workerHeartbeatStaleAfter' covers one job.
-}
processBatch :: [QueueMessage] -> WorkerM ()
processBatch = traverse_ $ \message -> do
    processMessage message
    recordWorkerProgress

{- Check the queue's delivery budget before running the job, so a poison message retires without
re-fetching its artifact, even on a queue with no dead-letter terminus. -}
processMessage :: QueueMessage -> WorkerM ()
processMessage message = do
    budget <- asks (deliveryBudget . wrQueue)
    if deliveryBudgetSpent budget message
        then do
            metrics <- asks wrMetrics
            liftIO (wmpMirrorJobProcessed metrics Metric.Discarded)
            retireTerminally (budgetSpentReason budget message) (msgReceipt message)
        else processDelivery message

-- Run the job and realise its outcome for a delivery still within the queue's budget.
processDelivery :: QueueMessage -> WorkerM ()
processDelivery message = do
    metrics <- asks wrMetrics
    outcome <- processJob (msgReceipt message) (msgJob message)
    liftIO (wmpMirrorJobProcessed metrics (jobResultMetric outcome))
    case outcome of
        Succeeded -> ackMessage (msgReceipt message)
        Dropped reason ->
            -- Non-retryable, and not worth a dead-letter forensic trail, so retire it instead.
            retireTerminally ("dropping unrecoverable mirror job: " <> reason) (msgReceipt message)
        DeadLettered reason -> do
            -- Alarm first: on the in-memory backend the log and metric are the only record.
            logFM ErrorS (ls ("dead-lettering unmirrorable mirror job (rides the backend's dead-letter terminus): " <> reason))
            deadLetterMessage (msgReceipt message)
        Retried reason ->
            logFM WarningS (ls ("leaving mirror job un-acked for retry (redelivered by a durable queue, re-mirrored on next demand by the in-memory one): " <> reason))

{- Retire a message the worker will never mirror: alarm, then ack so it stops cycling. On a
durable queue that ack is the delete that finally kills the message. -}
retireTerminally :: Text -> ReceiptHandle -> WorkerM ()
retireTerminally reason receipt = do
    logFM ErrorS (ls reason)
    ackMessage receipt

-- On a queue with no dead-letter terminus this line is the only record the message ever leaves.
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
