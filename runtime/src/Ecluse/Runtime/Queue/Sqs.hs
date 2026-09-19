-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The AWS SQS backend behind the 'MirrorQueue' handle, mapping its receive-process-ack shape
onto @SendMessage@, @ReceiveMessage@, @DeleteMessage@, and @ChangeMessageVisibility@. Retry is
__don't ack__: SQS redelivers an unacked message, and 'deadLetter' returns a terminal one under
a backoff longer than the processing window without deleting it, so it rides the operator's
redrive policy to the dead-letter queue. Every failure is a 'Ecluse.Core.Fault.TransportFault'
value, never an exception. The queue is an operator-declared destination, so the data-plane
egress controls of "Ecluse.Core.Security.Egress" do not apply to it. "Ecluse.Runtime.Queue.Sqs.Internal" implements it.
-}
module Ecluse.Runtime.Queue.Sqs (
    -- * Configuration
    SqsConfig (sqsQueueUrl, sqsRegion, sqsEndpoint, sqsMaxReceiveCount),
    defaultSqsConfig,

    -- * The backend
    newSqsQueue,
) where

import Ecluse.Runtime.Queue.Sqs.Internal (
    SqsConfig (sqsEndpoint, sqsMaxReceiveCount, sqsQueueUrl, sqsRegion),
    defaultSqsConfig,
    newSqsQueue,
 )
