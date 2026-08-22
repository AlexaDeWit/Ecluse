-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.MirrorQueueSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Queue (
    DeadLetterTerminus (TerminusAbsent, TerminusAttached),
    DeliveryBudget (DeliveryBudget),
    MirrorJob (..),
    MirrorQueue (..),
    QueueFault (qfCause),
    QueueMessage (..),
    Seconds (..),
 )
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Integration.Ministack (
    QueueOptions (qoDeadLetterAfter, qoTerminalBackoff, qoVisibilityTimeout),
    defaultQueueOptions,
    freshQueue,
    quietLogEnv,
    receiveUntil,
    unwrapQ,
    withMinistack,
 )
import Ecluse.Runtime.Queue.Sqs (
    SqsConfig (sqsEndpoint, sqsWaitSeconds),
    SqsEndpoint (SqsEndpoint, endpointHost, endpointPort, endpointSecure),
    defaultSqsConfig,
    newSqsQueue,
 )
import Ecluse.Test.Package (unsafeRegistryUrl)

{- | These cases drive the SQS 'MirrorQueue' backend against a real endpoint from a
@ministack@ container (launched via @testcontainers@, shared through
"Ecluse.Integration.Ministack"). The harness points @amazonka@ at the container with
throwaway credentials, so the cases are hermetic and __gating__. They require a running
Docker daemon and no real AWS.
-}
spec :: Spec
spec =
    aroundAll withMinistack $
        describe "mirror queue (ministack)" $ do
            it "round-trips a job: enqueue, receive, ack, then no redelivery" $ \container -> do
                queue <- freshQueue container "mirror-roundtrip" defaultQueueOptions
                unwrapQ (enqueue queue sampleJob)
                [message] <- receiveUntil queue
                msgJob message `shouldBe` sampleJob
                unwrapQ (ack queue (msgReceipt message))
                -- After the ack the job is gone: a poll past the (short) visibility
                -- window yields nothing.
                afterAck <- unwrapQ (receive queue)
                map msgJob afterAck `shouldBe` []

            it "redelivers a job that was received but never acked" $ \container -> do
                -- A one-second visibility timeout so the un-acked job becomes
                -- visible again within the test's patience.
                queue <- freshQueue container "mirror-redeliver" defaultQueueOptions{qoVisibilityTimeout = Seconds 1}
                unwrapQ (enqueue queue sampleJob)
                _firstDelivery <- receiveUntil queue
                -- Deliberately do not ack: retry-is-don't-ack means the job must
                -- reappear once its visibility window lapses.
                redelivered <- receiveUntil queue
                map msgJob redelivered `shouldBe` [sampleJob]

            it "extendVisibility holds an un-acked job past its original window" $ \container -> do
                -- Start with a 1s visibility timeout, then extend the in-flight
                -- message's window well past it. The job must NOT reappear inside
                -- the original timeout's redelivery gap, which proves the
                -- ChangeMessageVisibility call held it.
                queue <- freshQueue container "mirror-extend" defaultQueueOptions{qoVisibilityTimeout = Seconds 1}
                unwrapQ (enqueue queue sampleJob)
                [message] <- receiveUntil queue
                unwrapQ (extendVisibility queue (msgReceipt message) (Seconds 30))
                -- Past the original 1s window (poll twice over ~2s), still hidden.
                stillHidden1 <- unwrapQ (receive queue)
                stillHidden2 <- unwrapQ (receive queue)
                map msgJob (stillHidden1 <> stillHidden2) `shouldBe` []

            it "dead-letters a terminal fault without deleting it, so it rides the redrive policy (issue #846)" $ \container -> do
                -- deadLetter must NOT DeleteMessage, which would silently discard the
                -- terminal fault and lose the observability. It returns the message with
                -- the terminal backoff, so the message stays in the queue and rides the
                -- operator's redrive policy to the dead-letter queue. The message's
                -- reappearance after the deadLetter proves nothing deleted it, because
                -- an ack or delete never redelivers. The short terminal backoff keeps
                -- that reappearance within the poll's patience.
                queue <- freshQueue container "mirror-deadletter" defaultQueueOptions{qoVisibilityTimeout = Seconds 30, qoTerminalBackoff = Seconds 1}
                unwrapQ (enqueue queue sampleJob)
                [message] <- receiveUntil queue
                unwrapQ (deadLetter queue (msgReceipt message))
                redelivered <- receiveUntil queue
                map msgJob redelivered `shouldBe` [sampleJob]

            it "carries a real ApproximateReceiveCount, rising on each redelivery (issue #935)" $ \container -> do
                -- SQS omits this attribute unless the request asks for it. This case
                -- proves it against the real API. A missing request parameter would
                -- silently read as a first delivery forever, and the budget would
                -- never fire.
                queue <- freshQueue container "mirror-receive-count" defaultQueueOptions{qoVisibilityTimeout = Seconds 1}
                unwrapQ (enqueue queue sampleJob)
                [first'] <- receiveUntil queue
                msgReceiveCount first' `shouldBe` 1
                -- This case leaves the message unacked on purpose. The message comes
                -- back once its short visibility window lapses, this time on its
                -- second delivery.
                [second'] <- receiveUntil queue
                msgReceiveCount second' `shouldBe` 2

            it "probes a queue with no redrive policy as having no dead-letter terminus (issue #935)" $ \container -> do
                -- A plain CreateQueue leaves nothing to capture a poison message, so
                -- Écluse must warn the operator at boot.
                queue <- freshQueue container "mirror-no-dlq" defaultQueueOptions
                deadLetterTerminus queue `shouldBe` Right TerminusAbsent

            it "probes an attached redrive policy, reading its maxReceiveCount (issue #935)" $ \container -> do
                -- With a dead-letter queue attached, the boot warning must stay
                -- silent. The capture count holds Écluse's own budget above it. This
                -- case picks a count above the shipped floor of 5, so the raise is
                -- visible. At 5 the effective budget would equal the floor either way.
                queue <- freshQueue container "mirror-with-dlq" defaultQueueOptions{qoDeadLetterAfter = Just 9}
                deadLetterTerminus queue `shouldBe` Right (TerminusAttached (Just (DeliveryBudget 9)))
                -- The handle runs on a budget one past the policy's capture count, so
                -- the dead-letter queue always takes the message first.
                deliveryBudget queue `shouldBe` DeliveryBudget 10

            it "reports an unreachable endpoint as the handle's typed transport fault" $ \_container -> do
                -- Point the backend at a loopback port with nothing listening. The
                -- poll must come back as the typed 'Left' with the unreachable cause,
                -- classified at the adapter edge, never as an exception through the
                -- caller.
                queue <- deadEndpointQueue
                outcome <- receive queue
                case outcome of
                    Left fault -> qfCause fault `shouldBe` TransportUnreachable
                    Right messages -> expectationFailure ("expected a typed transport fault, got " <> show messages)

-- An SQS backend pointed at a loopback port with nothing listening, for the
-- typed-fault classification case. Port 1 is in the privileged range and never bound.
deadEndpointQueue :: IO MirrorQueue
deadEndpointQueue = do
    logEnv <- quietLogEnv
    newSqsQueue
        logEnv
        (Right . loopbackRegistryUrl)
        (defaultSqsConfig "http://127.0.0.1:1/000000000000/dead" "us-east-1")
            { sqsEndpoint =
                Just SqsEndpoint{endpointSecure = False, endpointHost = "127.0.0.1", endpointPort = 1}
            , sqsWaitSeconds = 1
            }

-- | A sample mirror job carried end-to-end through SQS.
sampleJob :: MirrorJob
sampleJob =
    MirrorJob
        { jobPackage = mkPackageName Npm Nothing "left-pad"
        , jobVersion = mkVersion Npm "1.3.0"
        , jobArtifactUrl = unsafeRegistryUrl "https://registry.npmjs.org/left-pad/-/left-pad-1.3.0.tgz"
        , jobArtifactFilename = "left-pad-1.3.0.tgz"
        , jobTraceContext = Nothing
        }
