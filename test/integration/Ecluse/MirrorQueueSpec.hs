-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.MirrorQueueSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), tfCause)
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Queue (
    DeadLetterTerminus (TerminusAbsent, TerminusAttached),
    DeliveryBudget (DeliveryBudget),
    MirrorJob (..),
    MirrorQueue (..),
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
import Ecluse.Runtime.Aws.Env (AwsEndpoint (AwsEndpoint, endpointHost, endpointPort, endpointSecure))
import Ecluse.Runtime.Queue.Sqs (
    SqsConfig (sqsEndpoint, sqsWaitSeconds),
    defaultSqsConfig,
    newSqsQueue,
 )
import Ecluse.Test.Package (unsafeFilename, unsafeRegistryUrl)

{- | These cases drive the SQS 'MirrorQueue' backend against a @ministack@ container from
"Ecluse.Integration.Ministack". They are gating and need a Docker daemon, never real AWS.
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
                -- Extend the in-flight message well past its 1s visibility timeout. No
                -- reappearance inside the original window proves the extension held it.
                queue <- freshQueue container "mirror-extend" defaultQueueOptions{qoVisibilityTimeout = Seconds 1}
                unwrapQ (enqueue queue sampleJob)
                [message] <- receiveUntil queue
                unwrapQ (extendVisibility queue (msgReceipt message) (Seconds 30))
                -- Past the original 1s window (poll twice over ~2s), still hidden.
                stillHidden1 <- unwrapQ (receive queue)
                stillHidden2 <- unwrapQ (receive queue)
                map msgJob (stillHidden1 <> stillHidden2) `shouldBe` []

            it "dead-letters a terminal fault without deleting it, so it rides the redrive policy (issue #846)" $ \container -> do
                -- deadLetter must NOT DeleteMessage, which would silently discard the terminal
                -- fault. It returns the message with the terminal backoff, so the message rides
                -- the operator's redrive policy. Reappearance proves nothing deleted it.
                queue <- freshQueue container "mirror-deadletter" defaultQueueOptions{qoVisibilityTimeout = Seconds 30, qoTerminalBackoff = Seconds 1}
                unwrapQ (enqueue queue sampleJob)
                [message] <- receiveUntil queue
                unwrapQ (deadLetter queue (msgReceipt message))
                redelivered <- receiveUntil queue
                map msgJob redelivered `shouldBe` [sampleJob]

            it "carries a real ApproximateReceiveCount, rising on each redelivery (issue #935)" $ \container -> do
                -- SQS omits this attribute unless the request asks for it. A missing
                -- parameter would read as a first delivery forever, so the budget never fires.
                queue <- freshQueue container "mirror-receive-count" defaultQueueOptions{qoVisibilityTimeout = Seconds 1}
                unwrapQ (enqueue queue sampleJob)
                [first'] <- receiveUntil queue
                msgReceiveCount first' `shouldBe` 1
                [second'] <- receiveUntil queue
                msgReceiveCount second' `shouldBe` 2

            it "probes a queue with no redrive policy as having no dead-letter terminus (issue #935)" $ \container -> do
                -- A plain CreateQueue leaves nothing to capture a poison message, so
                -- Écluse must warn the operator at boot.
                queue <- freshQueue container "mirror-no-dlq" defaultQueueOptions
                deadLetterTerminus queue `shouldBe` Right TerminusAbsent

            it "probes an attached redrive policy, reading its maxReceiveCount (issue #935)" $ \container -> do
                -- The capture count 9 sits above the shipped floor of 5, so the raise is
                -- visible. At 5 the effective budget would equal the floor either way.
                queue <- freshQueue container "mirror-with-dlq" defaultQueueOptions{qoDeadLetterAfter = Just 9}
                deadLetterTerminus queue `shouldBe` Right (TerminusAttached (Just (DeliveryBudget 9)))
                -- The handle runs on a budget one past the policy's capture count, so
                -- the dead-letter queue always takes the message first.
                deliveryBudget queue `shouldBe` DeliveryBudget 10

            it "reports an unreachable endpoint as the handle's typed transport fault" $ \_container -> do
                -- The poll must come back as a typed 'Left' with the unreachable cause,
                -- classified at the adapter edge, never as an exception through the caller.
                queue <- deadEndpointQueue
                outcome <- receive queue
                case outcome of
                    Left fault -> tfCause fault `shouldBe` TransportUnreachable
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
                Just AwsEndpoint{endpointSecure = False, endpointHost = "127.0.0.1", endpointPort = 1}
            , sqsWaitSeconds = 1
            }

-- | A sample mirror job carried end-to-end through SQS.
sampleJob :: MirrorJob
sampleJob =
    MirrorJob
        { jobPackage = mkPackageName Npm Nothing "left-pad"
        , jobVersion = mkVersion Npm "1.3.0"
        , jobArtifactUrl = unsafeRegistryUrl "https://registry.npmjs.org/left-pad/-/left-pad-1.3.0.tgz"
        , jobArtifactFilename = unsafeFilename "left-pad-1.3.0.tgz"
        , jobTraceContext = Nothing
        }
