-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.MirrorQueueSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Queue (
    MirrorJob (..),
    MirrorQueue (..),
    QueueFault (qfCause),
    QueueMessage (..),
    Seconds (..),
 )
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Integration.Ministack (
    QueueOptions (qoVisibilityTimeout),
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

{- | Integration tests exercise the SQS 'MirrorQueue' backend against a real
endpoint provided by a @ministack@ container (launched via @testcontainers@, shared
through "Ecluse.Integration.Ministack"). @amazonka@ is pointed at the container with
throwaway credentials, so they are hermetic and __gating__ -- but they require a
running Docker daemon and no real AWS.
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
                -- A very short visibility timeout so the un-acked job becomes
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
                -- message's window well past it. The job must NOT reappear in the
                -- gap the original timeout would have redelivered in, proving the
                -- ChangeMessageVisibility call held it.
                queue <- freshQueue container "mirror-extend" defaultQueueOptions{qoVisibilityTimeout = Seconds 1}
                unwrapQ (enqueue queue sampleJob)
                [message] <- receiveUntil queue
                unwrapQ (extendVisibility queue (msgReceipt message) (Seconds 30))
                -- Past the original 1s window (poll twice over ~2s); still hidden.
                stillHidden1 <- unwrapQ (receive queue)
                stillHidden2 <- unwrapQ (receive queue)
                map msgJob (stillHidden1 <> stillHidden2) `shouldBe` []

            it "reports an unreachable endpoint as the handle's typed transport fault" $ \_container -> do
                -- Point the backend at a loopback port with nothing listening: the
                -- poll must come back as the typed 'Left' with the unreachable
                -- cause -- classified at the adapter edge -- never as an exception
                -- through the caller.
                queue <- deadEndpointQueue
                outcome <- receive queue
                case outcome of
                    Left fault -> qfCause fault `shouldBe` TransportUnreachable
                    Right messages -> expectationFailure ("expected a typed transport fault, got " <> show messages)

-- An SQS backend pointed at a loopback port with nothing listening (port 1 is in
-- the privileged range and never bound), for the typed-fault classification case.
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
        , jobMirrorTarget = "https://mirror.example/left-pad/-/left-pad-1.3.0.tgz"
        , jobArtifactFilename = "left-pad-1.3.0.tgz"
        , jobTraceContext = Nothing
        }
