module Ecluse.MirrorQueueSpec (spec) where

import Test.Hspec

import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Integration.Ministack (
    QueueOptions (qoVisibilityTimeout),
    defaultQueueOptions,
    freshQueue,
    receiveUntil,
    withMinistack,
 )
import Ecluse.Package (HashAlg (SHA1), mkPackageName)
import Ecluse.Queue (
    MirrorArtifact (..),
    MirrorJob (..),
    MirrorQueue (..),
    QueueMessage (..),
    Seconds (..),
 )
import Ecluse.Test.Package (unsafeHash, validSha1)
import Ecluse.Version (mkVersion)

{- | Integration tests exercise the SQS 'MirrorQueue' backend against a real
endpoint provided by a @ministack@ container (launched via @testcontainers@, shared
through "Ecluse.Integration.Ministack"). @amazonka@ is pointed at the container with
throwaway credentials, so they are hermetic and __gating__ — but they require a
running Docker daemon and no real AWS.
-}
spec :: Spec
spec =
    around withMinistack $
        describe "mirror queue (ministack)" $ do
            it "round-trips a job: enqueue, receive, ack, then no redelivery" $ \container -> do
                queue <- freshQueue container "mirror-roundtrip" defaultQueueOptions
                enqueue queue sampleJob
                [message] <- receiveUntil queue
                msgJob message `shouldBe` sampleJob
                ack queue (msgReceipt message)
                -- After the ack the job is gone: a poll past the (short) visibility
                -- window yields nothing.
                afterAck <- receive queue
                map msgJob afterAck `shouldBe` []

            it "redelivers a job that was received but never acked" $ \container -> do
                -- A very short visibility timeout so the un-acked job becomes
                -- visible again within the test's patience.
                queue <- freshQueue container "mirror-redeliver" defaultQueueOptions{qoVisibilityTimeout = Seconds 1}
                enqueue queue sampleJob
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
                enqueue queue sampleJob
                [message] <- receiveUntil queue
                extendVisibility queue (msgReceipt message) (Seconds 30)
                -- Past the original 1s window (poll twice over ~2s); still hidden.
                stillHidden1 <- receive queue
                stillHidden2 <- receive queue
                map msgJob (stillHidden1 <> stillHidden2) `shouldBe` []

-- | A sample mirror job carried end-to-end through SQS.
sampleJob :: MirrorJob
sampleJob =
    MirrorJob
        { jobPackage = mkPackageName Npm Nothing "left-pad"
        , jobVersion = mkVersion Npm "1.3.0"
        , jobArtifactUrl = "https://registry.npmjs.org/left-pad/-/left-pad-1.3.0.tgz"
        , jobMirrorTarget = "https://mirror.example/left-pad/-/left-pad-1.3.0.tgz"
        , jobArtifact =
            MirrorArtifact
                { maFilename = "left-pad-1.3.0.tgz"
                , maHashes = unsafeHash SHA1 validSha1 :| []
                , maSize = Just 256
                }
        }
