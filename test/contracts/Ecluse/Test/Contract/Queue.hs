-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Queue laws separate payload preservation from optional delivery guarantees.
module Ecluse.Test.Contract.Queue (
    QueueDriver (..),
    queueContract,
    ephemeralQueueContract,
    leasedQueueContract,
) where

import Ecluse.Test.Contract.Catalogue (caseLabel)
import Test.Hspec

import Ecluse.Core.Queue (MirrorJob (..), MirrorQueue (..), QueueMessage (..), RemoteSpanContext (..))
import Ecluse.Test.Package (scopedNpm)
import Ecluse.Test.Queue (sampleJob)
import Ecluse.Test.Support (expectRightIO)

-- | Each example needs a fresh empty queue and a bounded delivery observation.
data QueueDriver = QueueDriver
    { queueHandle :: MirrorQueue
    , awaitDelivery :: IO [QueueMessage]
    }

-- | Every backend preserves the payload and accepts acknowledgement of its delivery.
queueContract :: Text -> SpecWith QueueDriver
queueContract contractGroup =
    describe "queue contract" $
        it (caseLabel (contractGroup <> "/payload") "preserves every payload field through delivery") queuePayloadLaw

-- Check payload and acknowledgement evidence. Broken-driver tests exercise this same law.
queuePayloadLaw :: QueueDriver -> Expectation
queuePayloadLaw driver =
    for_ [sampleJob, sampleJob{jobTriggerFilename = Nothing}, sampleJob{jobTraceContext = Just carrier}, sampleJob{jobPackage = scopedNpm "scope" "thing"}] $ \payload -> do
        expectRightIO (enqueue (queueHandle driver) payload)
        messages <- awaitDelivery driver
        map msgJob messages `shouldBe` [payload]
        map (jobPackage . msgJob) messages `shouldBe` [jobPackage payload]
        map (jobVersion . msgJob) messages `shouldBe` [jobVersion payload]
        map (jobTriggerFilename . msgJob) messages `shouldBe` [jobTriggerFilename payload]
        map (jobTraceContext . msgJob) messages `shouldBe` [jobTraceContext payload]
        traverse_ (expectRightIO . ack (queueHandle driver) . msgReceipt) messages
        expectRightIO (receive (queueHandle driver)) `shouldReturn` []
  where
    carrier = RemoteSpanContext "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01" "vendor=value"

-- | A destructive receive provides neither redelivery nor an increasing delivery count.
ephemeralQueueContract :: Text -> SpecWith QueueDriver
ephemeralQueueContract contractGroup = describe "ephemeral delivery contract" $ do
    it (caseLabel (contractGroup <> "/destructive-receive") "removes the job on receipt without requiring an acknowledgement") $ \driver -> do
        expectRightIO (enqueue (queueHandle driver) sampleJob)
        messages <- awaitDelivery driver
        map msgJob messages `shouldBe` [sampleJob]
        map msgReceiveCount messages `shouldBe` [1]
        expectRightIO (receive (queueHandle driver)) `shouldReturn` []
    it (caseLabel (contractGroup <> "/terminal-drop") "drops a terminal delivery without redelivery") $ \driver -> do
        expectRightIO (enqueue (queueHandle driver) sampleJob)
        messages <- awaitDelivery driver
        map msgJob messages `shouldBe` [sampleJob]
        traverse_ (expectRightIO . deadLetter (queueHandle driver) . msgReceipt) messages
        expectRightIO (receive (queueHandle driver)) `shouldReturn` []

-- | The driver uses a short lease and waits long enough to observe its expiry.
leasedQueueContract :: Text -> SpecWith QueueDriver
leasedQueueContract contractGroup = describe "leased delivery contract" $
    it (caseLabel (contractGroup <> "/redelivery") "redelivers an unacknowledged payload with an increasing delivery count") $ \driver -> do
        expectRightIO (enqueue (queueHandle driver) sampleJob)
        firstDelivery <- awaitDelivery driver
        map msgJob firstDelivery `shouldBe` [sampleJob]
        map msgReceiveCount firstDelivery `shouldBe` [1]
        secondDelivery <- awaitDelivery driver
        map msgJob secondDelivery `shouldBe` [sampleJob]
        map msgReceiveCount secondDelivery `shouldBe` [2]
        traverse_ (expectRightIO . ack (queueHandle driver) . msgReceipt) secondDelivery
