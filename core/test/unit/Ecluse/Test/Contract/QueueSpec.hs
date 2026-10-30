-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Broken queue observations must fail the canonical payload law.
module Ecluse.Test.Contract.QueueSpec (spec) where

import Test.Hspec

import Ecluse.Core.Queue (MirrorJob (jobTraceContext), MirrorQueue (receive), QueueMessage (msgJob))
import Ecluse.Test.Contract.Queue (QueueDriver (..), queuePayloadLaw)
import Ecluse.Test.Queue (newTestMemoryQueue, otherJob)
import Ecluse.Test.Support (expectRightIO)

-- | These controls exercise the same expectation used by both queue providers.
spec :: Spec
spec = describe "queue contract negative controls" $ do
    it "accepts the real memory backend" $ do
        queue <- newTestMemoryQueue
        queuePayloadLaw QueueDriver{queueHandle = queue, awaitDelivery = expectRightIO (receive queue)}
    it "rejects a driver that reports no delivered message" $ do
        queue <- newTestMemoryQueue
        queuePayloadLaw QueueDriver{queueHandle = queue, awaitDelivery = pure []} `shouldThrow` anyException
    it "rejects a driver that changes the delivered payload" $ do
        queue <- newTestMemoryQueue
        let corrupt message = message{msgJob = otherJob}
        queuePayloadLaw QueueDriver{queueHandle = queue, awaitDelivery = map corrupt <$> expectRightIO (receive queue)}
            `shouldThrow` anyException
    it "rejects a driver that drops the trace carrier" $ do
        queue <- newTestMemoryQueue
        let untraced message = message{msgJob = (msgJob message){jobTraceContext = Nothing}}
        queuePayloadLaw QueueDriver{queueHandle = queue, awaitDelivery = map untraced <$> expectRightIO (receive queue)}
            `shouldThrow` anyException
