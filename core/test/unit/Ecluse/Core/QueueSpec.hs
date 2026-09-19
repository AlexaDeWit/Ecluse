-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.QueueSpec (spec) where

import Test.Hspec

import Ecluse.Core.Queue (
    DeadLetterTerminus (TerminusAbsent, TerminusAttached),
    DeliveryBudget (DeliveryBudget),
    QueueMessage (QueueMessage, msgJob, msgLease, msgReceipt, msgReceiveCount),
    deliveryBudgetSpent,
    effectiveDeliveryBudget,
    mkReceiptHandle,
 )
import Ecluse.Test.Queue (sampleJob)

{- | Tests for the contract module's delivery-budget verdicts. The buffered producer hand-off
lives beside it in "Ecluse.Core.Queue.BufferSpec", the in-memory backend in
"Ecluse.Core.Queue.MemorySpec".
-}
spec :: Spec
spec = do
    describe "deliveryBudgetSpent -- the shared redelivery verdict" $ do
        it "grants every delivery below the budget" $
            map (deliveryBudgetSpent (DeliveryBudget 5) . deliveredTimes) [1, 2, 3, 4]
                `shouldBe` [False, False, False, False]

        it "is spent at the budget, and stays spent past it" $
            map (deliveryBudgetSpent (DeliveryBudget 5) . deliveredTimes) [5, 6, 50]
                `shouldBe` [True, True, True]

        it "still grants a first delivery under any budget, however small" $
            -- A budget of one (or zero, or below) would otherwise retire a job that never
            -- ran. The worker retires no message before it tries that message at least once.
            map (\budget -> deliveryBudgetSpent budget (deliveredTimes 1)) [DeliveryBudget 1, DeliveryBudget 0, DeliveryBudget (-3)]
                `shouldBe` [False, False, False]

        it "retires on the second delivery under a budget too small to reach" $
            map (\budget -> deliveryBudgetSpent budget (deliveredTimes 2)) [DeliveryBudget 1, DeliveryBudget 0]
                `shouldBe` [True, True]

    describe "effectiveDeliveryBudget -- the dead-letter queue captures first" $ do
        it "raises the configured floor one delivery past an attached terminus's capture count" $
            -- Écluse must not retire the message at the configured 5 and rob the
            -- dead-letter queue: the operator's redrive policy captures it at 10.
            effectiveDeliveryBudget (DeliveryBudget 5) (TerminusAttached (Just (DeliveryBudget 10)))
                `shouldBe` DeliveryBudget 11

        it "keeps the configured floor when it already sits above the capture count" $
            effectiveDeliveryBudget (DeliveryBudget 20) (TerminusAttached (Just (DeliveryBudget 3)))
                `shouldBe` DeliveryBudget 20

        it "keeps the configured floor when a terminus declares no capture count" $
            effectiveDeliveryBudget (DeliveryBudget 5) (TerminusAttached Nothing)
                `shouldBe` DeliveryBudget 5

        it "keeps the configured floor when nothing captures poison messages" $
            -- The no-terminus case the budget exists for: the budget is the only terminus.
            effectiveDeliveryBudget (DeliveryBudget 5) TerminusAbsent `shouldBe` DeliveryBudget 5
  where
    -- A delivery of the sample job on its n-th receive. These verdicts read only the
    -- count, so the rest of the message stays fixed.
    deliveredTimes :: Int -> QueueMessage
    deliveredTimes n =
        QueueMessage{msgJob = sampleJob, msgReceipt = mkReceiptHandle "receipt", msgReceiveCount = n, msgLease = Nothing}
