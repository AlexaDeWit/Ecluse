-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Active-work reservations retain their pre-retention-correction charges.
module Ecluse.Core.Mirror.State.CodecSpec (spec) where

import Test.Hspec

import Ecluse.Core.Mirror.State (recordExpectedInventory)
import Ecluse.Core.Mirror.State.Codec
import Ecluse.Core.Mirror.State.Internal
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Worker.Managed (readMirrorRecord)
import Ecluse.Core.Worker.Types (WorkerMirrorState (..), WorkerStateBudget (..))
import Ecluse.Test.Mirror.State
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "unchanged state work reservations" $ do
    it "keeps the old inventory, update, worker and encoder charges" $ do
        inventory <- mirrorSample [1] False "https://first.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        projectRecordWorkWeight defaultLimits record `shouldBe` Right 25698
        mirrorInventoryWeight inventory `shouldBe` 21074
        projectRecordWeight defaultLimits record `shouldSatisfy` either (const False) (< 25698)
        paid <- newIORef ([] :: [Int])
        let charge cost = modifyIORef' paid (cost :)
            store =
                MirrorState
                    { msReadProject = \_ _ _ -> pure (Right (StateLoaded revision record))
                    , msWriteProject = \_ _ _ _ -> fail "idempotent admission must not write"
                    , msRetainCompanion = \_ _ -> fail "unexpected companion write"
                    , msReadCompanion = \_ _ -> fail "unexpected companion read"
                    }
            budget = WorkerStateBudget charge charge charge charge charge charge charge
            binding = WorkerMirrorState store target defaultLimits (\action -> Just <$> action budget)
        void (recordExpectedInventory defaultLimits charge target store inventory >>= expectRight)
        readIORef paid `shouldReturn` [97640]
        writeIORef paid []
        void (readMirrorRecord budget binding mirrorPackage >>= expectRight)
        readIORef paid `shouldReturn` [77094]
        writeIORef paid []
        void (chargedEncodeProjectRecord charge defaultLimits record >>= expectRight)
        readIORef paid `shouldReturn` [135560]

target :: StateTarget
target = stateTarget "https://private.test"

revision :: StateRevision
revision = StateRevision target mirrorPackage "etag"
