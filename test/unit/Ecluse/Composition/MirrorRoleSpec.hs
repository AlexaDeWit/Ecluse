-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.MirrorRoleSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.BootError (BootError (MirrorRoleWithoutMirroring, SplitRoleNeedsDurableQueue), renderBootError)
import Ecluse.Composition.MirrorQueue (
    MirrorQueuePlan (MemoryBackend, SqsBackend),
    MirrorRuntimePlan (MirrorWith, NoMirroring),
 )
import Ecluse.Composition.MirrorRole (
    MirrorRole (MirrorOnly, ServeAndMirror, ServeOnly),
    enqueuesJobs,
    mirrorRoleRefusal,
    roleInvocation,
    runsWorker,
 )
import Ecluse.Runtime.Queue.Sqs (SqsConfig, defaultSqsConfig)

-- | A durable queue plan, the only backend a split deployment can hand jobs across.
durableQueue :: MirrorRuntimePlan
durableQueue = MirrorWith (SqsBackend sqsConfig)

sqsConfig :: SqsConfig
sqsConfig = defaultSqsConfig "https://sqs.us-east-1.amazonaws.com/123456789012/mirror" "us-east-1"

spec :: Spec
spec = do
    describe "runsWorker -- which roles spawn the consume loop" $ do
        it "keeps the worker embedded in the default proxy role" $
            runsWorker ServeAndMirror `shouldBe` True

        it "spawns no worker under --no-worker, so the proxy scales apart from queue depth" $
            runsWorker ServeOnly `shouldBe` False

        it "runs the worker in the dedicated mirror role" $
            runsWorker MirrorOnly `shouldBe` True

    describe "enqueuesJobs -- which roles need the enqueue buffer" $ do
        it "keeps enqueueing under --no-worker: the split moves the drain, not the producer" $
            enqueuesJobs ServeOnly `shouldBe` True

        it "enqueues nothing in the dedicated mirror role, which serves no request" $
            enqueuesJobs MirrorOnly `shouldBe` False

        it "enqueues in the single-process role" $
            enqueuesJobs ServeAndMirror `shouldBe` True

    describe "mirrorRoleRefusal -- a split role over the in-memory queue" $ do
        it "refuses --no-worker on the in-memory queue, whose jobs would never be consumed" $
            case mirrorRoleRefusal ServeOnly (MirrorWith MemoryBackend) of
                Left [SplitRoleNeedsDurableQueue invocation] -> invocation `shouldBe` "ecluse proxy --no-worker"
                other -> expectationFailure ("expected one SplitRoleNeedsDurableQueue, got " <> show other)

        it "refuses the dedicated worker on the in-memory queue, whose jobs never reach it" $
            case mirrorRoleRefusal MirrorOnly (MirrorWith MemoryBackend) of
                Left [SplitRoleNeedsDurableQueue invocation] -> invocation `shouldBe` "ecluse mirror"
                other -> expectationFailure ("expected one SplitRoleNeedsDurableQueue, got " <> show other)

        it "admits the single-process role on the in-memory queue (it consumes its own jobs)" $
            mirrorRoleRefusal ServeAndMirror (MirrorWith MemoryBackend) `shouldBe` Right ()

        it "admits either split role over a durable queue" $ do
            mirrorRoleRefusal ServeOnly durableQueue `shouldBe` Right ()
            mirrorRoleRefusal MirrorOnly durableQueue `shouldBe` Right ()

        it "admits --no-worker with no mount mirroring: there are no jobs to strand" $
            mirrorRoleRefusal ServeOnly NoMirroring `shouldBe` Right ()

        it "refuses the dedicated worker with no mount mirroring: it would have nothing to do" $
            mirrorRoleRefusal MirrorOnly NoMirroring `shouldBe` Left [MirrorRoleWithoutMirroring]

    describe "roleInvocation -- the refusal quotes what the operator typed" $
        it "spells each role as its command line, so the message names a runnable fix" $ do
            roleInvocation ServeAndMirror `shouldBe` "ecluse proxy"
            roleInvocation ServeOnly `shouldBe` "ecluse proxy --no-worker"
            roleInvocation MirrorOnly `shouldBe` "ecluse mirror"
            renderBootError (SplitRoleNeedsDurableQueue (roleInvocation MirrorOnly))
                `shouldSatisfy` T.isInfixOf "ecluse mirror"
