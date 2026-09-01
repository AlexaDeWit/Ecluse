-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.MirrorRoleSpec (spec) where

import Test.Hspec

import Ecluse.Composition.BootError (BootError (MirrorRoleWithoutMirroring, SplitRoleNeedsDurableQueue))
import Ecluse.Composition.MirrorQueue (
    MirrorQueuePlan (MemoryBackend, SqsBackend),
    MirrorRuntimePlan (MirrorWith, NoMirroring),
 )
import Ecluse.Composition.MirrorRole (
    enqueuesJobs,
    mirrorRoleRefusal,
    runsWorker,
    spawnsWorker,
 )
import Ecluse.Composition.Types (MirrorRole (MirrorOnly, ServeAndMirror, ServeOnly))
import Ecluse.Runtime.Queue.Sqs (SqsConfig, defaultSqsConfig)

-- | A durable queue plan, the only backend a split deployment can hand jobs across.
durableQueue :: MirrorRuntimePlan
durableQueue = MirrorWith (SqsBackend sqsConfig)

sqsConfig :: SqsConfig
sqsConfig = defaultSqsConfig "https://sqs.us-east-1.amazonaws.com/123456789012/mirror" "us-east-1"

spec :: Spec
spec = do
    describe "runsWorker -- which roles want a consume loop at all" $ do
        it "keeps the worker embedded in the default proxy role" $
            runsWorker ServeAndMirror `shouldBe` True

        it "spawns no worker under --no-worker, so the proxy scales apart from queue depth" $
            runsWorker ServeOnly `shouldBe` False

        it "runs the worker in the dedicated mirror role" $
            runsWorker MirrorOnly `shouldBe` True

    describe "spawnsWorker -- the one fact the spawn decision and /livez both read" $ do
        it "spawns no worker under --no-worker, so the proxy scales apart from queue depth" $
            spawnsWorker ServeOnly durableQueue `shouldBe` False

        it "spawns no worker with nothing to mirror, whatever the role wants" $ do
            -- The inert queue returns an empty batch at once, so a loop over it would spin
            -- unpaced and stamp a heartbeat for work that does not exist.
            spawnsWorker ServeAndMirror NoMirroring `shouldBe` False
            spawnsWorker ServeOnly NoMirroring `shouldBe` False
            spawnsWorker MirrorOnly NoMirroring `shouldBe` False

        it "spawns the embedded worker for the single-process role over a real queue" $
            spawnsWorker ServeAndMirror durableQueue `shouldBe` True

        it "spawns the worker for the dedicated mirror role over a real queue" $
            spawnsWorker MirrorOnly durableQueue `shouldBe` True

        it "spawns the embedded worker over the in-memory queue, the single-process default" $
            spawnsWorker ServeAndMirror (MirrorWith MemoryBackend) `shouldBe` True

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
