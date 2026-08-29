-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Worker.LivenessSpec (spec) where

import Data.Time (addUTCTime, getCurrentTime)
import Test.Hspec
import UnliftIO (timeout)

import Ecluse.Core.Queue (Seconds (Seconds))
import Ecluse.Core.Registry.Publish (MirrorPublish (mpPublishArtifact))
import Ecluse.Core.Worker (
    Liveness (Liveness, liveHealthy, liveLastPoll),
    alwaysLive,
    heartbeatHealthy,
    heartbeatLivenessNow,
    lastPoll,
    newWorkerHeartbeat,
    processBatch,
    recordPoll,
    workerHeartbeatStaleAfter,
    workerLoop,
    workerPublishVisibilityBudget,
    wrHeartbeat,
 )
import Ecluse.Test.Port (noopWorkerMetricsPort)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Worker.Support

spec :: Spec
spec = do
    describe "heartbeat" $ do
        it "advances the last-successful-poll once the loop has polled the queue" $
            withRuntime (Right ()) $ \runtime _queue _logRef -> do
                pollBefore <- lastPoll (wrHeartbeat runtime)
                pollBefore `shouldBe` Nothing
                -- Even an empty long-poll is a healthy poll, so the heartbeat must advance from
                -- 'Nothing'.
                _ <- timeout 200000 (runWM runtime (workerLoop testSupervision))
                pollAfter <- lastPoll (wrHeartbeat runtime)
                pollAfter `shouldSatisfy` isJust

        it "advances the heartbeat after each job in a batch, so a long batch cannot starve /livez" $
            -- 'processBatch' beats the heartbeat after each completed job, not once before the
            -- whole batch. A single pre-batch beat lets a healthy worker grinding through large
            -- artifacts read as stalled, and a liveness probe then kills the pod mid-publish.
            withUpstream $ \url -> do
                heartbeat <- newWorkerHeartbeat
                seen <- newIORef []
                logRef <- newIORef (PublishLog [] [])
                let base = recordingPublish logRef (Right ())
                    snapshotOnPublish =
                        base
                            { mpPublishArtifact = \p v art doc -> do
                                lastPoll heartbeat >>= \snap -> modifyIORef' seen (snap :)
                                mpPublishArtifact base p v art doc
                            }
                queue <- newTestMemoryQueue
                withWiredRuntimeHeartbeat heartbeat queue (withPublish snapshotOnPublish admitPolicies) noopWorkerMetricsPort $ \runtime -> do
                    traverse_ (enqueue_ queue) (replicate 3 (jobWith url))
                    messages <- receive_ queue
                    length messages `shouldBe` 3
                    runWM runtime (processBatch messages)
                    snapshots <- reverse <$> readIORef seen
                    length snapshots `shouldBe` 3
                    -- Every job after the first published against an already-advanced
                    -- heartbeat: the beat is per job, not once for the batch.
                    drop 1 snapshots `shouldSatisfy` all isJust
                    -- Distinct instants (not one shared pre-batch beat) confirm each job
                    -- advanced it in turn.
                    let advanced = catMaybes snapshots
                    length advanced `shouldSatisfy` (>= 2)
                    ordNub advanced `shouldBe` advanced
    describe "heartbeatHealthy (the /livez staleness rule)" $ do
        it "is healthy before the first poll (the worker is starting, not stalled)" $
            heartbeatHealthy epoch Nothing `shouldBe` True

        it "is healthy for a poll within the staleness window" $
            heartbeatHealthy (addUTCTime 10 epoch) (Just epoch) `shouldBe` True

        it "is unhealthy once the last poll is staler than the threshold" $
            heartbeatHealthy (addUTCTime (workerHeartbeatStaleAfter + 1) epoch) (Just epoch)
                `shouldBe` False
    describe "heartbeatLivenessNow (the verdict a running loop's probe renders)" $ do
        it "reports the poll instant beside the verdict, so a probe can show staleness" $ do
            heartbeat <- newWorkerHeartbeat
            now <- getCurrentTime
            recordPoll heartbeat now
            liveness <- heartbeatLivenessNow heartbeat
            liveness `shouldBe` Liveness{liveHealthy = True, liveLastPoll = Just now}

        it "is healthy with no poll recorded, reporting no instant" $ do
            liveness <- newWorkerHeartbeat >>= heartbeatLivenessNow
            liveness `shouldBe` alwaysLive

        it "is unhealthy once the recorded poll is staler than the threshold" $ do
            heartbeat <- newWorkerHeartbeat
            now <- getCurrentTime
            let stale = addUTCTime (negate (workerHeartbeatStaleAfter + 60)) now
            recordPoll heartbeat stale
            liveness <- heartbeatLivenessNow heartbeat
            -- The stale instant still rides along: an orchestrator sees how far behind it is.
            liveness `shouldBe` Liveness{liveHealthy = False, liveLastPoll = Just stale}

    describe "alwaysLive (the verdict of a process running no loop)" $
        it "is live with no poll to report, so a serve-only pod is never killed for a worker" $
            alwaysLive `shouldBe` Liveness{liveHealthy = True, liveLastPoll = Nothing}

    describe "workerHeartbeatStaleAfter -- the staleness budget covers one job's worst case" $
        it "exceeds a fetch and a publish of the maximum artifact (each the publish-visibility budget)" $ do
            -- The budget must clear one job's worst case: a fetch and then a publish of the 512 MiB
            -- cap, each no faster than 'workerPublishVisibilityBudget'. Lowering the staleness
            -- budget below the two, or raising the publish budget past half of it, reopens the mid-
            -- batch liveness kill.
            let Seconds budget = workerPublishVisibilityBudget
            workerHeartbeatStaleAfter `shouldSatisfy` (> fromIntegral (2 * budget))
