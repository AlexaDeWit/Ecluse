-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Worker.LivenessSpec (spec) where

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
import Ecluse.Core.Worker.Liveness (newWorkerHeartbeatWithClock)
import Ecluse.Test.Port (noopWorkerMetricsPort)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Support (newTestClock)
import Ecluse.Worker.Support

spec :: Spec
spec = do
    describe "heartbeat" $ do
        it "advances the last-successful-poll once the loop has polled the queue" $
            withRuntime (Right ()) $ \runtime _queue _logRef -> do
                pollBefore <- lastPoll (wrHeartbeat runtime)
                pollBefore `shouldBe` Nothing
                _ <- timeout 200000 (runWM runtime (workerLoop testSupervision))
                pollAfter <- lastPoll (wrHeartbeat runtime)
                pollAfter `shouldSatisfy` isJust

        it "advances the heartbeat after each job in a batch, so a long batch cannot starve /livez" $
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
                    drop 1 snapshots `shouldSatisfy` all isJust
                    let advanced = catMaybes snapshots
                    length advanced `shouldSatisfy` (>= 2)
                    ordNub advanced `shouldBe` advanced
    describe "heartbeatHealthy (the /livez staleness rule)" $ do
        it "is healthy at the startup deadline before the first poll" $
            heartbeatHealthy (addUTCTime workerHeartbeatStaleAfter epoch) epoch Nothing `shouldBe` True

        it "is unhealthy after the startup deadline before the first poll" $
            heartbeatHealthy (addUTCTime (workerHeartbeatStaleAfter + 1) epoch) epoch Nothing `shouldBe` False

        it "is healthy for a poll within the staleness window" $
            heartbeatHealthy (addUTCTime 10 epoch) epoch (Just epoch) `shouldBe` True

        it "is unhealthy once the last poll is staler than the threshold" $
            heartbeatHealthy (addUTCTime (workerHeartbeatStaleAfter + 1) epoch) epoch (Just epoch)
                `shouldBe` False
    describe "heartbeatLivenessNow (the verdict a running loop's probe renders)" $ do
        it "expires startup after failed receives without inventing successful progress" $ do
            (clock, setClock) <- newTestClock epoch
            heartbeat <- newWorkerHeartbeatWithClock clock
            calls <- newIORef (0 :: Int)
            queue <- faultingReceiveQueue calls
            withWiredRuntimeHeartbeat heartbeat queue admitPolicies noopWorkerMetricsPort $ \runtime -> do
                _ <- timeout 200000 (runWM runtime (workerLoop testSupervision))
                readIORef calls >>= (`shouldSatisfy` (> 0))
            let deadline = addUTCTime workerHeartbeatStaleAfter epoch
            setClock deadline
            heartbeatLivenessNow heartbeat `shouldReturn` alwaysLive
            setClock (addUTCTime 1 deadline)
            heartbeatLivenessNow heartbeat `shouldReturn` Liveness False Nothing
            lastPoll heartbeat `shouldReturn` Nothing

        it "recovers after expired startup and measures later stalls from successful progress" $ do
            (clock, setClock) <- newTestClock epoch
            heartbeat <- newWorkerHeartbeatWithClock clock
            let recoveredAt = addUTCTime (workerHeartbeatStaleAfter + 1) epoch
            setClock recoveredAt
            heartbeatLivenessNow heartbeat `shouldReturn` Liveness False Nothing
            clock >>= recordPoll heartbeat
            heartbeatLivenessNow heartbeat `shouldReturn` Liveness True (Just recoveredAt)
            setClock (addUTCTime workerHeartbeatStaleAfter recoveredAt)
            heartbeatLivenessNow heartbeat `shouldReturn` Liveness True (Just recoveredAt)
            setClock (addUTCTime (workerHeartbeatStaleAfter + 1) recoveredAt)
            heartbeatLivenessNow heartbeat `shouldReturn` Liveness False (Just recoveredAt)

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
            liveness `shouldBe` Liveness{liveHealthy = False, liveLastPoll = Just stale}

    describe "alwaysLive (the verdict of a process running no loop)" $
        it "is live with no poll to report, so a serve-only pod is never killed for a worker" $
            alwaysLive `shouldBe` Liveness{liveHealthy = True, liveLastPoll = Nothing}

    describe "workerHeartbeatStaleAfter -- the staleness budget covers one job's worst case" $
        it "exceeds a fetch and a publish of the maximum artifact (each the publish-visibility budget)" $ do
            let Seconds budget = workerPublishVisibilityBudget
            workerHeartbeatStaleAfter `shouldSatisfy` (> fromIntegral (2 * budget))
