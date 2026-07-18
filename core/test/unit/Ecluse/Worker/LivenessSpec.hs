-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -Wno-unused-imports -Wno-orphans #-}

module Ecluse.Worker.LivenessSpec (spec) where

import Data.Aeson (Key, Value (Object, String), eitherDecodeStrict')
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, secondsToDiffTime)
import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO (timeout)
import UnliftIO.Exception (throwString)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Artifact (..),
    ArtifactKind (Tarball),
    Availability (Available),
    CodeExecSignal (NoCodeOnInstall),
    Hash,
    HashAlg (Blake2b, MD5, SHA1, SHA256, SRI),
    PackageDetails (..),
    PackageName,
    Trust (Untrusted),
    mkPackageName,
 )
import Ecluse.Core.Package qualified as Pkg
import Ecluse.Core.Queue (
    MirrorJob (..),
    MirrorQueue (receive),
    QueueMessage (msgReceipt),
    ReceiptHandle,
    Seconds (Seconds),
    enqueue,
 )
import Ecluse.Core.Registry.Publish (MirrorPublish (mpPublishArtifact))
import Ecluse.Core.Rules (PreparedRule (PreparedRule, prepEval, prepName, prepPrecedence, prepResilience))
import Ecluse.Core.Rules.Types (RuleVerdict (Allow, Deny))
import Ecluse.Core.Telemetry.Metrics (MirrorResult (Failed, Published))
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Core.Worker (
    IntegrityResult (IntegrityMismatch, IntegrityVerified),
    JobOutcome (Dropped, Retried, Succeeded),
    WorkerM,
    WorkerPolicies,
    WorkerRuntime (WorkerRuntime, wrHeartbeat, wrInjectTraceContext, wrManager, wrMetrics, wrPolicies, wrQueue, wrTracing),
    heartbeatHealthy,
    lastPoll,
    newWorkerHeartbeat,
    processBatch,
    runWorkerM,
    workerHeartbeatStaleAfter,
    workerLoop,
    workerPublishVisibilityBudget,
 )
import Ecluse.Test.Package (unsafeHash)
import Ecluse.Test.Port (noopWorkerMetricsPort, passthroughWorkerTracingPort, recordingWorkerMetricsPort)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Worker.Support

spec :: Spec
spec = do
    describe "heartbeat" $ do
        it "advances the last-successful-poll once the loop has polled the queue" $
            withRuntime (Right ()) $ \runtime _queue _logRef -> do
                pollBefore <- lastPoll (wrHeartbeat runtime)
                pollBefore `shouldBe` Nothing
                -- Run the consume loop briefly against the (empty) queue, then cancel
                -- it. Even an empty long-poll is a healthy poll, so the heartbeat must
                -- have advanced from 'Nothing'.
                _ <- timeout 200000 (runWM runtime (workerLoop testSupervision))
                pollAfter <- lastPoll (wrHeartbeat runtime)
                pollAfter `shouldSatisfy` isJust

        it "advances the heartbeat after each job in a batch, so a long batch cannot starve /livez" $
            -- The starvation this closes: the loop advanced the heartbeat once, before the
            -- whole (up to ten-job) batch, so a healthy worker grinding through large
            -- artifacts read as stalled past the staleness window and an orchestrator
            -- liveness probe killed the pod mid-publish. 'processBatch' now beats after each
            -- completed job. Each job's publish snapshots the heartbeat: with the per-job
            -- beat every job past the first sees it already advanced by its predecessor
            -- (before the fix all three would see the same single pre-batch instant).
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
    describe "workerHeartbeatStaleAfter -- the staleness budget covers one job's worst case" $
        it "exceeds a fetch and a publish of the maximum artifact (each the publish-visibility budget)" $ do
            -- The bound must clear one job's worst case -- a fetch and then a publish of the
            -- 512 MiB cap, each no faster than the publish-visibility floor
            -- ('workerPublishVisibilityBudget') -- not merely the idle poll cadence. Pinned
            -- here so lowering the staleness budget below the two budgets it must cover, or
            -- raising the publish budget past half of it, reddens rather than silently
            -- reopening the mid-batch liveness kill.
            let Seconds budget = workerPublishVisibilityBudget
            workerHeartbeatStaleAfter `shouldSatisfy` (> fromIntegral (2 * budget))
