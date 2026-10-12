-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Worker.LoopIntegrationSpec (spec) where

import Data.Map.Strict qualified as Map
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Network.HTTP.Types (status200, status201, status409, status503)
import Test.Hspec
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (Artifact (artFilename), HashAlg (SRI), PackageDetails (pkgArtifacts), mkPackageName)
import Ecluse.Core.Queue (
    MirrorJob (..),
    MirrorQueue (enqueue, receive),
    Seconds (Seconds),
 )
import Ecluse.Core.Registry.Metadata (VersionDoc (vdDetails), VersionEvaluation (VersionPresent))
import Ecluse.Core.Version (mkVersion)
import Ecluse.Core.Worker (WorkerPolicies, WorkerPolicy (wpResolveVersion))
import Ecluse.Integration.Ministack (
    QueueOptions (qoVisibilityTimeout, qoWaitSeconds),
    defaultQueueOptions,
    freshQueue,
    withMinistack,
 )
import Ecluse.Integration.WorkerLoop (
    mirrorPoliciesAt,
    mirrorPoliciesUnderAt,
    newQueueEnv,
    observeAcknowledgement,
    publishedAtLeast,
    runLoopFor,
    runLoopUntil,
    withMirrorTarget,
 )
import Ecluse.Runtime.Env (envWorkerHeartbeat, lastPoll)
import Ecluse.Test.Package (sriSha512Of, unsafeFilename, unsafeHash)
import Ecluse.Test.Rules (admitRule, denyRule)
import Ecluse.Test.Stub (allCaptured, stubBaseUrl, withStub)
import Ecluse.Test.Support (expectRightIO)
import Ecluse.Test.Worker (workerSourceAt)

{- | The mirror worker end to end against real SQS (a @ministack@ container) and WAI stubs: the
visibility, redelivery, and boot-policy semantics the in-memory double cannot reproduce.
-}
spec :: Spec
spec =
    aroundAll withMinistack $
        describe "mirror worker (ministack + WAI stubs)" $ do
            it (caseLabel "integration/worker/native-001" "fetches, verifies, publishes, and acks a faithful job (via the loop)") $ \container ->
                withUpstream $ \upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-success" defaultQueueOptions
                        (observedQueue, acknowledged) <- observeAcknowledgement queue
                        env <- newQueueEnv observedQueue
                        policies <- faithfulPolicies mirrorUrl
                        expectRightIO (enqueue queue job)
                        runLoopUntil (workerSourceAt upstreamUrl policies) env acknowledged
                        acknowledged `shouldReturn` True
                        published <- readIORef publishLog
                        length published `shouldBe` 1
                        -- The worker acked the job, so it does not redeliver.
                        leftover <- expectRightIO (receive queue)
                        leftover `shouldBe` []

            it "retires an incomplete npm inventory without fetching or publishing a selected subset" $ \container ->
                withStub status200 "artifact bytes" $ \upstream ->
                    withMirrorTarget status201 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-incomplete-upload-data" defaultQueueOptions
                        (observedQueue, acknowledged) <- observeAcknowledgement queue
                        env <- newQueueEnv observedQueue
                        policies <- Map.map duplicateInventory <$> faithfulPolicies mirrorUrl
                        expectRightIO (enqueue queue job)
                        runLoopUntil (workerSourceAt (stubBaseUrl upstream) policies) env acknowledged
                        acknowledged `shouldReturn` True
                        readIORef publishLog `shouldReturn` []
                        allCaptured upstream `shouldReturn` []
                        expectRightIO (receive queue) `shouldReturn` []

            it (caseLabel "integration/worker/native-002" "publishes nothing when the artifact fails its integrity digest") $ \container ->
                withUpstream $ \upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-tamper" defaultQueueOptions
                        env <- newQueueEnv queue
                        -- The re-admitted digest is well-formed but does not match the served
                        -- bytes: a tampered artifact the worker must refuse to publish.
                        tamperPolicies <- mirrorPoliciesAt Nothing mirrorUrl (unsafeHash SRI mismatchSri :| [])
                        expectRightIO (enqueue queue job)
                        runLoopFor (workerSourceAt upstreamUrl tamperPolicies) env 4_000_000
                        published <- readIORef publishLog
                        published `shouldBe` []

            it (caseLabel "integration/worker/native-003" "requires target identity proof after a 409 and acknowledges the whole version") $ \container ->
                withUpstream $ \upstreamUrl ->
                    withMirrorTarget status409 $ \mirrorUrl _publishLog -> do
                        queue <- freshQueue container "worker-idempotent" defaultQueueOptions
                        (observedQueue, acknowledged) <- observeAcknowledgement queue
                        env <- newQueueEnv observedQueue
                        policies <- faithfulPolicies mirrorUrl
                        expectRightIO (enqueue queue job)
                        runLoopUntil (workerSourceAt upstreamUrl policies) env acknowledged
                        acknowledged `shouldReturn` True
                        leftover <- expectRightIO (receive queue)
                        leftover `shouldBe` []

            it (caseLabel "integration/worker/native-004" "leaves a transiently-rejected job un-acked, so it redelivers") $ \container ->
                withUpstream $ \upstreamUrl ->
                    -- The mirror target answers 503 (a retryable rejection). The worker
                    -- must not ack, so the message redelivers: a real second delivery.
                    withMirrorTarget status503 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-retry" defaultQueueOptions
                        env <- newQueueEnv queue
                        policies <- faithfulPolicies mirrorUrl
                        expectRightIO (enqueue queue job)
                        -- A second PUT exists only because the un-acked 503 message redelivered
                        -- through 'releaseForRetry'. Stopping at the first PUT races that release.
                        runLoopUntil (workerSourceAt upstreamUrl policies) env (publishedAtLeast publishLog 2)
                        published <- readIORef publishLog
                        -- At least twice, not an exact count: the loop keeps redriving until
                        -- teardown, so "redelivered at least once" is the invariant.
                        length published `shouldSatisfy` (>= 2)
                        published `shouldSatisfy` all (== npmPublishPath)

            it (caseLabel "integration/worker/native-005" "never mirrors an over-cap artifact; it dead-letters the job to ride the redrive policy (issue #846)") $ \container ->
                withUpstream $ \upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-overcap" defaultQueueOptions
                        env <- newQueueEnv queue
                        -- A fetch cap below the artifact's size. The bounded fetch aborts fail-
                        -- closed, so the worker dead-letters the job and never mirrors it.
                        policies <- mirrorPoliciesAt (Just 8) mirrorUrl (unsafeHash SRI trueSri :| [])
                        expectRightIO (enqueue queue job)
                        runLoopFor (workerSourceAt upstreamUrl policies) env 4_000_000
                        published <- readIORef publishLog
                        published `shouldBe` []

            it (caseLabel "integration/worker/native-006" "refuses a queued job under the stricter policy the consuming worker booted") $ \container ->
                withUpstream $ \upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-rollout-strict" rolloutQueueOptions
                        env <- newQueueEnv queue
                        -- A permissive role queued this job. The queue carries no policy, so the
                        -- worker's own boot rules decide, and a deny is terminal rather than retried.
                        strict <- mirrorPoliciesUnderAt [denyRule] mirrorUrl (unsafeHash SRI trueSri :| [])
                        expectRightIO (enqueue queue job)
                        runLoopFor (workerSourceAt upstreamUrl strict) env rolloutLoopWindow
                        readIORef publishLog `shouldReturn` []
                        assertQueueDrained queue

            it (caseLabel "integration/worker/native-007" "publishes that same queued job under the older policy the consuming worker booted") $ \container ->
                withUpstream $ \upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-rollout-old" rolloutQueueOptions
                        env <- newQueueEnv queue
                        old <- mirrorPoliciesUnderAt [admitRule] mirrorUrl (unsafeHash SRI trueSri :| [])
                        expectRightIO (enqueue queue job)
                        -- A fixed window rather than a stop at the first publish, so the ack lands
                        -- inside the run and a missing one shows up as a second delivery.
                        runLoopFor (workerSourceAt upstreamUrl old) env rolloutLoopWindow
                        readIORef publishLog `shouldReturn` [npmPublishPath]
                        assertQueueDrained queue

            it (caseLabel "integration/worker/native-008" "advances the heartbeat as the loop polls a real queue") $ \container ->
                withUpstream $ \_upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl _publishLog -> do
                        queue <- freshQueue container "worker-heartbeat" defaultQueueOptions{qoWaitSeconds = 1}
                        env <- newQueueEnv queue
                        policies <- faithfulPolicies mirrorUrl
                        pollBefore <- lastPoll (envWorkerHeartbeat env)
                        pollBefore `shouldBe` Nothing
                        -- No job enqueued: an idle loop still completes real polls, so
                        -- the heartbeat must advance from Nothing.
                        runLoopFor policies env 3_000_000
                        pollAfter <- lastPoll (envWorkerHeartbeat env)
                        pollAfter `shouldSatisfy` isJust

-- A visibility window short enough that the ack assertions can outwait it. The shipped 30s default
-- would hide an un-acked message behind its lease, so the read after the loop could never fail.
rolloutQueueOptions :: QueueOptions
rolloutQueueOptions = defaultQueueOptions{qoVisibilityTimeout = Seconds 2}

-- Well past a loopback publish and its ack, and past several redeliveries had that ack not run.
rolloutLoopWindow :: Int
rolloutLoopWindow = 8_000_000

{- | The queue holds nothing, read past 'rolloutQueueOptions' visibility window so a message the
worker left un-acked is visible again rather than hidden behind its lease.
-}
assertQueueDrained :: MirrorQueue -> IO ()
assertQueueDrained queue = do
    threadDelay 2_500_000
    leftover <- expectRightIO (receive queue)
    leftover `shouldBe` []

-- The artifact bytes the upstream stub serves.
tarballBytes :: LByteString
tarballBytes = "left-pad-artifact-bytes"

-- The true SRI of the served bytes: the digest the worker re-admits from current
-- metadata and verifies the fetched bytes against.
trueSri :: Text
trueSri = sriSha512Of (toStrict tarballBytes)

-- A well-formed sha512 SRI of OTHER bytes, the tamper fixture: current metadata whose
-- digest the served bytes cannot satisfy, distinct from a malformed digest.
mismatchSri :: Text
mismatchSri = sriSha512Of "completely-different-bytes"

-- Policies whose re-admitted artifact carries the served bytes' true digest, so
-- verification passes and the pipeline publishes at that mirror target.
faithfulPolicies :: Text -> IO WorkerPolicies
faithfulPolicies mirrorUrl = mirrorPoliciesAt Nothing mirrorUrl (unsafeHash SRI trueSri :| [])

npmPublishPath :: ByteString
npmPublishPath = "/left-pad"

-- Source metadata supplies every location and digest, independently of this version trigger.
job :: MirrorJob
job =
    MirrorJob
        { jobPackage = mkPackageName Npm Nothing "left-pad"
        , jobVersion = mkVersion Npm "1.3.0"
        , jobTriggerFilename = Just (unsafeFilename "left-pad-1.3.0.tgz")
        , jobTraceContext = Nothing
        }

-- A WAI upstream serving the artifact bytes at any path, yielding its base URL.
withUpstream :: (Text -> IO a) -> IO a
withUpstream body = withStub status200 tarballBytes (body . stubBaseUrl)

duplicateInventory :: WorkerPolicy -> WorkerPolicy
duplicateInventory policy = policy{wpResolveVersion = \pay name version -> duplicateVersionInventory <$> wpResolveVersion policy pay name version}

duplicateVersionInventory :: VersionEvaluation -> VersionEvaluation
duplicateVersionInventory (VersionPresent doc latest) =
    let details = vdDetails doc
     in VersionPresent doc{vdDetails = details{pkgArtifacts = fmap (\artifact -> artifact{artFilename = "another.tgz"}) (pkgArtifacts details) <> pkgArtifacts details}} latest
duplicateVersionInventory other = other
