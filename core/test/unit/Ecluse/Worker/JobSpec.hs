-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Worker.JobSpec (spec) where

import Data.Aeson (Value, eitherDecodeStrict')
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Test.Hspec
import UnliftIO.Exception (try)

import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Package (
    Artifact (artFilename, artHashes),
    HashAlg (Blake2b, SHA1, SHA256, SRI),
 )
import Ecluse.Core.Registry (
    PublishError (PublishError),
    PublishFault (PublishRejected, PublishUrlUnformable),
    UrlFormationError (EmptyBaseUrl),
 )
import Ecluse.Core.Registry.Metadata (
    MetadataError (MetadataUndecodable, MetadataUnreachable),
    VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent),
    fetchVersionDetails,
 )
import Ecluse.Core.Registry.Npm.Publish (npmPublishDocument)
import Ecluse.Core.Telemetry.Metrics (MirrorResult (Failed, Published))
import Ecluse.Core.Worker (
    JobOutcome (Succeeded),
    processBatch,
    processJob,
 )
import Ecluse.Test.Package (unsafeHash)
import Ecluse.Test.Port (noopWorkerMetricsPort, recordingWorkerMetricsPort)
import Ecluse.Worker.Support

spec :: Spec
spec = do
    describe "npmPublishDocument" $ do
        it "assembles a PUT document with the version, dist integrity, and base64 attachment" $ do
            let document =
                    npmPublishDocument pkg ver "thing-1.0.0.tgz" (Just trueSri) (Just trueSha1) tarballBytes
                decoded :: Either String Value
                decoded = eitherDecodeStrict' document
            case decoded of
                Left err -> expectationFailure ("publish document is not valid JSON: " <> err)
                Right value -> do
                    stringAt ["name"] value `shouldBe` Just "thing"
                    stringAt ["dist-tags", "latest"] value `shouldBe` Just "1.0.0"
                    stringAt ["versions", "1.0.0", "dist", "integrity"] value `shouldBe` Just trueSri
                    stringAt ["_attachments", "thing-1.0.0.tgz", "data"] value
                        `shouldBe` Just (decodeUtf8 (convertToBase Base64 tarballBytes :: ByteString))
    describe "processJob -- the integrity gate" $ do
        it "publishes and reports success when the bytes match the admitted digest" $
            withUpstream $ \url ->
                withRuntime (Right ()) $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "publishes a sha384-only job end to end (fetch, compute sha384, verify, publish)" $
            -- The end-to-end proof that a sha384-admitted artifact is not admit-but-
            -- uncomputable: the worker fetches, recomputes sha384, matches, and publishes.
            withUpstream $ \url ->
                withRuntime (Right ()) $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (unsafeHash SRI trueSha384Sri :| []))
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "publishes a sha256-only job end to end (the #409 fix on the default floor)" $
            -- A sha256-only artifact is admitted by the default public floor; before #409 the
            -- worker could not compute sha256 and Dropped it. Now it fetches, recomputes
            -- sha256, matches, and publishes, never the admitted-but-dropped defect.
            withUpstream $ \url ->
                withRuntime (Right ()) $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (unsafeHash SHA256 trueSha256 :| []))
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "publishes a blake2b-only job end to end (the #409 fix, the top tier)" $
            -- A blake2b-only artifact is admitted by the floor and was likewise Dropped before
            -- #409; the worker now recomputes blake2b-512, matches, and publishes.
            withUpstream $ \url ->
                withRuntime (Right ()) $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (unsafeHash Blake2b trueBlake2b :| []))
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "refuses to publish (no publish) when the bytes do not match the digest" $
            withUpstream $ \url ->
                withRuntime (Right ()) $ \runtime queue logRef -> do
                    -- A tampered/substituted artifact: the threaded digest does not match
                    -- the fetched bytes, so the worker must NOT publish.
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (unsafeHash SHA1 wrongSha1 :| []))
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldSatisfy` isDropped
                    published <- plDocuments <$> readIORef logRef
                    published `shouldBe` []

        it "leaves the job for redelivery on a transient fetch failure (no publish)" $
            -- An unreachable upstream (connection refused) is a transient fault: the
            -- fetch throws, so the job is left for redelivery and nothing is published.
            withRuntime (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl (unsafeHash SHA1 trueSha1 :| []))
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isRetried
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "treats a registry rejection as retryable (job left for redelivery)" $
            withUpstream $ \url ->
                withRuntime (Left (PublishRejected (PublishError "503"))) $ \runtime queue _logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldSatisfy` isRetried

        it "leaves the job for redelivery when the artifact URL is unformable (no publish)" $
            -- A job whose artifact URL cannot be parsed into a request never reaches a
            -- fetch: the by-URL build fails, which the worker treats as a transient
            -- reason (Retried) rather than crashing the iteration. Nothing is published.
            withRuntime (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unformableUrl (unsafeHash SHA1 trueSha1 :| []))
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isRetried
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "drops a job (non-retryable) when the publish URL is unformable (a config fault)" $
            -- An unformable PUBLISH URL is a misconfiguration redelivery cannot fix, so
            -- the registry handle surfaces it as PublishUrlUnformable and the worker
            -- DROPS the job rather than re-enqueueing it forever -- the non-retryable
            -- terminal outcome, distinct from a retryable registry rejection.
            withUpstream $ \url ->
                withRuntime (Left (PublishUrlUnformable EmptyBaseUrl)) $ \runtime queue _logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldSatisfy` isDropped
    describe "processJob: ingest-time policy re-evaluation" $ do
        it "drops a job whose version current policy denies, without publishing" $
            -- The drift-to-deny close: a version admitted at serve time but denied by current
            -- policy must be dropped (acked/retired), never frozen into the trusted mirror store.
            -- 'unreachableUrl' doubles as a guard: were re-evaluation skipped, the artifact
            -- fetch would surface a Retried, not the Dropped this asserts.
            withRuntimePolicies (npmPolicies presentResolver [denyRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl (unsafeHash SHA1 trueSha1 :| []))
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isDropped
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "publishes a job whose version current policy admits (happy path unregressed)" $
            withUpstream $ \url ->
                withRuntimePolicies (npmPolicies presentResolver [admitRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "drops a job whose version the upstream no longer offers (withdrawn), without publishing" $
            -- The re-fetch yields no version (a yanked/unpublished version): a non-retryable
            -- drop, since a version the upstream has withdrawn must not be mirrored.
            withRuntimePolicies (npmPolicies (\_ _ -> pure VersionMissing) [admitRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl (unsafeHash SHA1 trueSha1 :| []))
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isDropped
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "retries a job when the re-evaluation metadata cannot be re-fetched, without publishing" $
            -- A transient metadata outage maps to the serve path's transient degrade: leave the
            -- job for redelivery rather than dropping it or publishing it unvetted.
            withRuntimePolicies (npmPolicies (\_ _ -> pure VersionMetadataUnavailable) [admitRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl (unsafeHash SHA1 trueSha1 :| []))
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isRetried
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "drops a job whose ecosystem has no configured policy (fail-closed), without publishing" $
            -- A job for an ecosystem with no bundle is fail-closed: never mirrored unvetted.
            withRuntimePolicies mempty noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl (unsafeHash SHA1 trueSha1 :| []))
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isDropped
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "drops a job whose artifact host the current tarball-host policy refuses (payload re-gated)" $
            -- The queue payload is a trust boundary: the host gate the serve path
            -- applied before its public fetch is re-established at ingest, so a URL
            -- injected or no-longer-honoured since enqueue is refused before any fetch.
            withRuntimePolicies (withHostGate (const False) (npmPolicies presentResolver [admitRule])) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl (unsafeHash SHA1 trueSha1 :| []))
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isDropped
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "drops a job whose artifact's current digests fall below the integrity floor" $
            -- Admission-policy drift toward refuse: the upstream now serves only a
            -- legacy SHA-1 for the file. The serve gate would 403 it below the floor;
            -- the shared oracle refuses it at ingest identically, so a
            -- no-longer-admissible artifact is never frozen into the rule-exempt mirror.
            withRuntimePolicies (npmPolicies (resolverWithArtifact sampleArtifact{artHashes = [unsafeHash SHA1 trueSha1]}) [admitRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl (unsafeHash SHA1 trueSha1 :| []))
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isDropped
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "drops a job whose version no longer carries any integrity digest" $
            -- The stripped-digest degrade: current metadata offers nothing to tie the
            -- bytes to. The serve gate 403s it as MissingIntegrity; the worker drops it.
            withRuntimePolicies (npmPolicies (resolverWithArtifact sampleArtifact{artHashes = []}) [admitRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl (unsafeHash SHA1 trueSha1 :| []))
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isDropped
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "drops a job whose admitted artifact file the current metadata no longer carries" $
            -- The withdrawn-file degrade: the version survives upstream but its file
            -- set no longer names the admitted artifact. A forwarded miss on the serve
            -- path; a non-retryable drop here (redelivery cannot restore the file).
            withRuntimePolicies (npmPolicies (resolverWithArtifact sampleArtifact{artFilename = "renamed-9.9.9.tgz"}) [admitRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl (unsafeHash SHA1 trueSha1 :| []))
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isDropped
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "retries a job when a fail-closed rule cannot be computed (undecidable), without publishing" $
            -- The advisory-outage degrade: the serve path renders the same cause a
            -- transient 503; the worker leaves the job for redelivery rather than
            -- dropping a serviceable job or publishing it unvetted.
            withRuntimePolicies (npmPolicies presentResolver [cannotVetRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl (unsafeHash SHA1 trueSha1 :| []))
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isRetried
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "acks a policy-denied job so it is retired, not redelivered forever" $
            -- Mirrors the integrity-mismatch ack test for the deny path: a current-policy deny
            -- is non-retryable, so the job is acked (retired) rather than left to redeliver.
            withRuntimePolicies (npmPolicies presentResolver [denyRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                enqueue_ queue (jobWith unreachableUrl (unsafeHash SHA1 trueSha1 :| []))
                messages <- receive_ queue
                runWM runtime (processBatch messages)
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []
                redelivered <- pollUntilRedelivered queue 5
                redelivered `shouldBe` False
    describe "processJob: the mirror-presence dedup probe" $ do
        -- The default 'recordingClient' answers the probe with an unparseable body (the
        -- absent posture), so every other test in this file already covers that
        -- fall-through; these cover the confirmed-present skip and the cannot-tell arms.
        it "acks an already-mirrored version without fetching or publishing" $
            -- 'unreachableUrl' doubles as the no-fetch guard: were the probe's skip not
            -- taken, the artifact fetch would surface a Retried, not this Succeeded.
            withRuntimeRegistry (\logRef -> mirrorListingClient logRef (Right ()) [ver]) admitPolicies noopWorkerMetricsPort $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl (unsafeHash SHA1 trueSha1 :| []))
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldBe` Succeeded
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "falls through to the full pipeline when the probe cannot reach the mirror" $
            -- A mirror outage means the probe cannot tell: the transport fault arrives
            -- as a typed value and the job must run the full gated pipeline (here, to
            -- a publish), never be skipped or failed on the probe alone.
            withUpstream $ \url ->
                withRuntimeRegistry (`probeUnreachableClient` Right ()) admitPolicies noopWorkerMetricsPort $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "falls through when the mirror lists other versions but not this one" $
            -- Presence is judged per version: a package already partially mirrored must
            -- still mirror its missing versions.
            withUpstream $ \url ->
                withRuntimeRegistry (\logRef -> mirrorListingClient logRef (Right ()) [otherVer]) admitPolicies noopWorkerMetricsPort $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "acks the skipped duplicate so it is retired from the queue" $
            withRuntimeRegistry (\logRef -> mirrorListingClient logRef (Right ()) [ver]) admitPolicies noopWorkerMetricsPort $ \runtime queue logRef -> do
                enqueue_ queue (jobWith unreachableUrl (unsafeHash SHA1 trueSha1 :| []))
                messages <- receive_ queue
                runWM runtime (processBatch messages)
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []
                redelivered <- pollUntilRedelivered queue 5
                redelivered `shouldBe` False
    describe "fetchVersionDetails: the shared single-version evaluation boundary" $ do
        -- The serve-time tarball gate and the worker both resolve a version through this one
        -- function, so its classification (the no-divergence boundary) is asserted directly.
        it "classifies a resolved version as present" $
            fetchVersionDetails (versionClient (Right (Just (sampleDetails pkg ver)))) pkg ver
                `shouldReturn` VersionPresent (sampleDetails pkg ver)

        it "classifies an absent version (resolved, but no such version) as missing" $
            fetchVersionDetails (versionClient (Right Nothing)) pkg ver
                `shouldReturn` VersionMissing

        it "classifies a metadata error as unavailable (the transient degrade)" $
            fetchVersionDetails (versionClient (Left MetadataUndecodable)) pkg ver
                `shouldReturn` VersionMetadataUnavailable

        it "classifies an unreachable upstream as unavailable (transport in the typed channel)" $
            fetchVersionDetails (versionClient (Left (MetadataUnreachable (transportFault TransportUnreachable "refused")))) pkg ver
                `shouldReturn` VersionMetadataUnavailable

        it "propagates a client that escapes its total contract (the invariant channel)" $ do
            -- The typed channel reports every real failure, so nothing here catches:
            -- a throw out of the fetch is an invariant break that must reach the
            -- caller's supervision (the worker loop, or the serve boundary), never be
            -- laundered into the transient degrade.
            outcome <- try (fetchVersionDetails throwingVersionClient pkg ver) :: IO (Either SomeException VersionEvaluation)
            outcome `shouldSatisfy` isLeft
    describe "processBatch -- ack semantics over the in-memory queue" $ do
        it "acks a successfully-mirrored job so it is not redelivered" $
            withUpstream $ \url ->
                withRuntime (Right ()) $ \runtime queue _logRef -> do
                    enqueue_ queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    messages <- receive_ queue
                    runWM runtime (processBatch messages)
                    -- A redelivery pass past the (immediate) visibility window yields
                    -- nothing: the job was acked.
                    redelivered <- receive_ queue
                    redelivered `shouldBe` []

        it "does not ack a transiently-failed job, so it redelivers" $
            withUpstream $ \url ->
                withRuntime (Left (PublishRejected (PublishError "503"))) $ \runtime queue _logRef -> do
                    enqueue_ queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    messages <- receive_ queue
                    runWM runtime (processBatch messages)
                    -- The publish was rejected (retryable), so the job is left un-acked
                    -- and redelivers. The worker extended the message's visibility before
                    -- the (failing) publish, so the in-memory double holds it past one
                    -- reclaim pass; poll a few times so the held message reappears.
                    redelivered <- pollUntilRedelivered queue 5
                    redelivered `shouldBe` True

        it "acks a DROPPED job so a tampered artifact is retired, not redelivered forever" $
            -- An integrity mismatch is non-retryable: redelivery can never make the
            -- bytes match, so the worker must ack the job to retire it from the queue
            -- (having alarmed at the mismatch) rather than leave it to redeliver
            -- indefinitely. A second poll past the visibility window yields nothing.
            withUpstream $ \url ->
                withRuntime (Right ()) $ \runtime queue logRef -> do
                    enqueue_ queue (jobWith url (unsafeHash SHA1 wrongSha1 :| []))
                    messages <- receive_ queue
                    runWM runtime (processBatch messages)
                    -- Nothing was published (the mismatch refused the publish)...
                    published <- plDocuments <$> readIORef logRef
                    published `shouldBe` []
                    -- ...and the job was acked: it does not redeliver.
                    redelivered <- pollUntilRedelivered queue 5
                    redelivered `shouldBe` False
    describe "the worker metrics port" $ do
        it "records a Published result for a successfully-mirrored job, through the port" $
            -- Drive the recording 'WorkerMetricsPort' and assert the worker classified the
            -- terminal outcome and recorded it through the interface -- proof the port is wired.
            withUpstream $ \url -> do
                (metricsPort, readResults) <- recordingWorkerMetricsPort
                withRuntimeWith metricsPort (Right ()) $ \runtime queue _logRef -> do
                    enqueue_ queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    messages <- receive_ queue
                    runWM runtime (processBatch messages)
                    readResults >>= (`shouldBe` [Published])

        it "records a Failed result for a tampered job, through the port" $
            withUpstream $ \url -> do
                (metricsPort, readResults) <- recordingWorkerMetricsPort
                withRuntimeWith metricsPort (Right ()) $ \runtime queue _logRef -> do
                    enqueue_ queue (jobWith url (unsafeHash SHA1 wrongSha1 :| []))
                    messages <- receive_ queue
                    runWM runtime (processBatch messages)
                    readResults >>= (`shouldBe` [Failed])
