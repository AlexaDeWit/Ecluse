{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}

module Ecluse.Worker.JobSpec (spec) where

import Crypto.Hash (Blake2b_512, Digest, SHA1, SHA256, SHA384, SHA512, hashlazy)
import Data.Aeson (Key, Value (Object, String), eitherDecodeStrict')
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)
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
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    MirrorJob (..),
    MirrorQueue (receive),
    QueueMessage (msgReceipt),
    ReceiptHandle,
    enqueue,
    newInMemoryQueue,
 )
import Ecluse.Core.Registry (
    ParseError (ParseError),
    PublishError (PublishError),
    PublishFault (PublishRejected, PublishUrlUnformable),
    RegistryClient (..),
    UrlFormationError (EmptyBaseUrl),
 )
import Ecluse.Core.Registry.Metadata (
    MetadataClient (MetadataClient, fetchFullManifest, fetchVersionMetadata),
    MetadataError (MetadataUndecodable),
    VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent),
    fetchVersionDetails,
 )
import Ecluse.Core.Registry.Npm (npmPublishDocument)
import Ecluse.Core.Rules (PreparedRule (PreparedRule, prepEval, prepName, prepPrecedence, prepResilience))
import Ecluse.Core.Rules.Types (RuleResult (Allow, Deny))
import Ecluse.Core.Telemetry.Metrics (MirrorResult (Failed, Published))
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Core.Worker (
    IntegrityResult (IntegrityMismatch, IntegrityVerified),
    JobOutcome (Dropped, Retried, Succeeded),
    WorkerM,
    WorkerPolicies,
    WorkerPolicy (WorkerPolicy, wpNow, wpResolveVersion, wpRules),
    WorkerRuntime (WorkerRuntime, wrHeartbeat, wrInjectTraceContext, wrManager, wrMetrics, wrPolicies, wrQueue, wrRegistry, wrTracing),
    heartbeatHealthy,
    lastPoll,
    newWorkerHeartbeat,
    processBatch,
    processJob,
    runWorkerM,
    verifyIntegrity,
    workerHeartbeatStaleAfter,
    workerLoop,
 )
import Ecluse.Test.Package (unsafeHash)
import Ecluse.Test.Port (noopWorkerMetricsPort, passthroughWorkerTracingPort, recordingWorkerMetricsPort)
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

        it "acks a policy-denied job so it is retired, not redelivered forever" $
            -- Mirrors the integrity-mismatch ack test for the deny path: a current-policy deny
            -- is non-retryable, so the job is acked (retired) rather than left to redeliver.
            withRuntimePolicies (npmPolicies presentResolver [denyRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                enqueue queue (jobWith unreachableUrl (unsafeHash SHA1 trueSha1 :| []))
                messages <- receive queue
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

        it "classifies a transport throw as unavailable (total, never an escaping exception)" $
            fetchVersionDetails throwingVersionClient pkg ver
                `shouldReturn` VersionMetadataUnavailable
    describe "processBatch -- ack semantics over the in-memory queue" $ do
        it "acks a successfully-mirrored job so it is not redelivered" $
            withUpstream $ \url ->
                withRuntime (Right ()) $ \runtime queue _logRef -> do
                    enqueue queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    messages <- receive queue
                    runWM runtime (processBatch messages)
                    -- A redelivery pass past the (immediate) visibility window yields
                    -- nothing: the job was acked.
                    redelivered <- receive queue
                    redelivered `shouldBe` []

        it "does not ack a transiently-failed job, so it redelivers" $
            withUpstream $ \url ->
                withRuntime (Left (PublishRejected (PublishError "503"))) $ \runtime queue _logRef -> do
                    enqueue queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    messages <- receive queue
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
                    enqueue queue (jobWith url (unsafeHash SHA1 wrongSha1 :| []))
                    messages <- receive queue
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
                    enqueue queue (jobWith url (unsafeHash SHA1 trueSha1 :| []))
                    messages <- receive queue
                    runWM runtime (processBatch messages)
                    readResults >>= (`shouldBe` [Published])

        it "records a Failed result for a tampered job, through the port" $
            withUpstream $ \url -> do
                (metricsPort, readResults) <- recordingWorkerMetricsPort
                withRuntimeWith metricsPort (Right ()) $ \runtime queue _logRef -> do
                    enqueue queue (jobWith url (unsafeHash SHA1 wrongSha1 :| []))
                    messages <- receive queue
                    runWM runtime (processBatch messages)
                    readResults >>= (`shouldBe` [Failed])
