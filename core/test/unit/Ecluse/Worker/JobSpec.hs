-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Worker.JobSpec (spec) where

import Data.Aeson (Value, eitherDecodeStrict')
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec
import UnliftIO.Exception (try)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Package (
    Artifact (artFilename, artHashes),
    HashAlg (Blake2b, SHA1, SHA256, SRI),
 )
import Ecluse.Core.Package.Admission (ArtifactAdmission (AdmissionUndecidable))
import Ecluse.Core.Queue (DeliveryBudget (DeliveryBudget), MirrorQueue (deliveryBudget), QueueMessage (msgReceipt, msgReceiveCount))
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable),
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    PublishError (PublishError),
    PublishFault (PublishFetch, PublishRejected),
    UrlFormationError (EmptyBaseUrl),
 )
import Ecluse.Core.Registry.Metadata (
    MetadataError (MetadataFetch, MetadataUndecodable),
    VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent),
    fetchVersionDetails,
 )
import Ecluse.Core.Registry.Npm.Publish (npmPublishDocument)
import Ecluse.Core.Rules.Types (Decision (Undecidable), Transience (WillResolve, WontResolve))
import Ecluse.Core.Security (LimitError (BodyTooLarge))
import Ecluse.Core.Telemetry.Metrics (MirrorResult (Discarded, Failed, Published))
import Ecluse.Core.Worker (
    JobOutcome (DeadLettered, Dropped, Retried, Succeeded),
    WorkerPolicy (wpBuildArtifactRequest, wpPublish),
    processBatch,
    processJob,
 )
import Ecluse.Core.Worker.Job (ReevalOutcome (ReevalDrop, ReevalRetry), outcomeOfAdmission, outcomeOfFetchFault)
import Ecluse.Test.Package (unsafeHash)
import Ecluse.Test.Port (noopWorkerMetricsPort, recordingWorkerMetricsPort)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Worker.Support

spec :: Spec
spec = do
    describe "outcomeOfFetchFault (the worker's one retry-versus-drop table)" $ do
        -- One table serves the artifact fetch and the mirror write, so no exchange fault can
        -- drop on one leg and retry on the other.
        it "drops an unformable URL (a redelivery re-forms the same URL from the same inputs)" $
            outcomeOfFetchFault renderFault (FetchUrlUnformable EmptyBaseUrl)
                `shouldBe` Dropped "unformable"

        -- An over-bound response is terminal, so the backend dead-letters it. Treating every
        -- fetch Left as a retry would redeliver a deterministically over-cap tarball forever.
        it "dead-letters an over-bound response (it can never succeed, so it rides the terminus)" $
            outcomeOfFetchFault renderFault (FetchBoundExceeded (BodyTooLarge 1024))
                `shouldBe` DeadLettered "over the bound"

        it "retries a transport fault (a redelivery may succeed)" $
            outcomeOfFetchFault renderFault (FetchTransport (transportFault TransportUnreachable "connection reset"))
                `shouldBe` Retried "transport failed"

    describe "outcomeOfAdmission (the shared admission verdict, split on the shared transience)" $ do
        -- 'Ecluse.Core.Package.Admission.admissionTransience' is the one input to the split, and
        -- the serve gate reads it to choose a 503 over a 500. The two cannot disagree.
        it "retries an inability the evaluator expects to clear (an advisory source briefly down)" $
            case outcomeOfAdmission (jobWith unreachableUrl) (undecided (WillResolve Nothing) "no advisory database is loaded") of
                ReevalRetry reason -> reason `shouldSatisfy` T.isInfixOf "no advisory database is loaded"
                other -> expectationFailure ("expected a retry for a clearing inability, got " <> show other)

        it "drops an inability no retry can clear, rather than redelivering until the budget retires it" $
            -- WontResolve is the rule engine's own statement that no redelivery changes the
            -- verdict. A repaired advisory source rides the next request's enqueue instead.
            case outcomeOfAdmission (jobWith unreachableUrl) (undecided WontResolve "the advisory index is corrupt") of
                ReevalDrop reason -> reason `shouldSatisfy` T.isInfixOf "the advisory index is corrupt"
                other -> expectationFailure ("expected a drop for an unclearable inability, got " <> show other)

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
        it "publishes and reports success when the bytes match the re-admitted digest" $
            withUpstream $ \url ->
                withRuntime (Right ()) $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url)
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "publishes a sha384-only version end to end (fetch, compute sha384, verify, publish)" $
            -- Current metadata carries only the sha384, so this proves a sha384-admitted artifact
            -- is not admit-but-uncomputable.
            withUpstream $ \url ->
                withRuntimePolicies (admitPoliciesWithDigests [unsafeHash SRI trueSha384Sri]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url)
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "publishes a sha256-only version end to end (the #409 fix on the default floor)" $
            -- A worker that could not compute sha256 would drop an artifact the default public
            -- floor had already admitted.
            withUpstream $ \url ->
                withRuntimePolicies (admitPoliciesWithDigests [unsafeHash SHA256 trueSha256]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url)
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "publishes a blake2b-only version end to end (the #409 fix, the top tier)" $
            -- The floor admits a blake2b-only artifact, and the worker recomputes
            -- blake2b-512, matches, and publishes it.
            withUpstream $ \url ->
                withRuntimePolicies (admitPoliciesWithDigests [unsafeHash Blake2b trueBlake2b]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url)
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "refuses to publish (no publish) when the bytes do not match the re-admitted digest" $
            withUpstream $ \url ->
                -- A tampered/substituted artifact: current metadata's digest names
                -- other bytes than the upstream served, so the worker must NOT
                -- publish. The payload carries no digest that could weaken this gate.
                withRuntimePolicies (admitPoliciesWithDigests [unsafeHash SRI falseSri]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url)
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldSatisfy` isDropped
                    published <- plDocuments <$> readIORef logRef
                    published `shouldBe` []

        it "hands the publish step the re-admitted descriptor exactly" $
            -- The queue payload names the artifact by filename only. Every field of the trusted-
            -- tier publish document must come from current metadata.
            withUpstream $ \url ->
                withRuntime (Right ()) $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url)
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    descriptors <- plArtifacts <$> readIORef logRef
                    descriptors
                        `shouldBe` [ MirrorArtifact
                                        { maFilename = "thing-1.0.0.tgz"
                                        , maHashes = unsafeHash SRI trueSri :| []
                                        , maSize = Nothing
                                        }
                                   ]

        it "leaves the job for redelivery on a transient fetch failure (no publish)" $
            -- An unreachable upstream (connection refused) is a transient fault, not a terminal
            -- one.
            withRuntime (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl)
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isRetried
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "renders a failed artifact fetch without the URL's userinfo, path, or query" $
            -- The fault text becomes the Retried reason, which the queue-realisation site logs and
            -- the mirror-job span carries as an error status. It must name the authority and the
            -- bounded transport cause, never the location.
            withRuntime (Right ()) $ \runtime queue _logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith credentialBearingUnreachableUrl)
                outcome <- runWM runtime (processJob receipt job)
                case outcome of
                    Retried reason -> do
                        reason `shouldSatisfy` T.isInfixOf "127.0.0.1:1"
                        reason `shouldSatisfy` (not . T.isInfixOf "hunter2")
                        reason `shouldSatisfy` (not . T.isInfixOf "sig=abc")
                        reason `shouldSatisfy` (not . T.isInfixOf "/x")
                    other -> expectationFailure ("expected a Retried outcome from the refused connect, got " <> show other)

        it "treats a registry rejection as retryable (job left for redelivery)" $
            withUpstream $ \url ->
                withRuntime (Left (PublishRejected (PublishError "503"))) $ \runtime queue _logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url)
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldSatisfy` isRetried

        it "renders a transport fault's retry reason with the prefix exactly once" $
            -- The consumer renders the reason prefix, once. A fault carrying the prefix as raw text
            -- would make the consumer add it again, doubling it in the log line.
            withUpstream $ \url ->
                withRuntime (Left (PublishFetch (FetchTransport (transportFault TransportUnreachable "connection refused")))) $ \runtime queue _logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url)
                    outcome <- runWM runtime (processJob receipt job)
                    case outcome of
                        Retried reason -> do
                            reason `shouldSatisfy` T.isInfixOf "connection refused"
                            T.count "publish transport failure:" reason `shouldBe` 1
                        other -> expectationFailure ("expected a Retried transport outcome, got " <> show other)

        it "drops a job (non-retryable) when the artifact URL is unformable (no publish)" $
            -- A redelivery re-forms the same unformable URL from the same job payload, so the
            -- worker retires the job, the same verdict the publish leg reaches below.
            withRuntime (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unformableUrl)
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isDropped
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "drops a job (non-retryable) when the publish URL is unformable (a config fault)" $
            -- An unformable publish URL is a misconfiguration redelivery cannot fix, so the worker
            -- drops the job rather than re-enqueueing it forever. That is distinct from a retryable
            -- rejection.
            withUpstream $ \url ->
                withRuntime (Left (PublishFetch (FetchUrlUnformable EmptyBaseUrl))) $ \runtime queue _logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url)
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldSatisfy` isDropped
    describe "processJob: ingest-time policy re-evaluation" $ do
        it "drops a job whose version current policy denies, without publishing" $
            -- Current policy denies a version admitted at serve time, so the worker retires it
            -- unmirrored. 'unreachableUrl' guards the re-evaluation: skipping it would surface a
            -- Retried, not this Dropped.
            withRuntimePolicies (npmPolicies presentResolver [denyRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl)
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isDropped
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "publishes a job whose version current policy admits (happy path unregressed)" $
            withUpstream $ \url ->
                withRuntimePolicies (npmPolicies presentResolver [admitRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url)
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "drops a job whose version the upstream no longer offers (withdrawn), without publishing" $
            -- A version the upstream withdrew must not be mirrored, so the drop is non-retryable.
            withRuntimePolicies (npmPolicies (\_ _ -> pure VersionMissing) [admitRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl)
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isDropped
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "retries a job when the re-evaluation metadata cannot be re-fetched, without publishing" $
            -- A transient metadata outage maps to the serve path's transient degrade: leave the
            -- job for redelivery rather than dropping it or publishing it unvetted.
            withRuntimePolicies (npmPolicies (\_ _ -> pure VersionMetadataUnavailable) [admitRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl)
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isRetried
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "drops a job whose ecosystem has no configured policy (fail-closed), without publishing" $
            -- A job for an ecosystem with no bundle is fail-closed: never mirrored unvetted.
            withRuntimePolicies mempty noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl)
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isDropped
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "fetches through the request formation keyed by the job's own ecosystem" $
            -- The policies map also carries a PyPI bundle whose request formation
            -- refuses outright. The npm job must ride its own ecosystem's builder, so
            -- nothing consults the decoy entry and the publish succeeds.
            withUpstream $ \url -> do
                let refusing = (npmPolicy presentResolver [admitRule]){wpBuildArtifactRequest = \_ _ _ _ _ -> Left EmptyBaseUrl}
                    policies = Map.insert PyPI refusing (npmPolicies presentResolver [admitRule])
                withRuntimePolicies policies noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url)
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "publishes through the publish capability keyed by the job's own ecosystem" $
            -- The decoy PyPI bundle carries its own recording publish capability. The npm job's
            -- probe and publish must both ride npm's, so the decoy records nothing.
            withUpstream $ \url -> do
                npmLog <- newIORef (PublishLog [] [])
                decoyLog <- newIORef (PublishLog [] [])
                let npmBundle = (npmPolicy presentResolver [admitRule]){wpPublish = recordingPublish npmLog (Right ())}
                    decoyBundle = (npmPolicy presentResolver [admitRule]){wpPublish = recordingPublish decoyLog (Right ())}
                    policies = Map.fromList [(Npm, npmBundle), (PyPI, decoyBundle)]
                queue <- newTestMemoryQueue
                withWiredRuntime queue policies noopWorkerMetricsPort $ \runtime -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url)
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef npmLog
                    length published `shouldBe` 1
                    decoyPublished <- plDocuments <$> readIORef decoyLog
                    decoyPublished `shouldBe` []

        it "drops the job when the job ecosystem's own request formation refuses the URL, without publishing" $
            -- The npm bundle's own builder refuses, with no other builder to fall back to. A
            -- redelivery would refuse identically, so the worker retires the job.
            withRuntimePolicies (withArtifactRequest (\_ _ _ _ _ -> Left EmptyBaseUrl) (npmPolicies presentResolver [admitRule])) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl)
                outcome <- runWM runtime (processJob receipt job)
                case outcome of
                    Dropped reason -> reason `shouldSatisfy` T.isInfixOf "unformable artifact URL"
                    other -> expectationFailure ("expected a Dropped outcome from the refused request formation, got " <> show other)
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "drops a job whose artifact host the current tarball-host policy refuses (payload re-gated)" $
            -- The queue payload is a trust boundary. Ingest re-establishes the host gate
            -- the serve path applied before its public fetch. It refuses a URL injected or
            -- no-longer-honoured since enqueue, before any fetch.
            withRuntimePolicies (withHostGate (const False) (npmPolicies presentResolver [admitRule])) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl)
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isDropped
                -- The reason names the authority the fetch would dial, never the URL:
                -- a queue payload's location can carry userinfo or a signed query.
                case outcome of
                    Dropped reason -> do
                        reason `shouldSatisfy` T.isInfixOf "127.0.0.1:1"
                        reason `shouldSatisfy` (not . T.isInfixOf "/thing/-/thing-1.0.0.tgz")
                    other -> expectationFailure ("expected a Dropped outcome, got " <> show other)
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "drops a job whose artifact's current digests fall below the integrity floor" $
            -- The upstream now serves only a legacy SHA-1. The shared oracle refuses it at ingest
            -- exactly as the serve gate would, so a no-longer-admissible artifact never enters the
            -- mirror.
            withRuntimePolicies (npmPolicies (resolverWithArtifact sampleArtifact{artHashes = [unsafeHash SHA1 trueSha1]}) [admitRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl)
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isDropped
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "drops a job whose version no longer carries any integrity digest" $
            -- The stripped-digest degrade: current metadata offers nothing to tie the
            -- bytes to. The serve gate 403s it as MissingIntegrity, and the worker drops it.
            withRuntimePolicies (npmPolicies (resolverWithArtifact sampleArtifact{artHashes = []}) [admitRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl)
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isDropped
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "drops a job whose admitted artifact file the current metadata no longer carries" $
            -- The version survives upstream but its file set no longer names the admitted artifact.
            -- Redelivery cannot restore the file, so the drop is non-retryable.
            withRuntimePolicies (npmPolicies (resolverWithArtifact sampleArtifact{artFilename = "renamed-9.9.9.tgz"}) [admitRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl)
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isDropped
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "retries a job when a fail-closed rule cannot be computed (undecidable), without publishing" $
            -- The advisory-outage degrade: the serve path renders the same cause as a
            -- transient 503 status. The worker leaves the job for redelivery rather than
            -- dropping a serviceable job or publishing it unvetted.
            withRuntimePolicies (npmPolicies presentResolver [cannotVetRule]) noopWorkerMetricsPort (Right ()) $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl)
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldSatisfy` isRetried
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "acks a policy-denied job, retiring it from the queue" $ do
            -- A current-policy deny is non-retryable, so the worker acks the job rather than
            -- leaving it for the backend to redeliver. The ack is the retire decision, observed at
            -- the handle.
            (queue, ackedReceipts) <- recordingAckQueue
            withRuntimeQueue queue (`recordingPublish` Right ()) (npmPolicies presentResolver [denyRule]) noopWorkerMetricsPort $ \runtime logRef -> do
                enqueue_ queue (jobWith unreachableUrl)
                messages <- receive_ queue
                runWM runtime (processBatch messages)
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []
                acked <- ackedReceipts
                acked `shouldBe` map msgReceipt messages

        it "dead-letters an over-cap artifact on the memory backend: metered, never published, routed to deadLetter not ack (issue #846)" $
            -- A fetch cap below the served bytes makes the over-cap fault terminal, so the worker
            -- routes it to the dead-letter terminus and meters it. The memory backend drops it, its
            -- only terminus.
            withUpstream $ \url -> do
                (queue, deadReceipts) <- recordingDeadLetterQueue
                (metricsPort, recordedMetrics) <- recordingWorkerMetricsPort
                withRuntimeQueue queue (`recordingPublish` Right ()) (withArtifactCap 8 admitPolicies) metricsPort $ \runtime logRef -> do
                    enqueue_ queue (jobWith url)
                    messages <- receive_ queue
                    runWM runtime (processBatch messages)
                    published <- plDocuments <$> readIORef logRef
                    published `shouldBe` []
                    dead <- deadReceipts
                    dead `shouldBe` map msgReceipt messages
                    metered <- recordedMetrics
                    metered `shouldBe` [Failed]

    describe "processJob: the mirror-presence dedup probe" $ do
        -- The default 'recordingPublish' answers the probe with an unparseable body (the
        -- absent posture), so every other test in this file already covers that
        -- fall-through. These cover the confirmed-present skip and the cannot-tell arms.
        it "acks an already-mirrored version without fetching or publishing" $
            -- 'unreachableUrl' doubles as the no-fetch guard: were the probe's skip not
            -- taken, the artifact fetch would surface a Retried, not this Succeeded.
            withRuntimeRegistry (\logRef -> mirrorListingPublish logRef (Right ()) [ver]) admitPolicies noopWorkerMetricsPort $ \runtime queue logRef -> do
                (receipt, job) <- enqueueAndReceive queue (jobWith unreachableUrl)
                outcome <- runWM runtime (processJob receipt job)
                outcome `shouldBe` Succeeded
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []

        it "falls through to the full pipeline when the probe cannot reach the mirror" $
            -- A mirror outage means the probe cannot tell, so the job must run the full gated
            -- pipeline. It is never skipped or failed on the probe alone.
            withUpstream $ \url ->
                withRuntimeRegistry (`probeUnreachablePublish` Right ()) admitPolicies noopWorkerMetricsPort $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url)
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "falls through when the mirror lists other versions but not this one" $
            -- The worker judges presence per version: a package already partially
            -- mirrored must still mirror its missing versions.
            withUpstream $ \url ->
                withRuntimeRegistry (\logRef -> mirrorListingPublish logRef (Right ()) [otherVer]) admitPolicies noopWorkerMetricsPort $ \runtime queue logRef -> do
                    (receipt, job) <- enqueueAndReceive queue (jobWith url)
                    outcome <- runWM runtime (processJob receipt job)
                    outcome `shouldBe` Succeeded
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1

        it "acks the skipped duplicate, retiring it from the queue" $ do
            (queue, ackedReceipts) <- recordingAckQueue
            withRuntimeQueue queue (\logRef -> mirrorListingPublish logRef (Right ()) [ver]) admitPolicies noopWorkerMetricsPort $ \runtime logRef -> do
                enqueue_ queue (jobWith unreachableUrl)
                messages <- receive_ queue
                runWM runtime (processBatch messages)
                published <- plDocuments <$> readIORef logRef
                published `shouldBe` []
                acked <- ackedReceipts
                acked `shouldBe` map msgReceipt messages
    describe "fetchVersionDetails: the shared single-version evaluation boundary" $ do
        -- The serve-time tarball gate and the worker both resolve a version through this one
        -- function, so these cases pin its classification directly.
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
            fetchVersionDetails (versionClient (Left (MetadataFetch (FetchTransport (transportFault TransportUnreachable "refused"))))) pkg ver
                `shouldReturn` VersionMetadataUnavailable

        it "propagates a client that escapes its total contract (the invariant channel)" $ do
            -- The typed channel reports every real failure, so a throw out of the fetch is an
            -- invariant break. It must reach the caller's supervision, never be laundered into the
            -- transient degrade.
            outcome <- try (fetchVersionDetails throwingVersionClient pkg ver) :: IO (Either SomeException VersionEvaluation)
            outcome `shouldSatisfy` isLeft
    describe "processBatch -- ack decisions at the queue handle" $ do
        -- The worker's retire-vs-retry decision is its ack call, recorded by 'recordingAckQueue'.
        -- The memory backend's ack is a no-op, so "Ecluse.WorkerSpec" pins real redelivery against
        -- SQS.
        it "acks a successfully-mirrored job, retiring it from the queue" $
            withUpstream $ \url -> do
                (queue, ackedReceipts) <- recordingAckQueue
                withRuntimeQueue queue (`recordingPublish` Right ()) admitPolicies noopWorkerMetricsPort $ \runtime _logRef -> do
                    enqueue_ queue (jobWith url)
                    messages <- receive_ queue
                    runWM runtime (processBatch messages)
                    acked <- ackedReceipts
                    acked `shouldBe` map msgReceipt messages

        it "does not ack a transiently-failed job (left for the backend to redeliver)" $
            withUpstream $ \url -> do
                (queue, ackedReceipts) <- recordingAckQueue
                withRuntimeQueue queue (`recordingPublish` Left (PublishRejected (PublishError "503"))) admitPolicies noopWorkerMetricsPort $ \runtime _logRef -> do
                    enqueue_ queue (jobWith url)
                    messages <- receive_ queue
                    runWM runtime (processBatch messages)
                    -- The registry rejection is retryable, so the worker must not ack: over a
                    -- redelivering backend the un-acked message comes back ("retry is don't ack").
                    acked <- ackedReceipts
                    acked `shouldBe` []

        it "acks a DROPPED job, retiring a tampered artifact rather than retrying it" $
            -- An integrity mismatch is non-retryable, because redelivery could never make the bytes
            -- match, so the worker acks rather than leaving the job to redeliver indefinitely.
            withUpstream $ \url -> do
                (queue, ackedReceipts) <- recordingAckQueue
                withRuntimeQueue queue (`recordingPublish` Right ()) (admitPoliciesWithDigests [unsafeHash SRI falseSri]) noopWorkerMetricsPort $ \runtime logRef -> do
                    enqueue_ queue (jobWith url)
                    messages <- receive_ queue
                    runWM runtime (processBatch messages)
                    -- The worker published nothing (the mismatch refused the publish)...
                    published <- plDocuments <$> readIORef logRef
                    published `shouldBe` []
                    -- ...and it acked the job: retired at the handle.
                    acked <- ackedReceipts
                    acked `shouldBe` map msgReceipt messages
    describe "processBatch -- the redelivery budget, the terminus a queue without a DLQ has (issue #935)" $ do
        it "retires a delivery that has spent the budget, without ever running the job" $
            withUpstream $ \url -> do
                (metricsPort, readResults) <- recordingWorkerMetricsPort
                (base, ackedReceipts) <- recordingAckQueue
                let queue = base{deliveryBudget = DeliveryBudget 3}
                withRuntimeQueue queue (`recordingPublish` Right ()) admitPolicies metricsPort $ \runtime logRef -> do
                    enqueue_ queue (jobWith url)
                    [message] <- receive_ queue
                    runWM runtime (processBatch [message{msgReceiveCount = 3}])
                    -- The worker checks the budget before it runs the job, so it never
                    -- re-fetches the artifact and spares that repeated cost.
                    published <- plDocuments <$> readIORef logRef
                    published `shouldBe` []
                    -- The ack retires the delivery rather than let it cycle until the queue's
                    -- retention window drops it unseen...
                    acked <- ackedReceipts
                    acked `shouldBe` [msgReceipt message]
                    -- ...and it counts the delivery as a discard: the signal an operator
                    -- alerts on, distinct from an ordinary failure.
                    readResults >>= (`shouldBe` [Discarded])

        it "still runs the job on the delivery one below the budget" $
            withUpstream $ \url -> do
                (metricsPort, readResults) <- recordingWorkerMetricsPort
                (base, ackedReceipts) <- recordingAckQueue
                let queue = base{deliveryBudget = DeliveryBudget 3}
                withRuntimeQueue queue (`recordingPublish` Right ()) admitPolicies metricsPort $ \runtime logRef -> do
                    enqueue_ queue (jobWith url)
                    [message] <- receive_ queue
                    runWM runtime (processBatch [message{msgReceiveCount = 2}])
                    -- The worker mirrors the job and acks it on success.
                    published <- plDocuments <$> readIORef logRef
                    length published `shouldBe` 1
                    acked <- ackedReceipts
                    acked `shouldBe` [msgReceipt message]
                    readResults >>= (`shouldBe` [Published])

    describe "the worker metrics port" $ do
        it "records a Published result for a successfully-mirrored job, through the port" $
            -- Asserts the worker classified the terminal outcome and recorded it through the
            -- recording 'WorkerMetricsPort', which proves the port is wired.
            withUpstream $ \url -> do
                (metricsPort, readResults) <- recordingWorkerMetricsPort
                withRuntimeWith metricsPort (Right ()) $ \runtime queue _logRef -> do
                    enqueue_ queue (jobWith url)
                    messages <- receive_ queue
                    runWM runtime (processBatch messages)
                    readResults >>= (`shouldBe` [Published])

        it "records a Failed result for a tampered job, through the port" $
            withUpstream $ \url -> do
                (metricsPort, readResults) <- recordingWorkerMetricsPort
                withRuntimePolicies (admitPoliciesWithDigests [unsafeHash SRI falseSri]) metricsPort (Right ()) $ \runtime queue _logRef -> do
                    enqueue_ queue (jobWith url)
                    messages <- receive_ queue
                    runWM runtime (processBatch messages)
                    readResults >>= (`shouldBe` [Failed])

-- An admission verdict no rule could decide, with the given transience.
undecided :: Transience -> Text -> ArtifactAdmission
undecided transience reason = AdmissionUndecidable (Undecidable transience reason)

-- A stand-in reason renderer. The unit pins the verdict and that the reason is rendered from
-- the very fault being judged, never the wording each worker leg chooses.
renderFault :: FetchFault -> Text
renderFault = \case
    FetchUrlUnformable _ -> "unformable"
    FetchBoundExceeded _ -> "over the bound"
    FetchTransport _ -> "transport failed"
