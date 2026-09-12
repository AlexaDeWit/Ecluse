-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Deciding one mirror job: probe the mirror target, re-run current policy, fetch, verify, and
publish. Every step reports its verdict as a 'JobOutcome' value, which
"Ecluse.Core.Worker.Realise" realises at the queue handle.

The receipt is held for the whole job by the lease controller ("Ecluse.Core.Worker.Lease"), so
nothing here touches the queue. Nothing here acks either: a transient failure simply reports
'Retried', and the un-acked message redelivers.
-}
module Ecluse.Core.Worker.Job (
    JobOutcome (..),
    RetryLeg (..),
    mirrorLatest,
    outcomeOfAdmission,
    outcomeOfFetchFault,
    processJob,
) where

import Data.Map.Strict qualified as Map
import Katip (Severity (DebugS, ErrorS, InfoS), katipAddNamespace, logFM, ls)
import UnliftIO (withRunInIO)

import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Fault (tfCause)
import Ecluse.Core.Package (Artifact (artSize), Hash, pkgEcosystem)
import Ecluse.Core.Package.Admission (
    ArtifactAdmission (
        AdmissionAdmit,
        AdmissionBelowFloor,
        AdmissionDenied,
        AdmissionFileAbsent,
        AdmissionIntegrityMissing,
        AdmissionUndecidable
    ),
    admissionTransience,
    admitArtifact,
 )
import Ecluse.Core.Queue (MirrorJob (jobArtifactFilename, jobArtifactUrl, jobPackage, jobTraceContext, jobVersion))
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable),
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    ParseError (ParseError),
    PublishFault (PublishFetch, PublishRejected),
    RegistryResponse (responseStatusCode),
    isSuccessStatus,
    renderUrlFormationError,
 )
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByUrl))
import Ecluse.Core.Registry.Metadata (VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent), versionTransience)
import Ecluse.Core.Registry.Publish (
    MirrorPublish (mpParseVersionList, mpProbeMetadata, mpPublishArtifact),
    PublishPlan (PublishPlan, ppLatest, ppVersion),
 )
import Ecluse.Core.Rules.Types (Decision (Blocked, Undecidable), Transience (WillResolve, WontResolve), mkEvalContext)
import Ecluse.Core.Security (authorityLabel, hostPortAddress)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Path (Filename)
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (JobSpanOutcome (JobSpanOutcome), WorkerTracingPort (..))
import Ecluse.Core.Version (Version, selectLatest)
import Ecluse.Core.Worker.Fetch (fetchArtifactBytes)
import Ecluse.Core.Worker.Integrity (IntegrityResult (..), verifyIntegrity)
import Ecluse.Core.Worker.Types

{- | The terminal outcome of processing one mirror job. It decides whether the worker
acks the message or leaves it to redeliver.
-}
data JobOutcome
    = {- | The publish succeeded or the mirror already held the version.
      The worker acknowledges either result, including idempotent redelivery.
      -}
      Succeeded
    | {- | A __non-retryable__ rejection (a tampered artifact, an unformable request URL).
      Redelivery cannot help, so the job is acked to retire it after alarming.
      -}
      Dropped Text
    | {- | A __terminal__ fault handed to 'Ecluse.Core.Queue.deadLetter' rather than acked,
      because a plain delete would silently discard it on a durable queue.
      -}
      DeadLettered Text
    | {- | A __transient__ fault: a fetch failure, or a registry rejection worth retrying. The
      message is left un-acked so it redelivers, carrying the leg it gave up on.
      -}
      Retried RetryLeg Text
    deriving stock (Eq, Show)

{- | Which leg a transient failure gave up on. The realisation half reads it to decide whether
to reset the message's visibility, so the two legs cannot be conflated at the queue handle.
-}
data RetryLeg
    = {- | The job gave up before it published: the inventory probe, the re-evaluation, or the
      artifact fetch. The message keeps its lease and redelivers when that window lapses.
      -}
      BeforePublish
    | {- | The publish itself failed transiently, after the bytes were fetched and verified.
      The message is released so its redelivery does not wait out the lease.
      -}
      AfterPublish
    deriving stock (Eq, Show)

{- | Re-check policy before publishing because the queue wait is unbounded and mirrored bytes bypass later rules.
The outcome determines whether the worker acknowledges the message or permits redelivery.
-}
processJob :: MirrorJob -> WorkerM JobOutcome
processJob job = katipAddNamespace "job" $ do
    logFM DebugS (ls ("starting mirror job for " <> renderJob job))
    tracing <- asks wrTracing
    runtime <- ask
    withRunInIO $ \runInIO ->
        wtpMirrorJobSpan tracing (jobPackage job) (jobVersion job) (jobTraceContext job) jobSpanOutcome $
            runInIO $
                wrInjectTraceContext runtime (reevaluateThenMirror job)
  where
    -- The failure detail marks the span errored, so only a job that did not publish carries one.
    jobSpanOutcome :: JobOutcome -> JobSpanOutcome
    jobSpanOutcome = \case
        Succeeded -> JobSpanOutcome "succeeded" Nothing
        Dropped reason -> JobSpanOutcome "dropped" (Just reason)
        DeadLettered reason -> JobSpanOutcome "dead-lettered" (Just reason)
        Retried _ reason -> JobSpanOutcome "retried" (Just reason)

-- Use one ecosystem bundle throughout so a job cannot consult another ecosystem's policy or registry.
reevaluateThenMirror :: MirrorJob -> WorkerM JobOutcome
reevaluateThenMirror job = do
    policies <- asks wrPolicies
    case Map.lookup (pkgEcosystem (jobPackage job)) policies of
        -- Structurally unreachable: only an activated ecosystem enqueues jobs, and activation
        -- implies a bundle. Kept as the fail-closed drop.
        Nothing -> pure (Dropped (noPolicyReason job))
        Just policy
            -- An operator can declare the namespace after the enqueue, so the privilege is read
            -- ahead of the mirror probe: the public leg is never entered for a name it owns.
            | wpFirstParty policy (jobPackage job) -> pure (Dropped (firstPartyReason job))
            | otherwise -> mirrorUnlessPresent policy job

noPolicyReason :: MirrorJob -> Text
noPolicyReason job =
    "no rule policy is configured for the "
        <> ecosystemName (pkgEcosystem (jobPackage job))
        <> " ecosystem; refusing to mirror "
        <> renderJob job

firstPartyReason :: MirrorJob -> Text
firstPartyReason job =
    "this deployment owns the namespace of "
        <> renderJob job
        <> "; refusing to mirror public content under a first-party name"

mirrorUnlessPresent :: WorkerPolicy -> MirrorJob -> WorkerM JobOutcome
mirrorUnlessPresent policy job =
    probeInventory policy job >>= \case
        Left outcome -> pure outcome
        Right inventory
            | jobVersion job `elem` inventory -> do
                logFM InfoS (ls ("already present at the mirror target, acking without re-publish: " <> renderJob job))
                pure Succeeded
            | otherwise -> reevaluatePolicy policy job >>= either pure (publishAdmitted policy job inventory)

{- An unreadable answer is not an empty store, so it reports a fault rather than let the write
declare a tag chosen without the inventory. A 404 is a store holding this package not at all. -}
probeInventory :: WorkerPolicy -> MirrorJob -> WorkerM (Either JobOutcome [Version])
probeInventory policy job = do
    probed <- liftIO (mpProbeMetadata (wpPublish policy) (jobPackage job))
    pure $ case probed of
        Left fault -> Left (outcomeOfFetchFault BeforePublish (probeFaultReason job) fault)
        Right response
            | responseStatusCode response == 404 -> Right []
            | not (isSuccessStatus (responseStatusCode response)) ->
                Left (Retried BeforePublish (probeStatusReason job (responseStatusCode response)))
            | otherwise -> case mpParseVersionList (wpPublish policy) response of
                Left (ParseError detail) -> Left (Retried BeforePublish (probeParseReason job detail))
                Right versions -> Right versions

probeFaultReason :: MirrorJob -> FetchFault -> Text
probeFaultReason job = \case
    FetchUrlUnformable urlErr -> "unformable mirror probe URL: " <> renderUrlFormationError urlErr
    FetchBoundExceeded limitErr -> "the mirror target's metadata exceeded the response bound: " <> show limitErr
    FetchTransport fault -> "mirror inventory probe for " <> renderJob job <> " failed: " <> show (tfCause fault)

probeStatusReason :: MirrorJob -> Int -> Text
probeStatusReason job code =
    "the mirror target answered HTTP "
        <> show code
        <> " for the inventory probe of "
        <> renderJob job
        <> "; refusing to publish a release tag chosen without it"

probeParseReason :: MirrorJob -> Text -> Text
probeParseReason job detail =
    "could not read the mirror target's inventory for "
        <> renderJob job
        <> " ("
        <> detail
        <> "); refusing to publish a release tag chosen without it"

{- Re-check the fetch URL against the mount's tarball-host gate, because the queue payload is a
trust boundary. Then re-run current policy through 'Ecluse.Core.Package.Admission.admitArtifact'. -}
reevaluatePolicy :: WorkerPolicy -> MirrorJob -> WorkerM (Either JobOutcome (MirrorArtifact, Maybe Version))
reevaluatePolicy policy job
    | not (wpArtifactHostHonoured policy (hostPortAddress (registryUrlText (jobArtifactUrl job)))) =
        pure (Left (Dropped (artifactHostReason job)))
    | otherwise =
        liftIO (wpResolveVersion policy (jobPackage job) (jobVersion job)) >>= admitEvaluation policy job

artifactHostReason :: MirrorJob -> Text
artifactHostReason job =
    "the tarball-host policy refuses the artifact host of "
        <> renderJob job
        <> " ("
        <> jobArtifactAuthority job
        <> "); refusing to fetch or mirror it"

{- A version the upstream no longer offers, or cannot describe, never reaches the rules. A present
version also carries the upstream's own @latest@, read from the same metadata. -}
admitEvaluation :: WorkerPolicy -> MirrorJob -> VersionEvaluation -> WorkerM (Either JobOutcome (MirrorArtifact, Maybe Version))
admitEvaluation policy job evaluation = case evaluation of
    VersionMetadataUnavailable ->
        pure (Left (unresolved ("could not re-fetch metadata to re-evaluate current policy for " <> renderJob job)))
    VersionMissing ->
        pure (Left (unresolved ("the public upstream no longer offers " <> renderJob job <> "; refusing to mirror a withdrawn version")))
    VersionPresent details upstreamLatest -> do
        -- The back-fill path emits no per-decision audit line, so the audit-only advisory ETag
        -- is not resolved for its context.
        ctx <- liftIO (mkEvalContext (wpNow policy) (pure Nothing))
        admission <- liftIO (admitArtifact ctx (wpRules policy) (wpMinIntegrity policy) (jobArtifactFilename job) details)
        pure ((,upstreamLatest) <$> outcomeOfAdmission job admission)
  where
    unresolved = retryOrDrop (versionTransience evaluation)

{- | Render the shared 'ArtifactAdmission' as the descriptor to publish, or the outcome the queue
realises. 'admissionTransience' alone splits retry from drop, so no path can diverge from the gate.
-}
outcomeOfAdmission :: MirrorJob -> ArtifactAdmission -> Either JobOutcome MirrorArtifact
outcomeOfAdmission job admission = case admission of
    AdmissionAdmit filename artifact digests -> Right (readmittedDescriptor filename artifact digests)
    AdmissionDenied (Blocked ruleName _ reason) ->
        refused ("current policy denies " <> renderJob job <> ": blocked by " <> ruleName <> " (" <> reason <> ")")
    AdmissionDenied _ ->
        refused ("current policy denies " <> renderJob job <> ": no rule admits it")
    AdmissionUndecidable (Undecidable _ reason) ->
        refused ("current policy could not be evaluated for " <> renderJob job <> ": " <> reason)
    AdmissionUndecidable _ ->
        refused ("current policy could not be evaluated for " <> renderJob job)
    AdmissionFileAbsent ->
        refused ("the public upstream no longer offers the admitted artifact file of " <> renderJob job <> "; refusing to mirror a withdrawn artifact")
    AdmissionBelowFloor ->
        refused ("current admission policy refuses " <> renderJob job <> ": its strongest integrity digest is below the configured public floor")
    AdmissionIntegrityMissing ->
        refused ("current admission policy refuses " <> renderJob job <> ": it no longer carries any integrity digest")
  where
    refused :: Text -> Either JobOutcome MirrorArtifact
    refused = Left . retryOrDrop (admissionTransience admission)

{- The worker's one retry-versus-drop rule, over the shared transience. Only an inability the
evaluator expects to clear redelivers: the rest drop through the terminal path. -}
retryOrDrop :: Maybe Transience -> Text -> JobOutcome
retryOrDrop transience reason = case transience of
    Just (WillResolve _) -> Retried BeforePublish reason
    Just WontResolve -> Dropped reason
    Nothing -> Dropped reason

{- Derive the publish descriptor from what the gate settled, so nothing the queue payload asserted
reaches the trusted-tier publish document unchecked. The size is current metadata's. -}
readmittedDescriptor :: Filename -> Artifact -> NonEmpty Hash -> MirrorArtifact
readmittedDescriptor filename artifact digests =
    MirrorArtifact
        { maFilename = filename
        , maHashes = digests
        , maSize = artSize artifact
        }

{- | The worker's terminal-versus-transient split over the shared exchange-fault channel. The
artifact fetch and the mirror write read this one table, and each names its own retry leg.
-}
outcomeOfFetchFault :: RetryLeg -> (FetchFault -> Text) -> FetchFault -> JobOutcome
outcomeOfFetchFault leg render fault = verdict (render fault)
  where
    verdict = case fault of
        FetchUrlUnformable _ -> Dropped
        FetchBoundExceeded _ -> DeadLettered
        FetchTransport _ -> Retried leg

{- | The @latest@ one mirror write declares, over the upstream tag and the post-write inventory.
The published version always survives, so the chosen target is always present at the store.
-}
mirrorLatest :: Maybe Version -> [Version] -> Version -> Version
mirrorLatest upstreamLatest inventory published =
    fromMaybe published (selectLatest upstreamLatest (published : inventory))

{- Fix the release tag before the write, over the post-write inventory, so no job makes its own
version latest merely by finishing last. -}
publishAdmitted :: WorkerPolicy -> MirrorJob -> [Version] -> (MirrorArtifact, Maybe Version) -> WorkerM JobOutcome
publishAdmitted policy job inventory (admitted, upstreamLatest) =
    mirrorArtifact policy job plan admitted
  where
    plan =
        PublishPlan
            { ppVersion = jobVersion job
            , ppLatest = mirrorLatest upstreamLatest inventory (jobVersion job)
            }

mirrorArtifact :: WorkerPolicy -> MirrorJob -> PublishPlan -> MirrorArtifact -> WorkerM JobOutcome
mirrorArtifact policy job plan admitted = do
    logFM DebugS (ls ("fetching artifact bytes from " <> jobArtifactAuthority job))
    fetched <- fetchArtifactBytes (wpArtifactLimits policy) (artifactByUrl (wpArtifact policy)) (jobArtifactUrl job)
    case fetched of
        -- 'outcomeOfFetchFault' makes the terminal-versus-transient split, and the realisation
        -- half logs the reason at the queue handle.
        Left fault -> pure (outcomeOfFetchFault BeforePublish (artifactFetchReason job) fault)
        Right bytes -> publishIfIntact policy job plan admitted bytes

-- The client's rendered exception would print the request path, query, and headers, so a
-- transport reason names only the authority and the cause.
artifactFetchReason :: MirrorJob -> FetchFault -> Text
artifactFetchReason job = \case
    FetchUrlUnformable urlErr -> "unformable artifact URL: " <> renderUrlFormationError urlErr
    FetchBoundExceeded limitErr -> "artifact exceeded the response bound: " <> show limitErr
    FetchTransport fault -> "artifact fetch from " <> jobArtifactAuthority job <> " failed: " <> show (tfCause fault)

-- A tampered artifact must never reach the private upstream, which later serves it without the
-- rules, so the bytes are verified against the re-admitted digests before any publish.
publishIfIntact :: WorkerPolicy -> MirrorJob -> PublishPlan -> MirrorArtifact -> ByteString -> WorkerM JobOutcome
publishIfIntact policy job plan admitted bytes = case verifyIntegrity (maHashes admitted) bytes of
    IntegrityMismatch detail -> do
        logFM ErrorS (ls ("artifact integrity mismatch, refusing to publish: " <> detail))
        pure (Dropped ("integrity mismatch: " <> detail))
    IntegrityVerified -> publishVerified policy job plan admitted bytes

-- Publish already-verified bytes to the mirror target. The publish document is assembled from the
-- re-admitted descriptor, so no queue-payload text reaches the trusted-tier packument.
publishVerified :: WorkerPolicy -> MirrorJob -> PublishPlan -> MirrorArtifact -> ByteString -> WorkerM JobOutcome
publishVerified policy job plan admitted bytes = do
    metrics <- asks wrMetrics
    -- The publish is the long, network-bound step. Time it for the publish-latency
    -- histogram whichever way the registry responds.
    (result, seconds) <- timedSeconds (liftIO (mpPublishArtifact (wpPublish policy) (jobPackage job) plan admitted bytes))
    liftIO (wmpMirrorPublishDuration metrics seconds)
    outcomeOfPublish job result

outcomeOfPublish :: MirrorJob -> Either PublishFault () -> WorkerM JobOutcome
outcomeOfPublish job = \case
    Right () -> Succeeded <$ logFM InfoS (ls ("mirrored artifact published: " <> renderJob job))
    Left (PublishRejected err) -> pure (Retried AfterPublish ("registry rejected publish: " <> show err))
    Left (PublishFetch fault) -> pure (outcomeOfFetchFault AfterPublish publishFaultReason fault)

-- The mirror target is operator-configured, so its rendered transport detail is diagnosable
-- rather than attacker-supplied.
publishFaultReason :: FetchFault -> Text
publishFaultReason = \case
    FetchUrlUnformable urlErr -> "unformable publish URL: " <> renderUrlFormationError urlErr
    FetchBoundExceeded limitErr -> "the publication target's response exceeded the response bound: " <> show limitErr
    FetchTransport fault -> "publish transport failure: " <> show fault

{- The job's artifact location as a log-safe authority. The queue payload's URL can carry userinfo
or a pre-signed query, so a log line names only the host and port the worker dials. -}
jobArtifactAuthority :: MirrorJob -> Text
jobArtifactAuthority = authorityLabel . registryUrlText . jobArtifactUrl
