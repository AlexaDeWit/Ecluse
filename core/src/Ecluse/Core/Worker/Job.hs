-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Decide mirror admission, preparation, integrity, and publication outcomes.
The lease controller owns the receipt, and "Ecluse.Core.Worker.Realise" applies queue outcomes.
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
import Ecluse.Core.Package (Artifact (artFilename, artHashes, artSize, artUrl), Hash, PackageDetails (pkgArtifacts, pkgVersion), PackageInfo (..), pkgEcosystem)
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
import Ecluse.Core.Package.Integrity (VersionIntegrity (..), classifyDigests, partitionByFloor)
import Ecluse.Core.Queue (MirrorJob (jobPackage, jobTraceContext, jobTriggerFilename, jobVersion))
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable),
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    PublishFault (PublishFetch, PublishIntegrityMismatch, PublishRejected, PublishSourceUnavailable),
    renderUrlFormationError,
 )
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByUrl))
import Ecluse.Core.Registry.Metadata (
    MetadataError (..),
    VersionDoc (vdDetails),
    VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent),
    versionTransience,
 )
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.Publish (
    MirrorPublish (mpPrepareVersion, mpProbeInventory),
    PreparedMirrorUpload,
    PreparedMirrorVersion,
    PublishPlan (PublishPlan),
    pmvInventory,
    pmvPrepareUpload,
    publishPrepared,
 )
import Ecluse.Core.Rules.Render (renderReason)
import Ecluse.Core.Rules.Types (Decision (Blocked, Undecidable), Transience (WillResolve, WontResolve), mkEvalContext)
import Ecluse.Core.Security (authorityLabel, hostPortAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Server.Path (Filename, mkFilename, unFilename)
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (JobSpanOutcome (JobSpanOutcome), WorkerTracingPort (..))
import Ecluse.Core.Version (Version, renderVersion, selectLatest)
import Ecluse.Core.Worker.Fetch (fetchArtifactBytes)
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
    | {- | The source's own version object was unavailable, so no mirror write can reflect the
      package. Retired like 'Dropped', but reported apart from a policy deny.
      -}
      SourceUnavailable Text
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

-- The failure detail marks the span errored, so only a job that did not publish carries one.
jobSpanOutcome :: JobOutcome -> JobSpanOutcome
jobSpanOutcome = \case
    Succeeded -> JobSpanOutcome "succeeded" Nothing
    Dropped reason -> JobSpanOutcome "dropped" (Just reason)
    SourceUnavailable reason -> JobSpanOutcome "source-unavailable" (Just reason)
    DeadLettered reason -> JobSpanOutcome "dead-lettered" (Just reason)
    Retried _ reason -> JobSpanOutcome "retried" (Just reason)

{- | Decide one job, re-checking current policy before publishing, because the queue wait is
unbounded and mirrored bytes bypass every later rule.
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
            | Map.member (renderVersion (jobVersion job)) (infoVersions inventory) -> do
                logFM InfoS (ls ("already present at the mirror target, acking without re-publish: " <> renderJob job))
                pure Succeeded
            | otherwise -> reevaluatePolicy policy job >>= either pure (publishAdmitted policy job inventory)

{- An unreadable answer is not an empty store, so it reports a fault rather than let the write
declare a tag chosen without the inventory. A 404 is a store holding this package not at all. -}
probeInventory :: WorkerPolicy -> MirrorJob -> WorkerM (Either JobOutcome PackageInfo)
probeInventory policy job = do
    probed <- liftIO (mpProbeInventory (wpPublish policy) (jobPackage job))
    pure $ case probed of
        Right inventory -> Right inventory
        Left MetadataAbsent -> Right (PackageInfo (jobPackage job) mempty mempty [])
        Left (MetadataFetch fault) -> Left (outcomeOfFetchFault BeforePublish (probeFaultReason job) fault)
        Left (MetadataBoundExceeded limit) -> Left (outcomeOfFetchFault BeforePublish (probeFaultReason job) (FetchBoundExceeded limit))
        Left (MetadataAuthorisationFailure status) -> Left (Retried BeforePublish (probeStatusReason job status))
        Left (MetadataHttpFailure status) -> Left (Retried BeforePublish (probeStatusReason job status))
        Left MetadataUndecodable -> Left (Retried BeforePublish (probeParseReason job "invalid mirror metadata"))
        Left (MetadataNameMismatch _) -> Left (Retried BeforePublish (probeParseReason job "mismatched package identity"))

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

-- The complete version keeps its checked locations beside its adapter-owned upload preparation.
data Readmitted = Readmitted
    { raPrepared :: PreparedMirrorVersion
    , raLocations :: NonEmpty (ExpectedMirrorFile, RegistryUrl)
    , raUpstreamLatest :: Maybe Version
    }

reevaluatePolicy :: WorkerPolicy -> MirrorJob -> WorkerM (Either JobOutcome Readmitted)
reevaluatePolicy policy job =
    liftIO (wpResolveVersion policy (jobPackage job) (jobVersion job)) >>= admitEvaluation policy job

admitEvaluation :: WorkerPolicy -> MirrorJob -> VersionEvaluation -> WorkerM (Either JobOutcome Readmitted)
admitEvaluation policy job evaluation = case evaluation of
    VersionMetadataUnavailable -> pure (Left (unresolved ("could not re-fetch metadata to re-evaluate current policy for " <> renderJob job)))
    VersionMissing -> pure (Left (unresolved ("the public upstream no longer offers " <> renderJob job)))
    VersionPresent doc upstreamLatest -> case triggerFile policy job doc of
        Nothing -> pure (Left (Dropped "the source inventory has no supported trigger artifact"))
        Just trigger -> do
            ctx <- liftIO (mkEvalContext (wpNow policy) (pure Nothing))
            admission <- liftIO (admitArtifact ctx (wpRules policy) (wpMinIntegrity policy) trigger (vdDetails doc))
            case outcomeOfAdmission job admission of
                Left outcome -> pure (Left outcome)
                Right _ -> case supportedVersion policy job trigger doc of
                    Left outcome -> pure (Left outcome)
                    Right supported -> case mpPrepareVersion (wpPublish policy) (jobPackage job) (jobVersion job) supported of
                        Left fault -> Left <$> outcomeOfPublish job (Left fault)
                        Right prepared -> pure $ do
                            locations <- traverse (admittedLocation policy job) (inventoryFiles (pmvInventory prepared))
                            pure (Readmitted prepared locations upstreamLatest)
  where
    unresolved = retryOrDrop (versionTransience evaluation)

supportedVersion :: WorkerPolicy -> MirrorJob -> Filename -> VersionDoc -> Either JobOutcome VersionDoc
supportedVersion policy job trigger doc = do
    strong <- first (const (Dropped "no source artifact clears the integrity floor")) (partitionByFloor (wpMinIntegrity policy) artHashes (pkgArtifacts details))
    let kept = filter (isRight . sourceLocation policy) (toList strong)
    survivors <- maybeToRight (locationRefusal strong) (nonEmpty kept)
    unless (any ((== unFilename trigger) . artFilename) survivors) $
        Left (Dropped ("the current location policy refuses the requested artifact of " <> renderJob job))
    pure doc{vdDetails = details{pkgArtifacts = survivors}}
  where
    details = vdDetails doc
    locationRefusal (firstArtifact :| _) = fromLeft (Dropped "no supported artifact location") (sourceLocation policy firstArtifact)

sourceLocation :: WorkerPolicy -> Artifact -> Either JobOutcome RegistryUrl
sourceLocation policy artifact = do
    unless (wpArtifactHostHonoured policy (hostPortAddress (artUrl artifact))) $
        Left (Dropped ("the artifact-host policy refuses " <> authorityLabel (artUrl artifact)))
    first (const (Dropped "the source artifact URL violates the egress policy")) (wpEgressUrl policy (artUrl artifact))

triggerFile :: WorkerPolicy -> MirrorJob -> VersionDoc -> Maybe Filename
triggerFile policy job doc = jobTriggerFilename job <|> (find supported (pkgArtifacts (vdDetails doc)) >>= mkFilename . artFilename)
  where
    supported artifact = classifyDigests (wpMinIntegrity policy) (artHashes artifact) == MeetsFloor && isRight (sourceLocation policy artifact)

admittedLocation :: WorkerPolicy -> MirrorJob -> ExpectedMirrorFile -> Either JobOutcome (ExpectedMirrorFile, RegistryUrl)
admittedLocation policy job file = do
    case classifyDigests (wpMinIntegrity policy) (artHashes artifact) of
        MeetsFloor -> Right ()
        BelowFloor -> void (outcomeOfAdmission job AdmissionBelowFloor)
        NoIntegrity -> void (outcomeOfAdmission job AdmissionIntegrityMissing)
    location <- sourceLocation policy artifact
    pure (file, location)
  where
    artifact = expectedArtifact file

sourceUnavailableReason :: MirrorJob -> Text -> Text
sourceUnavailableReason job detail =
    "the source version object for "
        <> renderJob job
        <> " was unavailable ("
        <> detail
        <> "); refusing to mirror a reduced manifest"

{- | Render the shared 'ArtifactAdmission' as the descriptor to publish, or the outcome the queue
realises. 'admissionTransience' alone splits retry from drop, so no path can diverge from the gate.
-}
outcomeOfAdmission :: MirrorJob -> ArtifactAdmission -> Either JobOutcome MirrorArtifact
outcomeOfAdmission job admission = case admission of
    AdmissionAdmit filename artifact digests -> Right (readmittedDescriptor filename artifact digests)
    AdmissionDenied (Blocked ruleName _ reason) ->
        refused ("current policy denies " <> renderJob job <> ": blocked by " <> ruleName <> " (" <> renderReason reason <> ")")
    AdmissionDenied _ ->
        refused ("current policy denies " <> renderJob job <> ": no rule admits it")
    AdmissionUndecidable (Undecidable _ reason) ->
        refused ("current policy could not be evaluated for " <> renderJob job <> ": " <> renderReason reason)
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
publishAdmitted :: WorkerPolicy -> MirrorJob -> PackageInfo -> Readmitted -> WorkerM JobOutcome
publishAdmitted policy job target readmitted = copyFiles (toList (raLocations readmitted))
  where
    plan = PublishPlan (mirrorLatest (raUpstreamLatest readmitted) (map pkgVersion (Map.elems (infoVersions target))) (jobVersion job))
    copyFiles [] = pure Succeeded
    copyFiles ((file, location) : rest) =
        mirrorArtifact policy job plan (raPrepared readmitted) file location >>= \case
            Succeeded -> copyFiles rest
            failed -> pure failed

mirrorArtifact :: WorkerPolicy -> MirrorJob -> PublishPlan -> PreparedMirrorVersion -> ExpectedMirrorFile -> RegistryUrl -> WorkerM JobOutcome
mirrorArtifact policy job plan prepared file location = do
    fetched <- fetchArtifactBytes (wpArtifactLimits policy) (artifactByUrl (wpArtifact policy)) location
    case fetched of
        Left fault -> pure (outcomeOfFetchFault BeforePublish (artifactFetchReason location) fault)
        Right bytes -> case verifyMirrorBytes file bytes of
            Left fault -> outcomeOfPublish job (Left fault)
            Right verified -> do
                upload <- liftIO (pmvPrepareUpload prepared file verified)
                case upload of
                    Left fault -> outcomeOfPublish job (Left fault)
                    Right ready -> publishVerified job plan ready

artifactFetchReason :: RegistryUrl -> FetchFault -> Text
artifactFetchReason location = \case
    FetchUrlUnformable urlErr -> "unformable artifact URL: " <> renderUrlFormationError urlErr
    FetchBoundExceeded limitErr -> "artifact exceeded the response bound: " <> show limitErr
    FetchTransport fault -> "artifact fetch from " <> authorityLabel (registryUrlText location) <> " failed: " <> show (tfCause fault)

publishVerified :: MirrorJob -> PublishPlan -> PreparedMirrorUpload -> WorkerM JobOutcome
publishVerified job plan prepared = do
    metrics <- asks wrMetrics
    (result, seconds) <- timedSeconds (liftIO (publishPrepared prepared plan))
    liftIO (wmpMirrorPublishDuration metrics seconds)
    outcomeOfPublish job result

outcomeOfPublish :: MirrorJob -> Either PublishFault () -> WorkerM JobOutcome
outcomeOfPublish job = \case
    Right () -> Succeeded <$ logFM InfoS (ls ("mirrored artifact published: " <> renderJob job))
    Left (PublishIntegrityMismatch detail) -> do
        logFM ErrorS (ls ("artifact integrity mismatch, refusing to publish: " <> detail))
        pure (Dropped ("integrity mismatch: " <> detail))
    Left (PublishRejected err) -> pure (Retried AfterPublish ("registry rejected publish: " <> show err))
    Left (PublishSourceUnavailable detail) -> pure (SourceUnavailable (sourceUnavailableReason job detail))
    Left (PublishFetch fault) -> pure (outcomeOfFetchFault AfterPublish publishFaultReason fault)

-- The mirror target is operator-configured, so its rendered transport detail is diagnosable
-- rather than attacker-supplied.
publishFaultReason :: FetchFault -> Text
publishFaultReason = \case
    FetchUrlUnformable urlErr -> "unformable publish URL: " <> renderUrlFormationError urlErr
    FetchBoundExceeded limitErr -> "the publication target's response exceeded the response bound: " <> show limitErr
    FetchTransport fault -> "publish transport failure: " <> show fault
