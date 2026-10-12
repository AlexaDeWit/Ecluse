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

import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Data.Map.Strict qualified as Map
import Katip (Severity (DebugS, ErrorS, InfoS), katipAddNamespace, logFM, ls)
import UnliftIO (withRunInIO)

import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Fault (tfCause)
import Ecluse.Core.Mirror.State
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
    MirrorPublish (mpNativeComplete, mpPrepareVersion, mpProbeInventory),
    PreparedMirrorUpload,
    PreparedMirrorVersion,
    PublishPlan (PublishPlan),
    pmvInventory,
    pmvPrepareUpload,
    publishPrepared,
 )
import Ecluse.Core.Registry.Request (sidecarUrl)
import Ecluse.Core.Rules.Render (renderReason)
import Ecluse.Core.Rules.Types (Decision (Blocked, Undecidable), Transience (WillResolve, WontResolve), mkEvalContext)
import Ecluse.Core.Security (Limits (maxMetadataBytes, maxMirrorArtifactBytes), authorityLabel, hostPortAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Server.Path (Filename, mkFilename, unFilename)
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (JobSpanOutcome (JobSpanOutcome), WorkerTracingPort (..))
import Ecluse.Core.Version (Version, renderVersion, selectLatest)
import Ecluse.Core.Worker.Fetch (fetchArtifactBytes)
import Ecluse.Core.Worker.Managed
import Ecluse.Core.Worker.Types

{- | The terminal outcome of processing one mirror job. It decides whether the worker
acks the message or leaves it to redeliver.
-}
data JobOutcome
    = {- | Every expected file and companion has current completion proof.
      The worker acknowledges the whole version, including idempotent redelivery.
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

-- Incomplete versions mark the span errored even if some physical uploads succeeded.
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
            | otherwise -> withMirrorBudget policy (\charge -> mirrorUnlessPresent charge policy job)

withMirrorBudget :: WorkerPolicy -> (WorkerStateBudget -> WorkerM JobOutcome) -> WorkerM JobOutcome
withMirrorBudget policy action = case wpMirrorState policy of
    Nothing -> action (WorkerStateBudget ignore ignore ignore)
      where
        ignore = const (pure ())
    Just binding -> withRunInIO $ \run ->
        fromMaybe (Retried BeforePublish "worker memory admission refused managed mirror job") <$> wmsWithinBudget binding (run . action)

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

mirrorUnlessPresent :: WorkerStateBudget -> WorkerPolicy -> MirrorJob -> WorkerM JobOutcome
mirrorUnlessPresent charge policy job = do
    result <- runExceptT $ do
        target <- ExceptT (probeInventory policy job)
        if isNothing (wpMirrorState policy) && mpNativeComplete (wpPublish policy) (jobPackage job) (jobVersion job) target
            then pure ()
            else do
                previous <- readRecord charge policy job Nothing
                case previous >>= recordVersion (jobVersion job) of
                    Just version
                        | checkMirrorCompletion version (targetFiles job target) == MirrorComplete ->
                            verifyRecordedCompanions charge policy version
                    _ -> completePass charge policy job 3 target previous
    case result of
        Left failure -> pure failure
        Right () -> Succeeded <$ logFM InfoS (ls ("mirror version complete: " <> renderJob job))

-- A pass rereads native inventory before state, so concurrent additions cannot hide behind an old proof.
completePass :: WorkerStateBudget -> WorkerPolicy -> MirrorJob -> Int -> PackageInfo -> Maybe ProjectMirrorRecord -> ExceptT JobOutcome WorkerM ()
completePass charge policy job attempts target previous = do
    admitted <- ExceptT (reevaluatePolicy policy job)
    record <- stateAction (persistInventory charge (wpArtifactLimits policy) (wpMirrorState policy) (pmvInventory (raPrepared admitted)) previous)
    copied <- copyRecorded charge policy job target (raUpstreamLatest admitted) record
    finalTarget <- ExceptT (probeInventory policy job)
    latest <- readRecord charge policy job (Just copied)
    current <- maybe (throwError (Retried AfterPublish "mirror record disappeared before completion")) pure latest
    version <- requiredVersion job current
    let copiedNames = maybe mempty versionFiles (recordVersion (jobVersion job) record)
    case checkMirrorCompletion version (targetFiles job finalTarget) of
        MirrorComplete -> verifyRecordedCompanions charge policy version
        MirrorIncomplete faults
            | attempts > 1 && any (newlyRequired copiedNames) faults -> completePass charge policy job (attempts - 1) finalTarget (Just current)
            | otherwise -> throwError (Retried AfterPublish ("mirror remains incomplete: " <> show faults))
  where
    newlyRequired known (MissingMirrorFile name) = Map.notMember name known
    newlyRequired known (PendingMirrorCompanion name) = Map.notMember name known
    newlyRequired _ _ = False

readRecord :: WorkerStateBudget -> WorkerPolicy -> MirrorJob -> Maybe ProjectMirrorRecord -> ExceptT JobOutcome WorkerM (Maybe ProjectMirrorRecord)
readRecord charge policy job fallback = case wpMirrorState policy of
    Nothing -> pure fallback
    Just binding -> stateAction (readMirrorRecord charge binding (jobPackage job))

verifyRecordedCompanions :: WorkerStateBudget -> WorkerPolicy -> MirrorVersion -> ExceptT JobOutcome WorkerM ()
verifyRecordedCompanions charge policy version = ExceptT $ withRunInIO $ \run ->
    first stateOutcome <$> verifyCompanions charge (run recordWorkerProgress) (wpMirrorState policy) version

stateAction :: IO (Either MirrorStateFault a) -> ExceptT JobOutcome WorkerM a
stateAction action = ExceptT (fmap (first stateOutcome) (liftIO action))

stateOutcome :: MirrorStateFault -> JobOutcome
stateOutcome fault = case fault of
    StateBound _ -> DeadLettered detail
    StateInvalid _ -> DeadLettered detail
    StateIdentityMismatch -> DeadLettered detail
    StateFileConflict _ -> Dropped detail
    StateCompanionConflict _ -> Dropped detail
    StateTransport _ -> Retried BeforePublish detail
    StateRevisionMismatch -> Retried BeforePublish detail
    StateMissingCompanion -> Retried BeforePublish detail
    StateRetryExhausted -> Retried BeforePublish detail
  where
    detail = "mirror state unavailable: " <> show fault

requiredVersion :: MirrorJob -> ProjectMirrorRecord -> ExceptT JobOutcome WorkerM MirrorVersion
requiredVersion job = maybe (throwError (Retried BeforePublish "missing recorded mirror version")) pure . recordVersion (jobVersion job)

targetFiles :: MirrorJob -> PackageInfo -> [Artifact]
targetFiles job target = maybe [] (toList . pkgArtifacts) (Map.lookup (renderVersion (jobVersion job)) (infoVersions target))

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

-- Each fragment retains its original source and is readmitted under the current policy before a new write.
copyRecorded :: WorkerStateBudget -> WorkerPolicy -> MirrorJob -> PackageInfo -> Maybe Version -> ProjectMirrorRecord -> ExceptT JobOutcome WorkerM ProjectMirrorRecord
copyRecorded charge policy job target upstreamLatest record = do
    version <- requiredVersion job record
    foldM copyFragment record (toList (versionFragments version))
  where
    plan = PublishPlan (mirrorLatest upstreamLatest (map pkgVersion (Map.elems (infoVersions target))) (jobVersion job))
    currentFiles = targetFiles job target
    copyFragment current fragment = do
        version <- requiredVersion job current
        let files = filter (needsWork version) (toList (pkgArtifacts (fragmentDetails fragment)))
        if null files
            then pure current
            else do
                admitted <- ExceptT (admitEvaluation policy job{jobTriggerFilename = Nothing} (VersionPresent (fragmentDocument fragment) upstreamLatest))
                foldM (copyFile (raPrepared admitted)) current (toList (raLocations admitted))
    needsWork version artifact =
        Map.lookup (artFilename artifact) (versionCompanions version) == Just CompanionPending
            || not (any ((== artFilename artifact) . artFilename) currentFiles)
    copyFile prepared current (file, location) = do
        version <- requiredVersion job current
        let name = artFilename (expectedArtifact file)
            matches = filter ((== name) . artFilename) currentFiles
            verified = Map.lookup name (versionVerifiedIdentities version)
        unless (all (expectedTargetMatchesWith file verified) matches) $
            throwError (Retried BeforePublish ("mirror file lacks compatible current identity: " <> name))
        uploaded <- if null matches then mirrorArtifact charge policy job plan prepared file location current else pure current
        retained <- retainPending charge policy job file uploaded
        lift recordWorkerProgress
        pure retained

retainPending :: WorkerStateBudget -> WorkerPolicy -> MirrorJob -> ExpectedMirrorFile -> ProjectMirrorRecord -> ExceptT JobOutcome WorkerM ProjectMirrorRecord
retainPending charge policy job file record = do
    version <- requiredVersion job record
    case Map.lookup (artFilename (expectedArtifact file)) (versionCompanions version) of
        Just CompanionPending -> case wpMirrorState policy of
            Nothing -> throwError (Retried BeforePublish "declared companion requires durable mirror state")
            Just binding -> do
                location <- either throwError pure (sourceLocation policy (expectedArtifact file){artUrl = sidecarUrl (artUrl (expectedArtifact file))})
                let limits = (wmsLimits binding){maxMirrorArtifactBytes = maxMetadataBytes (wmsLimits binding)}
                liftIO (wsbCompanions charge (fromInteger (min (toInteger (maxBound :: Int)) (2 * toInteger (maxMetadataBytes limits)))))
                bytes <- ExceptT (first (outcomeOfFetchFault BeforePublish (artifactFetchReason location)) <$> fetchArtifactBytes limits (artifactByUrl (wpArtifact policy)) location)
                verified <- either (throwError . stateOutcome) pure (verifyCompanionBytes (wmsLimits binding) file bytes)
                stateAction (retainCompanion charge binding (jobVersion job) verified record)
        _ -> pure record

mirrorArtifact :: WorkerStateBudget -> WorkerPolicy -> MirrorJob -> PublishPlan -> PreparedMirrorVersion -> ExpectedMirrorFile -> RegistryUrl -> ProjectMirrorRecord -> ExceptT JobOutcome WorkerM ProjectMirrorRecord
mirrorArtifact charge policy job plan prepared file location record = do
    bytes <- ExceptT (first (outcomeOfFetchFault BeforePublish (artifactFetchReason location)) <$> fetchArtifactBytes (wpArtifactLimits policy) (artifactByUrl (wpArtifact policy)) location)
    verified <- publishAction job (pure (verifyMirrorBytes file bytes))
    retained <- stateAction (persistVerified charge (wpMirrorState policy) (jobVersion job) verified record)
    ready <- publishAction job (liftIO (pmvPrepareUpload prepared file verified))
    outcome <- lift (publishVerified job plan ready)
    case outcome of
        Succeeded -> pure retained
        failure -> throwError failure

publishAction :: MirrorJob -> WorkerM (Either PublishFault a) -> ExceptT JobOutcome WorkerM a
publishAction job action =
    ExceptT $
        action >>= \case
            Right value -> pure (Right value)
            Left fault -> Left <$> outcomeOfPublish job (Left fault)

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
