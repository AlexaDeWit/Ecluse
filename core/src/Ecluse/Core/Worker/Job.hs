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

import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Data.Aeson (object, (.=))
import Data.Map.Strict qualified as Map
import Data.Traversable (for)
import Katip (Severity (DebugS, ErrorS, InfoS), SimpleLogPayload, katipAddContext, katipAddNamespace, logFM, ls, sl)
import UnliftIO (withRunInIO)

import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Fault (tfCause)
import Ecluse.Core.Mirror.State
import Ecluse.Core.Package (Artifact (artFilename, artHashes, artSize, artUrl), Hash, PackageDetails (pkgArtifacts, pkgVersion), PackageInfo (..), hashAlg, hashValue, pkgEcosystem, renderHashAlg, renderPackageName)
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
    admitArtifactWithDecision,
 )
import Ecluse.Core.Package.Integrity (VersionIntegrity (..), classifyDigests, partitionByFloor)
import Ecluse.Core.Queue (MirrorJob (jobPackage, jobTraceContext, jobTriggerFilename, jobVersion))
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchCooldown, FetchTransport, FetchUrlUnformable),
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    PublishFault (PublishFetch, PublishIntegrityMismatch, PublishRejected, PublishSourceUnavailable),
    renderUrlFormationError,
 )
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByUrl))
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecDeadlines))
import Ecluse.Core.Registry.Metadata (
    MetadataError (..),
    VersionDoc (vdDetails),
    VersionEvaluation (VersionCooling, VersionMetadataUnavailable, VersionMissing, VersionPresent),
    versionTransience,
 )
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.Publish (
    MirrorCompanion (..),
    MirrorPublish (mpMirrorCompanion, mpNativeComplete, mpPrepareVersion, mpProbeInventory),
    PreparedMirrorUpload,
    PreparedMirrorVersion,
    PublishPlan (PublishPlan),
    pmvInventory,
    pmvPrepareUpload,
    publishPrepared,
 )
import Ecluse.Core.Registry.Request (sidecarUrl)
import Ecluse.Core.Rules.Outage (logSkippedCheckEvidence)
import Ecluse.Core.Rules.Render (renderDecision, renderReason)
import Ecluse.Core.Rules.Types (Decision (Admitted, Blocked, BlockedByDefault, Undecidable), Transience (WillResolve, WontResolve), identityEvidence, mkEvalContext, skippedChecks)
import Ecluse.Core.Security (Limits (maxMetadataBytes, maxMirrorArtifactBytes), authorityLabel, hostPortAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Server.Path (Filename, mkFilename, unFilename)
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (JobSpanOutcome (JobSpanOutcome), WorkerTracingPort (..))
import Ecluse.Core.Version (Version, renderVersion, selectLatest)
import Ecluse.Core.Worker.Fetch (fetchArtifactBytesCharged)
import Ecluse.Core.Worker.Managed
import Ecluse.Core.Worker.Types

-- | A mirror outcome determines acknowledgement, redelivery, or dead-letter handling.
data JobOutcome
    = -- | Completion proof covers every expected file and companion, including idempotent redelivery.
      Succeeded
    | -- | A non-retryable rejection. The worker alarms and acknowledges the message.
      Dropped Text
    | -- | The source version object is unavailable. Retire the job separately from a policy denial.
      SourceUnavailable Text
    | -- | A terminal fault sent to the dead-letter handler instead of silently deleted.
      DeadLettered Text
    | -- | A transient fault leaves the message unacknowledged and retains the failed leg.
      Retried RetryLeg Text
    deriving stock (Eq, Show)

-- | The failed leg determines whether the worker resets queue visibility.
data RetryLeg
    = -- | A failure before publication keeps the lease until redelivery.
      BeforePublish
    | -- | A transient publication failure releases the message for immediate redelivery.
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

-- | Recheck current policy before publishing because queue delays are unbounded.
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
    Nothing -> action (WorkerStateBudget ignore ignore ignore ignore ignore ignore ignore)
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
        target <- ExceptT (probeInventory charge policy job)
        if isNothing (wpMirrorState policy) && triggerPresent (\name -> any ((== name) . artFilename) (targetFiles job target)) && mpNativeComplete (wpPublish policy) (jobPackage job) (jobVersion job) target
            then pure DebugS
            else do
                previous <- readRecord charge policy job Nothing
                case previous >>= recordVersion (jobVersion job) of
                    Just version
                        | triggerPresent (`Map.member` versionFiles version)
                        , checkMirrorCompletion version (targetFiles job target) == MirrorComplete ->
                            DebugS <$ verifyRecordedCompanions charge policy version
                    _ -> InfoS <$ completePass charge policy job 3 target previous
    case result of
        Left failure -> pure failure
        Right severity -> Succeeded <$ logFM severity (ls ("mirror version complete: " <> renderJob job))
  where
    triggerPresent contains = maybe True (contains . unFilename) (jobTriggerFilename job)

-- A pass rereads native inventory before state, so concurrent additions cannot hide behind an old proof.
completePass :: WorkerStateBudget -> WorkerPolicy -> MirrorJob -> Int -> PackageInfo -> Maybe ProjectMirrorRecord -> ExceptT JobOutcome WorkerM ()
completePass charge policy job attempts target previous = do
    admitted <- ExceptT (reevaluatePolicy charge policy job)
    record <- stateAction (persistInventory charge (wpArtifactLimits policy) (wpMirrorState policy) (pmvInventory (raPrepared admitted)) previous)
    copied <- copyRecorded charge policy job target admitted record
    finalTarget <- ExceptT (probeInventory charge policy job)
    latest <- readRecord charge policy job (Just copied)
    current <- maybe (throwError (Retried AfterPublish "mirror record disappeared before completion")) pure latest
    version <- requiredVersion job current
    let copiedNames = maybe mempty versionFiles (recordVersion (jobVersion job) record)
    case checkMirrorCompletion version (targetFiles job finalTarget) of
        MirrorComplete -> verifyRecordedCompanions charge policy version
        MirrorIncomplete faults
            | any conflicting faults -> throwError (Dropped ("mirror target identity conflicts: " <> show faults))
            | attempts > 1 && any (newlyRequired copiedNames) faults -> completePass charge policy job (attempts - 1) finalTarget (Just current)
            | otherwise -> throwError (Retried AfterPublish ("mirror remains incomplete: " <> show faults))
  where
    conflicting (ConflictingMirrorFile _) = True
    conflicting _ = False
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
probeInventory :: WorkerStateBudget -> WorkerPolicy -> MirrorJob -> WorkerM (Either JobOutcome PackageInfo)
probeInventory charge policy job = do
    probed <- liftIO (mpProbeInventory (wpPublish policy) (wsbTargets charge) (jobPackage job))
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
    FetchCooldown _ -> "the public upstream is cooling down"
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
    , raDecision :: Decision
    }

reevaluatePolicy :: WorkerStateBudget -> WorkerPolicy -> MirrorJob -> WorkerM (Either JobOutcome Readmitted)
reevaluatePolicy charge policy job =
    liftIO (wpResolveVersion policy (wsbSource charge) (jobPackage job) (jobVersion job)) >>= admitEvaluation policy job

admitEvaluation :: WorkerPolicy -> MirrorJob -> VersionEvaluation -> WorkerM (Either JobOutcome Readmitted)
admitEvaluation policy job evaluation = case evaluation of
    VersionCooling _ -> pure (Left (unresolved "the public upstream is cooling down"))
    VersionMetadataUnavailable -> pure (Left (unresolved ("could not re-fetch metadata to re-evaluate current policy for " <> renderJob job)))
    VersionMissing -> pure (Left (unresolved ("the public upstream no longer offers " <> renderJob job)))
    VersionPresent doc upstreamLatest -> case triggerFile policy job doc of
        Nothing -> pure (Left (Dropped "the source inventory has no supported trigger artifact"))
        Just trigger -> do
            ctx <- liftIO (mkEvalContext (wpNow policy) (pure Nothing))
            (admission, decision) <- liftIO (admitArtifactWithDecision ctx (wpRules policy) (wpMinIntegrity policy) trigger (vdDetails doc))
            case outcomeOfAdmission job admission of
                Left outcome -> pure (Left outcome)
                Right _ -> do
                    logSkippedCheckEvidence
                        (sl "package" (renderPackageName (jobPackage job)) <> sl "version" (renderVersion (jobVersion job)))
                        (skippedChecks decision)
                    case supportedVersion policy job trigger doc of
                        Left outcome -> pure (Left outcome)
                        Right supported -> case mpPrepareVersion (wpPublish policy) (jobPackage job) (jobVersion job) supported of
                            Left fault -> Left <$> outcomeOfPublish job (Left fault)
                            Right prepared -> pure $ do
                                locations <- traverse (admittedLocation policy job) (inventoryFiles (pmvInventory prepared))
                                pure (Readmitted prepared locations upstreamLatest decision)
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

-- | Project admission to publication or a queue outcome. 'admissionTransience' determines retry versus drop.
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

-- | Classify fetch and publication faults through one retry-versus-drop table.
outcomeOfFetchFault :: RetryLeg -> (FetchFault -> Text) -> FetchFault -> JobOutcome
outcomeOfFetchFault leg render fault = verdict (render fault)
  where
    verdict = case fault of
        FetchUrlUnformable _ -> Dropped
        FetchBoundExceeded _ -> DeadLettered
        FetchTransport _ -> Retried leg
        FetchCooldown _ -> Retried leg

-- | Choose the mirror latest tag from the public tag and post-write inventory, which includes the published version.
mirrorLatest :: Maybe Version -> [Version] -> Version -> Version
mirrorLatest upstreamLatest inventory published =
    fromMaybe published (selectLatest upstreamLatest (published : inventory))

-- Keep fixed inputs lazy so inspecting the context does not force the plan or target inventory.
data CopyPass = CopyPass
    { cpCharge :: ~WorkerStateBudget
    , cpPolicy :: ~WorkerPolicy
    , cpJob :: ~MirrorJob
    , cpPlan :: ~PublishPlan
    , cpTargetFiles :: ~[Artifact]
    , cpLatest :: ~Readmitted
    }

-- Each fragment requires current admission before the pass traverses all its admitted locations.
copyRecorded :: WorkerStateBudget -> WorkerPolicy -> MirrorJob -> PackageInfo -> Readmitted -> ProjectMirrorRecord -> ExceptT JobOutcome WorkerM ProjectMirrorRecord
copyRecorded charge policy job target latest record = do
    version <- requiredVersion job record
    foldM (copyFragment context) record (toList (versionFragments version))
  where
    context =
        CopyPass
            { cpCharge = charge
            , cpPolicy = policy
            , cpJob = job
            , cpPlan = PublishPlan (mirrorLatest (raUpstreamLatest latest) (map pkgVersion (Map.elems (infoVersions target))) (jobVersion job))
            , cpTargetFiles = targetFiles job target
            , cpLatest = latest
            }

copyFragment :: CopyPass -> ProjectMirrorRecord -> StoredMirrorFragment -> ExceptT JobOutcome WorkerM ProjectMirrorRecord
copyFragment context current fragment = do
    version <- requiredVersion job current
    let files = filter (needsCopy (cpTargetFiles context) version) (toList (pkgArtifacts (fragmentDetails fragment)))
    if null files
        then pure current
        else do
            admitted <- ExceptT (admitEvaluation (cpPolicy context) job{jobTriggerFilename = Nothing} (VersionPresent (fragmentDocument fragment) (raUpstreamLatest (cpLatest context))))
            foldM (copyFile context admitted) current (toList (raLocations admitted))
  where
    job = cpJob context

needsCopy :: [Artifact] -> MirrorVersion -> Artifact -> Bool
needsCopy currentFiles version artifact =
    let name = artFilename artifact
        matches = filter ((== name) . artFilename) currentFiles
        verified = Map.lookup name (versionVerifiedIdentities version)
     in Map.lookup name (versionCompanions version) == Just CompanionPending
            || null matches
            || maybe True (\file -> not (all (expectedTargetMatchesWith file verified) matches)) (Map.lookup name (versionFiles version))

copyFile :: CopyPass -> Readmitted -> ProjectMirrorRecord -> (ExpectedMirrorFile, RegistryUrl) -> ExceptT JobOutcome WorkerM ProjectMirrorRecord
copyFile context admitted current (file, location) = do
    version <- requiredVersion job current
    let name = artFilename (expectedArtifact file)
        matches = filter ((== name) . artFilename) (cpTargetFiles context)
        verified = Map.lookup name (versionVerifiedIdentities version)
        locations = location :| [candidate | (incoming, candidate) <- toList (raLocations (cpLatest context)), expectedIdentityMatches file incoming, candidate /= location]
    when (any (targetIdentityConflict file verified) matches) $
        throwError (Dropped ("mirror target identity conflicts: " <> name))
    let targetProven = all (expectedTargetMatchesWith file verified) matches
        needsNativeMetadata = isJust (mpMirrorCompanion (wpPublish policy)) && Map.lookup name (versionCompanions version) == Just CompanionPending
    uploaded <- case matches of
        [] -> mirrorArtifact charge policy job (cpPlan context) admitted file locations current
        _ | targetProven && not needsNativeMetadata -> pure current
        _ -> verifyExistingTarget context admitted file locations matches current
    retained <- retainPending charge policy job (pmvInventory (raPrepared admitted)) file locations uploaded
    lift recordWorkerProgress
    pure retained
  where
    charge = cpCharge context
    policy = cpPolicy context
    job = cpJob context

verifyExistingTarget :: CopyPass -> Readmitted -> ExpectedMirrorFile -> NonEmpty RegistryUrl -> [Artifact] -> ProjectMirrorRecord -> ExceptT JobOutcome WorkerM ProjectMirrorRecord
verifyExistingTarget context admitted file locations matches current = do
    (bytes, retained) <- fetchVerified (cpCharge context) (cpPolicy context) (cpJob context) file locations current
    for_ (mpMirrorCompanion (wpPublish (cpPolicy context))) $ \_ ->
        void (publishAction (cpJob context) (liftIO (pmvPrepareUpload (raPrepared admitted) (wsbArchive (cpCharge context)) file bytes)))
    let proof = Just (verifiedTargetHash bytes)
    when (any (targetIdentityConflict file proof) matches) $
        throwError (Dropped ("mirror target identity conflicts: " <> name))
    unless (all (expectedTargetMatchesWith file proof) matches) $
        throwError (Retried BeforePublish ("mirror file lacks compatible current identity: " <> name))
    pure retained
  where
    name = artFilename (expectedArtifact file)

retainPending :: WorkerStateBudget -> WorkerPolicy -> MirrorJob -> MirrorInventory -> ExpectedMirrorFile -> NonEmpty RegistryUrl -> ProjectMirrorRecord -> ExceptT JobOutcome WorkerM ProjectMirrorRecord
retainPending charge policy job inventory file locations record = do
    version <- requiredVersion job record
    case Map.lookup (artFilename (expectedArtifact file)) (versionCompanions version) of
        Just CompanionPending -> case wpMirrorState policy of
            Nothing -> throwError (Retried BeforePublish "declared companion requires durable mirror state")
            Just binding -> do
                let native = mpMirrorCompanion (wpPublish policy)
                case (inventoryReadData inventory, native) of
                    (RubyGemsMirrorRead _, Nothing) -> throwError (Retried BeforePublish "native companion preparation is unavailable")
                    _ -> pure ()
                candidates <- for locations $ \location -> do
                    url <- case native of
                        Nothing -> pure (sidecarUrl (registryUrlText location))
                        Just companion -> publishAction job (pure (mcLocation companion file (registryUrlText location)))
                    either throwError pure (sourceLocation policy (expectedArtifact file){artUrl = url})
                let limits = (wmsLimits binding){maxMirrorArtifactBytes = maxMetadataBytes (wmsLimits binding)}
                bytes <- fetchLocations (wsbCompanions charge) limits policy candidates
                deadlines <- asks (ecDeadlines . wrClient)
                checked <- case native of
                    Nothing -> pure bytes
                    Just companion -> publishAction job (liftIO (mcValidate companion deadlines limits (wsbCompanions charge) inventory file bytes))
                verified <- either (throwError . stateOutcome) pure (verifySourceCompanionBytes (wmsLimits binding) (inventoryReadData inventory) file checked)
                stateAction (retainCompanion charge binding (jobVersion job) verified record)
        _ -> pure record

mirrorArtifact :: WorkerStateBudget -> WorkerPolicy -> MirrorJob -> PublishPlan -> Readmitted -> ExpectedMirrorFile -> NonEmpty RegistryUrl -> ProjectMirrorRecord -> ExceptT JobOutcome WorkerM ProjectMirrorRecord
mirrorArtifact charge policy job plan admitted file locations record = do
    (verified, retained) <- fetchVerified charge policy job file locations record
    ready <- publishAction job (liftIO (pmvPrepareUpload (raPrepared admitted) (wsbArchive charge) file verified))
    outcome <- lift (katipAddContext (mirrorAdmissionPayload job (raDecision admitted) file (verifiedTargetHash verified)) (publishVerified job plan ready))
    case outcome of
        Succeeded -> pure retained
        failure -> throwError failure

fetchVerified :: WorkerStateBudget -> WorkerPolicy -> MirrorJob -> ExpectedMirrorFile -> NonEmpty RegistryUrl -> ProjectMirrorRecord -> ExceptT JobOutcome WorkerM (VerifiedArtifactBytes, ProjectMirrorRecord)
fetchVerified charge policy job file locations record = do
    bytes <- fetchLocations (wsbArtifacts charge) (wpArtifactLimits policy) policy locations
    verified <- publishAction job (pure (verifyMirrorBytes file bytes))
    retained <- stateAction (persistVerified charge (wpMirrorState policy) (jobVersion job) verified record)
    pure (verified, retained)

fetchLocations :: StateCharge -> Limits -> WorkerPolicy -> NonEmpty RegistryUrl -> ExceptT JobOutcome WorkerM ByteString
fetchLocations charge limits policy (location :| alternatives) = do
    result <- lift (fetchArtifactBytesCharged charge limits (artifactByUrl (wpArtifact policy)) location)
    case (result, nonEmpty alternatives) of
        (Right bytes, _) -> pure bytes
        (Left (FetchTransport _), Just remaining) -> fetchLocations charge limits policy remaining
        (Left fault, _) -> throwError (outcomeOfFetchFault BeforePublish (artifactFetchReason location) fault)

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
    FetchCooldown _ -> "the public upstream is cooling down"
    FetchTransport fault -> "artifact fetch from " <> authorityLabel (registryUrlText location) <> " failed: " <> show (tfCause fault)

mirrorAdmissionPayload :: MirrorJob -> Decision -> ExpectedMirrorFile -> Hash -> SimpleLogPayload
mirrorAdmissionPayload job decision file digest =
    sl "event" ("mirror_publication" :: Text)
        <> sl "package" (renderPackageName (jobPackage job))
        <> sl "version" (renderVersion (jobVersion job))
        <> sl "ecosystem" (ecosystemName (pkgEcosystem (jobPackage job)))
        <> sl "target" ("mirrorTarget" :: Text)
        <> sl "artifact" (artFilename artifact)
        <> sl "digest" (object ["algorithm" .= renderHashAlg (hashAlg digest), "value" .= hashValue digest])
        <> sl "admission" (renderDecision (identityEvidence (jobPackage job) (jobVersion job)) decision)
        <> ruleField
  where
    artifact = expectedArtifact file
    ruleField = case decision of
        Admitted name _ _ -> sl "rule" name
        Blocked name _ _ -> sl "rule" name
        BlockedByDefault _ -> sl "rule" ("BlockedByDefault" :: Text)
        Undecidable _ _ -> mempty

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
    FetchCooldown _ -> "the public upstream is cooling down"
    FetchTransport fault -> "publish transport failure: " <> show fault
