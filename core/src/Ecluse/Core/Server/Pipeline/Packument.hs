-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Merge authorised private metadata with admitted public versions. First-party names use only
private metadata, and a private read that fails refuses every name with the status its cause sets.
-}
module Ecluse.Core.Server.Pipeline.Packument (
    PackumentReplies (..),
    DocumentRejectionObserver (..),
    packumentAction,
    servePackument,
    headPackument,
    withObservedAdmittedDocument,
    answerMetadataBytes,
    answerDigestedMetadataBytes,
    mapPackumentReplies,

    -- * The first-party private miss (exported for its unit spec)
    firstPartyMissDecision,
    firstPartyMissReply,

    -- * The refusal when no version survives (exported for its unit spec)
    noSurvivors,
) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Katip (Severity (DebugS), logFM, ls)
import Network.HTTP.Types (Method, ResponseHeaders, hContentLength)
import Network.Wai (Request, ResponseReceived, requestHeaders, requestMethod)
import UnliftIO (concurrently)
import UnliftIO.Exception (catchAny, evaluate, throwIO)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Cve.Types (DbEtag)
import Ecluse.Core.Mirror.Documents (MirrorDocuments)
import Ecluse.Core.Mirror.State (ProjectMirrorRecord, StateRevision)
import Ecluse.Core.Package (
    Artifact (artHashes),
    PackageDetails (pkgArtifacts),
    PackageInfo (infoInvalidEntries, infoVersions),
    PackageName,
    renderPackageName,
 )
import Ecluse.Core.Package.Filter (filterPlanFromDecisions, fpDecisions, fpSurvivors, restrictToSurvivors)
import Ecluse.Core.Package.Integrity (
    IntegrityFloor,
    MinTrustedIntegrity,
    VersionIntegrity (BelowFloor, MeetsFloor, NoIntegrity),
    classifyDigests,
 )
import Ecluse.Core.Package.Merge (
    MergePlan (mpDivergences, mpSurvivors),
    Provenance (GatedSource, TrustedSource),
    integrityDivergences,
    mergePackuments,
    withoutTrustedReleases,
 )
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors, metadataSerialise))
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (
    Manifest (manifestBodyBytes, manifestDigest, manifestInfo, manifestRaw),
 )
import Ecluse.Core.Rules (newEvaluator)
import Ecluse.Core.Rules.Types (Decision (Blocked), EvalContext (ctxAdvisoryEtag), completeEvidence, mkEvalContext)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Meter (ChargePart, MemoryTicket, awaitingFlight, charge, chargePart, newChargePart, servingFlight, stepDown)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfOutputPermille))
import Ecluse.Core.Server.Cache (resolveAssembled)
import Ecluse.Core.Server.Cache.Types (assembledKey)
import Ecluse.Core.Server.Conditional (Conditional (Modified, NotModified), ETag, etagHeader, evaluateETag, mkStrongETag)
import Ecluse.Core.Server.Context (
    Handler,
    MountBinding (bindingPackumentDeps),
    PackumentDeps (..),
    ResponseAction (RunPipeline),
    ServeRuntime (..),
    ctxMount,
    ctxRuntime,
    pdPrivateBaseUrl,
    pdPublicBaseUrl,
 )
import Ecluse.Core.Server.Encoding (
    CodingChoice (NoAcceptableCoding, ServeCoding),
    ContentCoding (GzipCoding, IdentityCoding),
    MetadataCompression (GzipWhenAccepted, IdentityOnly),
    chooseCoding,
    gzipEncodingHeader,
    varyAcceptEncoding,
 )
import Ecluse.Core.Server.Fault (RenderEscape (RenderEscape))
import Ecluse.Core.Server.MetadataGzip (GzipBody (gzipLength), gzipLazyBody, gzipMetadata)
import Ecluse.Core.Server.MirrorView (mirrorProjectViews, mirrorViewDocuments, publicMirrorCoverage, validateMirrorProject)
import Ecluse.Core.Server.Pipeline.Diagnostics (warnDivergences)
import Ecluse.Core.Server.Pipeline.Internal (
    VersionVerdict (..),
    admitByIntegrity,
    evalTier,
    logDenials,
    packumentServeDecision,
    recordDenials,
    recordEffectfulFailures,
    serveDecisionClass,
    statusServeDecision,
 )
import Ecluse.Core.Server.Pipeline.Origin (
    OriginMiss (MissAbsent, MissUnresolved),
    OriginResult (..),
    ReadFailure (ReadInvalid, ReadUnavailable),
    fetchPrivateOriginObserved,
    fetchPublicOriginObserved,
    originManifest,
    originMiss,
    readFailureRefusal,
 )
import Ecluse.Core.Server.Pipeline.Packument.Representation
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Response (
    HelpMessage,
    PackumentStatus (PackumentBadGateway, PackumentDenied, PackumentForbidden, PackumentOk, PackumentServerError, PackumentUnavailable),
    Refusal,
    RejectReason (ByPolicy, Unavailable, UpstreamInvalid),
    Rejection (Rejection),
    RetryAfter,
    ServeDecision (Admit, Reject),
    Transience (WillResolve, WontResolve),
    mkRefusal,
    packumentStatus,
    rejectUnavailable,
    serveDecisionOf,
 )
import Ecluse.Core.Server.Response.Summary (detailedRejections)
import Ecluse.Core.Server.Route (isHead)
import Ecluse.Core.Snapshot (Snapshot (..))
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (TracingPort, spanPackumentGate)

-- | The route-owned ways the ecosystem-neutral packument pipeline may answer.
data PackumentReplies response = PackumentReplies
    { packumentDeniedText :: [ServeDecision] -> Text
    -- ^ Render the refusals of a listing with no survivor and no outage, after status selection.
    , packumentOk :: ResponseHeaders -> LByteString -> response
    , packumentNotModified :: ResponseHeaders -> response
    , packumentUnauthorised :: ResponseHeaders -> Refusal -> response
    , packumentPrivateRefused :: Int -> ResponseHeaders -> Refusal -> response
    -- ^ Preserve or collapse the private origin status according to the native protocol.
    , packumentForbidden :: ResponseHeaders -> Refusal -> response
    , packumentDenied :: ResponseHeaders -> Refusal -> response
    -- ^ Answer a listing that admitted nothing because every exclusion was a policy or integrity refusal.
    , packumentNotFound :: ResponseHeaders -> Refusal -> response
    , packumentNotAcceptable :: ResponseHeaders -> Refusal -> response
    , packumentInternal :: ResponseHeaders -> Refusal -> response
    , packumentBadGateway :: ResponseHeaders -> Refusal -> response
    , packumentUnavailable :: ResponseHeaders -> Refusal -> response
    }

-- | Observe checked version and file rejections for native coordinate resolution.
data DocumentRejectionObserver response = DocumentRejectionObserver
    { observeVersionRejection :: EvalContext -> PackageDetails -> Rejection -> IO ()
    -- ^ Keep the original gate context with each checked version or file exclusion.
    , onCheckedNoSurvivorAdmission :: IO response
    -- ^ Resolve a checked public inventory after settled private absence and no surviving contribution.
    }

-- | Change only response construction while retaining shared admission and refusal decisions.
mapPackumentReplies :: (a -> b) -> PackumentReplies a -> PackumentReplies b
mapPackumentReplies convert replies =
    PackumentReplies
        { packumentDeniedText = packumentDeniedText replies
        , packumentOk = \headers -> convert . packumentOk replies headers
        , packumentNotModified = convert . packumentNotModified replies
        , packumentUnauthorised = \headers -> convert . packumentUnauthorised replies headers
        , packumentPrivateRefused = \code headers -> convert . packumentPrivateRefused replies code headers
        , packumentForbidden = \headers -> convert . packumentForbidden replies headers
        , packumentDenied = \headers -> convert . packumentDenied replies headers
        , packumentNotFound = \headers -> convert . packumentNotFound replies headers
        , packumentNotAcceptable = \headers -> convert . packumentNotAcceptable replies headers
        , packumentInternal = \headers -> convert . packumentInternal replies headers
        , packumentBadGateway = \headers -> convert . packumentBadGateway replies headers
        , packumentUnavailable = \headers -> convert . packumentUnavailable replies headers
        }

-- | GET and HEAD use the same gate and merge. HEAD withholds the body.
packumentAction :: PackumentReplies response -> Method -> PackageName -> ResponseAction response
packumentAction replies method name
    | isHead method = RunPipeline perimeterFallback (headPackument replies name)
    | otherwise = RunPipeline perimeterFallback (servePackument replies name)
  where
    perimeterFallback = packumentInternal replies [] (mkRefusal Nothing "internal server error")

-- | Serve merged metadata while retaining private access authority and package identity checks.
servePackument ::
    PackumentReplies response ->
    PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
servePackument = packumentWith PackumentFull

-- | Serve the packument's GET status and headers without its body.
headPackument ::
    PackumentReplies response ->
    PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
headPackument = packumentWith PackumentHead

data PackumentServe
    = -- A @GET@: serve the merged packument body.
      PackumentFull
    | -- HEAD shares GET admission and representation headers.

      PackumentHead

data PackumentServing response = PackumentServing
    { psvMode :: PackumentServe
    , psvReplies :: PackumentReplies response
    , psvDeps :: PackumentDeps
    , psvName :: PackageName
    , psvRequest :: Request
    , psvRuntime :: ServeRuntime
    , psvTicket :: MemoryTicket
    -- ^ The request's account with the memory meter, which its reads and its output pay into.
    , psvPrivateRead :: ChargePart
    -- ^ What the private read paid. It steps down to what the read keeps, and goes once the body exists.
    , psvInputBytes :: Int -> IO ()
    , psvDocumentBytes :: Int -> IO (Either response ())
    , psvRejectionObserver :: Maybe (DocumentRejectionObserver response)
    , psvAbsent :: Maybe response
    , psvResolved :: PackumentServing response -> [Contribution] -> MergePlan -> Handler response
    , psvPublicRead :: ChargePart
    -- ^ The same for the public read, which pays only when this request leads the shared fetch.
    }

servingMetrics :: PackumentServing response -> MetricsPort
servingMetrics = srMetrics . psvRuntime

-- Authenticate before egress. Write after the CPU slot releases, under the memory ticket.
packumentWith ::
    PackumentServe ->
    PackumentReplies response ->
    PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
packumentWith mode replies name request respond = do
    ctx <- ask
    let mount = ctxMount ctx
        deps = bindingPackumentDeps mount
        clientToken = forwardedCredential mount request
        serving ticket = newServing mode replies name request ticket answerPackumentConditional
    if edgeTokenMatches (pdInboundToken deps) clientToken
        then withListingAdmission (ctxRuntime ctx) (\headers -> refuse packumentUnavailable headers shedMessage) (serving >=> (`serveAdmittedPackument` clientToken) >=> evaluate) (liftIO . respond)
        else refuse packumentUnauthorised [] unauthorisedMessage
  where
    refuse reply headers message = liftIO (respond (reply replies headers (mkRefusal Nothing message)))

{- | Observe source sizes and checked rejections, including admission with no surviving contribution.
A caller that supplies an absent answer receives it for a name both origins answered @404@ for.
-}
withObservedAdmittedDocument ::
    PackumentReplies response ->
    MemoryTicket ->
    (Int -> IO ()) ->
    (Int -> IO (Either response ())) ->
    Maybe (DocumentRejectionObserver response) ->
    PackageName ->
    Request ->
    Maybe response ->
    (CachedDoc -> Int -> Map Text Provenance -> Handler response) ->
    Handler response
withObservedAdmittedDocument replies ticket observe checkDocument rejected name request absent consume = do
    mount <- asks ctxMount
    let deps = bindingPackumentDeps mount
        clientToken = forwardedCredential mount request
        refuse reply message = pure (reply replies [] (mkRefusal Nothing message))
    if edgeTokenMatches (pdInboundToken deps) clientToken
        then do
            serving <- newServing PackumentFull replies name request ticket admitted
            serveAdmittedPackument serving{psvAbsent = absent, psvInputBytes = observe, psvDocumentBytes = checkDocument, psvRejectionObserver = rejected} clientToken
        else refuse packumentUnauthorised unauthorisedMessage
  where
    admitted serving sources plan = do
        let basis = outputBasisBytes plan sources
            factor = cfOutputPermille (metadataChargeFactors (pdMetadata (psvDeps serving)))
        output <- liftIO (newChargePart ticket)
        liftIO (chargePart output (scaleCharge factor basis))
        document <- evaluate (renderServedBody (psvDeps serving) sources plan)
        let origins = Map.mapMaybe (`Map.lookup` Map.fromList (zip [0 ..] (map srcProvenance sources))) (mpSurvivors plan)
        result <- consume document (sum (map srcBodyBytes sources)) origins
        liftIO (releaseReads serving >> stepDown output 0)
        pure result

newServing ::
    PackumentServe ->
    PackumentReplies response ->
    PackageName ->
    Request ->
    MemoryTicket ->
    (PackumentServing response -> [Contribution] -> MergePlan -> Handler response) ->
    Handler (PackumentServing response)
newServing mode replies name request ticket resolved = do
    ctx <- ask
    privateRead <- liftIO (newChargePart ticket)
    publicRead <- liftIO (newChargePart ticket)
    pure
        PackumentServing
            { psvMode = mode
            , psvReplies = replies
            , psvDeps = bindingPackumentDeps (ctxMount ctx)
            , psvName = name
            , psvRequest = request
            , psvRuntime = ctxRuntime ctx
            , psvTicket = ticket
            , psvPrivateRead = privateRead
            , psvPublicRead = publicRead
            , psvInputBytes = const pass
            , psvDocumentBytes = const (pure (Right ()))
            , psvRejectionObserver = Nothing
            , psvAbsent = Nothing
            , psvResolved = resolved
            }

serveAdmittedPackument :: PackumentServing response -> Maybe ClientCredential -> Handler response
serveAdmittedPackument serving clientToken = do
    logFM DebugS (ls ("serving packument request for " <> renderPackageName (psvName serving)))
    evalCtx <- liftIO (mkEvalContext (pdNow deps) (pdAdvisoryEtag deps))
    case pdMirrorState deps of
        Just mirror | not (pdFirstParty deps (psvName serving)) -> do
            ((private, checked), public) <-
                concurrently
                    ( do
                        private <- readPrivate serving clientToken
                        checked <-
                            if refusesBeforeMirror private
                                then pure (Right Nothing)
                                else liftIO (validateMirrorProject mirror (psvTicket serving) (psvName serving))
                        pure (private, checked)
                    )
                    (readPublic serving)
            case private of
                OriginAuthorisationFailure code -> privateAccessRefused serving code
                _ -> case checked of
                    Left _ -> mirrorUnavailable serving
                    Right record -> serveMergedPackument serving evalCtx ((mirrorViewDocuments mirror,) <$> record) private public
        _ -> do
            (private, public) <- resolveOrigins serving clientToken
            case private of
                OriginAuthorisationFailure code -> privateAccessRefused serving code
                _ -> serveMergedPackument serving evalCtx Nothing private public
  where
    deps = psvDeps serving

-- An access refusal or a failed private read decides the request before mirror state can.
refusesBeforeMirror :: OriginResult -> Bool
refusesBeforeMirror = \case
    OriginAuthorisationFailure _ -> True
    result -> case originMiss result of
        Just (MissUnresolved _) -> True
        _ -> False

resolveOrigins :: PackumentServing response -> Maybe ClientCredential -> Handler (OriginResult, OriginResult)
resolveOrigins serving clientToken
    | pdFirstParty (psvDeps serving) (psvName serving) = (,OriginAbsent) <$> readPrivate serving clientToken
    | otherwise = concurrently (readPrivate serving clientToken) (readPublic serving)

readPrivate :: PackumentServing response -> Maybe ClientCredential -> Handler OriginResult
readPrivate serving clientToken = fetchPrivateOriginObserved (psvInputBytes serving) (psvDeps serving) (psvRuntime serving) (psvPrivateRead serving) clientToken (psvName serving)

readPublic :: PackumentServing response -> Handler OriginResult
readPublic serving = fetchPublicOriginObserved (psvInputBytes serving) (psvDeps serving) (psvRuntime serving) (psvTicket serving) (psvPublicRead serving) (psvName serving)

privateAccessRefused :: PackumentServing response -> Int -> Handler response
privateAccessRefused serving code = do
    liftIO (mpServeDecision (servingMetrics serving) Metric.Deny)
    pure (packumentPrivateRefused (psvReplies serving) code [] (privateAuthorisationRefusal (pdHelp (psvDeps serving))))

serveMergedPackument ::
    PackumentServing response ->
    EvalContext ->
    Maybe (MirrorDocuments, (StateRevision, ProjectMirrorRecord)) ->
    OriginResult ->
    OriginResult ->
    Handler response
serveMergedPackument serving evalCtx record privResult pubResult
    | OriginBudgetExceeded fault <- privResult = budgetRefused fault
    | Just (MissUnresolved failure) <- originMiss privResult = privateUnresolved serving evalCtx privResult failure
    | OriginBudgetExceeded fault <- pubResult = budgetRefused fault
    | not (pdFirstParty deps name)
    , settledAbsence privResult
    , settledAbsence pubResult
    , Just answer <- unheldAnswer =
        answer
    | otherwise = withCheckedSourceDocuments serving (catMaybes [originManifest privResult, originManifest pubResult]) $ case views of
        Left _ -> mirrorUnavailable serving
        Right (unmanagedManifest, managedManifest, pending) -> do
            let (unmanaged, privateExclusions) = admitTrusted (pdMinTrustedIntegrity deps) unmanagedManifest
                (managed, managedExclusions) = admitTrusted (pdMinTrustedIntegrity deps) managedManifest
                privateSources = catMaybes [unmanaged, managed]
                trustedVersions = Map.unions (map (infoVersions . srcInfo) privateSources)
                -- A public version that a private one claims under another spelling of its release is not offered.
                publicManifest = (\manifest -> manifest{manifestInfo = withoutTrustedReleases trustedVersions (manifestInfo manifest)}) <$> originManifest pubResult
            public <- liftIO (gatePublic (observeVersionRejection <$> psvRejectionObserver serving) (srTracing rt) (servingMetrics serving) deps name evalCtx trustedVersions (Map.keysSet pending) publicManifest)
            let sources = privateSources <> maybeToList (paContribution public)
                required = Map.withoutKeys pending (paBlockedPending public)
                covered = Map.null required || maybe False (publicMirrorCoverage required . srcInfo) (paContribution public)
            if not covered || fmap (infoVersions . manifestInfo) managedManifest /= fmap (infoVersions . srcInfo) managed
                then mirrorUnavailableWithRetry serving (case pubResult of OriginCooling retry -> Just retry; _ -> Nothing)
                else case originMiss privResult of
                    Just MissAbsent | pdFirstParty deps name -> firstPartyMissed serving MissAbsent
                    _ -> case packumentPlan sources trustedVersions (paDeniedEvidence public) of
                        Just plan -> do
                            liftIO $ do
                                paReportRejections public
                                for_ (psvRejectionObserver serving) $ \observer ->
                                    for_ (catMaybes [unmanagedManifest, managedManifest]) $ \manifest ->
                                        reportIntegrityRejections (observeVersionRejection observer) evalCtx (pdMinTrustedIntegrity deps) trustedIntegrityBelowFloor trustedIntegrityMissing (manifestInfo manifest)
                            serveResolved serving sources plan
                        Nothing
                            | Just observer <- psvRejectionObserver serving
                            , settledAbsence privResult
                            , Just manifest <- originManifest pubResult
                            , null (infoInvalidEntries (manifestInfo manifest)) ->
                                liftIO (paReportRejections public >> onCheckedNoSurvivorAdmission observer)
                            | otherwise ->
                                noServeableVersions serving (ctxAdvisoryEtag evalCtx) (paVerdicts public) $
                                    collectDecisions privResult pubResult (privateExclusions <> managedExclusions <> paExclusions public)
  where
    budgetRefused fault = pure (packumentBadGateway (psvReplies serving) [] (mkRefusal (pdHelp deps) ("the cumulative metadata input exceeded its bound: " <> show fault)))
    settledAbsence OriginAbsent = True
    settledAbsence OriginNotFound = True
    settledAbsence _ = False
    -- A listing answers 404 unless a recorded version survives in the mirror. A coordinate lookup takes
    -- its own answer, and a recorded name keeps its managed path there.
    unheldAnswer = case psvAbsent serving of
        Nothing | noRecordedCopy -> Just (liftIO (unheldNameReply (servingMetrics serving) (psvReplies serving) deps))
        Just absent | isNothing record -> Just (pure absent)
        _ -> Nothing
    noRecordedCopy = either (const False) (\(_, managed, _) -> isNothing managed) views
    deps = psvDeps serving
    name = psvName serving
    rt = psvRuntime serving
    views = case record of
        Nothing -> Right (originManifest privResult, Nothing, Map.empty)
        Just (documents, (revision, current)) -> mirrorProjectViews documents revision current (originManifest privResult)

withCheckedSourceDocuments :: PackumentServing response -> [Manifest] -> Handler response -> Handler response
withCheckedSourceDocuments serving manifests action = go manifests
  where
    go [] = action
    go (manifest : rest) = do
        checked <- liftIO (psvDocumentBytes serving (manifestBodyBytes manifest))
        case checked of
            Left refusal -> pure refusal
            Right () -> go rest

mirrorUnavailable :: PackumentServing response -> Handler response
mirrorUnavailable serving = mirrorUnavailableWithRetry serving Nothing

mirrorUnavailableWithRetry :: PackumentServing response -> Maybe RetryAfter -> Handler response
mirrorUnavailableWithRetry serving retry = do
    liftIO (mpServeDecision (servingMetrics serving) Metric.Unavailable)
    pure (packumentUnavailable (psvReplies serving) (retryAfterHeaders retry) (mkRefusal (pdHelp (psvDeps serving)) "managed mirror view is unavailable"))

admitTrusted :: MinTrustedIntegrity -> Maybe Manifest -> (Maybe Contribution, [ServeDecision])
admitTrusted minTrusted = \case
    Nothing -> (Nothing, [])
    Just manifest ->
        let (admissible, integrityRefusals) =
                admitByIntegrity minTrusted trustedIntegrityBelowFloor trustedIntegrityMissing (manifestInfo manifest)
         in if Map.null (infoVersions admissible)
                then (Nothing, integrityRefusals)
                else (Just (Contribution TrustedSource admissible (manifestRaw manifest) (manifestDigest manifest) (manifestBodyBytes manifest)), integrityRefusals)

data PublicAdmission = PublicAdmission
    { paContribution :: Maybe Contribution
    , paExclusions :: [ServeDecision]
    , paVerdicts :: [VersionVerdict]
    , paBlockedPending :: Set Text
    , -- Only named denials release a missing recorded version from coverage.
      paDeniedEvidence :: Map Text PackageDetails
    , -- Integrity-admitted but rule-denied versions contribute alarms, never served entries.
      paReportRejections :: IO ()
    }

gatePublic :: Maybe (EvalContext -> PackageDetails -> Rejection -> IO ()) -> TracingPort -> MetricsPort -> PackumentDeps -> PackageName -> EvalContext -> Map Text PackageDetails -> Set Text -> Maybe Manifest -> IO PublicAdmission
gatePublic rejected tracing metrics deps name ctx trustedVersions pendingVersions = \case
    Nothing -> pure (PublicAdmission Nothing [] [] Set.empty Map.empty pass)
    Just manifest -> spanPackumentGate tracing name $ do
        let (admissible, integrityRefusals) = admitByIntegrity (pdMinIntegrity deps) integrityBelowFloor integrityMissing (manifestInfo manifest)
        (decisions, seconds) <- timedSeconds (decideVersions deps ctx admissible)
        mpRuleEvalDuration metrics (evalTier (pdRules deps)) seconds
        recordEffectfulFailures metrics (Map.elems decisions)
        let plan = filterPlanFromDecisions decisions
            blockedPending = Set.fromList [version | (version, Blocked{}) <- Map.toList (Map.restrictKeys decisions pendingVersions)]
            deniedEvidence = Map.withoutKeys (Map.intersection (infoVersions admissible) trustedVersions) (fpSurvivors plan)
            reportRejections = for_ rejected $ \report -> do
                reportIntegrityRejections report ctx (pdMinIntegrity deps) integrityBelowFloor integrityMissing (manifestInfo manifest)
                for_ (Map.toList (infoVersions admissible)) $ \(version, details) ->
                    for_ (Map.lookup version decisions) $ \decision -> case serveDecisionOf (completeEvidence details) decision of
                        Admit -> pass
                        Reject rejection -> report ctx details rejection
        pure $
            if Set.null (fpSurvivors plan)
                then
                    let verdicts = projectDecisions admissible (fpDecisions plan)
                     in PublicAdmission Nothing (map vvDecision verdicts <> integrityRefusals) verdicts blockedPending deniedEvidence reportRejections
                else
                    PublicAdmission
                        (Just (Contribution GatedSource (restrictToSurvivors (fpSurvivors plan) admissible) (manifestRaw manifest) (manifestDigest manifest) (manifestBodyBytes manifest)))
                        integrityRefusals
                        []
                        blockedPending
                        deniedEvidence
                        reportRejections

reportIntegrityRejections :: (IntegrityFloor floor) => (EvalContext -> PackageDetails -> Rejection -> IO ()) -> EvalContext -> floor -> ServeDecision -> ServeDecision -> PackageInfo -> IO ()
reportIntegrityRejections report ctx floorSpec below missing info =
    for_ (Map.elems (infoVersions info)) $ \details ->
        for_ (pkgArtifacts details) $ \artifact ->
            case classifyDigests floorSpec (artHashes artifact) of
                MeetsFloor -> pass
                BelowFloor -> rejected details artifact below
                NoIntegrity -> rejected details artifact missing
  where
    rejected details artifact = \case
        Admit -> pass
        Reject rejection -> report ctx details{pkgArtifacts = artifact :| []} rejection

decideVersions :: PackumentDeps -> EvalContext -> PackageInfo -> IO (Map Text Decision)
decideVersions deps ctx info = do
    decide <- newEvaluator ctx (pdRules deps)
    traverse (decide . completeEvidence) (infoVersions info)

projectDecisions :: PackageInfo -> [Decision] -> [VersionVerdict]
projectDecisions info =
    zipWith versionVerdict (Map.toList (infoVersions info))
  where
    versionVerdict (ver, details) d = VersionVerdict ver (serveDecisionOf (completeEvidence details) d)

packumentPlan :: [Contribution] -> Map Text PackageDetails -> Map Text PackageDetails -> Maybe MergePlan
packumentPlan sources trustedVersions deniedEvidence = do
    plan <- mergePackuments [(srcProvenance s, Snapshot (srcDigest s) (srcInfo s)) | s <- sources]
    guard (not (Map.null (mpSurvivors plan)))
    pure plan{mpDivergences = mpDivergences plan <> integrityDivergences trustedVersions deniedEvidence}

serveResolved :: PackumentServing response -> [Contribution] -> MergePlan -> Handler response
serveResolved serving sources plan = do
    warnDivergences (servingMetrics serving) (psvName serving) plan
    liftIO (mpServeDecision (servingMetrics serving) Metric.Admit)
    psvResolved serving serving sources plan

answerPackumentConditional :: PackumentServing response -> [Contribution] -> MergePlan -> Handler response
answerPackumentConditional serving sources plan =
    answerMetadataConditional (psvReplies serving) deps (psvTicket serving) (psvRequest serving) (psvMode serving) identityTag $ do
        bytes <- servedBytes serving sources plan identityTag
        releaseReads serving
        pure bytes
  where
    deps = psvDeps serving
    origins = map registryUrlText (maybeToList (pdPrivateBaseUrl deps) <> [pdPublicBaseUrl deps])
    identityTag = packumentETag (pdMountBaseUrl deps) origins (psvName serving) (map fingerprintPiece sources)

-- | Finalise bounded native bytes only after current authorisation, admission and validation.
answerMetadataBytes :: PackumentReplies response -> PackumentDeps -> MemoryTicket -> Request -> ByteString -> Handler response
answerMetadataBytes replies deps ticket request bytes = answerDigestedMetadataBytes replies deps ticket request (hash bytes) bytes

-- | 'answerMetadataBytes' with the caller's SHA-256 of the same bytes, so a caller that also sends it hashes once.
answerDigestedMetadataBytes :: PackumentReplies response -> PackumentDeps -> MemoryTicket -> Request -> Digest SHA256 -> ByteString -> Handler response
answerDigestedMetadataBytes replies deps ticket request digest bytes =
    answerMetadataConditional replies deps ticket request mode (mkStrongETag digest) (pure bytes)
  where
    mode = if isHead (requestMethod request) then PackumentHead else PackumentFull

-- Choose coding before its validator. An input-derived tag can skip assembly and compression on 304.
answerMetadataConditional :: PackumentReplies response -> PackumentDeps -> MemoryTicket -> Request -> PackumentServe -> ETag -> IO ByteString -> Handler response
answerMetadataConditional replies deps ticket request mode identityTag render =
    case chooseCoding (pdMetadataCompression deps) headers of
        NoAcceptableCoding ->
            pure (packumentNotAcceptable replies (represented []) (mkRefusal (pdHelp deps) "the request's Accept-Encoding excludes both gzip and identity"))
        ServeCoding coding -> case evaluateETag headers (codedETag coding identityTag) of
            NotModified matched -> pure (packumentNotModified replies (represented [etagHeader matched]))
            Modified fresh -> do
                bytes <- liftIO render
                listing <- liftIO (codedListing ticket coding bytes)
                pure (packumentResponse replies mode (represented [etagHeader fresh]) listing)
  where
    headers = requestHeaders request
    represented = case pdMetadataCompression deps of
        GzipWhenAccepted -> varyAcceptEncoding
        IdentityOnly -> id

-- Once the body exists nothing reaches the reads' documents, so only the output charge stays while it is sent.
releaseReads :: PackumentServing response -> IO ()
releaseReads serving = traverse_ (`stepDown` 0) [psvPrivateRead serving, psvPublicRead serving]

data CodedListing
    = IdentityListing ByteString
    | GzipListing GzipBody

-- The request's ticket pays for the deflater and its output, on a cache hit as on a miss.
codedListing :: MemoryTicket -> ContentCoding -> ByteString -> IO CodedListing
codedListing ticket = \case
    IdentityCoding -> pure . IdentityListing
    GzipCoding -> fmap GzipListing . gzipMetadata (charge ticket)

-- Distinct private views produce distinct cache keys. A cache miss pays for the merged document and
-- its encoding before either exists, and wraps a render escape, which breaks the totality contract.
servedBytes :: PackumentServing response -> [Contribution] -> MergePlan -> ETag -> IO ByteString
servedBytes serving sources plan etag =
    awaitingFlight (psvTicket serving) flight . resolveAssembled (srMetrics rt) (srMetadataCache rt) key $ do
        charge (servingFlight flight (psvTicket serving)) outputCharge
        markRenderEscape $
            either throwIO (\body -> pure $! LBS.toStrict body) (metadataSerialise (pdMetadata deps) (renderServedBody deps sources plan))
  where
    rt = psvRuntime serving
    deps = psvDeps serving
    key = assembledKey etag
    flight = flightOf key
    outputCharge = scaleCharge (cfOutputPermille (metadataChargeFactors (pdMetadata deps))) (outputBasisBytes plan sources)
    markRenderEscape :: IO ByteString -> IO ByteString
    markRenderEscape render = render `catchAny` (throwIO . RenderEscape)

renderServedBody :: PackumentDeps -> [Contribution] -> MergePlan -> CachedDoc
renderServedBody deps = assembleServedBody (pdMetadata deps) (pdMountBaseUrl deps)

-- A HEAD states the GET's exact length. A gzip GET states it too, because its length is known before it is sent.
packumentResponse :: PackumentReplies response -> PackumentServe -> ResponseHeaders -> CodedListing -> response
packumentResponse replies mode headers = \case
    IdentityListing bytes -> case mode of
        PackumentFull -> packumentOk replies headers (LBS.fromStrict bytes)
        PackumentHead -> packumentOk replies (headers <> [contentLength (BS.length bytes)]) (LBS.fromStrict bytes)
    GzipListing body -> packumentOk replies (headers <> [gzipEncodingHeader, contentLength (gzipLength body)]) (gzipLazyBody body)
  where
    contentLength size = (hContentLength, show size)

noServeableVersions ::
    PackumentServing response ->
    Maybe DbEtag ->
    [VersionVerdict] ->
    [ServeDecision] ->
    Handler response
noServeableVersions serving etag verdicts decisions = do
    liftIO (mpServeDecision metrics (statusServeDecision status))
    liftIO (recordDenials metrics decisions)
    logDenials (psvName serving) etag verdicts
    pure (noSurvivors (psvReplies serving) (pdHelp (psvDeps serving)) status decisions)
  where
    metrics = servingMetrics serving
    status = packumentStatus decisions

-- | Answer an empty listing, finishing the selected refusal text before writing the reply.
noSurvivors :: PackumentReplies response -> Maybe HelpMessage -> PackumentStatus -> [ServeDecision] -> response
noSurvivors replies help status decisions = case status of
    PackumentOk -> packumentInternal replies [] body
    PackumentDenied -> packumentDenied replies [] body
    PackumentForbidden -> packumentForbidden replies [] body
    PackumentUnavailable retry -> packumentUnavailable replies (retryAfterHeaders retry) body
    PackumentBadGateway -> packumentBadGateway replies [] body
    PackumentServerError -> packumentInternal replies [] body
  where
    message = case status of
        PackumentDenied -> packumentDeniedText replies decisions
        PackumentForbidden -> packumentDeniedText replies decisions
        _ -> detailedRejections decisions

    !body = mkRefusal help message

collectDecisions :: OriginResult -> OriginResult -> [ServeDecision] -> [ServeDecision]
collectDecisions privResult pubResult exclusions =
    privateLeg <> publicLeg <> exclusions
  where
    -- A private 404 beside a public document the gate refused in full is settled, so the refusals decide.
    privateLeg = case (privResult, pubResult) of
        (OriginNotFound, OriginResolved _) | packumentStatus exclusions == PackumentDenied -> []
        _ -> privateDecision privResult
    -- With no private origin the public one is the only source, so its outage refuses as a private one does.
    publicLeg = case (privResult, pubResult) of
        (OriginAbsent, OriginUnresolved (ReadUnavailable retry)) -> [neededUpstreamUnavailable retry]
        _ -> publicDecision pubResult

privateDecision :: OriginResult -> [ServeDecision]
privateDecision = \case
    OriginBudgetExceeded _ -> [upstreamInvalidDecision]
    OriginAuthorisationFailure _ -> []
    OriginResolved _ -> []
    -- Unless the public origin settles the name, a private 404 with no public survivor keeps the outage refusal.
    OriginNotFound -> [neededUpstreamUnavailable Nothing]
    OriginUnresolved failure -> [readFailureRefusal neededUpstreamUnavailableMessage failure]
    OriginCooling retry -> [upstreamCooling retry]
    OriginNameMismatch -> [upstreamInvalidDecision]
    -- An unconfigured private leg (a serve-only pure gate) is not an outage:
    -- nothing was needed, so nothing is unavailable.
    OriginAbsent -> []

publicDecision :: OriginResult -> [ServeDecision]
publicDecision = \case
    OriginBudgetExceeded _ -> [upstreamInvalidDecision]
    OriginAuthorisationFailure _ -> []
    OriginNameMismatch -> [upstreamInvalidDecision]
    OriginResolved _ -> []
    OriginNotFound -> []
    OriginUnresolved _ -> []
    OriginCooling retry -> [upstreamCooling retry]
    OriginAbsent -> []

neededUpstreamUnavailable :: Maybe RetryAfter -> ServeDecision
neededUpstreamUnavailable retry = rejectUnavailable (WillResolve retry) neededUpstreamUnavailableMessage

neededUpstreamUnavailableMessage :: Text
neededUpstreamUnavailableMessage = "a needed upstream was unavailable"

upstreamInvalidDecision :: ServeDecision
upstreamInvalidDecision = Reject (Rejection UpstreamInvalid "an upstream returned a packument for a different package")

-- Answer 404 for a name no configured origin holds, recorded as the artifact path records a missing version.
unheldNameReply :: MetricsPort -> PackumentReplies response -> PackumentDeps -> IO response
unheldNameReply metrics replies deps = do
    mpServeDecision metrics (serveDecisionClass decision)
    recordDenials metrics [decision]
    pure (packumentNotFound replies [] (mkRefusal (pdHelp deps) message))
  where
    message
        | isJust (pdPrivateBaseUrl deps) = "neither the private nor the public upstream has the requested package"
        | otherwise = "the public upstream does not have the requested package"
    decision = rejectUnavailable WontResolve message

-- A private origin that was not read refuses every name, so no public namesake stands in for a
-- private package. Its cause alone sets the status, whatever the public leg did.
privateUnresolved :: PackumentServing response -> EvalContext -> OriginResult -> ReadFailure -> Handler response
privateUnresolved serving evalCtx privResult failure
    | pdFirstParty (psvDeps serving) (psvName serving) = firstPartyMissed serving (MissUnresolved failure)
    | otherwise = noServeableVersions serving (ctxAdvisoryEtag evalCtx) [] (privateDecision privResult)

firstPartyMissed :: PackumentServing response -> OriginMiss -> Handler response
firstPartyMissed serving miss = do
    liftIO (mpServeDecision metrics (packumentServeDecision [decision]))
    liftIO (recordDenials metrics [decision])
    pure (firstPartyMissReply (psvReplies serving) (pdHelp (psvDeps serving)) name miss)
  where
    metrics = servingMetrics serving
    name = psvName serving
    decision = firstPartyMissDecision name miss

-- Why a first-party name did not resolve, in the words the client reads.
firstPartyMissMessage :: PackageName -> OriginMiss -> Text
firstPartyMissMessage name = \case
    MissAbsent ->
        "'"
            <> rendered
            <> "' did not resolve from the private upstream, and its namespace is first-party to this deployment, so it is never fetched from the public registry"
    MissUnresolved (ReadUnavailable _) ->
        "the private upstream did not answer for '"
            <> rendered
            <> "', and its namespace is first-party to this deployment, so no public document may stand in for it"
    MissUnresolved ReadInvalid ->
        "the private upstream returned an invalid or unusable reply for '"
            <> rendered
            <> "', and its namespace is first-party to this deployment, so no public document may stand in for it"
  where
    rendered = renderPackageName name

-- | Classify a first-party absence as a policy refusal, and an unread origin by its cause.
firstPartyMissDecision :: PackageName -> OriginMiss -> ServeDecision
firstPartyMissDecision name miss = Reject (Rejection reason (firstPartyMissMessage name miss))
  where
    reason = case miss of
        MissAbsent -> ByPolicy firstPartyRule []
        MissUnresolved (ReadUnavailable retry) -> Unavailable (WillResolve retry)
        MissUnresolved ReadInvalid -> UpstreamInvalid

-- | Render a settled absence as @404@, an unavailable origin as @503@ with its delay, and an invalid reply as @502@.
firstPartyMissReply :: PackumentReplies response -> Maybe HelpMessage -> PackageName -> OriginMiss -> response
firstPartyMissReply replies help name miss = case miss of
    MissAbsent -> packumentNotFound replies [] body
    MissUnresolved (ReadUnavailable retry) -> packumentUnavailable replies (retryAfterHeaders retry) body
    MissUnresolved ReadInvalid -> packumentBadGateway replies [] body
  where
    body = mkRefusal help (firstPartyMissMessage name miss)
