-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Merge authorised private metadata with admitted public versions. First-party names use only private metadata.
module Ecluse.Core.Server.Pipeline.Packument (
    PackumentReplies (..),
    packumentAction,
    servePackument,
    headPackument,

    -- * The first-party private miss (exported for its unit spec)
    firstPartyMissDecision,
    firstPartyMissReply,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Katip (Severity (DebugS), logFM, ls)
import Network.HTTP.Types (Method, ResponseHeaders, hContentLength)
import Network.Wai (Request, ResponseReceived, requestHeaders)
import UnliftIO (concurrently)
import UnliftIO.Exception (catchAny, throwIO)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Cve.Types (DbEtag)
import Ecluse.Core.Mirror.State (ProjectMirrorRecord, StateRevision)
import Ecluse.Core.Package (
    PackageDetails,
    PackageInfo (infoVersions),
    PackageName,
    renderPackageName,
 )
import Ecluse.Core.Package.Filter (filterPlanFromDecisions, fpDecisions, fpSurvivors, restrictToSurvivors)
import Ecluse.Core.Package.Integrity (
    MinTrustedIntegrity,
 )
import Ecluse.Core.Package.Merge (
    MergePlan (mpDivergences, mpSurvivors),
    Provenance (GatedSource, TrustedSource),
    integrityDivergences,
    mergePackuments,
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
import Ecluse.Core.Server.Admission.Meter (MemoryTicket, awaitingFlight, charge, servingFlight)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfOutputPermille))
import Ecluse.Core.Server.Cache (resolveAssembled)
import Ecluse.Core.Server.Cache.Types (assembledKey)
import Ecluse.Core.Server.Conditional (Conditional (Modified, NotModified), ETag, etagHeader, evaluateETag)
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
import Ecluse.Core.Server.MirrorView (mirrorProjectViews, publicMirrorCoverage, validateMirrorProject)
import Ecluse.Core.Server.Pipeline.Diagnostics (warnDivergences)
import Ecluse.Core.Server.Pipeline.Internal (
    VersionVerdict (..),
    admitByIntegrity,
    evalTier,
    logDenials,
    packumentServeDecision,
    recordDenials,
    recordEffectfulFailures,
    statusServeDecision,
 )
import Ecluse.Core.Server.Pipeline.Origin (
    OriginMiss (MissAbsent, MissUnresolved),
    OriginResult (..),
    fetchPrivateOrigin,
    fetchPublicOrigin,
    originManifest,
    originMiss,
 )
import Ecluse.Core.Server.Pipeline.Packument.Representation
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Response (
    HelpMessage,
    PackumentStatus (PackumentBadGateway, PackumentForbidden, PackumentOk, PackumentServerError, PackumentUnavailable),
    Refusal,
    RejectReason (ByPolicy, Unavailable, UpstreamInvalid),
    Rejection (Rejection, rejectionMessage),
    RetryAfter,
    ServeDecision (Admit, Reject),
    Transience (WillResolve),
    mkRefusal,
    packumentStatus,
    serveDecisionOf,
 )
import Ecluse.Core.Server.Route (isHead)
import Ecluse.Core.Snapshot (Snapshot (..))
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (TracingPort, spanPackumentGate)

-- | The route-owned ways the ecosystem-neutral packument pipeline may answer.
data PackumentReplies response = PackumentReplies
    { packumentOk :: ResponseHeaders -> LByteString -> response
    , packumentNotModified :: ResponseHeaders -> response
    , packumentUnauthorised :: ResponseHeaders -> Refusal -> response
    , packumentForbidden :: ResponseHeaders -> Refusal -> response
    , packumentNotFound :: ResponseHeaders -> Refusal -> response
    , packumentNotAcceptable :: ResponseHeaders -> Refusal -> response
    , packumentInternal :: ResponseHeaders -> Refusal -> response
    , packumentBadGateway :: ResponseHeaders -> Refusal -> response
    , packumentUnavailable :: ResponseHeaders -> Refusal -> response
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
    | -- A @HEAD@: serve the identical status and headers (the would-be body's
      -- @Content-Length@ and the own @ETag@) with no body.
      PackumentHead

-- Everything a terminal arm of one admitted packument serve builds its response from, assembled once
-- per request so each arm takes it whole instead of one positional parameter per field.
data PackumentServing response = PackumentServing
    { psvMode :: PackumentServe
    , psvReplies :: PackumentReplies response
    , psvDeps :: PackumentDeps
    , psvName :: PackageName
    , psvRequest :: Request
    , psvRuntime :: ServeRuntime
    , psvTicket :: MemoryTicket
    -- ^ The request's account with the memory meter, which its reads and its output pay into.
    }

servingMetrics :: PackumentServing response -> MetricsPort
servingMetrics = srMetrics . psvRuntime

-- The edge token is compared before any upstream is touched, so an unauthenticated client
-- cannot drive egress. The CPU slot is held for the gated work, and the memory ticket until the
-- response is written.
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
        serving ticket =
            PackumentServing
                { psvMode = mode
                , psvReplies = replies
                , psvDeps = deps
                , psvName = name
                , psvRequest = request
                , psvRuntime = ctxRuntime ctx
                , psvTicket = ticket
                }
    if edgeTokenMatches (pdInboundToken deps) clientToken
        then withListingAdmission (ctxRuntime ctx) (refuse packumentUnavailable [shedRetryAfter] shedMessage) (\ticket -> serveAdmittedPackument (serving ticket) clientToken) (liftIO . respond)
        else refuse packumentUnauthorised [] unauthorisedMessage
  where
    refuse reply headers message = liftIO (respond (reply replies headers (mkRefusal Nothing message)))

serveAdmittedPackument :: PackumentServing response -> Maybe ClientCredential -> Handler response
serveAdmittedPackument serving clientToken = do
    logFM DebugS (ls ("serving packument request for " <> renderPackageName (psvName serving)))
    evalCtx <- liftIO (mkEvalContext (pdNow deps) (pdAdvisoryEtag deps))
    case pdMirrorState deps of
        Just mirror | not (pdFirstParty deps (psvName serving)) -> do
            ((private, checked), public) <-
                concurrently
                    ( do
                        private <- fetchPrivateOrigin deps (psvRuntime serving) (psvTicket serving) clientToken (psvName serving)
                        checked <- case private of
                            OriginAuthorisationFailure _ -> pure (Right Nothing)
                            _ -> liftIO (validateMirrorProject mirror (psvTicket serving) (psvName serving))
                        pure (private, checked)
                    )
                    (fetchPublicOrigin deps (psvRuntime serving) (psvTicket serving) (psvName serving))
            case private of
                OriginAuthorisationFailure _ -> privateAccessRefused serving
                _ -> case checked of
                    Left _ -> mirrorUnavailable serving
                    Right record -> serveMergedPackument serving evalCtx record private public
        _ -> do
            (private, public) <- resolveOrigins deps (psvRuntime serving) (psvTicket serving) clientToken (psvName serving)
            case private of
                OriginAuthorisationFailure _ -> privateAccessRefused serving
                _ -> serveMergedPackument serving evalCtx Nothing private public
  where
    deps = psvDeps serving

resolveOrigins :: PackumentDeps -> ServeRuntime -> MemoryTicket -> Maybe ClientCredential -> PackageName -> Handler (OriginResult, OriginResult)
resolveOrigins deps rt ticket clientToken name
    | pdFirstParty deps name = do
        privResult <- fetchPrivateOrigin deps rt ticket clientToken name
        pure (privResult, OriginAbsent)
    | otherwise =
        concurrently
            (fetchPrivateOrigin deps rt ticket clientToken name)
            (fetchPublicOrigin deps rt ticket name)

-- An explicit private refusal stops the request: no public document may stand in for it.
privateAccessRefused :: PackumentServing response -> Handler response
privateAccessRefused serving = do
    liftIO (mpServeDecision (servingMetrics serving) Metric.Deny)
    pure (packumentForbidden (psvReplies serving) [] (privateAuthorisationRefusal (pdHelp (psvDeps serving))))

serveMergedPackument ::
    PackumentServing response ->
    EvalContext ->
    Maybe (StateRevision, ProjectMirrorRecord) ->
    OriginResult ->
    OriginResult ->
    Handler response
serveMergedPackument serving evalCtx record privResult pubResult =
    case views of
        Left _ -> mirrorUnavailable serving
        Right (unmanagedManifest, managedManifest, pending) -> do
            let (unmanaged, privateExclusions) = admitTrusted (pdMinTrustedIntegrity deps) unmanagedManifest
                (managed, managedExclusions) = admitTrusted (pdMinTrustedIntegrity deps) managedManifest
                privateSources = catMaybes [unmanaged, managed]
                trustedVersions = Map.unions (map (infoVersions . srcInfo) privateSources)
            public <- liftIO (gatePublic (srTracing rt) (servingMetrics serving) deps name evalCtx trustedVersions (Map.keysSet pending) (originManifest pubResult))
            let sources = privateSources <> maybeToList (paContribution public)
                required = Map.withoutKeys pending (paBlockedPending public)
                covered = Map.null required || maybe False (publicMirrorCoverage required . srcInfo) (paContribution public)
            if not covered || fmap (infoVersions . manifestInfo) managedManifest /= fmap (infoVersions . srcInfo) managed
                then mirrorUnavailableWithRetry serving (case pubResult of OriginCooling retry -> Just retry; _ -> Nothing)
                else case originMiss privResult of
                    Just miss | pdFirstParty deps name -> firstPartyMissed serving miss
                    _ -> case packumentPlan sources trustedVersions (paDeniedEvidence public) of
                        Just plan -> serveResolved serving sources plan
                        Nothing ->
                            noServeableVersions serving (ctxAdvisoryEtag evalCtx) (paVerdicts public) $
                                collectDecisions privResult pubResult (privateExclusions <> managedExclusions <> paExclusions public)
  where
    deps = psvDeps serving
    name = psvName serving
    rt = psvRuntime serving
    views = case record of
        Nothing -> Right (originManifest privResult, Nothing, Map.empty)
        Just (revision, current) -> mirrorProjectViews revision current (originManifest privResult)

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
      -- Integrity-admitted but rule-denied versions contribute alarms, never served entries.
    }

gatePublic :: TracingPort -> MetricsPort -> PackumentDeps -> PackageName -> EvalContext -> Map Text PackageDetails -> Set Text -> Maybe Manifest -> IO PublicAdmission
gatePublic tracing metrics deps name ctx trustedVersions pendingVersions = \case
    Nothing -> pure (PublicAdmission Nothing [] [] Set.empty Map.empty)
    Just manifest -> spanPackumentGate tracing name $ do
        let (admissible, integrityRefusals) = admitByIntegrity (pdMinIntegrity deps) integrityBelowFloor integrityMissing (manifestInfo manifest)
        (decisions, seconds) <- timedSeconds (decideVersions deps ctx admissible)
        mpRuleEvalDuration metrics (evalTier (pdRules deps)) seconds
        recordEffectfulFailures metrics (Map.elems decisions)
        let plan = filterPlanFromDecisions decisions
            blockedPending = Set.fromList [version | (version, Blocked{}) <- Map.toList (Map.restrictKeys decisions pendingVersions)]
            deniedEvidence = Map.withoutKeys (Map.intersection (infoVersions admissible) trustedVersions) (fpSurvivors plan)
        pure $
            if Set.null (fpSurvivors plan)
                then
                    let verdicts = projectDecisions admissible (fpDecisions plan)
                     in PublicAdmission Nothing (map vvDecision verdicts <> integrityRefusals) verdicts blockedPending deniedEvidence
                else
                    PublicAdmission
                        (Just (Contribution GatedSource (restrictToSurvivors (fpSurvivors plan) admissible) (manifestRaw manifest) (manifestDigest manifest) (manifestBodyBytes manifest)))
                        integrityRefusals
                        []
                        blockedPending
                        deniedEvidence

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
    answerPackumentConditional serving sources plan

-- The coding is chosen before the validator, so a 304 names the coding's own tag. It skips assembly and compression.
answerPackumentConditional :: PackumentServing response -> [Contribution] -> MergePlan -> Handler response
answerPackumentConditional serving sources plan =
    case chooseCoding (pdMetadataCompression deps) headers of
        NoAcceptableCoding ->
            pure (packumentNotAcceptable replies (represented []) (mkRefusal (pdHelp deps) "the request's Accept-Encoding excludes both gzip and identity"))
        ServeCoding coding -> case evaluateETag headers (codedETag coding identityTag) of
            NotModified matched -> do
                logFM DebugS (ls ("packument unchanged for " <> renderPackageName name <> " (304, unassembled)"))
                pure (packumentNotModified replies (represented [etagHeader matched]))
            Modified fresh -> do
                logFM DebugS (ls ("serving packument for " <> renderPackageName name))
                -- The assembled cache holds the identity bytes under the identity tag, whatever the coding.
                bytes <- liftIO (servedBytes serving sources plan identityTag)
                listing <- liftIO (codedListing (psvTicket serving) coding bytes)
                pure (packumentResponse replies (psvMode serving) (represented [etagHeader fresh]) listing)
  where
    deps = psvDeps serving
    name = psvName serving
    replies = psvReplies serving
    headers = requestHeaders (psvRequest serving)
    origins = map registryUrlText (maybeToList (pdPrivateBaseUrl deps) <> [pdPublicBaseUrl deps])
    identityTag = packumentETag (pdMountBaseUrl deps) origins name (map fingerprintPiece sources)
    -- An identity-only mount negotiates nothing, so its responses do not vary by Accept-Encoding.
    represented = case pdMetadataCompression deps of
        GzipWhenAccepted -> varyAcceptEncoding
        IdentityOnly -> id

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

-- The status folds the decision list once and both the metric and the response read that value.
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
    pure (noSurvivors (psvReplies serving) (psvDeps serving) status decisions)
  where
    metrics = servingMetrics serving
    status = packumentStatus decisions

noSurvivors :: PackumentReplies response -> PackumentDeps -> PackumentStatus -> [ServeDecision] -> response
noSurvivors replies deps status decisions = case status of
    PackumentOk -> packumentInternal replies [] body
    PackumentForbidden -> packumentForbidden replies [] body
    PackumentUnavailable retry -> packumentUnavailable replies (retryAfterHeaders retry) body
    PackumentBadGateway -> packumentBadGateway replies [] body
    PackumentServerError -> packumentInternal replies [] body
  where
    -- No versions still require a deny-by-default response.
    message :: Text
    message = case mapMaybe rejectionText decisions of
        [] -> "no versions are available for this package"
        reasons -> T.intercalate "; " reasons

    body = mkRefusal (pdHelp deps) message

rejectionText :: ServeDecision -> Maybe Text
rejectionText = \case
    Admit -> Nothing
    Reject rej -> Just (rejectionMessage rej)

collectDecisions :: OriginResult -> OriginResult -> [ServeDecision] -> [ServeDecision]
collectDecisions privResult pubResult publicExclusions =
    privateDecision privResult <> publicDecision pubResult <> publicExclusions

privateDecision :: OriginResult -> [ServeDecision]
privateDecision = \case
    OriginAuthorisationFailure _ -> []
    OriginResolved _ -> []
    -- A merged name keeps a private 404 and a private outage on one refusal, so a name
    -- neither leg could serve still invites a retry.
    OriginNotFound -> [neededUpstreamUnavailable]
    OriginUnresolved -> [neededUpstreamUnavailable]
    OriginCooling retry -> [upstreamCooling retry]
    OriginNameMismatch -> [upstreamInvalidDecision]
    -- An unconfigured private leg (a serve-only pure gate) is not an outage:
    -- nothing was needed, so nothing is unavailable.
    OriginAbsent -> []

publicDecision :: OriginResult -> [ServeDecision]
publicDecision = \case
    OriginAuthorisationFailure _ -> []
    OriginNameMismatch -> [upstreamInvalidDecision]
    OriginResolved _ -> []
    OriginNotFound -> []
    OriginUnresolved -> []
    OriginCooling retry -> [upstreamCooling retry]
    OriginAbsent -> []

neededUpstreamUnavailable :: ServeDecision
neededUpstreamUnavailable = Reject (Rejection (Unavailable (WillResolve Nothing)) "a needed upstream was unavailable")

upstreamInvalidDecision :: ServeDecision
upstreamInvalidDecision = Reject (Rejection UpstreamInvalid "an upstream returned a packument for a different package")

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
    MissUnresolved ->
        "the private upstream did not answer for '"
            <> rendered
            <> "', and its namespace is first-party to this deployment, so no public document may stand in for it"
  where
    rendered = renderPackageName name

-- | Classify a first-party absence as a policy refusal and an unread origin as an outage.
firstPartyMissDecision :: PackageName -> OriginMiss -> ServeDecision
firstPartyMissDecision name miss = Reject (Rejection reason (firstPartyMissMessage name miss))
  where
    reason = case miss of
        MissAbsent -> ByPolicy firstPartyRule []
        MissUnresolved -> Unavailable (WillResolve Nothing)

-- | Render a settled absence as @404@ and an unread origin as @503@ without @Retry-After@.
firstPartyMissReply :: PackumentReplies response -> Maybe HelpMessage -> PackageName -> OriginMiss -> response
firstPartyMissReply replies help name miss = case miss of
    MissAbsent -> packumentNotFound replies [] body
    MissUnresolved -> packumentUnavailable replies [] body
  where
    body = mkRefusal help (firstPartyMissMessage name miss)
