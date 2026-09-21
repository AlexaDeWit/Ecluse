-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Merge trusted private metadata with admitted public versions and serve conditional responses.
Private access refusals stop the request even when the public origin succeeds.
First-party names consult only the private origin.
-}
module Ecluse.Core.Server.Pipeline.Packument (
    PackumentReplies (..),
    packumentAction,
    servePackument,
    headPackument,

    -- * The first-party private miss (exported for its unit spec)
    firstPartyMissDecision,
    firstPartyMissReply,

    -- * The derived validator (exported for its unit spec)
    packumentETag,
) where

import Crypto.Hash (Context, SHA256, hashFinalize, hashInit, hashUpdates)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, intDec, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Katip (Severity (DebugS, InfoS), logFM, ls)
import Network.HTTP.Types (Method, ResponseHeaders, hContentLength)
import Network.Wai (Request, ResponseReceived, requestHeaders)
import UnliftIO (concurrently)
import UnliftIO.Exception (catchAny, throwIO)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Cve.Types (DbEtag)
import Ecluse.Core.Package (
    PackageDetails,
    PackageInfo (infoVersions),
    PackageName,
    renderPackageName,
 )
import Ecluse.Core.Package.Entry (EntryKey (..))
import Ecluse.Core.Package.Filter (filterPlanFromDecisions, fpDecisions, fpSurvivors, restrictToSurvivors)
import Ecluse.Core.Package.Integrity (
    MinTrustedIntegrity,
 )
import Ecluse.Core.Package.Merge (
    MergePlan (mpDivergences, mpSurvivors),
    Provenance (GatedSource, TrustedSource),
    SourceId,
    integrityDivergences,
    mergePackuments,
 )
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataAssemble, metadataSerialise))
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (
    ContentDigest,
    Manifest (manifestDigest, manifestInfo, manifestRaw),
    digestBytes,
 )
import Ecluse.Core.Rules (evalRules)
import Ecluse.Core.Rules.Types (Decision, EvalContext (ctxAdvisoryEtag), completeEvidence, mkEvalContext)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Cache (resolveAssembled)
import Ecluse.Core.Server.Conditional (Conditional (Modified, NotModified), ETag, etagHeader, evaluateETag, mkStrongETag, renderETag)
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
import Ecluse.Core.Server.Fault (RenderEscape (RenderEscape))
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
    Contribution (..),
    OriginMiss (MissAbsent, MissUnresolved),
    OriginResult (..),
    fetchPrivateOrigin,
    fetchPublicOrigin,
    fingerprintPiece,
    originManifest,
    originMiss,
 )
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Response (
    HelpMessage,
    PackumentStatus (PackumentBadGateway, PackumentForbidden, PackumentOk, PackumentServerError, PackumentUnavailable),
    Refusal,
    RejectReason (ByPolicy, Unavailable, UpstreamInvalid),
    Rejection (Rejection, rejectionMessage),
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
    , packumentInternal :: ResponseHeaders -> Refusal -> response
    , packumentBadGateway :: ResponseHeaders -> Refusal -> response
    , packumentUnavailable :: ResponseHeaders -> Refusal -> response
    }

{- | The action a read route names for a package unit. A @HEAD@ takes the head-mode handler,
which runs the identical gating and merge but withholds the body.
-}
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

-- Everything a terminal arm of one packument serve answers through. It is assembled once
-- per request so each arm takes it whole instead of seven positional parameters.
data PackumentServing response = PackumentServing
    { psvMode :: PackumentServe
    , psvReplies :: PackumentReplies response
    , psvDeps :: PackumentDeps
    , psvName :: PackageName
    , psvRequest :: Request
    , psvRespond :: response -> IO ResponseReceived
    , psvRuntime :: ServeRuntime
    }

servingMetrics :: PackumentServing response -> MetricsPort
servingMetrics = srMetrics . psvRuntime

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
        serving =
            PackumentServing
                { psvMode = mode
                , psvReplies = replies
                , psvDeps = bindingPackumentDeps mount
                , psvName = name
                , psvRequest = request
                , psvRespond = respond
                , psvRuntime = ctxRuntime ctx
                }
    serveWithinGuards serving (forwardedCredential mount request)

-- The edge token is compared before any upstream is touched, so an unauthenticated client
-- cannot drive egress. Admission is held only for the gated work.
serveWithinGuards :: PackumentServing response -> Maybe ClientCredential -> Handler ResponseReceived
serveWithinGuards serving clientToken
    | not (edgeTokenMatches (pdInboundToken (psvDeps serving)) clientToken) =
        liftIO (respond (packumentUnauthorised replies [] (mkRefusal Nothing unauthorisedMessage)))
    | otherwise =
        withAdmissionOrShed
            (servingMetrics serving)
            (srAdmission (psvRuntime serving))
            (liftIO (respond (packumentUnavailable replies [shedRetryAfter] (mkRefusal Nothing shedMessage))))
            (serveAdmittedPackument serving clientToken)
            pure
  where
    replies = psvReplies serving
    respond = psvRespond serving

serveAdmittedPackument :: PackumentServing response -> Maybe ClientCredential -> Handler ResponseReceived
serveAdmittedPackument serving clientToken = do
    logFM InfoS (ls ("serving packument request for " <> renderPackageName (psvName serving)))
    evalCtx <- liftIO (mkEvalContext (pdNow deps) (pdAdvisoryEtag deps))
    (privResult, pubResult) <- resolveOrigins deps (psvRuntime serving) clientToken (psvName serving)
    case privResult of
        OriginAuthorisationFailure _ -> privateAccessRefused serving
        _ -> serveMergedPackument serving evalCtx privResult pubResult
  where
    deps = psvDeps serving

{- Resolve the origins this request may read: a first-party name reads the private origin alone
and never the public leg. Every other name reads both concurrently. -}
resolveOrigins :: PackumentDeps -> ServeRuntime -> Maybe ClientCredential -> PackageName -> Handler (OriginResult, OriginResult)
resolveOrigins deps rt clientToken name
    | pdFirstParty deps name = do
        privResult <- fetchPrivateOrigin deps rt clientToken name
        pure (privResult, OriginAbsent)
    | otherwise =
        concurrently
            (fetchPrivateOrigin deps rt clientToken name)
            (fetchPublicOrigin deps rt name)

-- An explicit private refusal stops the request: no public document may stand in for it.
privateAccessRefused :: PackumentServing response -> Handler ResponseReceived
privateAccessRefused serving = do
    liftIO (mpServeDecision (servingMetrics serving) Metric.Deny)
    liftIO . psvRespond serving $
        packumentForbidden (psvReplies serving) [] (privateAuthorisationRefusal (pdHelp (psvDeps serving)))

serveMergedPackument ::
    PackumentServing response ->
    EvalContext ->
    OriginResult ->
    OriginResult ->
    Handler ResponseReceived
serveMergedPackument serving evalCtx privResult pubResult = do
    let (private, privateExclusions) = admitTrusted (pdMinTrustedIntegrity deps) (originManifest privResult)
        trustedVersions = maybe Map.empty (infoVersions . srcInfo) private
    public <- liftIO (gatePublic (srTracing rt) (servingMetrics serving) deps name evalCtx trustedVersions (originManifest pubResult))
    let sources = catMaybes [private, paContribution public]
    case originMiss privResult of
        Just miss | pdFirstParty deps name -> firstPartyMissed serving miss
        _ -> case packumentPlan sources trustedVersions (paDeniedEvidence public) of
            Just plan -> serveResolved serving sources plan
            Nothing ->
                noServeableVersions serving (ctxAdvisoryEtag evalCtx) (paVerdicts public) $
                    collectDecisions privResult pubResult (privateExclusions <> paExclusions public)
  where
    deps = psvDeps serving
    name = psvName serving
    rt = psvRuntime serving

admitTrusted :: MinTrustedIntegrity -> Maybe Manifest -> (Maybe Contribution, [ServeDecision])
admitTrusted minTrusted = \case
    Nothing -> (Nothing, [])
    Just manifest ->
        let (admissible, integrityRefusals) =
                admitByIntegrity minTrusted trustedIntegrityBelowFloor trustedIntegrityMissing (manifestInfo manifest)
         in if Map.null (infoVersions admissible)
                then (Nothing, integrityRefusals)
                else (Just (Contribution TrustedSource admissible (manifestRaw manifest) (manifestDigest manifest)), integrityRefusals)

data PublicAdmission = PublicAdmission
    { paContribution :: Maybe Contribution
    , paExclusions :: [ServeDecision]
    , paVerdicts :: [VersionVerdict]
    , paDeniedEvidence :: Map Text PackageDetails
    -- Integrity-admitted but rule-denied versions contribute alarms, never served entries.
    }

gatePublic :: TracingPort -> MetricsPort -> PackumentDeps -> PackageName -> EvalContext -> Map Text PackageDetails -> Maybe Manifest -> IO PublicAdmission
gatePublic tracing metrics deps name ctx trustedVersions = \case
    Nothing -> pure (PublicAdmission Nothing [] [] Map.empty)
    Just manifest -> spanPackumentGate tracing name $ do
        let (admissible, integrityRefusals) = admitByIntegrity (pdMinIntegrity deps) integrityBelowFloor integrityMissing (manifestInfo manifest)
        (decisions, seconds) <- timedSeconds (decideVersions deps ctx admissible)
        mpRuleEvalDuration metrics (evalTier (pdRules deps)) seconds
        recordEffectfulFailures metrics (Map.elems decisions)
        let plan = filterPlanFromDecisions decisions
            deniedEvidence = Map.withoutKeys (Map.intersection (infoVersions admissible) trustedVersions) (fpSurvivors plan)
        pure $
            if Set.null (fpSurvivors plan)
                then
                    let verdicts = projectDecisions admissible (fpDecisions plan)
                     in PublicAdmission Nothing (map vvDecision verdicts <> integrityRefusals) verdicts deniedEvidence
                else
                    PublicAdmission
                        (Just (Contribution GatedSource (restrictToSurvivors (fpSurvivors plan) admissible) (manifestRaw manifest) (manifestDigest manifest)))
                        integrityRefusals
                        []
                        deniedEvidence

decideVersions :: PackumentDeps -> EvalContext -> PackageInfo -> IO (Map Text Decision)
decideVersions deps ctx info =
    traverse (evalRules ctx (pdRules deps) . completeEvidence) (infoVersions info)

projectDecisions :: PackageInfo -> [Decision] -> [VersionVerdict]
projectDecisions info =
    zipWith versionVerdict (Map.toList (infoVersions info))
  where
    versionVerdict (ver, details) d = VersionVerdict ver (serveDecisionOf details d)

{- The trusted version map is the caller's own, the one the public gate was given, so the merge
and the gate can never disagree about which versions are trusted. -}
packumentPlan :: [Contribution] -> Map Text PackageDetails -> Map Text PackageDetails -> Maybe MergePlan
packumentPlan sources trustedVersions deniedEvidence = do
    plan <- mergePackuments [(srcProvenance s, Snapshot (srcDigest s) (srcInfo s)) | s <- sources]
    guard (not (Map.null (mpSurvivors plan)))
    pure plan{mpDivergences = mpDivergences plan <> integrityDivergences trustedVersions deniedEvidence}

serveResolved :: PackumentServing response -> [Contribution] -> MergePlan -> Handler ResponseReceived
serveResolved serving sources plan = do
    warnDivergences (servingMetrics serving) (psvName serving) plan
    liftIO (mpServeDecision (servingMetrics serving) Metric.Admit)
    answerPackumentConditional serving sources plan

{- Answer the conditional packument request before any assembly. A 304 costs the fetches
and the plan, never the document rebuild, the encode, or an output hash. -}
answerPackumentConditional :: PackumentServing response -> [Contribution] -> MergePlan -> Handler ResponseReceived
answerPackumentConditional serving sources plan = do
    let origins = map registryUrlText (maybeToList (pdPrivateBaseUrl deps) <> [pdPublicBaseUrl deps])
        etag = packumentETag (pdMountBaseUrl deps) origins name (map fingerprintPiece sources)
    case evaluateETag (requestHeaders (psvRequest serving)) etag of
        NotModified matched -> do
            logFM DebugS (ls ("packument unchanged for " <> renderPackageName name <> " (304, unassembled)"))
            liftIO (respond (packumentNotModified replies [etagHeader matched]))
        Modified fresh -> do
            logFM DebugS (ls ("serving packument for " <> renderPackageName name))
            bytes <- liftIO (servedBytes (psvRuntime serving) deps sources plan fresh)
            liftIO (respond (packumentResponse replies (psvMode serving) fresh bytes))
  where
    deps = psvDeps serving
    name = psvName serving
    replies = psvReplies serving
    respond = psvRespond serving

-- | A validator derived from framed inputs so unchanged requests skip assembly. Bump the salt when assembly behaviour changes.
packumentETag :: Text -> [Text] -> PackageName -> [(Provenance, ContentDigest, [(Text, [EntryKey])])] -> ETag
packumentETag mountBaseUrl originBaseUrls name sources =
    mkStrongETag (hashFinalize (hashUpdates (hashInit :: Context SHA256) pieces))
  where
    pieces :: [ByteString]
    pieces = LBS.toChunks (toLazyByteString fingerprint)

    fingerprint :: Builder
    fingerprint =
        "ecluse:packument-etag:v4\0"
            <> foldMap (etagFrame . encodeUtf8) originBaseUrls
            <> "\0"
            <> byteString (encodeUtf8 mountBaseUrl)
            <> "\0"
            <> byteString (encodeUtf8 (renderPackageName name))
            <> "\0"
            <> foldMap etagSourcePiece sources

etagSourcePiece :: (Provenance, ContentDigest, [(Text, [EntryKey])]) -> Builder
etagSourcePiece (provenance, digest, survivors) =
    etagProvenanceTag provenance
        <> byteString (digestBytes digest)
        <> foldMap etagVersionPiece survivors
        <> "\1"

etagVersionPiece :: (Text, [EntryKey]) -> Builder
etagVersionPiece (version, entries) =
    etagFrame (encodeUtf8 version) <> foldMap etagEntryPiece entries <> "\2"

etagEntryPiece :: EntryKey -> Builder
etagEntryPiece = \case
    ArrayEntry index -> "a" <> etagFrame (show index)
    ObjectEntry key -> "o" <> etagFrame (encodeUtf8 key)
    SingletonEntry -> "s"

etagFrame :: ByteString -> Builder
etagFrame bytes = intDec (BS.length bytes) <> ":" <> byteString bytes

etagProvenanceTag :: Provenance -> Builder
etagProvenanceTag = \case
    TrustedSource -> "t\0"
    GatedSource -> "g\0"

-- Distinct private views produce distinct cache keys, preventing reuse across clients.
-- A render escape breaks the totality contract and is wrapped only on a cache miss.
servedBytes :: ServeRuntime -> PackumentDeps -> [Contribution] -> MergePlan -> ETag -> IO ByteString
servedBytes rt deps sources plan etag =
    resolveAssembled (srMetrics rt) (srMetadataCache rt) (renderETag etag) $
        markRenderEscape $
            pure $!
                LBS.toStrict (metadataSerialise (pdMetadata deps) (renderServedBody deps sources plan))
  where
    markRenderEscape :: IO ByteString -> IO ByteString
    markRenderEscape render = render `catchAny` (throwIO . RenderEscape)

renderServedBody :: PackumentDeps -> [Contribution] -> MergePlan -> CachedDoc
renderServedBody deps sources plan =
    metadataAssemble (pdMetadata deps) (pdMountBaseUrl deps) bySource plan (baseDocument sources)
  where
    bySource :: Map SourceId (Snapshot CachedDoc)
    bySource = Map.fromList (zip [0 ..] [Snapshot (srcDigest source) (srcValue source) | source <- sources])

baseDocument :: [Contribution] -> Maybe CachedDoc
baseDocument sources =
    srcValue <$> (find ((== TrustedSource) . srcProvenance) sources <|> listToMaybe sources)

packumentResponse :: PackumentReplies response -> PackumentServe -> ETag -> ByteString -> response
packumentResponse replies mode etag bytes = case mode of
    PackumentFull ->
        packumentOk replies [etagHeader etag] (LBS.fromStrict bytes)
    PackumentHead ->
        packumentOk
            replies
            [etagHeader etag, (hContentLength, show (BS.length bytes))]
            (LBS.fromStrict bytes)

-- The status folds the decision list once and both the metric and the response read that value.
noServeableVersions ::
    PackumentServing response ->
    Maybe DbEtag ->
    [VersionVerdict] ->
    [ServeDecision] ->
    Handler ResponseReceived
noServeableVersions serving etag verdicts decisions = do
    liftIO (mpServeDecision metrics (statusServeDecision status))
    liftIO (recordDenials metrics decisions)
    logDenials (psvName serving) etag verdicts
    liftIO (psvRespond serving (noSurvivors (psvReplies serving) (psvDeps serving) status decisions))
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
    -- An empty reason set (no versions at all) renders a deny-by-default message rather
    -- than an empty body.
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
    privateDecision privResult <> publicMismatch pubResult <> publicExclusions

privateDecision :: OriginResult -> [ServeDecision]
privateDecision = \case
    OriginAuthorisationFailure _ -> []
    OriginResolved _ -> []
    -- A merged name keeps a private 404 and a private outage on one refusal, so a name
    -- neither leg could serve still invites a retry.
    OriginNotFound -> [neededUpstreamUnavailable]
    OriginUnresolved -> [neededUpstreamUnavailable]
    OriginNameMismatch -> [upstreamInvalidDecision]
    -- An unconfigured private leg (a serve-only pure gate) is not an outage:
    -- nothing was needed, so nothing is unavailable.
    OriginAbsent -> []

publicMismatch :: OriginResult -> [ServeDecision]
publicMismatch = \case
    OriginAuthorisationFailure _ -> []
    OriginNameMismatch -> [upstreamInvalidDecision]
    OriginResolved _ -> []
    OriginNotFound -> []
    OriginUnresolved -> []
    OriginAbsent -> []

neededUpstreamUnavailable :: ServeDecision
neededUpstreamUnavailable = Reject (Rejection (Unavailable (WillResolve Nothing)) "a needed upstream was unavailable")

upstreamInvalidDecision :: ServeDecision
upstreamInvalidDecision = Reject (Rejection UpstreamInvalid "an upstream returned a packument for a different package")

firstPartyMissed :: PackumentServing response -> OriginMiss -> Handler ResponseReceived
firstPartyMissed serving miss = do
    liftIO (mpServeDecision metrics (packumentServeDecision [decision]))
    liftIO (recordDenials metrics [decision])
    liftIO . psvRespond serving $
        firstPartyMissReply (psvReplies serving) (pdHelp (psvDeps serving)) name miss
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
        MissAbsent -> ByPolicy firstPartyRule
        MissUnresolved -> Unavailable (WillResolve Nothing)

-- | Render a settled absence as @404@ and an unread origin as @503@ without @Retry-After@.
firstPartyMissReply :: PackumentReplies response -> Maybe HelpMessage -> PackageName -> OriginMiss -> response
firstPartyMissReply replies help name miss = case miss of
    MissAbsent -> packumentNotFound replies [] body
    MissUnresolved -> packumentUnavailable replies [] body
  where
    body = mkRefusal help (firstPartyMissMessage name miss)
