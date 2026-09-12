-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Merge trusted private metadata with admitted public versions and serve conditional responses.
Private access refusals stop the request even when the public origin succeeds.
First-party names consult only the private origin.
-}
module Ecluse.Core.Server.Pipeline.Packument (
    PackumentReplies (..),
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
import Network.HTTP.Types (ResponseHeaders, hContentLength)
import Network.Wai (Request, ResponseReceived, requestHeaders)
import UnliftIO (concurrently)
import UnliftIO.Exception (catchAny, throwIO)

import Ecluse.Core.Credential (ClientCredential)
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
import Ecluse.Core.Server.Cache (resolveAssembled)
import Ecluse.Core.Server.Conditional (Conditional (Modified, NotModified), ETag, etagHeader, evaluateETag, mkStrongETag, renderETag)
import Ecluse.Core.Server.Context (
    Handler,
    MountBinding (bindingPackumentDeps),
    PackumentDeps (..),
    ServeRuntime (..),
    ctxMount,
    ctxRuntime,
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

packumentWith ::
    PackumentServe ->
    PackumentReplies response ->
    PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
packumentWith mode replies name request respond = do
    mount <- asks ctxMount
    serveWithDeps mode replies (bindingPackumentDeps mount) (forwardedCredential mount request) name request respond

-- Serve a packument once the mount's dependencies are known. The edge token is compared
-- before any upstream is touched, so an unauthenticated client cannot drive egress.
serveWithDeps ::
    PackumentServe ->
    PackumentReplies response ->
    PackumentDeps ->
    Maybe ClientCredential ->
    PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
serveWithDeps mode replies deps clientToken name request respond
    | not (edgeTokenMatches (pdInboundToken deps) clientToken) =
        liftIO (respond (packumentUnauthorised replies [] (mkRefusal Nothing unauthorisedMessage)))
    | otherwise = do
        rt <- asks ctxRuntime
        withAdmissionOrShed
            (srMetrics rt)
            (srAdmission rt)
            (liftIO (respond (packumentUnavailable replies [shedRetryAfter] (mkRefusal Nothing shedMessage))))
            (serveAdmittedPackument mode replies deps clientToken name request respond rt)
            pure

serveAdmittedPackument ::
    PackumentServe ->
    PackumentReplies response ->
    PackumentDeps ->
    Maybe ClientCredential ->
    PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    ServeRuntime ->
    Handler ResponseReceived
serveAdmittedPackument mode replies deps clientToken name request respond rt = do
    logFM InfoS (ls ("serving packument request for " <> renderPackageName name))
    let metrics = srMetrics rt
    evalCtx <- liftIO (mkEvalContext (pdNow deps) (pdAdvisoryEtag deps))
    (privResult, pubResult) <- resolveOrigins deps rt clientToken name
    case privResult of
        OriginAuthorisationFailure _ -> do
            liftIO (mpServeDecision metrics Metric.Deny)
            liftIO (respond (packumentForbidden replies [] (privateAuthorisationRefusal (pdHelp deps))))
        _ -> do
            let (private, privateExclusions) = admitTrusted (pdMinTrustedIntegrity deps) (originManifest privResult)
                trustedVersions = maybe Map.empty (infoVersions . srcInfo) private
            public <- liftIO (gatePublic (srTracing rt) metrics deps name evalCtx trustedVersions (originManifest pubResult))
            let sources = catMaybes [private, paContribution public]
                noServeableVersions = do
                    let decisions = collectDecisions privResult pubResult (privateExclusions <> paExclusions public)
                    liftIO (mpServeDecision metrics (packumentServeDecision decisions))
                    liftIO (recordDenials metrics decisions)
                    logDenials name (ctxAdvisoryEtag evalCtx) (paVerdicts public)
                    liftIO (respond (noSurvivors replies deps decisions))
                serveResolved served = do
                    liftIO (mpServeDecision metrics Metric.Admit)
                    answerPackumentConditional mode replies deps name request respond rt sources served
                firstPartyMissed miss = do
                    let decision = firstPartyMissDecision name miss
                    liftIO (mpServeDecision metrics (packumentServeDecision [decision]))
                    liftIO (recordDenials metrics [decision])
                    liftIO (respond (firstPartyMissReply replies (pdHelp deps) name miss))
            case originMiss privResult of
                Just miss | pdFirstParty deps name -> firstPartyMissed miss
                _ -> case packumentPlan sources (paDeniedEvidence public) of
                    Nothing -> noServeableVersions
                    Just plan -> do
                        warnDivergences metrics name plan
                        serveResolved plan

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

{- Answer the conditional packument request before any assembly. A 304 costs the fetches
and the plan, never the document rebuild, the encode, or an output hash. -}
answerPackumentConditional ::
    PackumentServe ->
    PackumentReplies response ->
    PackumentDeps ->
    PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    ServeRuntime ->
    [Contribution] ->
    MergePlan ->
    Handler ResponseReceived
answerPackumentConditional mode replies deps name request respond rt sources plan = do
    let etag = packumentETag (pdMountBaseUrl deps) name (map fingerprintPiece sources)
    case evaluateETag (requestHeaders request) etag of
        NotModified matched -> do
            logFM DebugS (ls ("packument unchanged for " <> renderPackageName name <> " (304, unassembled)"))
            liftIO (respond (packumentNotModified replies [etagHeader matched]))
        Modified fresh -> do
            logFM DebugS (ls ("serving packument for " <> renderPackageName name))
            bytes <- liftIO (servedBytes rt deps sources plan fresh)
            liftIO (respond (packumentResponse replies mode fresh bytes))

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

newtype ServedBody = ServedBody {servedDoc :: CachedDoc}

packumentPlan :: [Contribution] -> Map Text PackageDetails -> Maybe MergePlan
packumentPlan sources deniedEvidence = do
    plan <- mergePackuments [(srcProvenance s, Snapshot (srcDigest s) (srcInfo s)) | s <- sources]
    guard (not (Map.null (mpSurvivors plan)))
    let trustedVersions = maybe Map.empty (infoVersions . srcInfo) (find ((== TrustedSource) . srcProvenance) sources)
    pure plan{mpDivergences = mpDivergences plan <> integrityDivergences trustedVersions deniedEvidence}

-- | A validator derived from framed inputs so unchanged requests skip assembly. Bump the salt when assembly behaviour changes.
packumentETag :: Text -> PackageName -> [(Provenance, ContentDigest, [(Text, [EntryKey])])] -> ETag
packumentETag mountBaseUrl name sources =
    mkStrongETag (hashFinalize (hashUpdates (hashInit :: Context SHA256) pieces))
  where
    pieces :: [ByteString]
    pieces = LBS.toChunks (toLazyByteString fingerprint)

    fingerprint :: Builder
    fingerprint =
        "ecluse:packument-etag:v2\0"
            <> byteString (encodeUtf8 mountBaseUrl)
            <> "\0"
            <> byteString (encodeUtf8 (renderPackageName name))
            <> "\0"
            <> foldMap sourcePieces sources

    sourcePieces :: (Provenance, ContentDigest, [(Text, [EntryKey])]) -> Builder
    sourcePieces (provenance, digest, survivors) =
        provenanceTag provenance
            <> byteString (digestBytes digest)
            <> foldMap versionPieces survivors
            <> "\1"

    versionPieces :: (Text, [EntryKey]) -> Builder
    versionPieces (version, entries) =
        frame (encodeUtf8 version) <> foldMap entryPiece entries <> "\2"

    entryPiece :: EntryKey -> Builder
    entryPiece = \case
        ArrayEntry index -> "a" <> frame (show index)
        ObjectEntry key -> "o" <> frame (encodeUtf8 key)
        SingletonEntry -> "s"

    frame :: ByteString -> Builder
    frame bytes = intDec (BS.length bytes) <> ":" <> byteString bytes

    provenanceTag :: Provenance -> Builder
    provenanceTag = \case
        TrustedSource -> "t\0"
        GatedSource -> "g\0"

-- Distinct private views produce distinct cache keys, preventing reuse across clients.
-- A render escape breaks the totality contract and is wrapped only on a cache miss.
servedBytes :: ServeRuntime -> PackumentDeps -> [Contribution] -> MergePlan -> ETag -> IO ByteString
servedBytes rt deps sources plan etag =
    resolveAssembled (srMetrics rt) (srMetadataCache rt) (renderETag etag) $
        markRenderEscape $
            pure $!
                LBS.toStrict (metadataSerialise (pdMetadata deps) (servedDoc (renderServedBody deps sources plan)))
  where
    markRenderEscape :: IO ByteString -> IO ByteString
    markRenderEscape render = render `catchAny` (throwIO . RenderEscape)

renderServedBody :: PackumentDeps -> [Contribution] -> MergePlan -> ServedBody
renderServedBody deps sources plan =
    ServedBody (metadataAssemble (pdMetadata deps) (pdMountBaseUrl deps) bySource plan (baseDocument sources))
  where
    bySource :: Map SourceId (Snapshot CachedDoc)
    bySource = Map.fromList (zip [0 ..] [Snapshot (srcDigest source) (srcValue source) | source <- sources])

baseDocument :: [Contribution] -> Maybe CachedDoc
baseDocument sources =
    srcValue <$> (find ((== TrustedSource) . srcProvenance) sources <|> listToMaybe sources)

collectDecisions :: OriginResult -> OriginResult -> [ServeDecision] -> [ServeDecision]
collectDecisions privResult pubResult publicExclusions =
    privateDecision privResult <> publicMismatch pubResult <> publicExclusions
  where
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

packumentResponse :: PackumentReplies response -> PackumentServe -> ETag -> ByteString -> response
packumentResponse replies mode etag bytes = case mode of
    PackumentFull ->
        packumentOk replies [etagHeader etag] (LBS.fromStrict bytes)
    PackumentHead ->
        packumentOk
            replies
            [etagHeader etag, (hContentLength, show (BS.length bytes))]
            (LBS.fromStrict bytes)

noSurvivors :: PackumentReplies response -> PackumentDeps -> [ServeDecision] -> response
noSurvivors replies deps decisions = case status of
    PackumentOk -> packumentInternal replies [] body
    PackumentForbidden -> packumentForbidden replies [] body
    PackumentUnavailable retry -> packumentUnavailable replies (retryAfterHeaders retry) body
    PackumentBadGateway -> packumentBadGateway replies [] body
    PackumentServerError -> packumentInternal replies [] body
  where
    status :: PackumentStatus
    status = packumentStatus decisions

    -- The collected denial reasons. An empty set (no versions at all) renders a
    -- deny-by-default message rather than an empty body.
    message :: Text
    message = case mapMaybe rejectionText decisions of
        [] -> "no versions are available for this package"
        reasons -> T.intercalate "; " reasons

    body = mkRefusal (pdHelp deps) message

    rejectionText :: ServeDecision -> Maybe Text
    rejectionText = \case
        Admit -> Nothing
        Reject rej -> Just (rejectionMessage rej)
