-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The serve paths behind the package routes: the packument merge behind
@GET \/{pkg}@.

This is the data-plane handler module for packuments. It composes the slices that decide
/what/ to serve into one action in the 'Ecluse.Core.Server.Context.Handler' reader:

* the origin resolution ("Ecluse.Core.Server.Pipeline.Origin")
* the per-version rules ("Ecluse.Core.Rules")
* the structural filter ("Ecluse.Core.Registry.Npm.Filter")
* the cross-upstream merge ("Ecluse.Core.Package.Merge")
* the metadata cache ("Ecluse.Core.Server.Cache")
* the own-ETag conditional ("Ecluse.Core.Server.Conditional")
* the serve-outcome status ("Ecluse.Core.Server.Response")

It reads its mount's serve dependencies and the request runtime
'Ecluse.Core.Server.Context.ServeRuntime' from the request's
'Ecluse.Core.Server.Context.RequestCtx'.

== Credential authority

This handler applies the default @passthrough@ credential posture (see
@docs\/architecture\/access-model.md@). The invariant that holds under __every__ strategy
is the __public strip__: the client's credential is __stripped before any public-upstream
fetch__, which is always anonymous. Sending an internal token to the public registry
would be a credential disclosure, so the public-upstream fetch carries no token at all.
Under @passthrough@ the handler additionally __forwards the client's own credential
verbatim to the private upstream__, which is the authority for who may read what. It
fetches the two origins concurrently, each with its own credential posture, and nothing
shares a token across the trust split.

@passthrough@ makes the private upstream the __per-client authority__, so its metadata is
__not cached across clients__ here. Every request fetches and parses the private origin
with that client's own credential, so the upstream re-authorises each client itself. Only
the anonymous public origin is cached: one shared document, with no per-client authority
to preserve. A private-origin cache keyed by base URL alone would let one client's entry
serve another client's private document within the TTL, bypassing the upstream's
authorisation. That is a cross-client disclosure. Other strategies make the private origin
shareable by authorising each serve differently, and the metadata cache itself stays
credential-free either way: see @docs\/architecture\/access-model.md@ → "Caching".

== Merge, not fallback

A packument is the /set of available versions/, spread across upstreams. The handler
therefore __merges__ them rather than short-circuiting on a private hit (see
@docs\/architecture\/registry-model.md@ → "Packument merge across upstreams"). Private
versions are trusted and enter unfiltered. The rules and the structural filter gate public
versions first, where the 'FilterPlan''s survivors restrict the typed view. The merge then
combines the two: a private version wins a collision, and an integrity divergence is
flagged. If one upstream is unavailable while the other succeeds, the handler serves the
best-effort union of what resolved. Only when /nothing/ resolves does the request error.

== Decision surface vs served surface

The merge and filter reason over the /typed/ 'PackageInfo'. The document served is the
__raw upstream document__, held opaquely here as a
'Ecluse.Core.Registry.CachedDocument.CachedDoc' and rebuilt from the winning sources.
Every unmodeled wire key therefore survives (see
@docs\/architecture\/registry-model.md@ → "Decision surface vs served surface").

The 'MergePlan' names, for each surviving version, the source that won it. The mount's
injected assembly capability ('Ecluse.Core.Registry.Npm.Filter.assembleMergedDocument' for
npm) builds the served body in one pass, reading the raw documents in the adapter's own
representation. It takes each survivor's object from its winning source and rewrites the
tarball URL under the mount base as it places it. It carries the reconciled @dist-tags@
and @time@ from the plan, and relays every other top-level key from the
precedence-winning document. The typed model is never re-serialised.

The merge /owns/ two fields as a decision: @dist-tags.latest@ and the @time@ instants.
Both are re-rendered from that decision, the times as normalised ISO-8601, so they may
differ byte-for-byte from any single upstream while denoting the same value.
Integrity-bearing fields (@dist.integrity@, and @dist.tarball@ up to the rewrite's own
prefix) are relayed raw and untouched. The served bytes get our __own ETag__, since a
merged or filtered body matches no single upstream's.
-}
module Ecluse.Core.Server.Pipeline.Packument (
    PackumentReplies (..),
    servePackument,
    headPackument,

    -- * The derived validator (exported for its unit spec)
    packumentETag,
) where

import Crypto.Hash (Context, SHA256, hashFinalize, hashInit, hashUpdates)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Katip (Severity (DebugS, InfoS), logFM, ls)
import Network.HTTP.Types (ResponseHeaders, hContentLength)
import Network.Wai (Request, ResponseReceived, requestHeaders)
import UnliftIO (concurrently)
import UnliftIO.Exception (catchAny, throwIO)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Package (
    PackageInfo (infoVersions),
    PackageName,
    renderPackageName,
 )
import Ecluse.Core.Package.Filter (filterPlanFromDecisions, fpDecisions, fpSurvivors, restrictToSurvivors)
import Ecluse.Core.Package.Integrity (
    MinTrustedIntegrity,
 )
import Ecluse.Core.Package.Merge (
    DivergencePolicy,
    MergePlan (mpSurvivors),
    Provenance (GatedSource, TrustedSource),
    SourceId,
    applyDivergencePolicy,
    mergePackuments,
 )
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (
    ContentDigest,
    Manifest (manifestDigest, manifestInfo, manifestRaw),
    digestBytes,
 )
import Ecluse.Core.Rules (evalRules)
import Ecluse.Core.Rules.Types (Decision, EvalContext (ctxAdvisoryEtag), mkEvalContext)
import Ecluse.Core.Server.Admission (withServeAdmission)
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
    OriginResult (..),
    fetchPrivateOrigin,
    fetchPublicOrigin,
    fingerprintPiece,
    originManifest,
 )
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Response (
    PackumentStatus (PackumentBadGateway, PackumentForbidden, PackumentOk, PackumentServerError, PackumentUnavailable),
    RejectReason (Unavailable, UpstreamInvalid),
    Rejection (Rejection, rejectionMessage),
    RetryAfter (RetryAfter),
    ServeDecision (Admit, Reject),
    Transience (WillResolve),
    appendHelp,
    packumentStatus,
    serveDecisionOf,
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (TracingPort, spanPackumentGate)

{- | The route-owned ways the ecosystem-neutral packument pipeline may answer.

The npm adapter supplies these constructors from its closed 'ResponseContract'. The
pipeline receives no WAI responder, so every branch below selects one of these declared
alternatives.
-}
data PackumentReplies response = PackumentReplies
    { packumentOk :: ResponseHeaders -> LByteString -> response
    , packumentNotModified :: ResponseHeaders -> response
    , packumentUnauthorised :: ResponseHeaders -> Text -> response
    , packumentForbidden :: ResponseHeaders -> Text -> response
    , packumentInternal :: ResponseHeaders -> Text -> response
    , packumentBadGateway :: ResponseHeaders -> Text -> response
    , packumentUnavailable :: ResponseHeaders -> Text -> response
    }

{- | Serve a @GET \/{pkg}@ packument request end to end, over the request's
'RequestCtx'.

The mount's 'PackumentDeps' are read from the matched 'MountBinding' in context, not
threaded as arguments. When the mount has no packument-serve dependencies wired, the
route is recognised but not served. It answers a @501@ in the mount's surface, rather than
a fabricated result.

With dependencies wired, the handler validates the edge token, if configured, before it
touches any upstream. It then fetches the private and public upstreams __concurrently__,
with the client's credential forwarded to the private origin and the public origin
anonymous. Each parse failure or unavailable upstream degrades to a missing contribution
rather than an error. Private versions are trusted as-is. The rules and the structural
filter (the 'FilterPlan') gate public versions. The handler merges the surviving sets
('mergePackuments'). It then assembles the 'MergePlan' onto the raw upstream documents,
through the mount's injected assembly capability, to build the served body. It answers
that body against the client's conditional request with our own ETag. When nothing
survives, the status follows the most recoverable cause via 'packumentStatus'.

An origin whose self-reported packument name disagrees with the route is validated out:
dropped as untrusted for this request, and logged. A single misreporting upstream
therefore never denies a package another upstream serves. When that leaves __no__ valid
origin, the request is a @502@ (a responding upstream returned an invalid response),
distinct from a genuine absence. Every refusal, the edge @401@ and the no-survivors
@403@\/@503@\/@502@\/@500@, is selected through the route's injected 'PackumentReplies'.
-}
servePackument ::
    PackumentReplies response ->
    PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
servePackument = packumentWith PackumentFull

{- | Serve a @HEAD \/{pkg}@ packument request. The pipeline and gating are __identical__
to 'servePackument': the same fetch, merge, filter, rule decision, and no-survivors
status. The reply carries the __identical status and headers__ as the @GET@, the would-be
merged body's @Content-Length@ and the own @ETag@ the conditional-request machinery
computes. The route's 'Ecluse.Core.Server.Contract.bodilessContract' suppresses the body,
as HTTP semantics require of a @HEAD@ reply.

A packument body is assembled __locally__, from a metadata fetch plus the cross-upstream
merge. So, unlike the tarball @HEAD@ ('headTarball'), answering it pumps __no artifact
body__ and carries no egress-amplification risk. This is the HTTP-correctness half of the
explicit-@HEAD@ handling, not the DoS lever the tarball path closes. The merged body is
still materialised, to size it and compute its @ETag@, and only the bytes are withheld
from the reply.
-}
headPackument ::
    PackumentReplies response ->
    PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
headPackument = packumentWith PackumentHead

{- The packument serve mode threaded through the handler. A full @GET@ serves the merged
body. A @HEAD@ answers the identical status and headers with the body suppressed. It
changes exactly one thing in the pipeline: whether the @200@ success path
stamps the would-be body's @Content-Length@. A @HEAD@ does, so a client sees the framing a
@GET@ would. A @GET@ leaves that to the serving layer, which frames the body it actually
writes. The route contract withholds the body uniformly, and the gating is byte-for-byte
identical between the two. -}
data PackumentServe
    = -- A @GET@: serve the merged packument body.
      PackumentFull
    | -- A @HEAD@: serve the identical status and headers (the would-be body's
      -- @Content-Length@ and the own @ETag@) with no body.
      PackumentHead

-- Dispatch shared by 'servePackument' and 'headPackument': read the mount's
-- dependencies and serve in the given mode.
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

-- Serve a packument once the mount's dependencies are known: fetch, gate, merge, and
-- answer, under the credential-authority and merge rules the module header states. The
-- mount's ecosystem presentation recovered the credential, scanned out of the headers
-- once at the entry point. The request runtime comes from the request context. The
-- 'PackumentServe' mode reaches the success path so a @HEAD@ stamps the would-be body's
-- @Content-Length@ (the route contract withholds the bytes).
serveWithDeps ::
    PackumentServe ->
    PackumentReplies response ->
    PackumentDeps ->
    Maybe Secret ->
    PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
serveWithDeps mode replies deps clientToken name request respond
    | not (edgeTokenMatches (pdInboundToken deps) clientToken) =
        liftIO (respond (packumentUnauthorised replies [] "authentication required"))
    | otherwise = do
        rt <- asks ctxRuntime
        withServeAdmission (srMetrics rt) (srAdmission rt) (serveAdmittedPackument mode replies deps clientToken name request respond rt) >>= \case
            Just received -> pure received
            Nothing -> liftIO $ do
                mpServeDecision (srMetrics rt) Metric.Unavailable
                respond (packumentUnavailable replies [shedRetryAfter] "server is busy; retry later")

{- Serve a packument once past the admission gate: fetch both origins, then gate and
merge them. The result either answers the conditional serve or takes the no-survivors
terminal. The private-origin fetch forwards the client's credential, which the edge gate
already compared before admission. Its serve context arrives as parameters rather than a
large @where@ closure, so the request flow reads as a flat sequence rather than deep
nesting. -}
serveAdmittedPackument ::
    PackumentServe ->
    PackumentReplies response ->
    PackumentDeps ->
    Maybe Secret ->
    PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    ServeRuntime ->
    Handler ResponseReceived
serveAdmittedPackument mode replies deps clientToken name request respond rt = do
    logFM InfoS (ls ("serving packument request for " <> renderPackageName name))
    let metrics = srMetrics rt
    evalCtx <- liftIO (mkEvalContext (pdNow deps) (pdAdvisoryEtag deps))
    (privResult, pubResult) <-
        concurrently
            (fetchPrivateOrigin deps rt clientToken name)
            (fetchPublicOrigin deps rt name)
    (public, publicExclusions, publicVerdicts) <- liftIO (gatePublic (srTracing rt) metrics deps name evalCtx (originManifest pubResult))
    let (private, privateExclusions) = admitTrusted (pdMinTrustedIntegrity deps) (originManifest privResult)
        sources = catMaybes [private, public]
        -- The terminal for a request that leaves nothing serveable: the merge found no
        -- survivors, or the divergence policy withheld the last of them (fail-closed).
        noServeableVersions = do
            let decisions = collectDecisions privResult pubResult (privateExclusions <> publicExclusions)
            liftIO (mpServeDecision metrics (packumentServeDecision decisions))
            liftIO (recordDenials metrics decisions)
            logDenials name (ctxAdvisoryEtag evalCtx) publicVerdicts
            liftIO (respond (noSurvivors replies deps decisions))
        -- Serve a plan that survived the divergence policy: record the admit, then answer
        -- the conditional request.
        serveResolved served = do
            liftIO (mpServeDecision metrics Metric.Admit)
            answerPackumentConditional mode replies deps name request respond rt sources served
    case packumentPlan sources of
        Nothing -> noServeableVersions
        Just plan -> do
            -- Every policy logs and meters a cross-upstream integrity divergence
            -- (threat #11). Only 'FailClosed' then withholds the contested versions,
            -- which 'survivingPlan' folds into the no-survivors terminal.
            warnDivergences metrics name plan
            maybe noServeableVersions serveResolved (survivingPlan (pdDivergencePolicy deps) plan)

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

{- Apply the __trusted integrity floor__ to a private (trusted) contribution before it
enters the merge. Returns the surviving 'Contribution', if any version survived, and the
per-version exclusions for the dropped ones, which feed the no-survivors status.

This is the trusted-path mirror of 'gatePublic'. A private version whose strongest digest
is below the trusted floor ('pdMinTrustedIntegrity') is dropped from the served listing.
The floor defaults to SHA-256, so a SHA-1-only or hashless private version is not listed.
An operator who loosens the trusted floor admits it again. Trusted versions stay
__unfiltered by the rules__, because the trust split is the caller's, and only the
integrity floor applies. The raw @Value@ is kept whole: the merge replays only surviving
keys onto it, so it never yields a dropped version. Tarball URLs are rewritten at
assembly, uniformly across sources. -}
admitTrusted :: MinTrustedIntegrity -> Maybe Manifest -> (Maybe Contribution, [ServeDecision])
admitTrusted minTrusted = \case
    Nothing -> (Nothing, [])
    Just manifest ->
        let (admissible, integrityRefusals) =
                admitByIntegrity minTrusted trustedIntegrityBelowFloor trustedIntegrityMissing (manifestInfo manifest)
         in if Map.null (infoVersions admissible)
                then (Nothing, integrityRefusals)
                else (Just (Contribution TrustedSource admissible (manifestRaw manifest) (manifestDigest manifest)), integrityRefusals)

{- Gate a public-upstream contribution through the rules engine and the structural
filter. Returns the surviving 'Contribution', if any survived, and the per-version
exclusion outcomes, which the no-survivors status uses when nothing survives anywhere.

A public origin that did not resolve contributes nothing and no exclusions. The
__integrity-floor admission policy__ applies to a resolved origin first. Any version whose
strongest digest misses the configured floor ('pdMinIntegrity') is dropped from the gated
set up front ('admitByIntegrity'). A below-floor public version is therefore never listed,
and it never contributes its fingerprint to the merge. A client cannot fetch it either,
because the artifact gate would refuse it anyway.

The rules engine then decides the remaining versions ('Ecluse.Core.Rules.evalRules' walks
the boot order to the first decisive result). The resulting decisions go to the agnostic
'filterPlanFromDecisions', and the plan is consumed directly. A plan with survivors yields
a gated 'Contribution': the typed view restricted to the survivors, beside the
__unrestricted raw document__. The assembly takes only plan-surviving version objects from
that document. Restricting it here would rebuild a many-version object only for the
assembly to rebuild it again. A plan with no survivors yields no contribution and the
per-version 'ServeDecision's, one projected from each excluded version's decision. A
fail-closed 'Ecluse.Core.Rules.Types.Undecidable' carries its transient or permanent
cause, so the no-survivors status is a @503@\/@500@ rather than a @403@.

The dropped below-floor versions are projected as 'MissingIntegrity' (no digest at all) or
'BelowIntegrityFloor' (a digest, but too weak) refusals, and appended to those exclusions.
A packument with /only/ inadmissible public versions is therefore a @403@ rather than an
empty success. Evaluation is IO, because an effectful rule may do IO, so this gate is IO.
With only pure rules it short-circuits without launching any IO.

The gated contribution's typed 'PackageInfo' is __restricted to the survivors__.
'mergePackuments' treats a 'GatedSource' as the already-filtered set and never re-filters.
Feeding it the unfiltered view would let a denied version reach the merge plan and skew
the reconciled @latest@\/@time@. The raw document needs no matching restriction. Only
versions named by the plan's survivors are ever taken from it at assembly, so a denied
version's object is unreachable by construction.

This gate runs on the public path only. 'admitTrusted' admits the trusted (private)
contribution separately, against the trusted integrity floor. The rules never run on it,
because the trust split is the caller's. -}
gatePublic :: TracingPort -> MetricsPort -> PackumentDeps -> PackageName -> EvalContext -> Maybe Manifest -> IO (Maybe Contribution, [ServeDecision], [VersionVerdict])
gatePublic tracing metrics deps name ctx = \case
    Nothing -> pure (Nothing, [], [])
    Just manifest -> spanPackumentGate tracing name $ do
        let (admissible, integrityRefusals) = admitByIntegrity (pdMinIntegrity deps) integrityBelowFloor integrityMissing (manifestInfo manifest)
        (decisions, seconds) <- timedSeconds (decideVersions deps ctx admissible)
        mpRuleEvalDuration metrics (evalTier (pdRules deps)) seconds
        recordEffectfulFailures metrics (Map.elems decisions)
        let plan = filterPlanFromDecisions decisions admissible
        pure $
            if Set.null (fpSurvivors plan)
                then
                    let verdicts = projectDecisions admissible (fpDecisions plan)
                     in (Nothing, map vvDecision verdicts <> integrityRefusals, verdicts)
                else
                    ( Just (Contribution GatedSource (restrictToSurvivors (fpSurvivors plan) admissible) (manifestRaw manifest) (manifestDigest manifest))
                    , integrityRefusals
                    , []
                    )

{- Decide every version of a public packument against the rules engine, keyed by raw
version string, the map 'filterPlanFromDecisions' consumes. Each version goes through
'Ecluse.Core.Rules.evalRules', so a fail-closed rule that cannot be computed yields a
'Ecluse.Core.Rules.Types.Undecidable' decision. With only pure rules the per-version call
short-circuits without launching any IO. -}
decideVersions :: PackumentDeps -> EvalContext -> PackageInfo -> IO (Map Text Decision)
decideVersions deps ctx info =
    traverse (evalRules ctx (pdRules deps)) (infoVersions info)

{- Project each excluded version's 'Decision' to a 'VersionVerdict', keeping the version
string so a denial's audit line can name it. The plan carries its decisions
('fpDecisions') in @versions@-key order. They zip back onto the same-ordered version keys,
which recovers the package and version each denial is about. -}
projectDecisions :: PackageInfo -> [Decision] -> [VersionVerdict]
projectDecisions info =
    zipWith versionVerdict (Map.toList (infoVersions info))
  where
    versionVerdict (ver, details) d = VersionVerdict ver (serveDecisionOf details d)

-- The fully-assembled served body: the served document ('CachedDoc') to serialise and
-- answer against the conditional request.
newtype ServedBody = ServedBody {servedDoc :: CachedDoc}

{- Merge the resolved sources into the serve plan. 'Nothing' when no version survives the
merge: no source resolved, or every public version was excluded and no private versions
exist. It is split from the rendering so the conditional evaluation can sit
between them. The plan is typed and cheap, and it decides serve against no-survivors. Only
a 'Modified' outcome pays for 'renderServedBody'. -}
packumentPlan :: [Contribution] -> Maybe MergePlan
packumentPlan sources = do
    plan <- mergePackuments [(srcProvenance s, srcInfo s) | s <- sources]
    guard (not (Map.null (mpSurvivors plan)))
    pure plan

{- The plan a request should serve under an operator divergence policy. 'Nothing' when the
policy withheld the last surviving version, which takes the no-survivors terminal.
'Warn' never withholds. 'FailClosed' drops the contested versions and may leave no
survivors. -}
survivingPlan :: DivergencePolicy -> MergePlan -> Maybe MergePlan
survivingPlan policy plan =
    let served = applyDivergencePolicy policy plan
     in if Map.null (mpSurvivors served) then Nothing else Just served

{- | The derived packument validator: a SHA-256 over the serve's __inputs__:

* the mount base URL
* the package name
* per source, in merge order, its provenance, its origin body's digest, and the version
  keys that survived its gate

The served document is a deterministic function of exactly these. The merge plan derives
from the gated typed views, which derive from the origin bytes and the survivor sets. The
assembly then edits the origin documents under the mount base URL. So this tag can never
call a changed document unchanged. It may change when the re-assembled bytes would not
have: a spurious @200@, never a wrong @304@. That is the correct slack for a validator.
Deriving it from inputs is what lets a @304@ skip assembly, encoding, and any output
hashing entirely.

Fields reach the hash with unambiguous framing: the digest is fixed-width, the
variable-length pieces are @NUL@-terminated, and each source block closes with an
@\\SOH@ terminator. No concatenation of adjacent fields can then collide with another
split of the same bytes. The leading salt versions the scheme: bump it when the assembly's
behaviour changes, so pre-change client caches revalidate as modified.
-}
packumentETag :: Text -> PackageName -> [(Provenance, ContentDigest, [Text])] -> ETag
packumentETag mountBaseUrl name sources =
    mkStrongETag (hashFinalize (hashUpdates (hashInit :: Context SHA256) pieces))
  where
    pieces :: [ByteString]
    pieces =
        [ "ecluse:packument-etag:v1\0"
        , encodeUtf8 mountBaseUrl <> "\0"
        , encodeUtf8 (renderPackageName name) <> "\0"
        ]
            <> concatMap sourcePieces sources

    sourcePieces :: (Provenance, ContentDigest, [Text]) -> [ByteString]
    sourcePieces (provenance, digest, survivors) =
        provenanceTag provenance
            : digestBytes digest
            : map (\v -> encodeUtf8 v <> "\0") survivors
                <> ["\1"]

    provenanceTag :: Provenance -> ByteString
    provenanceTag = \case
        TrustedSource -> "t\0"
        GatedSource -> "g\0"

-- The validator is a content address over every serve input, so the assembled, encoded
-- document is memoised under it. A recurring triple of public entry, private content,
-- and plan serves the stored bytes with no assembly and no encode. Concurrent identical
-- renders coalesce onto one leader. A changed input is a changed key, so the
-- store cannot serve stale bytes. A different private view is a different key, so it
-- cannot cross a client boundary.
--
-- The render is total by contract, a pure assembly over already-validated inputs, so a
-- synchronous escape here is an invariant break. The confined 'RenderEscape' marker wraps
-- it on the miss leg only, since a hit never runs this action. The request perimeter can
-- then name the leg it escaped from.
servedBytes :: ServeRuntime -> PackumentDeps -> [Contribution] -> MergePlan -> ETag -> IO ByteString
servedBytes rt deps sources plan etag =
    resolveAssembled (srMetrics rt) (srMetadataCache rt) (renderETag etag) $
        markRenderEscape $
            pure $!
                LBS.toStrict (pdSerialise deps (servedDoc (renderServedBody deps sources plan)))
  where
    markRenderEscape :: IO ByteString -> IO ByteString
    markRenderEscape render = render `catchAny` (throwIO . RenderEscape)

{- Assemble the served packument by replaying the 'MergePlan' onto the sources' raw
documents, through the mount's injected 'pdAssemble'.

The merge decides over the typed 'PackageInfo's. The served body is built from the raw
documents so unmodeled keys survive. The pipeline hands the per-source documents and the
precedence-winning base document ('CachedDoc', opaque here) to 'pdAssemble'. That
capability reads them in the adapter's own representation. It rebuilds @versions@ /
@dist-tags@ / @time@ from the plan onto the base and rewrites each surviving version's
tarball under the mount base. Then it returns the assembled document. It runs only on a
'Modified' outcome, so a @304@ never pays for it. -}
renderServedBody :: PackumentDeps -> [Contribution] -> MergePlan -> ServedBody
renderServedBody deps sources plan =
    ServedBody (pdAssemble deps (pdMountBaseUrl deps) bySource plan (baseDocument sources))
  where
    bySource :: Map SourceId CachedDoc
    bySource = Map.fromList (zip [0 ..] (map srcValue sources))

{- The document whose unmodeled top-level keys are relayed into the served body: the
precedence-winning source's raw document. That is the first trusted source if any, else
the first source. The merge takes its identity from the first input likewise. 'Nothing'
only for an empty source list, which never reaches here. The injected assembly then has no
base document to relay. -}
baseDocument :: [Contribution] -> Maybe CachedDoc
baseDocument sources =
    srcValue <$> (find ((== TrustedSource) . srcProvenance) sources <|> listToMaybe sources)

{- The per-version serve decisions weighed for the no-survivors status: the public-set
exclusions, plus the per-origin signals each upstream contributes.

A private upstream that did not resolve is a needed-but-unavailable transient signal,
since it may resolve on retry. A private outage with no public survivors is therefore a
@503@ rather than a @403@. An origin, private or public, that __answered with a packument
for a different package__ contributes an 'UpstreamInvalid' signal. A request whose only
responding origins were invalid that way renders a @502@, distinct from a genuine absence.
A public upstream that merely did not resolve degrades silently: its absence is not by
itself a needed-upstream outage. -}
collectDecisions :: OriginResult -> OriginResult -> [ServeDecision] -> [ServeDecision]
collectDecisions privResult pubResult publicExclusions =
    privateDecision privResult <> publicMismatch pubResult <> publicExclusions
  where
    privateDecision :: OriginResult -> [ServeDecision]
    privateDecision = \case
        OriginResolved _ -> []
        OriginUnresolved -> [neededUpstreamUnavailable]
        OriginNameMismatch -> [upstreamInvalidDecision]
        -- An unconfigured private leg (a serve-only pure gate) is not an outage:
        -- nothing was needed, so nothing is unavailable.
        OriginAbsent -> []

    publicMismatch :: OriginResult -> [ServeDecision]
    publicMismatch = \case
        OriginNameMismatch -> [upstreamInvalidDecision]
        OriginResolved _ -> []
        OriginUnresolved -> []
        OriginAbsent -> []

    neededUpstreamUnavailable :: ServeDecision
    neededUpstreamUnavailable = Reject (Rejection (Unavailable (WillResolve Nothing)) "a needed upstream was unavailable")

    upstreamInvalidDecision :: ServeDecision
    upstreamInvalidDecision = Reject (Rejection UpstreamInvalid "an upstream returned a packument for a different package")

{- The served packument @200@ over the (possibly memoised) assembled bytes, carrying the
derived 'ETag' the caller already evaluated the conditional against. The caller reaches
here only on 'Modified', never on a match. The bytes come from 'resolveAssembled': strict,
encoded once per content address, and shared across every request whose inputs coincide. A
'PackumentHead' additionally advertises the body's exact @Content-Length@, free off the
memoised bytes, and the route contract then withholds the bytes. -}
packumentResponse :: PackumentReplies response -> PackumentServe -> ETag -> ByteString -> response
packumentResponse replies mode etag bytes = case mode of
    PackumentFull ->
        packumentOk replies [etagHeader etag] (LBS.fromStrict bytes)
    PackumentHead ->
        packumentOk
            replies
            [etagHeader etag, (hContentLength, show (BS.length bytes))]
            (LBS.fromStrict bytes)

{- Render the no-survivors outcome: the status 'packumentStatus' chose over the
exclusions, with a denial body collecting the reasons. Never a @404@, because the package
existed and its versions were withheld. -}
noSurvivors :: PackumentReplies response -> PackumentDeps -> [ServeDecision] -> response
noSurvivors replies deps decisions = case status of
    PackumentOk -> packumentInternal replies [] body
    PackumentForbidden -> packumentForbidden replies [] body
    PackumentUnavailable{} -> packumentUnavailable replies (retryAfterHeader status) body
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

    body = appendHelp (pdHelp deps) message

    rejectionText :: ServeDecision -> Maybe Text
    rejectionText = \case
        Admit -> Nothing
        Reject rej -> Just (rejectionMessage rej)

-- The @Retry-After@ header for a transient no-survivors status that suggested a delay.
-- Nothing for the other statuses.
retryAfterHeader :: PackumentStatus -> ResponseHeaders
retryAfterHeader = \case
    PackumentUnavailable (Just (RetryAfter secs)) -> [("Retry-After", show secs)]
    _ -> []
