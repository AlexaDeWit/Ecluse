{- | The serve paths behind the package routes: the packument merge behind
@GET \/{pkg}@.

This is the data-plane handler module for packuments. It composes the
slices that decide /what/ to serve -- the registry client
("Ecluse.Core.Registry.Npm"), the per-version rules ("Ecluse.Core.Rules"), the structural
filter ("Ecluse.Core.Registry.Npm.Filter"), the cross-upstream merge
("Ecluse.Core.Package.Merge"), the metadata cache ("Ecluse.Core.Server.Cache"), the
own-ETag conditional ("Ecluse.Core.Server.Conditional"), and the serve-outcome status
("Ecluse.Core.Server.Response") -- into one action in the
'Ecluse.Core.Server.Context.Handler' reader, reading its mount's serve dependencies and
the request runtime 'Ecluse.Core.Server.Context.ServeRuntime' from the request's
'Ecluse.Core.Server.Context.RequestCtx'.

== Credential authority

This handler implements the default @passthrough@ credential posture (see
@docs\/architecture\/access-model.md@). The invariant that holds under __every__
strategy is the __public strip__: the client's credential is __stripped before any
public-upstream fetch__, which is always anonymous -- sending an internal token to the
public registry would be a credential disclosure, so the public-upstream fetch is built
with no token at all. Under @passthrough@ the client's own credential is additionally
__forwarded verbatim to the private upstream__, which is the authority for who may
read what. The two origins are fetched concurrently, each with its own credential
posture; nothing shares a token across the trust split.

Because @passthrough@ makes the private upstream the __per-client authority__, its
metadata is __not cached across clients__ here: the private origin is fetched and parsed on
every request with that client's own credential, so the upstream re-authorises each
client itself, and only the anonymous public origin is cached (one shared document, no
per-client authority to preserve). Caching the private origin keyed by base URL alone
would let one client's cached entry serve another client's private document within the
TTL, bypassing the upstream's authorisation -- a cross-client disclosure. (Other
strategies make the private origin shareable by authorising each serve differently; the
metadata cache itself stays credential-free regardless -- see
@docs\/architecture\/access-model.md@ → "Caching".)

== Merge, not fallback

A packument is the /set of available versions/, spread across upstreams, so it is
__merged__ rather than short-circuited on a private hit (see
@docs\/architecture\/registry-model.md@ → "Packument merge across upstreams").
Private versions are trusted and enter unfiltered; public versions are gated
through the rules and the structural filter (the 'FilterPlan''s survivors restrict
the typed view) before they enter; the two are combined, private winning a collision and
an integrity divergence flagged. If one upstream
is unavailable while the other succeeds, the best-effort union of what resolved is
served -- only when /nothing/ resolves does the request error.

== Decision surface vs served surface

The merge and filter reason over the /typed/ 'PackageInfo' but the document served
is the __raw upstream JSON__, so every unmodeled wire key survives (see
@docs\/architecture\/registry-model.md@ → "Decision surface vs served surface").
The 'MergePlan' names, for each surviving version, the source that won it; the
served body is assembled in one pass by the mount's assembly hook
('Ecluse.Core.Registry.Npm.Filter.assembleMergedPackument' for npm): each
survivor's object is taken from the /raw @Value@/ of its winning source with its
tarball URL rewritten under the mount base as it is placed, the reconciled
@dist-tags@ and @time@ are carried from the plan, and every other top-level key is
relayed from the precedence-winning document. The typed model is never
re-serialised. The two fields the merge /owns/ as a decision -- @dist-tags.latest@
and the @time@ instants -- are re-rendered from that decision (the times as
normalised ISO-8601), so they may differ byte-for-byte from any single upstream
while denoting the same value; integrity-bearing fields (@dist.integrity@,
@dist.tarball@ up to the rewrite's own prefix) are relayed raw and untouched. The
served bytes get our __own ETag__, since a merged\/filtered body matches no single
upstream's.
-}
module Ecluse.Core.Server.Pipeline.Packument (
    servePackument,
    headPackument,
    withPublicMetadataClient,

    -- * The derived validator (exported for its unit spec)
    packumentETag,
) where

import Crypto.Hash (Context, SHA256, hashFinalize, hashInit, hashUpdates)
import Data.Aeson (Value (Object))
import Data.Aeson qualified as Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Katip (KatipContext, Severity (DebugS, InfoS, WarningS), katipAddContext, logFM, ls, sl)
import Network.HTTP.Client (Manager)
import Network.HTTP.Types (ResponseHeaders, Status, hContentLength, mkStatus, status200)
import Network.Wai (Request, Response, ResponseReceived, requestHeaders)
import UnliftIO (concurrently, withRunInIO)
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Credential (Secret)

import Ecluse.Core.Package (
    HashAlg,
    InvalidEntry (invalidKey, invalidKind, invalidReason, invalidValue),
    InvalidEntryKind (InvalidDistTag, InvalidPublishTime, InvalidVersionManifest),
    PackageInfo (infoVersions),
    PackageName,
    renderHashAlg,
    renderPackageName,
 )
import Ecluse.Core.Package.Filter (filterPlanFromDecisions, fpDecisions, fpSurvivors, restrictToSurvivors)
import Ecluse.Core.Package.Integrity (
    MinTrustedIntegrity,
 )
import Ecluse.Core.Package.Merge (
    Divergence (divLosing, divVersion, divWinning),
    DivergencePolicy,
    IntegrityFingerprint,
    MergePlan (mpDivergences, mpSurvivors),
    Provenance (GatedSource, TrustedSource),
    SourceId,
    applyDivergencePolicy,
    integrityHashes,
    mergePackuments,
 )
import Ecluse.Core.Registry.Metadata (
    ContentDigest,
    Manifest (manifestDigest, manifestInfo, manifestRaw),
    MetadataClient (fetchFullManifest),
    MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable, MetadataUrlUnformable),
    digestBytes,
 )
import Ecluse.Core.Rules (evalRules)
import Ecluse.Core.Rules.Types (Decision, EvalContext (EvalContext, ctxAdvisoryEtag))
import Ecluse.Core.Security (
    LimitError (BodyTooLarge, TooDeeplyNested, TooManyVersions),
    Limits,
 )
import Ecluse.Core.Server.Admission (withServeAdmission)
import Ecluse.Core.Server.Cache (Source (Source), resolveAssembled)
import Ecluse.Core.Server.Conditional (Conditional (Modified, NotModified), ETag, etagHeader, evaluateETag, mkStrongETag, renderETag)
import Ecluse.Core.Server.Context (
    Handler,
    MountBinding (bindingPackumentDeps, bindingRenderer),
    PackumentDeps (..),
    ServeRuntime (..),
    ctxMount,
    ctxRuntime,
 )
import Ecluse.Core.Server.Metadata (ManifestCaching (Cached, Uncached))
import Ecluse.Core.Server.Pipeline.Internal (
    VersionVerdict (..),
    admitByIntegrity,
    evalTier,
    logDecodeFailure,
    logDenials,
    logNameMismatch,
    logUpstreamUnformable,
    packumentServeDecision,
    recordDenials,
    recordEffectfulFailures,
 )
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Response (
    MountRenderer,
    PackumentStatus (PackumentBadGateway, PackumentForbidden, PackumentOk, PackumentServerError, PackumentUnavailable),
    RejectReason (Unavailable, UpstreamInvalid),
    Rejection (Rejection, rejectionMessage),
    RetryAfter (RetryAfter),
    ServeDecision (Admit, Reject),
    Transience (WillResolve),
    packumentStatus,
    packumentStatusCode,
    renderError,
    serveDecisionOf,
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (TracingPort, spanPackumentGate)

{- | Serve a @GET \/{pkg}@ packument request end to end, over the request's
'RequestCtx'.

The mount's 'PackumentDeps' and error renderer are read from the matched
'MountBinding' in context, not threaded as arguments. When the mount has no
packument-serve dependencies wired, the route is recognised but not served -- a
@501@ in the mount's surface -- rather than fabricating a result.

With dependencies wired: the edge token, if configured, is validated before any
upstream is touched. Then the private and public upstreams are fetched
__concurrently__ -- the client's credential forwarded to the private origin, the public
origin anonymous -- each parse failure or unavailable upstream degrading to a missing
contribution rather than an error. Private versions are trusted as-is; public
versions are gated through the rules and the structural filter (the 'FilterPlan');
the surviving sets are merged ('mergePackuments') and the 'MergePlan' assembled
onto the raw upstream @Value@s to build the served body,
which is then answered against the client's conditional request with our own ETag.
When nothing survives, the status follows the most recoverable cause via
'packumentStatus'. An origin whose self-reported packument name disagrees with the
route is validated out -- dropped as untrusted for this request and logged -- so a
single misreporting upstream never denies a package another upstream serves; when
that leaves __no__ valid origin, the request is a @502@ (a responding upstream
returned an invalid response), distinct from a genuine absence. Every refusal -- the
edge @401@ and the no-survivors @403@\/@503@\/@502@\/@500@ -- is rendered through the
mount's 'MountRenderer'.
-}
servePackument ::
    PackageName ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
servePackument = packumentWith PackumentFull

{- | Serve a @HEAD \/{pkg}@ packument request: the __identical pipeline and gating__ as
'servePackument' -- the same fetch, merge, filter, rule decision, and no-survivors
status -- answered with the __identical status and headers__ as the @GET@ (the would-be
merged body's @Content-Length@ and the own @ETag@ the conditional-request machinery
computes), but with the body suppressed ('bodiless'), as HTTP semantics require of a
@HEAD@ reply.

A packument body is assembled __locally__ (a metadata fetch plus the cross-upstream
merge), so -- unlike the tarball @HEAD@ ('headTarball') -- answering it pumps __no
artifact body__ and carries no egress-amplification risk: this is the HTTP-correctness
half of the explicit-@HEAD@ handling, not the DoS lever the tarball path closes. The
merged body is still materialised, to size it and compute its @ETag@; only the bytes
are withheld from the reply.
-}
headPackument ::
    PackageName ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
headPackument name request respond =
    packumentWith PackumentHead name request (respond . bodiless)

{- The packument serve mode threaded through the handler: a full @GET@ that serves the
merged body, or a @HEAD@ that answers the identical status and headers with the body
suppressed. It changes exactly one thing in the pipeline -- whether the @200@ success
path stamps the would-be body's @Content-Length@ (a @HEAD@ does, so a client sees the
framing a @GET@ would; a @GET@ leaves that to the serving layer, which frames the body
it actually writes). The body itself is withheld uniformly by the 'bodiless' wrapper
'headPackument' applies, and the gating is byte-for-byte identical between the two. -}
data PackumentServe
    = -- A @GET@: serve the merged packument body.
      PackumentFull
    | -- A @HEAD@: serve the identical status and headers (the would-be body's
      -- @Content-Length@ and the own @ETag@) with no body.
      PackumentHead

-- Dispatch shared by 'servePackument' and 'headPackument': resolve the mount's
-- dependencies (or the recognised-but-unserved @501@ stub) and serve in the given mode.
packumentWith ::
    PackumentServe ->
    PackageName ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
packumentWith mode name request respond = do
    renderer <- asks (bindingRenderer . ctxMount)
    asks (bindingPackumentDeps . ctxMount) >>= \case
        Nothing -> liftIO (respond (recognisedButUnserved renderer))
        Just deps -> serveWithDeps mode renderer deps name request respond

-- Serve a packument once the mount's dependencies are known: fetch, gate, merge,
-- and answer -- the credential-authority and merge logic the module header
-- describes. The request runtime is read from the request context. The
-- 'PackumentServe' mode is threaded to the success path so a @HEAD@ stamps the
-- would-be body's @Content-Length@ (the 'bodiless' wrapper withholds the bytes).
serveWithDeps ::
    PackumentServe ->
    MountRenderer ->
    PackumentDeps ->
    PackageName ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
serveWithDeps mode renderer deps name request respond
    | not (edgeTokenMatches (pdInboundToken deps) (forwardedToken request)) = liftIO (respond (edgeUnauthorised renderer))
    | otherwise = do
        rt <- asks ctxRuntime
        withServeAdmission (srMetrics rt) (srAdmission rt) (serveAdmittedPackument mode renderer deps name request respond rt) >>= \case
            Just received -> pure received
            Nothing -> liftIO $ do
                mpServeDecision (srMetrics rt) Metric.Unavailable
                respond (serveOverloaded renderer)

{- Serve a packument once past the admission gate: fetch both origins, gate and merge
them, then either answer the conditional serve or take the no-survivors terminal. Hoisted
to the module level, taking its serve context as parameters rather than closing over a
large @where@, so the request flow reads as a flat sequence rather than deep nesting. -}
serveAdmittedPackument ::
    PackumentServe ->
    MountRenderer ->
    PackumentDeps ->
    PackageName ->
    Request ->
    (Response -> IO ResponseReceived) ->
    ServeRuntime ->
    Handler ResponseReceived
serveAdmittedPackument mode renderer deps name request respond rt = do
    logFM InfoS (ls ("serving packument request for " <> renderPackageName name))
    let metrics = srMetrics rt
        -- The client's bearer, scanned out of the headers once; the private-origin fetch
        -- forwards it (the edge gate already compared it before admission).
        clientToken = forwardedToken request
    evalCtx <- liftIO (EvalContext <$> pdNow deps <*> pdAdvisoryEtag deps)
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
            liftIO (respond (noSurvivors renderer deps decisions))
        -- Serve a plan that survived the divergence policy: record the admit, then answer
        -- the conditional request.
        serveResolved served = do
            liftIO (mpServeDecision metrics Metric.Admit)
            answerPackumentConditional mode deps name request respond rt sources served
    case packumentPlan sources of
        Nothing -> noServeableVersions
        Just plan -> do
            -- A cross-upstream integrity divergence (threat #11) is logged and metered
            -- under every policy; only 'FailClosed' then withholds the contested
            -- version(s), which 'survivingPlan' folds into the no-survivors terminal.
            warnDivergences metrics name plan
            maybe noServeableVersions serveResolved (survivingPlan (pdDivergencePolicy deps) plan)

{- Answer the conditional packument request BEFORE any assembly: a 304 costs the fetches
and the plan, never the document rebuild, encode, or output hash. Hoisted to the module
level, its serve context passed in, rather than nested inside 'serveWithDeps'. -}
answerPackumentConditional ::
    PackumentServe ->
    PackumentDeps ->
    PackageName ->
    Request ->
    (Response -> IO ResponseReceived) ->
    ServeRuntime ->
    [Contribution] ->
    MergePlan ->
    Handler ResponseReceived
answerPackumentConditional mode deps name request respond rt sources plan = do
    let etag = packumentETag (pdMountBaseUrl deps) name (map fingerprintPiece sources)
    case evaluateETag (requestHeaders request) etag of
        NotModified matched -> do
            logFM DebugS (ls ("packument unchanged for " <> renderPackageName name <> " (304, unassembled)"))
            liftIO (respond (notModifiedResponse matched))
        Modified fresh -> do
            logFM DebugS (ls ("serving packument for " <> renderPackageName name))
            bytes <- liftIO (servedBytes rt deps sources plan fresh)
            liftIO (respond (packumentResponse mode fresh bytes))

-- A recognised-but-unserved packument route: a @501@ in the mount's surface, for a
-- mount whose packument-serve dependencies are not wired. The decision to serve or
-- stub is the handler's, so the routing layer need not re-derive it.

{- A successfully resolved upstream contribution: the parsed packument used to
decide, alongside the raw @Value@ that is edited in place to serve, and the
origin body's 'ContentDigest' for the derived validator. Pairing the views is
the decision-surface\/served-surface contract -- every stage carries the raw
@Value@ next to the typed view so losslessness survives the pipeline. -}
data Contribution = Contribution
    { srcProvenance :: Provenance
    , srcInfo :: PackageInfo
    , srcValue :: Value
    , srcDigest :: ContentDigest
    }

{- One source's slice of the derived validator: its provenance, its origin body's
digest, and the version keys that actually survived its gate -- together with the
mount base URL and package name, exactly the inputs the assembled document is a
deterministic function of (the plan itself derives from these). -}
fingerprintPiece :: Contribution -> (Provenance, ContentDigest, [Text])
fingerprintPiece s = (srcProvenance s, srcDigest s, Map.keys (infoVersions (srcInfo s)))

{- The outcome of resolving one upstream origin for a packument, beyond the
plain "resolved or not" the merge consumes: a name mismatch is kept distinct from a
plain non-resolution so the no-valid-origin terminal status can render a @502@ (a
responding upstream returned a packument for a different package) apart from a
transient outage or a genuine absence. -}
data OriginResult
    = -- | A packument that decoded and whose self-reported name matched the request.
      OriginResolved Manifest
    | {- | The origin answered, but its packument self-reported a name for a /different/
      package -- dropped as untrusted for this request, and a @502@ signal when no
      origin is valid.
      -}
      OriginNameMismatch
    | {- | The origin did not yield a usable packument -- unreachable, undecodable, or a
      genuine absence -- the existing degrade (no contribution).
      -}
      OriginUnresolved

-- The resolved manifest an origin contributed, if any. A name mismatch and a plain
-- non-resolution alike contribute no document to the merge.
originManifest :: OriginResult -> Maybe Manifest
originManifest = \case
    OriginResolved manifest -> Just manifest
    OriginNameMismatch -> Nothing
    OriginUnresolved -> Nothing

{- Classify a per-origin full-manifest fetch into an 'OriginResult'. A genuine
transport\/async fault (the 'tryAny' channel) and a 'MetadataError' degrade alike yield
no document, but a 'MetadataNameMismatch' is kept distinct as 'OriginNameMismatch' so the
no-valid-origin terminal status can render a @502@ (a responding upstream answered for a
different package) apart from a transient outage, an undecodable body, or a bound breach. -}
originResultOf :: Either SomeException (Either MetadataError Manifest) -> OriginResult
originResultOf = \case
    Left _ -> OriginUnresolved
    Right (Left (MetadataNameMismatch _)) -> OriginNameMismatch
    Right (Left _) -> OriginUnresolved
    Right (Right manifest) -> OriginResolved manifest

{- Resolve the private (trusted) upstream origin, __uncached__, forwarding the client's
own credential (the default @passthrough@ posture). Returns its coherent (parsed
packument, raw @Value@) pair -- or 'Nothing' when the origin is unavailable or its body
does not parse. A failed fetch is a degraded contribution, not an error: the merge
serves the best-effort union of whatever resolved (partial-upstream availability).

Under @passthrough@ the private upstream is the per-client authority for who may read
what, so its metadata is __not__ shared across clients: it is fetched and parsed on
__every__ request with that client's own forwarded token, so the upstream re-authorises
each client itself. Caching it would key on the base URL alone (no credential
dimension), so within the TTL one client's cache hit would skip the fetch and serve
another client's private document -- bypassing the upstream's authorisation. The private
origin is therefore deliberately kept out of the metadata cache; only the anonymous
public origin is cached. (How a non-@passthrough@ strategy can instead share the private
origin safely is the serve-time authorisation it adds -- see
@docs\/architecture\/access-model.md@.) -}
fetchPrivateOrigin :: PackumentDeps -> ServeRuntime -> Maybe Secret -> PackageName -> Handler OriginResult
fetchPrivateOrigin deps rt token name = do
    logFM DebugS (ls ("fetching private origin for " <> renderPackageName name))
    resolved <-
        tryAny $
            withMetadataClient rt deps Metric.Private Uncached (pdLimits deps) (srPrivateManager rt) (pdPrivateBaseUrl deps) token $ \client ->
                fetchFullManifest client name
    pure (originResultOf resolved)

{- Resolve the public (gated, anonymous) upstream origin through the metadata cache,
keyed by the origin's base URL as its 'Source', returning its coherent (parsed
packument, raw @Value@) pair -- or 'Nothing' when the origin is unavailable or its body
does not parse. A failed fetch is a degraded contribution, not an error.

The public origin is anonymous (no client credential), so a single cached entry serves
every client without crossing any trust boundary -- there is no per-client authority
to preserve, only one shared anonymous document. A hit returns the cached pair
(typed view and the exact bytes it was decoded from), so the served document and the
decision over it stay coherent across the TTL, and concurrent resolutions of a
popular package __collapse to one upstream call__ -- as does the tarball gate's
single-version read, which shares this very cache entry ('fetchVersionMetadata'). -}
fetchPublicOrigin :: PackumentDeps -> ServeRuntime -> PackageName -> Handler OriginResult
fetchPublicOrigin deps rt name = do
    logFM DebugS (ls ("fetching public origin for " <> renderPackageName name))
    resolved <-
        tryAny $
            withPublicMetadataClient rt deps (pdPublicBaseUrl deps) $ \client ->
                fetchFullManifest client name
    pure (originResultOf resolved)

{- Construct a per-request read handle for one origin and run an action over it, with the
ambient @katip@ context captured into the handle's failure log.

The handle's operations run the fetch in plain 'IO' (the public origin's cache leader runs
under @mask@); 'withRunInIO' discharges the 'Handler' logs to 'IO' while capturing the
request's trace-correlated context, so a breach\/decode\/name-mismatch warning, or the
dropped-entry warning a successful-but-degraded projection emits ('logInvalidEntries'),
still rides that context. The npm origin's credential posture, manager, base URL, and
response budget are the per-fetch 'NpmClientConfig'; the 'ManifestCaching' decides whether
the origin resolves through the shared metadata cache.

Every response bound (security.md invariant 4) is enforced inside the handle's fetch
against the mount's 'Limits' budget -- a body-size, nesting-depth, or version-count breach
becomes a 'MetadataBoundExceeded', logged once at a 'WarningS' (naming the package and the
ceiling crossed) before it degrades the contribution fail-closed, so an operator can tell a
hostile\/oversized upstream from an ordinary parse failure. -}
withMetadataClient ::
    ServeRuntime ->
    PackumentDeps ->
    Metric.Upstream ->
    ManifestCaching ->
    Limits ->
    Manager ->
    Text ->
    Maybe Secret ->
    (MetadataClient -> IO a) ->
    Handler a
withMetadataClient rt deps upstream caching limits manager baseUrl token k =
    withRunInIO $ \runInIO ->
        k $
            pdNewMetadataClient
                deps
                (srTracing rt)
                (srMetrics rt)
                upstream
                caching
                (\nm err -> runInIO (logMetadataFailure nm baseUrl err))
                (\nm entries -> runInIO (logInvalidEntries nm baseUrl entries))
                (\nm -> runInIO (logFM DebugS (ls ("fetching packument from origin for " <> renderPackageName nm))))
                limits
                manager
                baseUrl
                token

{- The public origin's read handle: anonymous (no token), resolved through the shared
metadata cache under the base URL's 'Source'. Both the packument fetch ('fetchFullManifest')
and the tarball gate's single-version read ('fetchVersionMetadata') go through this handle,
so they share one cache entry. -}
withPublicMetadataClient :: ServeRuntime -> PackumentDeps -> Text -> (MetadataClient -> IO a) -> Handler a
withPublicMetadataClient rt deps baseUrl =
    withMetadataClient rt deps Metric.Public (Cached (srMetadataCache rt) (Source baseUrl)) (pdLimits deps) (srPublicManager rt) baseUrl Nothing

{- Log a per-origin metadata-fetch failure at the point and severity it has always been
logged: a response-bound breach names the ceiling crossed ('logBreach'); an undecodable
body is the silent-guard decode log ('logDecodeFailure'); a self-reported /different/ name
is the name-mismatch log ('logNameMismatch'); an unformable configured base URL is the
config-fault log ('logUpstreamUnformable'). Invoked once per real fetch, inside the
single-flight leader, in the request's context. -}
logMetadataFailure :: PackageName -> Text -> MetadataError -> Handler ()
logMetadataFailure name baseUrl = \case
    MetadataBoundExceeded err -> logBreach name err
    MetadataUndecodable -> logDecodeFailure name
    MetadataNameMismatch reported -> logNameMismatch name baseUrl reported
    MetadataUrlUnformable urlErr -> logUpstreamUnformable name baseUrl urlErr

{- Log a response-bound breach at 'WarningS' before the contribution is degraded
fail-closed, so an operator can distinguish a bound breach (a hostile\/oversized
upstream, or a too-tight cap) from an ordinary parse failure or upstream outage. The
structured payload names the package, which @bound@ was crossed, and the observed
value against its @cap@ -- the high-cardinality identifiers that belong on the log
line, not a metric label. Emitted through the ambient @katip@ context (the request's,
so the line carries its trace-correlation @dd@), under the @ecluse@ namespace the rest
of the stream uses. -}
logBreach :: (KatipContext m) => PackageName -> LimitError -> m ()
logBreach name err =
    katipAddContext payload $
        logFM WarningS (ls message)
  where
    -- The package the refused document was for, plus the breach detail, as the
    -- structured @data@ object on the line.
    payload =
        sl "module" pipelineModule
            <> sl "package" (renderPackageName name)
            <> sl "bound" boundName
            <> sl "observed" observed
            <> sl "cap" cap

    -- A human-readable one-line summary; the structured fields carry the detail.
    message :: Text
    message = "refused an upstream metadata document: it exceeded the " <> boundName <> " response bound (observed " <> observed <> ", cap " <> cap <> ")"

    -- Which ceiling, the observed value, and the cap -- pulled from the typed error so
    -- the three are always consistent with what was enforced.
    boundName :: Text
    observed :: Text
    cap :: Text
    (boundName, observed, cap) = case err of
        BodyTooLarge c -> ("body-size", "over " <> show c <> " bytes", show c <> " bytes")
        TooManyVersions seen c -> ("version-count", show seen, show c)
        TooDeeplyNested c -> ("nesting-depth", "over " <> show c <> " levels", show c <> " levels")

{- Log a cross-upstream integrity divergence (threat #11) at 'WarningS' and meter it. A
public copy contradicts the trusted one on a shared integrity algorithm for a shared
version; the trusted copy still won the merge (and is served, or withheld under
'FailClosed'), so this is the supply-chain signal the operator alarms on, never a silent
reconciliation. The structured payload names the package and the contradicting versions;
the @ecluse.registry.merge.divergence@ counter is incremented once per contradicting
version. Nothing is logged or metered for a clean merge. -}
warnDivergences :: (KatipContext m) => MetricsPort -> PackageName -> MergePlan -> m ()
warnDivergences metrics name plan =
    case toList (mpDivergences plan) of
        [] -> pass
        divs -> do
            liftIO (for_ divs (const (mpMergeDivergence metrics)))
            katipAddContext (payload divs) $ logFM WarningS (ls (message divs))
  where
    payload divs =
        sl "module" pipelineModule
            <> sl "package" (renderPackageName name)
            <> sl "versions" (T.intercalate "," (map divVersion divs))
    message divs =
        "cross-upstream integrity divergence: the trusted copy of "
            <> renderPackageName name
            <> " is served, but a public copy contradicts it on a shared integrity algorithm for "
            <> show (length divs)
            <> " version(s): "
            <> T.intercalate "; " (map renderDivergence divs)

-- One divergence rendered for the log line: the version key and the contradicting trusted
-- vs public integrity fingerprints, read back via 'integrityHashes'.
renderDivergence :: Divergence -> Text
renderDivergence d =
    divVersion d
        <> " (trusted "
        <> renderFingerprint (divWinning d)
        <> " vs public "
        <> renderFingerprint (divLosing d)
        <> ")"

renderFingerprint :: IntegrityFingerprint -> Text
renderFingerprint fp = "{" <> T.intercalate ", " (map renderHash (integrityHashes fp)) <> "}"

renderHash :: (Maybe HashAlg, Text) -> Text
renderHash (alg, body) = maybe "none" renderHashAlg alg <> ":" <> body

{- Log the malformed packument entries an upstream served that the projection dropped
rather than failing the whole document on, at 'WarningS', so an operator can see that an
upstream served a malformed entry, which kind (a version manifest, a dist-tag, or a
per-version publish time), and __the raw value it sent__. The structured payload names the
package, the per-kind drop counts, and a bounded sample of the dropped entries each
rendering its raw 'Aeson.Value' (truncated if large, and capped to 'maxRenderedDrops'
entries so a flood of drops cannot bloat the line). The dropped versions are still served
minus those entries (graceful degradation), so this is an observability signal, not a
refusal. Emitted once per real fetch (inside the cache leader, so a coalesced follower
never re-logs) through the request's @katip@ context. The caller guards on a non-empty
list, so this never logs for a clean document. -}
logInvalidEntries :: (KatipContext m) => PackageName -> Text -> [InvalidEntry] -> m ()
logInvalidEntries name baseUrl entries =
    katipAddContext payload $
        logFM WarningS (ls message)
  where
    payload =
        sl "module" pipelineModule
            <> sl "package" (renderPackageName name)
            <> sl "upstream" baseUrl
            <> sl "droppedVersionManifests" manifests
            <> sl "droppedDistTags" distTags
            <> sl "droppedPublishTimes" publishTimes
            <> sl "droppedEntries" (map renderDroppedEntry (take maxRenderedDrops entries))

    (manifests, distTags, publishTimes, entriesLen) =
        foldl'
            accumulateDropCounts
            (0 :: Int, 0 :: Int, 0 :: Int, 0 :: Int)
            entries

    accumulateDropCounts (m, d, p, l) e =
        case invalidKind e of
            InvalidVersionManifest -> (m + 1, d, p, l + 1)
            InvalidDistTag -> (m, d + 1, p, l + 1)
            InvalidPublishTime -> (m, d, p + 1, l + 1)

    message :: Text
    message =
        "dropped " <> show entriesLen <> " malformed entr" <> plural <> " from an upstream packument (the rest is served)"
    plural = if entriesLen == 1 then "y" else "ies"

-- One dropped entry rendered for the operator: its kind, key, reason, and the raw
-- value the upstream sent (truncated), so the actual offending bytes are visible.
renderDroppedEntry :: InvalidEntry -> Text
renderDroppedEntry e =
    renderInvalidKind (invalidKind e)
        <> " "
        <> invalidKey e
        <> " = "
        <> truncatedValue (invalidValue e)
        <> " ("
        <> invalidReason e
        <> ")"

renderInvalidKind :: InvalidEntryKind -> Text
renderInvalidKind = \case
    InvalidVersionManifest -> "version-manifest"
    InvalidDistTag -> "dist-tag"
    InvalidPublishTime -> "publish-time"

-- The raw value as compact JSON, truncated to 'maxRenderedValueChars' (only that many
-- characters are ever forced, so a huge value never balloons the log line).
truncatedValue :: Value -> Text
truncatedValue v =
    let rendered = TL.toStrict (TL.take (fromIntegral maxRenderedValueChars + 1) (encodeToLazyText v))
     in if T.length rendered > maxRenderedValueChars
            then T.take maxRenderedValueChars rendered <> "…"
            else rendered

-- How many dropped entries the drop-tracking log renders in full, and how many characters
-- of each raw value, so an unbounded flood of malformed entries (or one huge value) cannot
-- bloat a single log line. The per-kind counts in the payload still report the full totals.
maxRenderedDrops :: Int
maxRenderedDrops = 20

maxRenderedValueChars :: Int
maxRenderedValueChars = 200

-- The @module@ tag this module's breach log carries -- the operator-facing log filter
-- key, held stable as the current value rather than the source module path, so an
-- operator's saved filter keeps matching across the move into ecluse-core (the only
-- change to these lines is the trace-correlation @dd@ the ambient context adds). The
-- decode-failure log lives in "Ecluse.Core.Server.Pipeline.Internal", tagged likewise.
pipelineModule :: Text
pipelineModule = "Ecluse.Server.Pipeline"

{- Apply the __trusted integrity floor__ to a private (trusted) contribution before it
enters the merge, returning the surviving 'Contribution' (if any version survived) and the
per-version exclusions for the dropped ones (for the no-survivors status). This is the
trusted-path mirror of 'gatePublic': a private version whose strongest digest is below the
trusted floor ('pdMinTrustedIntegrity') is dropped from the served listing, so by default
(floor = SHA-256) a SHA-1-only or hashless private version is not listed, while an operator
who loosens the trusted floor admits it again. Trusted versions stay __unfiltered by the
rules__ (the trust split is the caller's); only the integrity floor applies. The raw
@Value@ is kept whole -- the merge replays only surviving keys onto it, so a dropped version
is never taken from it; tarball URLs are rewritten at assembly, uniformly across sources. -}
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
filter, returning the surviving 'Contribution' (if any survived) and the per-version
exclusion outcomes (for the no-survivors status when nothing survives anywhere).

A public origin that did not resolve contributes nothing and no exclusions. A resolved
origin first has the __integrity-floor admission policy__ applied: any version whose
strongest digest does not meet the configured floor ('pdMinIntegrity') is dropped from
the gated set up front ('admitByIntegrity'), so a below-floor public version is never
listed (a client cannot fetch it -- the artifact gate would refuse it anyway) and never
contributes its fingerprint to the merge. The remaining versions are decided by the
rules engine ('Ecluse.Core.Rules.evalRules' -- the boot order walked to the first decisive
result), the resulting decisions handed to the agnostic
'filterPlanFromDecisions', and the plan consumed directly: a plan with survivors
yields a gated 'Contribution' -- the typed view restricted to the survivors beside
the __unrestricted raw @Value@__ (the assembly takes only plan-surviving version
objects from it, so restricting the raw document here would rebuild a many-version
object only for the assembly to rebuild it again); a plan with no survivors yields
no contribution and the per-version 'ServeDecision's, each excluded
version's decision projected (a fail-closed 'Ecluse.Core.Rules.Types.Undecidable' carrying
its transient\/permanent cause, so the no-survivors status is a @503@\/@500@ rather than
a @403@). The dropped below-floor versions are projected as 'MissingIntegrity' (no digest
at all) or 'BelowIntegrityFloor' (a digest, but too weak) refusals and appended to those
exclusions, so a packument with /only/ inadmissible public versions is a @403@ rather
than an empty success. Evaluation is IO (an effectful rule may do IO), so this gate is
IO; with only pure rules it short-circuits without launching any IO.

The gated contribution's typed 'PackageInfo' is __restricted to the survivors__:
'mergePackuments' treats a 'GatedSource' as the already-filtered set and never
re-filters, so feeding it the unfiltered view would let a denied version reach the
merge plan (and skew the reconciled @latest@\/@time@). The raw @Value@ needs no
matching restriction: only versions named by the plan's survivors are ever taken
from it at assembly, so a denied version's object is unreachable by construction.

This gate runs on the public path only; the trusted (private) contribution is admitted
separately by 'admitTrusted' against the trusted integrity floor (the rules never run on
it -- the trust split is the caller's). -}
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
version string (the map 'filterPlanFromDecisions' consumes). Each version is run
through 'Ecluse.Core.Rules.evalRules', so a fail-closed rule that cannot be computed
yields a 'Ecluse.Core.Rules.Types.Undecidable' decision. With only pure rules the
per-version call short-circuits without launching any IO. -}
decideVersions :: PackumentDeps -> EvalContext -> PackageInfo -> IO (Map Text Decision)
decideVersions deps ctx info =
    traverse (evalRules ctx (pdRules deps)) (infoVersions info)

{- Project each excluded version's 'Decision' to a 'VersionVerdict', keeping the
version string so a denial's audit line can name it. The plan carries its
decisions ('fpDecisions') in @versions@-key order, so they zip back onto the
same-ordered version keys to recover the package\/version each denial is about. -}
projectDecisions :: PackageInfo -> [Decision] -> [VersionVerdict]
projectDecisions info =
    zipWith versionVerdict (Map.toList (infoVersions info))
  where
    versionVerdict (ver, details) d = VersionVerdict ver (serveDecisionOf details d)

-- The fully-edited served body: the raw @Value@ to encode and answer against the
-- conditional request.
newtype ServedBody = ServedBody {servedValue :: Value}

{- Merge the resolved sources into the serve plan, or 'Nothing' when no version
survives the merge (no source resolved, or every public version was excluded and no
private versions exist). Split from the rendering so the conditional evaluation can
sit between them: the plan (typed, cheap) decides serve-vs-no-survivors, and only a
'Modified' outcome pays for 'renderServedBody'. -}
packumentPlan :: [Contribution] -> Maybe MergePlan
packumentPlan sources = do
    plan <- mergePackuments [(srcProvenance s, srcInfo s) | s <- sources]
    guard (not (Map.null (mpSurvivors plan)))
    pure plan

{- The plan a request should serve under an operator divergence policy, or 'Nothing' when
the policy withheld the last surviving version (take the no-survivors terminal). 'Warn'
never withholds; 'FailClosed' drops the contested versions and may leave no survivors. -}
survivingPlan :: DivergencePolicy -> MergePlan -> Maybe MergePlan
survivingPlan policy plan =
    let served = applyDivergencePolicy policy plan
     in if Map.null (mpSurvivors served) then Nothing else Just served

{- | The derived packument validator: a SHA-256 over the serve's __inputs__ -- the
mount base URL, the package name, and per source (in merge order) its provenance,
its origin body's digest, and the version keys that survived its gate.

The served document is a deterministic function of exactly these (the merge plan
derives from the gated typed views, which derive from the origin bytes and the
survivor sets; the assembly then edits the origin documents under the mount base
URL), so this tag can never call a changed document unchanged. It may change when
the re-assembled bytes would not have -- a spurious @200@, never a wrong @304@ --
which is the correct slack for a validator. Deriving it from inputs is what lets a
@304@ skip assembly, encoding, and any output hashing entirely.

Fields are fed to the hash with unambiguous framing: the digest is fixed-width, the
variable-length pieces are @NUL@-terminated, and each source block closes with an
@\\SOH@ terminator, so no concatenation of adjacent fields can collide with another
split of the same bytes. The leading salt versions the scheme: bump it when the
assembly's behaviour changes so pre-change client caches revalidate as modified.
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

-- The validator is a content address over every serve input, so the assembled,
-- encoded document is memoised under it: a recurring triple (public entry,
-- private content, plan) serves the stored bytes with no assembly or encode, and
-- concurrent identical renders coalesce onto one leader. A changed input is a
-- changed key, so the store cannot serve stale bytes; a different private view is
-- a different key, so it cannot cross a client boundary.
servedBytes :: ServeRuntime -> PackumentDeps -> [Contribution] -> MergePlan -> ETag -> IO ByteString
servedBytes rt deps sources plan etag =
    resolveAssembled (srMetrics rt) (srMetadataCache rt) (renderETag etag) $
        pure $!
            LBS.toStrict (Aeson.encode (servedValue (renderServedBody deps sources plan)))

{- Assemble the served packument by replaying the 'MergePlan' onto the sources' raw
@Value@s.

The merge decides over the typed 'PackageInfo's; the served body is built from the
raw @Value@s so unmodeled keys survive. For each surviving @(version, SourceId)@
the version object is taken from that source's raw @Value@; @dist-tags@ and @time@
come from the plan (with @time@'s non-version bookkeeping keys retained from the
sources); every other top-level key is relayed from the precedence-winning
document. Tarball URLs are rewritten under the mount base so artifacts route back
through the gate. Runs only on a 'Modified' outcome -- a @304@ never pays for it. -}
renderServedBody :: PackumentDeps -> [Contribution] -> MergePlan -> ServedBody
renderServedBody deps sources plan =
    ServedBody (pdAssemble deps (pdMountBaseUrl deps) bySource plan (baseDocument sources))
  where
    bySource :: Map SourceId Value
    bySource = Map.fromList (zip [0 ..] (map srcValue sources))

{- The document whose unmodeled top-level keys are relayed into the served body:
the precedence-winning source's raw @Value@ -- the first trusted source if any,
else the first source. (The merge takes its identity from the first input
likewise.) An empty source list never reaches here. -}
baseDocument :: [Contribution] -> Value
baseDocument sources =
    case find ((== TrustedSource) . srcProvenance) sources of
        Just s -> srcValue s
        Nothing -> case sources of
            s : _ -> srcValue s
            [] -> Object mempty

{- The per-version serve decisions weighed for the no-survivors status: the
public-set exclusions, plus the per-origin signals each upstream contributes.

A private upstream that did not resolve is a needed-but-unavailable transient signal
(it may resolve on retry), so a private outage with no public survivors is a @503@
rather than a @403@. An origin (private or public) that __answered with a packument
for a different package__ contributes an 'UpstreamInvalid' signal, so a request whose
only responding origins were invalid this way renders a @502@ -- distinct from a
genuine absence. A public upstream that merely did not resolve degrades silently, as
before: its absence is not by itself a needed-upstream outage. -}
collectDecisions :: OriginResult -> OriginResult -> [ServeDecision] -> [ServeDecision]
collectDecisions privResult pubResult publicExclusions =
    privateDecision privResult <> publicMismatch pubResult <> publicExclusions
  where
    privateDecision :: OriginResult -> [ServeDecision]
    privateDecision = \case
        OriginResolved _ -> []
        OriginUnresolved -> [neededUpstreamUnavailable]
        OriginNameMismatch -> [upstreamInvalidDecision]

    publicMismatch :: OriginResult -> [ServeDecision]
    publicMismatch = \case
        OriginNameMismatch -> [upstreamInvalidDecision]
        OriginResolved _ -> []
        OriginUnresolved -> []

    neededUpstreamUnavailable :: ServeDecision
    neededUpstreamUnavailable = Reject (Rejection (Unavailable (WillResolve Nothing)) "a needed upstream was unavailable")

    upstreamInvalidDecision :: ServeDecision
    upstreamInvalidDecision = Reject (Rejection UpstreamInvalid "an upstream returned a packument for a different package")

{- The served packument @200@ over the (possibly memoised) assembled bytes, carrying
the derived 'ETag' the caller already evaluated the conditional against ('Modified' --
a match never reaches here). The bytes come from 'resolveAssembled': strict, encoded
once per content address, shared across every request whose inputs coincide. A
'PackumentHead' additionally advertises the body's exact @Content-Length@ (the
'bodiless' wrapper then withholds the bytes), free off the memoised bytes. -}
packumentResponse :: PackumentServe -> ETag -> ByteString -> Response
packumentResponse mode etag bytes = case mode of
    PackumentFull ->
        jsonResponse status200 [etagHeader etag] (LBS.fromStrict bytes)
    PackumentHead ->
        jsonResponse
            status200
            [etagHeader etag, (hContentLength, show (BS.length bytes))]
            (LBS.fromStrict bytes)

-- The bodiless conditional answer: the client's validator matched, so only the tag
-- travels. Identical between GET and HEAD (a 304 carries no body either way).
notModifiedResponse :: ETag -> Response
notModifiedResponse etag =
    jsonResponse (mkStatus 304 "Not Modified") [etagHeader etag] ""

{- Render the no-survivors outcome: the status 'packumentStatus' chose over the
exclusions, with a denial body collecting the reasons. Never a @404@ -- the package
existed and its versions were withheld. -}
noSurvivors :: MountRenderer -> PackumentDeps -> [ServeDecision] -> Response
noSurvivors renderer deps decisions =
    renderedResponse (toStatus status) (retryAfterHeader status) (renderError renderer (pdHelp deps) message)
  where
    status :: PackumentStatus
    status = packumentStatus decisions

    toStatus :: PackumentStatus -> Status
    toStatus s = mkStatus (packumentStatusCode s) (statusReason s)

    statusReason :: PackumentStatus -> ByteString
    statusReason = \case
        PackumentOk -> "OK"
        PackumentForbidden -> "Forbidden"
        PackumentUnavailable{} -> "Service Unavailable"
        PackumentBadGateway -> "Bad Gateway"
        PackumentServerError -> "Internal Server Error"

    -- The collected denial reasons; an empty set (no versions at all) renders a
    -- deny-by-default message rather than an empty body.
    message :: Text
    message = case mapMaybe rejectionText decisions of
        [] -> "no versions are available for this package"
        reasons -> T.intercalate "; " reasons

    rejectionText :: ServeDecision -> Maybe Text
    rejectionText = \case
        Admit -> Nothing
        Reject rej -> Just (rejectionMessage rej)

-- The @Retry-After@ header for a transient no-survivors status, when a delay was
-- suggested; nothing for the other statuses.
retryAfterHeader :: PackumentStatus -> ResponseHeaders
retryAfterHeader = \case
    PackumentUnavailable (Just (RetryAfter secs)) -> [("Retry-After", show secs)]
    _ -> []
