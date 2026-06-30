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
through the rules and the structural filter ('filterPlan' decides, 'applyFilterPlan'
replays) before they enter; the two are combined, private winning a collision and
an integrity divergence flagged. If one upstream
is unavailable while the other succeeds, the best-effort union of what resolved is
served -- only when /nothing/ resolves does the request error.

== Decision surface vs served surface

The merge and filter reason over the /typed/ 'PackageInfo' but the document served
is the __raw upstream JSON__, edited in place, so every unmodeled wire key
survives (see @docs\/architecture\/registry-model.md@ → "Decision surface vs
served surface"). The 'MergePlan' names, for each surviving version, the source
that won it; the served body is assembled by taking each survivor's object from
the /raw @Value@/ of its winning source, carrying the reconciled @dist-tags@ and
@time@, and relaying every other top-level key from the precedence-winning
document. The typed model is never re-serialised. The two fields the merge /owns/ as
a decision -- @dist-tags.latest@ and the @time@ instants -- are re-rendered from that
decision (the times as normalised ISO-8601), so they may differ byte-for-byte from
any single upstream while denoting the same value; integrity-bearing fields
(@dist.integrity@, @dist.tarball@) are relayed raw and untouched. The served bytes
get our __own ETag__, since a merged\/filtered body matches no single upstream's.
-}
module Ecluse.Core.Server.Pipeline.Packument (
    servePackument,
    headPackument,
    withPublicMetadataClient,
) where

import Data.Aeson (Value (Object, String))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Katip (KatipContext, Severity (WarningS), katipAddContext, logFM, ls, sl)
import Lens.Micro ((^?))
import Lens.Micro.Aeson (key, _Object)
import Network.HTTP.Client (Manager)
import Network.HTTP.Types (ResponseHeaders, Status, hContentLength, mkStatus, status200)
import Network.Wai (Request, Response, ResponseReceived, requestHeaders)
import UnliftIO (concurrently, withRunInIO)
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Credential (Secret)

import Ecluse.Core.Package (
    InvalidEntry (invalidKey, invalidKind, invalidReason, invalidValue),
    InvalidEntryKind (InvalidDistTag, InvalidPublishTime, InvalidVersionManifest),
    PackageInfo (infoDistTags, infoVersions),
    PackageName,
    renderPackageName,
 )
import Ecluse.Core.Package.Filter (FilterResult (Filtered, NoSurvivors), filterPlanFromDecisions, fpSurvivors)
import Ecluse.Core.Package.Integrity (
    MinTrustedIntegrity,
 )
import Ecluse.Core.Package.Merge (
    MergePlan (mpDistTags, mpSurvivors, mpTime),
    Provenance (GatedSource, TrustedSource),
    SourceId,
    mergePackuments,
 )
import Ecluse.Core.Registry.Metadata (
    MetadataClient (fetchFullManifest),
    MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable),
 )
import Ecluse.Core.Rules (evalRules)
import Ecluse.Core.Rules.Types (Decision, EvalContext (EvalContext))
import Ecluse.Core.Security (
    LimitError (BodyTooLarge, TooDeeplyNested, TooManyVersions),
    Limits,
 )
import Ecluse.Core.Server.Admission (withServeAdmission)
import Ecluse.Core.Server.Cache (Source (Source))
import Ecluse.Core.Server.Conditional (Conditional (Modified, NotModified), etagHeader, evaluateOwnETag)
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
    admitByIntegrity,
    evalTier,
    logDecodeFailure,
    logNameMismatch,
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
import Ecluse.Core.Version (renderVersion)

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
versions are gated through the rules and the structural filter ('filterPlan' then
'applyFilterPlan'); the surviving sets are merged ('mergePackuments') and the
'MergePlan' replayed onto the raw upstream @Value@s to assemble the served body,
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
    | not (edgeAuthorised deps request) = liftIO (respond (edgeUnauthorised renderer))
    | otherwise = do
        rt <- asks ctxRuntime
        withServeAdmission (srAdmission rt) (serveAdmitted rt) >>= \case
            Just received -> pure received
            Nothing -> liftIO $ do
                mpServeDecision (srMetrics rt) Metric.Unavailable
                respond (serveOverloaded renderer)
  where
    serveAdmitted rt = do
        let metrics = srMetrics rt
        evalCtx <- liftIO (EvalContext <$> pdNow deps)
        let clientToken = forwardedToken request
        (privResult, pubResult) <-
            concurrently
                (fetchPrivateOrigin deps rt clientToken name)
                (fetchPublicOrigin deps rt name)
        (public, publicExclusions) <- liftIO (gatePublic metrics deps evalCtx (originPackument pubResult))
        let (private, privateExclusions) = admitTrusted (pdMinTrustedIntegrity deps) (originPackument privResult)
            sources = catMaybes [private, public]
        case assemble deps sources of
            Just body -> do
                liftIO (mpServeDecision metrics Metric.Admit)
                liftIO (respond (servePackumentBody mode request body))
            Nothing -> do
                let decisions = collectDecisions privResult pubResult (privateExclusions <> publicExclusions)
                liftIO (mpServeDecision metrics (packumentServeDecision decisions))
                liftIO (recordDenials metrics decisions)
                liftIO (respond (noSurvivors renderer deps decisions))

-- A recognised-but-unserved packument route: a @501@ in the mount's surface, for a
-- mount whose packument-serve dependencies are not wired. The decision to serve or
-- stub is the handler's, so the routing layer need not re-derive it.

{- A successfully resolved upstream contribution: the parsed packument used to
decide, alongside the raw @Value@ that is edited in place to serve. Pairing them
is the decision-surface\/served-surface contract -- every stage carries the raw
@Value@ next to the typed view so losslessness survives the pipeline. -}
data Contribution = Contribution
    { srcProvenance :: Provenance
    , srcInfo :: PackageInfo
    , srcValue :: Value
    }

{- The outcome of resolving one upstream origin for a packument, beyond the
plain "resolved or not" the merge consumes: a name mismatch is kept distinct from a
plain non-resolution so the no-valid-origin terminal status can render a @502@ (a
responding upstream returned a packument for a different package) apart from a
transient outage or a genuine absence. -}
data OriginResult
    = -- | A packument that decoded and whose self-reported name matched the request.
      OriginResolved (PackageInfo, Value)
    | {- | The origin answered, but its packument self-reported a name for a /different/
      package -- dropped as untrusted for this request, and a @502@ signal when no
      origin is valid.
      -}
      OriginNameMismatch
    | {- | The origin did not yield a usable packument -- unreachable, undecodable, or a
      genuine absence -- the existing degrade (no contribution).
      -}
      OriginUnresolved

-- The resolved (packument, raw @Value@) pair an origin contributed, if any. A name
-- mismatch and a plain non-resolution alike contribute no document to the merge.
originPackument :: OriginResult -> Maybe (PackageInfo, Value)
originPackument = \case
    OriginResolved pair -> Just pair
    OriginNameMismatch -> Nothing
    OriginUnresolved -> Nothing

{- Classify a per-origin full-manifest fetch into an 'OriginResult'. A genuine
transport\/async fault (the 'tryAny' channel) and a 'MetadataError' degrade alike yield
no document, but a 'MetadataNameMismatch' is kept distinct as 'OriginNameMismatch' so the
no-valid-origin terminal status can render a @502@ (a responding upstream answered for a
different package) apart from a transient outage, an undecodable body, or a bound breach. -}
originResultOf :: Either SomeException (Either MetadataError (PackageInfo, Value)) -> OriginResult
originResultOf = \case
    Left _ -> OriginUnresolved
    Right (Left (MetadataNameMismatch _)) -> OriginNameMismatch
    Right (Left _) -> OriginUnresolved
    Right (Right pair) -> OriginResolved pair

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
                (srMetrics rt)
                upstream
                caching
                (\nm err -> runInIO (logMetadataFailure nm baseUrl err))
                (\nm entries -> runInIO (logInvalidEntries nm baseUrl entries))
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
is the name-mismatch log ('logNameMismatch'). Invoked once per real fetch, inside the
single-flight leader, in the request's context. -}
logMetadataFailure :: PackageName -> Text -> MetadataError -> Handler ()
logMetadataFailure name baseUrl = \case
    MetadataBoundExceeded err -> logBreach name err
    MetadataUndecodable -> logDecodeFailure name
    MetadataNameMismatch reported -> logNameMismatch name baseUrl reported

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
            <> sl "droppedVersionManifests" (countOf InvalidVersionManifest)
            <> sl "droppedDistTags" (countOf InvalidDistTag)
            <> sl "droppedPublishTimes" (countOf InvalidPublishTime)
            <> sl "droppedEntries" (map renderDrop (take maxRenderedDrops entries))

    countOf :: InvalidEntryKind -> Int
    countOf kind = length (filter ((== kind) . invalidKind) entries)

    -- One dropped entry rendered for the operator: its kind, key, reason, and the raw
    -- value the upstream sent (truncated), so the actual offending bytes are visible.
    renderDrop :: InvalidEntry -> Text
    renderDrop e =
        renderKind (invalidKind e)
            <> " "
            <> invalidKey e
            <> " = "
            <> truncated (invalidValue e)
            <> " ("
            <> invalidReason e
            <> ")"

    renderKind :: InvalidEntryKind -> Text
    renderKind = \case
        InvalidVersionManifest -> "version-manifest"
        InvalidDistTag -> "dist-tag"
        InvalidPublishTime -> "publish-time"

    -- The raw value as compact JSON, truncated to 'maxRenderedValueChars' (only that many
    -- characters are ever forced, so a huge value never balloons the log line).
    truncated :: Value -> Text
    truncated v =
        let rendered = TL.toStrict (TL.take (fromIntegral maxRenderedValueChars + 1) (encodeToLazyText v))
         in if T.length rendered > maxRenderedValueChars
                then T.take maxRenderedValueChars rendered <> "…"
                else rendered

    message :: Text
    message =
        "dropped " <> show (length entries) <> " malformed entr" <> plural <> " from an upstream packument (the rest is served)"
    plural = if length entries == 1 then "y" else "ies"

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
admitTrusted :: MinTrustedIntegrity -> Maybe (PackageInfo, Value) -> (Maybe Contribution, [ServeDecision])
admitTrusted minTrusted = \case
    Nothing -> (Nothing, [])
    Just (info, value) ->
        let (admissible, integrityRefusals) =
                admitByIntegrity minTrusted trustedIntegrityBelowFloor trustedIntegrityMissing info
         in if Map.null (infoVersions admissible)
                then (Nothing, integrityRefusals)
                else (Just (Contribution TrustedSource admissible value), integrityRefusals)

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
'filterPlanFromDecisions', and that plan replayed by 'applyFilterPlan' onto the raw
@Value@: 'Filtered' yields a gated 'Contribution' over the surviving versions;
'NoSurvivors' yields no contribution and the per-version 'ServeDecision's, each excluded
version's decision projected (a fail-closed 'Ecluse.Core.Rules.Types.Undecidable' carrying
its transient\/permanent cause, so the no-survivors status is a @503@\/@500@ rather than
a @403@). The dropped below-floor versions are projected as 'MissingIntegrity' (no digest
at all) or 'BelowIntegrityFloor' (a digest, but too weak) refusals and appended to those
exclusions, so a packument with /only/ inadmissible public versions is a @403@ rather
than an empty success. Evaluation is IO (an effectful rule may do IO), so this gate is
IO; with only pure rules it short-circuits without launching any IO.

The gated contribution's typed 'PackageInfo' is __restricted to the survivors__ to
match its filtered @Value@: 'mergePackuments' treats a 'GatedSource' as the
already-filtered set and never re-filters, so feeding it the unfiltered view would
let a denied version reach the merge plan (and skew the reconciled @latest@\/@time@).

This gate runs on the public path only; the trusted (private) contribution is admitted
separately by 'admitTrusted' against the trusted integrity floor (the rules never run on
it -- the trust split is the caller's). -}
gatePublic :: MetricsPort -> PackumentDeps -> EvalContext -> Maybe (PackageInfo, Value) -> IO (Maybe Contribution, [ServeDecision])
gatePublic metrics deps ctx = \case
    Nothing -> pure (Nothing, [])
    Just (info, value) -> do
        let (admissible, integrityRefusals) = admitByIntegrity (pdMinIntegrity deps) integrityBelowFloor integrityMissing info
        (decisions, seconds) <- timedSeconds (decideVersions deps ctx admissible)
        mpRuleEvalDuration metrics (evalTier (pdRules deps)) seconds
        recordEffectfulFailures metrics (Map.elems decisions)
        let plan = filterPlanFromDecisions decisions admissible
        pure $ case pdApplyFilter deps plan value of
            Filtered filtered ->
                (Just (Contribution GatedSource (restrictToSurvivors (fpSurvivors plan) admissible) filtered), integrityRefusals)
            NoSurvivors leftover -> (Nothing, projectDecisions admissible leftover <> integrityRefusals)

{- Decide every version of a public packument against the rules engine, keyed by raw
version string (the map 'filterPlanFromDecisions' consumes). Each version is run
through 'Ecluse.Core.Rules.evalRules', so a fail-closed rule that cannot be computed
yields a 'Ecluse.Core.Rules.Types.Undecidable' decision. With only pure rules the
per-version call short-circuits without launching any IO. -}
decideVersions :: PackumentDeps -> EvalContext -> PackageInfo -> IO (Map Text Decision)
decideVersions deps ctx info =
    traverse (evalRules ctx (pdRules deps)) (infoVersions info)

{- Restrict a 'PackageInfo' to the version keys that survived filtering -- the
'Ecluse.Core.Package.Filter.FilterPlan'\'s own 'fpSurvivors', which 'applyFilterPlan' kept
in the filtered @Value@'s @versions@, so the typed view handed to the merge matches
the filtered document. Taking the survivor set from the plan reuses the 'Set' the
filter already built rather than re-deriving it from the filtered @Value@'s keys (a
@Set.fromList@ of every survivor key, each 'Key.toText'-converted, over a packument of
up to the version cap). @dist-tags@ is pruned to the surviving keys likewise (the merge
reconciles tags over the union); @dist-tags@ targets absent from the survivors are
dropped. Each surviving version carries its own publish time, so restricting the
versions carries the times with it (the merge reconstructs the served @time@ from the
survivors). -}
restrictToSurvivors :: Set Text -> PackageInfo -> PackageInfo
restrictToSurvivors survivors info =
    info
        { infoVersions = Map.restrictKeys (infoVersions info) survivors
        , infoDistTags = Map.filter ((`Set.member` survivors) . renderVersion) (infoDistTags info)
        }

{- Project each excluded version's 'Decision' to a 'ServeDecision' for the
no-survivors status. 'applyFilterPlan' carries the plan's decisions in
@versions@-key order ('Data.Map.elems'), so they zip back onto the same-ordered
'PackageDetails' to recover the package\/version each denial is about. -}
projectDecisions :: PackageInfo -> [Decision] -> [ServeDecision]
projectDecisions info =
    zipWith serveDecisionOf (Map.elems (infoVersions info))

-- The fully-edited served body: the raw @Value@ to encode and answer against the
-- conditional request.
newtype ServedBody = ServedBody {servedValue :: Value}

{- Assemble the served packument by merging the resolved sources and replaying the
'MergePlan' onto their raw @Value@s, or 'Nothing' when no version survives the
merge (no source resolved, or every public version was excluded and no private
versions exist).

The merge decides over the typed 'PackageInfo's; the served body is built from the
raw @Value@s so unmodeled keys survive. For each surviving @(version, SourceId)@
the version object is taken from that source's raw @Value@; @dist-tags@ and @time@
come from the plan (with @time@'s non-version bookkeeping keys retained from the
sources); every other top-level key is relayed from the precedence-winning
document. Tarball URLs are rewritten under the mount base so artifacts route back
through the gate. -}
assemble :: PackumentDeps -> [Contribution] -> Maybe ServedBody
assemble deps sources = do
    plan <- mergePackuments [(srcProvenance s, srcInfo s) | s <- sources]
    guard (not (Map.null (mpSurvivors plan)))
    let bySource :: Map SourceId Contribution
        bySource = Map.fromList (zip [0 ..] sources)
    pure (ServedBody (replayPlan deps bySource plan (baseDocument sources)))

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

{- Replay a 'MergePlan' onto the raw source @Value@s: rebuild @versions@,
@dist-tags@, and @time@ from the plan, then rewrite the tarball URLs of the
result. Other top-level keys are inherited from the base document. -}
replayPlan :: PackumentDeps -> Map SourceId Contribution -> MergePlan -> Value -> Value
replayPlan deps bySource plan base =
    pdRewriteUrls deps (pdMountBaseUrl deps) (Object rebuilt)
  where
    rebuilt :: KeyMap Value
    rebuilt =
        baseObject
            & KeyMap.insert "versions" (Object survivingVersions)
            & KeyMap.insert "dist-tags" (Object distTags)
            & KeyMap.insert "time" (Object reconciledTime)

    baseObject :: KeyMap Value
    baseObject = case base of
        Object o -> o
        _ -> mempty

    -- Each surviving version's object, taken from the raw @Value@ of the source
    -- that won the key (so the served bytes are the winning upstream's, unmodeled
    -- keys and all). A survivor whose source object is missing is dropped rather
    -- than fabricated -- coherence with the plan is preserved by construction.
    survivingVersions :: KeyMap Value
    survivingVersions =
        KeyMap.fromList
            [ (Key.fromText version, object)
            | (version, sid) <- Map.toList (mpSurvivors plan)
            , Just object <- [versionObjectFrom sid version]
            ]

    -- Each source's raw @versions@ object, extracted once per source.
    -- 'versionObjectFrom' runs once per surviving version (up to the packument's
    -- version cap), so resolving the source's @versions@ object inside it would
    -- re-extract the same object on every version; hoisting it here leaves each
    -- survivor a single inner lookup. ('bySource' holds one entry per upstream.)
    versionsBySource :: Map SourceId (KeyMap Value)
    versionsBySource = Map.mapMaybe ((^? key "versions" . _Object) . srcValue) bySource

    versionObjectFrom :: SourceId -> Text -> Maybe Value
    versionObjectFrom sid version =
        Map.lookup sid versionsBySource >>= KeyMap.lookup (Key.fromText version)

    -- @dist-tags@ rebuilt from the plan's reconciled tags (each a rendered version
    -- string). The plan has already resolved @latest@ and dropped absent-target
    -- tags over the union.
    distTags :: KeyMap Value
    distTags =
        KeyMap.fromList
            [ (Key.fromText tag, String (renderVersion v))
            | (tag, v) <- Map.toList (mpDistTags plan)
            ]

    -- @time@ rebuilt from the plan's surviving-version times, with the sources'
    -- non-version bookkeeping keys (@created@\/@modified@) retained. The plan
    -- restricts @time@ to surviving versions; the bookkeeping keys are not versions
    -- and so are carried separately, from the base document's @time@.
    reconciledTime :: KeyMap Value
    reconciledTime =
        bookkeepingTime
            <> KeyMap.fromList
                [ (Key.fromText version, String (renderTime t))
                | (version, t) <- Map.toList (mpTime plan)
                ]

    -- The base @time@ map carries one entry per published version (up to the
    -- packument's version cap) plus the @created@\/@modified@ bookkeeping keys.
    -- Look those two keys up directly rather than filtering the whole map, so this
    -- is a pair of lookups, not a full traversal of every version's publish time.
    bookkeepingTime :: KeyMap Value
    bookkeepingTime =
        case base ^? key "time" . _Object of
            Nothing -> mempty
            Just timeObject ->
                KeyMap.fromList
                    [ (k, value)
                    | name <- timeBookkeepingKeys
                    , let k = Key.fromText name
                    , Just value <- [KeyMap.lookup k timeObject]
                    ]

-- The non-version keys an npm @time@ object carries that must be relayed unchanged.
timeBookkeepingKeys :: [Text]
timeBookkeepingKeys = ["created", "modified"]

-- Render a publish time as the ISO-8601 instant npm serves in its @time@ map.
renderTime :: UTCTime -> Text
renderTime = toText . iso8601Show

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

{- Render the served packument body: @200@ with our own ETag over the served
bytes, or a @304@ when the client's conditional validator already matches. The
ETag is computed over exactly the bytes served, so it changes iff the served
document changes.

On a 'PackumentHead' the @200@ additionally carries the would-be body's
@Content-Length@ (the 'bodiless' wrapper then withholds the bytes), so the @HEAD@ reply
advertises the same framing a @GET@ would; a 'PackumentFull' leaves the @Content-Length@
to the serving layer, which frames the body it actually writes. The @304@ carries no
body either way, so it is identical between the two. -}
servePackumentBody :: PackumentServe -> Request -> ServedBody -> Response
servePackumentBody mode request body =
    case evaluateOwnETag (requestHeaders request) encoded of
        NotModified etag ->
            jsonResponse (mkStatus 304 "Not Modified") [etagHeader etag] ""
        Modified etag ->
            jsonResponse status200 (etagHeader etag : headContentLength mode encoded) encoded
  where
    encoded :: LByteString
    encoded = Aeson.encode (servedValue body)

{- The @Content-Length@ header a packument @200@ carries: on a 'PackumentHead', the
length of the would-be merged body, so the @HEAD@ reply advertises the framing a @GET@
would (the body itself is withheld by 'bodiless'); on a 'PackumentFull', none -- the
serving layer frames the body it actually writes. -}
headContentLength :: PackumentServe -> LByteString -> ResponseHeaders
headContentLength = \case
    PackumentFull -> const []
    PackumentHead -> \bytes -> [(hContentLength, show (LBS.length bytes))]

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
