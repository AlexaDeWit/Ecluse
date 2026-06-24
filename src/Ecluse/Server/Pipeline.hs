{- | The serve paths behind the package routes: the packument merge behind
@GET \/{pkg}@ and the artifact relay behind @GET \/{pkg}\/-\/{file}.tgz@.

This is the data-plane handler module. It composes the
slices that decide /what/ to serve — the registry client
("Ecluse.Registry.Npm"), the per-version rules ("Ecluse.Rules"), the structural
filter ("Ecluse.Registry.Npm.Filter"), the cross-upstream merge
("Ecluse.Package.Merge"), the metadata cache ("Ecluse.Server.Cache"), the
own-ETag conditional ("Ecluse.Server.Conditional"), and the serve-outcome status
("Ecluse.Server.Response") — into one action in the
'Ecluse.Server.Context.Handler' reader, reading its mount's serve dependencies and
the composition-root 'Ecluse.Env.Env' from the request's
'Ecluse.Server.Context.RequestCtx'.

== Credential authority

This handler implements the default @passthrough@ credential posture (see
@docs\/architecture\/access-model.md@). The invariant that holds under __every__
strategy is the __public strip__: the client's credential is __stripped before any
public-upstream fetch__, which is always anonymous — sending an internal token to the
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
TTL, bypassing the upstream's authorisation — a cross-client disclosure. (Other
strategies make the private origin shareable by authorising each serve differently; the
metadata cache itself stays credential-free regardless — see
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
served — only when /nothing/ resolves does the request error.

== Decision surface vs served surface

The merge and filter reason over the /typed/ 'PackageInfo' but the document served
is the __raw upstream JSON__, edited in place, so every unmodeled wire key
survives (see @docs\/architecture\/registry-model.md@ → "Decision surface vs
served surface"). The 'MergePlan' names, for each surviving version, the source
that won it; the served body is assembled by taking each survivor's object from
the /raw @Value@/ of its winning source, carrying the reconciled @dist-tags@ and
@time@, and relaying every other top-level key from the precedence-winning
document. The typed model is never re-serialised. The two fields the merge /owns/ as
a decision — @dist-tags.latest@ and the @time@ instants — are re-rendered from that
decision (the times as normalised ISO-8601), so they may differ byte-for-byte from
any single upstream while denoting the same value; integrity-bearing fields
(@dist.integrity@, @dist.tarball@) are relayed raw and untouched. The served bytes
get our __own ETag__, since a merged\/filtered body matches no single upstream's.

== Ecosystem coupling

This is the __npm__ packument pipeline: it reaches for the npm registry
client, projection, and structural filter directly, so it is the one
@Ecluse.Server.*@ module that depends on a concrete adapter. The coupling is
expedient, not intended — the agnostic handles that would let it dispatch through an
adapter (a per-adapter router, and an ecosystem-neutral filter\/projection) would
let a second ecosystem reuse this orchestration unchanged.

== Artifact path

The tarball handler ('serveTarball') is the demand-driven artifact relay. It fetches
each tarball from its __authoritative upstream location__ — the @Artifact.artUrl@ the
projection preserved from the upstream @dist.tarball@, selected from the gated
version by the requested filename — rather than reconstructing
@{base}\/{pkg}\/-\/{file}@ by npm convention. Honouring the upstream-declared
location is what lets the proxy front a registry that serves its artifacts from a
separate host or an off-convention path (a CDN\/files host, a signed URL); a
reconstruction would silently fetch the wrong place. The location is gated, not
trusted: it is fetched only when the tarball-host policy
('Ecluse.Security.tarballHostAllowed', per @PROXY_RESPECT_UPSTREAM_TARBALL_HOST@)
admits its host — the default refuses a cross-host @dist.tarball@ — and the
untrusted egress additionally carries the resolved-IP recheck.

The private origin is tried first, __uncached__, forwarding the client's credential:
its packument is fetched, the requested artifact selected, and its @artUrl@ fetched
over the __trusted__ manager; a hit streams the artifact through with __bounded
memory__ (the @withResponse@\/@responseStream@ relay, never a buffering fetch), and
any non-served outcome — the packument not resolving, no artifact matching the
filename, the policy refusing the host, a non-@2xx@ — falls through. The public
origin is anonymous: it gates __that one version__ against the rules (the same
machinery the packument path gates the whole set with) and selects the artifact, and
on an admit __streams the public bytes from @artUrl@ and enqueues a
'Ecluse.Queue.MirrorJob'__ (naming that authoritative URL) for the worker to
back-fill the mirror target; on a reject — including a host the tarball-host policy
refuses — it renders the serve error model (@403@\/@503@\/@500@\/@404@) through the
mount's renderer. The enqueue is __serve-then-enqueue, best-effort and
non-blocking__: the artifact reaches the client first, and an enqueue failure is
swallowed rather than failing or delaying the response. Mirroring is
__demand-driven__ — a job is enqueued only here, on a tarball-path admit, never when
a packument is filtered. The serve path does __not__ verify @dist.integrity@; the
client checks the artifact's own hash and the worker re-verifies before publishing.
-}
module Ecluse.Server.Pipeline (
    -- * The packument handler
    servePackument,

    -- * The tarball handler
    serveTarball,
) where

import Data.Aeson (Value (Object, String))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Katip (LogEnv, Severity (WarningS), logFM, ls, sl)
import Katip.Monadic (runKatipContextT)
import Lens.Micro ((^?))
import Lens.Micro.Aeson (key, _Object)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (ResponseHeaders, Status, hAuthorization, hContentType, mkStatus, status200, status401, status501, statusIsSuccessful)
import Network.Wai (Request, Response, ResponseReceived, requestHeaders, responseLBS)
import UnliftIO (concurrently)
import UnliftIO.Exception (handle, throwIO, tryAny)

import Ecluse.Credential (Secret, mkSecret)
import Ecluse.Env (Env (envLogEnv, envManager, envMetadataCache, envPrivateManager, envQueue))
import Ecluse.Log (moduleField)
import Ecluse.Package (
    Artifact (artFilename, artHashes, artUrl),
    PackageDetails (pkgArtifacts),
    PackageInfo (infoDistTags, infoPublishedAt, infoVersions),
    PackageName,
    renderPackageName,
 )
import Ecluse.Package.Filter (filterPlanFromDecisions)
import Ecluse.Package.Merge (
    MergePlan (mpDistTags, mpSurvivors, mpTime),
    Provenance (GatedSource, TrustedSource),
    SourceId,
    mergePackuments,
 )
import Ecluse.Queue (MirrorJob (MirrorJob, jobArtifactUrl, jobMirrorTarget, jobPackage, jobVersion), enqueue)
import Ecluse.Registry (RegistryResponse (responseBody))
import Ecluse.Registry.Npm (
    MetadataForm (Full),
    NpmClientConfig (..),
    ResponseBoundExceeded (ResponseBoundExceeded),
    artifactRequestByUrl,
    fetchMetadataForm,
    noValidators,
 )
import Ecluse.Registry.Npm.Filter (FilterResult (Filtered, NoSurvivors), applyFilterPlan, rewriteTarballUrls)
import Ecluse.Registry.Npm.Project (parsePackageInfoFromValue)
import Ecluse.Rules.Effectful (evalRulesEffectful)
import Ecluse.Rules.Types (Decision, EvalContext (EvalContext))
import Ecluse.Security (
    LimitError (BodyTooLarge, TooDeeplyNested, TooManyVersions),
    Limits,
    LoweredHostSet,
    Origin (TrustedOrigin, UntrustedOrigin),
    checkNestingDepth,
    checkVersionCount,
    hostAddress,
    lowerCaseHosts,
    tarballHostAllowed,
 )
import Ecluse.Server.Cache (CacheEntry (CacheEntry, entryInfo, entryRaw), Source (Source), resolveMetadata)
import Ecluse.Server.Conditional (Conditional (Modified, NotModified), etagHeader, evaluateOwnETag)
import Ecluse.Server.Context (
    Handler,
    MountBinding (bindingPackumentDeps, bindingRenderer),
    PackumentDeps (..),
    ctxEnv,
    ctxMount,
 )
import Ecluse.Server.Response (
    ArtifactStatus (Forbidden, NotFound, Ok, ServerError, Unavailable'),
    MountRenderer,
    PackumentStatus (PackumentForbidden, PackumentOk, PackumentServerError, PackumentUnavailable),
    RejectReason (MissingIntegrity, Unavailable),
    Rejection (Rejection, rejectionMessage),
    RenderedBody (RenderedBody),
    RetryAfter (RetryAfter),
    ServeDecision (Admit, Reject),
    Transience (WillResolve, WontResolve),
    artifactStatus,
    artifactStatusCode,
    packumentStatus,
    packumentStatusCode,
    renderError,
    serveDecisionOf,
 )
import Ecluse.Server.Route (Filename (Filename))
import Ecluse.Server.Stream (streamUpstreamWhen)
import Ecluse.Version (Version, renderVersion)

-- ── the handler ─────────────────────────────────────────────────────────────

{- | Serve a @GET \/{pkg}@ packument request end to end, over the request's
'RequestCtx'.

The mount's 'PackumentDeps' and error renderer are read from the matched
'MountBinding' in context, not threaded as arguments. When the mount has no
packument-serve dependencies wired, the route is recognised but not served — a
@501@ in the mount's surface — rather than fabricating a result.

With dependencies wired: the edge token, if configured, is validated before any
upstream is touched. Then the private and public upstreams are fetched
__concurrently__ — the client's credential forwarded to the private origin, the public
origin anonymous — each parse failure or unavailable upstream degrading to a missing
contribution rather than an error. Private versions are trusted as-is; public
versions are gated through the rules and the structural filter ('filterPlan' then
'applyFilterPlan'); the surviving sets are merged ('mergePackuments') and the
'MergePlan' replayed onto the raw upstream @Value@s to assemble the served body,
which is then answered against the client's conditional request with our own ETag.
When nothing survives, the status follows the most recoverable cause via
'packumentStatus'. Every refusal — the edge @401@ and the no-survivors
@403@\/@503@\/@500@ — is rendered through the mount's 'MountRenderer'.
-}
servePackument ::
    PackageName ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
servePackument name request respond = do
    renderer <- asks (bindingRenderer . ctxMount)
    asks (bindingPackumentDeps . ctxMount) >>= \case
        Nothing -> liftIO (respond (recognisedButUnserved renderer))
        Just deps -> serveWithDeps renderer deps name request respond

-- Serve a packument once the mount's dependencies are known: fetch, gate, merge,
-- and answer — the credential-authority and merge logic the module header
-- describes. The composition-root 'Env' is read from the request context.
serveWithDeps ::
    MountRenderer ->
    PackumentDeps ->
    PackageName ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
serveWithDeps renderer deps name request respond
    | not (edgeAuthorised deps request) = liftIO (respond (edgeUnauthorised renderer))
    | otherwise = do
        env <- asks ctxEnv
        liftIO $ do
            evalCtx <- EvalContext <$> pdNow deps
            let clientToken = forwardedToken request
            (privResult, pubResult) <-
                concurrently
                    (fetchPrivateOrigin (pdLimits deps) env (pdPrivateBaseUrl deps) clientToken name)
                    (fetchPublicOrigin (pdLimits deps) env (pdPublicBaseUrl deps) name)
            (public, publicExclusions) <- gatePublic deps evalCtx pubResult
            let private = trustedSource <$> privResult
                sources = catMaybes [private, public]
            case assemble deps sources of
                Just body -> respond (servePackumentBody request body)
                Nothing -> respond (noSurvivors renderer deps (collectDecisions privResult publicExclusions))

-- A recognised-but-unserved packument route: a @501@ in the mount's surface, for a
-- mount whose packument-serve dependencies are not wired. The decision to serve or
-- stub is the handler's, so the routing layer need not re-derive it.
recognisedButUnserved :: MountRenderer -> Response
recognisedButUnserved renderer =
    renderedResponse status501 [] (renderError renderer Nothing "this route is recognised but not yet served by this proxy")

-- ── edge authentication ──────────────────────────────────────────────────────

{- Whether the request carries the configured inbound token. With no token
configured the edge is open; with one configured the request's bearer
@Authorization@ must match it exactly. Deny-by-default: a missing or mismatched
token is rejected. -}
edgeAuthorised :: PackumentDeps -> Request -> Bool
edgeAuthorised deps request = case pdInboundToken deps of
    Nothing -> True
    Just expected -> forwardedToken request == Just expected

-- A @401@ for a request that failed edge authentication, before any upstream
-- fetch; the body is shaped by the mount's renderer.
edgeUnauthorised :: MountRenderer -> Response
edgeUnauthorised renderer =
    renderedResponse status401 [] (renderError renderer Nothing "authentication required")

{- The client's forwarded bearer credential, recovered from the request's
@Authorization: Bearer …@ header. 'Nothing' when no bearer credential is present;
the recovered 'Secret' is what is forwarded to the private upstream and compared
against the edge token. The scheme name is matched case-insensitively (npm sends
@Bearer@), the token taken verbatim after it. -}
forwardedToken :: Request -> Maybe Secret
forwardedToken request = do
    (_, raw) <- find ((== hAuthorization) . fst) (requestHeaders request)
    let value = decodeUtf8 raw
        (scheme, rest) = T.break (== ' ') value
    guard (T.toLower scheme == "bearer")
    let token = T.dropWhile (== ' ') rest
    guard (not (T.null token))
    pure (mkSecret token)

-- ── per-origin fetch ──────────────────────────────────────────────────────────

{- A successfully resolved upstream contribution: the parsed packument used to
decide, alongside the raw @Value@ that is edited in place to serve. Pairing them
is the decision-surface\/served-surface contract — every stage carries the raw
@Value@ next to the typed view so losslessness survives the pipeline. -}
data Contribution = Contribution
    { srcProvenance :: Provenance
    , srcInfo :: PackageInfo
    , srcValue :: Value
    }

{- Resolve the private (trusted) upstream origin, __uncached__, forwarding the client's
own credential (the default @passthrough@ posture). Returns its coherent (parsed
packument, raw @Value@) pair — or 'Nothing' when the origin is unavailable or its body
does not parse. A failed fetch is a degraded contribution, not an error: the merge
serves the best-effort union of whatever resolved (partial-upstream availability).

Under @passthrough@ the private upstream is the per-client authority for who may read
what, so its metadata is __not__ shared across clients: it is fetched and parsed on
__every__ request with that client's own forwarded token, so the upstream re-authorises
each client itself. Caching it would key on the base URL alone (no credential
dimension), so within the TTL one client's cache hit would skip the fetch and serve
another client's private document — bypassing the upstream's authorisation. The private
origin is therefore deliberately kept out of the metadata cache; only the anonymous
public origin is cached. (How a non-@passthrough@ strategy can instead share the private
origin safely is the serve-time authorisation it adds — see
@docs\/architecture\/access-model.md@.) -}
fetchPrivateOrigin :: Limits -> Env -> Text -> Maybe Secret -> PackageName -> IO (Maybe (PackageInfo, Value))
fetchPrivateOrigin limits env baseUrl token name = do
    resolved <- tryAny (fetchEntry (envLogEnv env) limits (envPrivateManager env) baseUrl token name)
    pure (either (const Nothing) (Just . unpair) resolved)

{- Resolve the public (gated, anonymous) upstream origin through the metadata cache,
keyed by the origin's base URL as its 'Source', returning its coherent (parsed
packument, raw @Value@) pair — or 'Nothing' when the origin is unavailable or its body
does not parse. A failed fetch is a degraded contribution, not an error.

The public origin is anonymous (no client credential), so a single cached entry serves
every client without crossing any trust boundary — there is no per-client authority
to preserve, only one shared anonymous document. A hit returns the cached pair
(typed view and the exact bytes it was decoded from), so the served document and the
decision over it stay coherent across the TTL, and concurrent resolutions of a
popular package __collapse to one upstream call__. -}
fetchPublicOrigin :: Limits -> Env -> Text -> PackageName -> IO (Maybe (PackageInfo, Value))
fetchPublicOrigin limits env baseUrl name = do
    resolved <- tryAny (resolveMetadata (envMetadataCache env) (Source baseUrl) name (fetchEntry (envLogEnv env) limits (envManager env) baseUrl Nothing name))
    pure (either (const Nothing) (Just . unpair) resolved)

{- Fetch one upstream's full packument (the @Full@ form, for the @time@ map a
publish-age rule needs) and decode it once into both the typed 'PackageInfo' used to
decide and the raw @Value@ edited in place to serve. The two come from the /same/
fetch, so the decision is taken over exactly the bytes served. A body that does not
decode into both throws, so the fetch degrades to a missing contribution rather than
failing the whole request. The injected token is the fetch's credential posture (the
client's for the private origin, 'Nothing' for the anonymous public origin).

The response bounds (security.md invariant 4) are enforced here against the mount's
'Limits' budget, every breach mapped onto the same fail-closed degraded path a parse
failure already takes (the throw that 'fetchPrivateOrigin'\/'fetchPublicOrigin' catch
to 'Nothing') — but each breach is __logged at a 'WarningS'__ first, naming the
package and the ceiling it crossed, so an operator can tell a hostile\/oversized
upstream (or a too-tight cap) from an ordinary parse failure:

\* __body size__ — 'fetchMetadataForm' reads the body through
  'Ecluse.Security.boundedRead' against the budget's @maxBodyBytes@, so an oversized
  body raises a 'ResponseBoundExceeded' from the fetch before it is ever buffered
  whole; it is caught here, logged, and re-raised;
\* __nesting depth__ — 'Ecluse.Security.checkNestingDepth' is applied on the decoded
  @Value@, before it is projected or deeply traversed, so a pathologically nested
  payload is refused before any deep walk. (The structure is already
  /bounded-by-body-size/ at the parser — the @maxBodyBytes@ cap above precedes the
  decode — so this guard bounds the /traversal/ cost of a within-size-but-deep
  document, not an unbounded one.)
\* __version count__ — 'Ecluse.Security.checkVersionCount' is applied after projection,
  before the document threads into rule evaluation, so a version-flood packument is
  refused before per-version rules run.

A pathological document is therefore refused outright, never partially served. -}
fetchEntry :: LogEnv -> Limits -> Manager -> Text -> Maybe Secret -> PackageName -> IO CacheEntry
fetchEntry logEnv limits manager baseUrl token name = do
    -- The body-size breach is raised from the bounded read as a typed
    -- 'ResponseBoundExceeded'; log it (which ceiling, observed-vs-cap) before letting
    -- it propagate to the origin fetcher's @tryAny@, the same fail-closed degrade.
    response <-
        handle (\(ResponseBoundExceeded err) -> logBreach logEnv name err *> throwIO (ResponseBoundExceeded err)) $
            fetchMetadataForm (clientConfig limits manager baseUrl token) Full noValidators name
    -- Decode the body once into the raw @Value@, then project the typed view from
    -- that same parse: aeson decodes bytes to a 'Value' and runs the 'FromJSON'
    -- instance either way, so projecting from the @Value@ reuses the one parse
    -- rather than tokenising a multi-megabyte packument a second time.
    case Aeson.eitherDecodeStrict (responseBody response) of
        -- Bound the nesting depth on the decoded @Value@, before projecting or
        -- traversing, then the version count after projection, before the document
        -- reaches rule evaluation. A breach of either is logged and then degraded
        -- exactly like a parse failure.
        Right value -> case checkNestingDepth limits value of
            Right bounded -> case parsePackageInfoFromValue bounded of
                Right info -> case checkVersionCount limits info of
                    Right boundedInfo -> pure (CacheEntry{entryInfo = boundedInfo, entryRaw = bounded})
                    Left err -> boundBreach err
                Left _ -> decodeFailure
            Left err -> boundBreach err
        Left _ -> decodeFailure
  where
    -- A nesting/version breach: log which ceiling was crossed, then fail closed as a
    -- typed 'ResponseBoundExceeded' (caught by the origin fetcher's @tryAny@).
    boundBreach :: LimitError -> IO CacheEntry
    boundBreach err = logBreach logEnv name err *> throwIO (ResponseBoundExceeded err)

    decodeFailure :: IO CacheEntry
    decodeFailure = logDecodeFailure logEnv name *> throwIO PackumentUndecodable

unpair :: CacheEntry -> (PackageInfo, Value)
unpair entry = (entryInfo entry, entryRaw entry)

{- The one bad-upstream condition the response-bound guards leave silent: the
upstream answered, but its body did not decode into the typed view and raw document
the serve path needs. A (typed) throw, not a stringly one, caught by the origin
fetcher's @tryAny@ and degraded to a missing contribution like a bound breach. -}
data PackumentUndecodable = PackumentUndecodable
    deriving stock (Eq, Show)

instance Exception PackumentUndecodable

{- Log a response-bound breach at 'WarningS' before the contribution is degraded
fail-closed, so an operator can distinguish a bound breach (a hostile\/oversized
upstream, or a too-tight cap) from an ordinary parse failure or upstream outage. The
structured payload names the package, which @bound@ was crossed, and the observed
value against its @cap@ — the high-cardinality identifiers that belong on the log
line, not a metric label. Run through the composition-root 'LogEnv' since the fetch
path is plain 'IO' (off the 'Handler' reader), under the same @ecluse@ namespace the
rest of the stream uses. -}
logBreach :: LogEnv -> PackageName -> LimitError -> IO ()
logBreach logEnv name err =
    runKatipContextT logEnv payload mempty $
        logFM WarningS (ls message)
  where
    -- The package the refused document was for, plus the breach detail, as the
    -- structured @data@ object on the line.
    payload =
        moduleField pipelineModule
            <> sl "package" (renderPackageName name)
            <> sl "bound" boundName
            <> sl "observed" observed
            <> sl "cap" cap

    -- A human-readable one-line summary; the structured fields carry the detail.
    message :: Text
    message = "refused an upstream metadata document: it exceeded the " <> boundName <> " response bound (observed " <> observed <> ", cap " <> cap <> ")"

    -- Which ceiling, the observed value, and the cap — pulled from the typed error so
    -- the three are always consistent with what was enforced.
    boundName :: Text
    observed :: Text
    cap :: Text
    (boundName, observed, cap) = case err of
        BodyTooLarge c -> ("body-size", "over " <> show c <> " bytes", show c <> " bytes")
        TooManyVersions seen c -> ("version-count", show seen, show c)
        TooDeeplyNested c -> ("nesting-depth", "over " <> show c <> " levels", show c <> " levels")

-- The module these serve-path logs are emitted from, tagged on each line via
-- 'Ecluse.Log.moduleField' so the stream can be filtered by emitter.
pipelineModule :: Text
pipelineModule = "Ecluse.Server.Pipeline"

{- Log a parse failure at 'WarningS' — the one bad-upstream condition the response
bound guards leave silent: the upstream answered, but its body did not decode into
the typed view and raw document the serve path needs. Same fail-closed degrade and
the same @module@\/@package@ payload as 'logBreach', so an operator sees an
undecodable upstream distinctly rather than as silence. -}
logDecodeFailure :: LogEnv -> PackageName -> IO ()
logDecodeFailure logEnv name =
    runKatipContextT logEnv payload mempty $
        logFM WarningS (ls message)
  where
    payload = moduleField pipelineModule <> sl "package" (renderPackageName name)
    message :: Text
    message = "refused an upstream metadata document: it did not decode into a usable packument"

{- The npm client config for one fetch: its response-bound budget, 'Manager', base
URL, and injected token (the client's credential for the private origin, 'Nothing'
for the anonymous public origin). The client never originates a token; the authority
model is decided here. The 'Manager' is passed explicitly per fetch — the trusted
'envPrivateManager' for the private upstream, the guarded 'envManager' for the
public\/artifact fetches — so the resolved-IP SSRF recheck applies only to the
untrusted egress. The 'Limits' carries the mount's @maxBodyBytes@ to the bounded
metadata read in 'fetchMetadataForm'. -}
clientConfig :: Limits -> Manager -> Text -> Maybe Secret -> NpmClientConfig
clientConfig limits manager baseUrl token =
    NpmClientConfig
        { npmBaseUrl = baseUrl
        , npmManager = manager
        , npmToken = token
        , npmLimits = limits
        }

-- A trusted (private) source enters the merge unfiltered, its raw @Value@ kept as
-- fetched (tarball URLs are rewritten at assembly, uniformly across sources).
trustedSource :: (PackageInfo, Value) -> Contribution
trustedSource (info, value) = Contribution TrustedSource info value

{- Gate a public-upstream contribution through both rule tiers and the structural
filter, returning the surviving 'Contribution' (if any survived) and the per-version
exclusion outcomes (for the no-survivors status when nothing survives anywhere).

A public origin that did not resolve contributes nothing and no exclusions. A resolved
origin first has the __integrity-presence admission policy__ applied: any version whose
artifact carries no integrity digest of any kind is dropped from the gated set up front
('dropHashless'), so a hashless public version is never listed (a client cannot fetch
it — the artifact gate would refuse it anyway) and never contributes a hashless
fingerprint to the merge. The remaining versions are decided by both tiers
('evalRulesEffectful' — the pure tier first, the effectful tier only where it could
change the outcome), the resulting decisions handed to the agnostic
'filterPlanFromDecisions', and that plan replayed by 'applyFilterPlan' onto the raw
@Value@: 'Filtered' yields a gated 'Contribution' over the surviving versions;
'NoSurvivors' yields no contribution and the per-version 'ServeDecision's, each excluded
version's decision projected (a fail-closed 'Ecluse.Rules.Types.Undecidable' carrying
its transient\/permanent cause, so the no-survivors status is a @503@\/@500@ rather than
a @403@). The dropped hashless versions are projected as 'MissingIntegrity' refusals and
appended to those exclusions, so a packument with /only/ hashless public versions is a
@403@ rather than an empty success. The effectful tier is IO, so this gate is IO; with
no effectful rules configured it reduces exactly to the pure tier.

The gated contribution's typed 'PackageInfo' is __restricted to the survivors__ to
match its filtered @Value@: 'mergePackuments' treats a 'GatedSource' as the
already-filtered set and never re-filters, so feeding it the unfiltered view would
let a denied version reach the merge plan (and skew the reconciled @latest@\/@time@).

The trusted private upstream is exempt: this gate runs on the public path only, so a
hashless private version still enters the merge unfiltered. -}
gatePublic :: PackumentDeps -> EvalContext -> Maybe (PackageInfo, Value) -> IO (Maybe Contribution, [ServeDecision])
gatePublic deps ctx = \case
    Nothing -> pure (Nothing, [])
    Just (info, value) -> do
        let (admissible, hashless) = dropHashless info
            hashlessRefusals = integrityMissing <$ hashless
        decisions <- decideVersions deps ctx admissible
        pure $ case applyFilterPlan (pdMountBaseUrl deps) (filterPlanFromDecisions decisions admissible) value of
            Filtered filtered ->
                (Just (Contribution GatedSource (restrictToSurvivors filtered admissible) filtered), hashlessRefusals)
            NoSurvivors leftover -> (Nothing, projectDecisions admissible leftover <> hashlessRefusals)

{- Apply the integrity-presence admission policy to a public 'PackageInfo', splitting
its versions into the admissible (carrying at least one integrity digest) and the
hashless. A version without any integrity digest cannot be tied to a tamper-evident
fingerprint, so it is inadmissible from an untrusted public upstream — dropped from the
gated set (and from the served listing) rather than served a client could never verify.
Returns the admissible 'PackageInfo' (with @dist-tags@\/@time@ pruned to the kept keys,
exactly as 'restrictToSurvivors' prunes for the rules) and the hashless version keys,
each of which the caller projects to a 'MissingIntegrity' refusal for the no-survivors
status. -}
dropHashless :: PackageInfo -> (PackageInfo, [Text])
dropHashless info =
    ( info
        { infoVersions = admissible
        , infoDistTags = Map.filter ((`Set.member` admissibleKeys) . renderVersion) (infoDistTags info)
        , infoPublishedAt = Map.restrictKeys (infoPublishedAt info) admissibleKeys
        }
    , Map.keys hashless
    )
  where
    -- One pass splits the versions into the admissible (integrity-bearing) and the
    -- hashless, rather than scanning the up-to-100k-version map twice with
    -- complementary 'Map.filter's and re-deriving the admissible map a third time
    -- with 'Map.restrictKeys'. 'hasIntegrity' is evaluated once per version, and
    -- the admissible map is reused directly as the kept 'infoVersions'.
    admissible, hashless :: Map Text PackageDetails
    (admissible, hashless) = Map.partition hasIntegrity (infoVersions info)

    admissibleKeys :: Set Text
    admissibleKeys = Map.keysSet admissible

    -- A version carries an integrity digest iff not all of its artifacts are
    -- hashless. npm publishes exactly one artifact per version, but the check is
    -- over the whole 'NonEmpty' so it holds for a multi-artifact ecosystem too.
    hasIntegrity :: PackageDetails -> Bool
    hasIntegrity = not . all (null . artHashes) . pkgArtifacts

{- Decide every version of a public packument against both rule tiers, keyed by raw
version string (the map 'filterPlanFromDecisions' consumes). Each version is run
through 'evalRulesEffectful', so a needed effectful rule that cannot be consulted
yields a fail-closed 'Ecluse.Rules.Types.Undecidable' decision. With no effectful
rules the per-version call collapses to the pure tier. -}
decideVersions :: PackumentDeps -> EvalContext -> PackageInfo -> IO (Map Text Decision)
decideVersions deps ctx info =
    traverse (evalRulesEffectful ctx (pdRules deps) (pdEffectfulRules deps)) (infoVersions info)

{- Restrict a 'PackageInfo' to the version keys that survived filtering — those
present in the filtered @Value@'s @versions@ — so the typed view handed to the merge
matches the filtered document. @dist-tags@ and @time@ are pruned to the surviving
keys likewise (the merge reconciles them over the union); @dist-tags@ targets absent
from the survivors are dropped. -}
restrictToSurvivors :: Value -> PackageInfo -> PackageInfo
restrictToSurvivors filtered info =
    info
        { infoVersions = Map.restrictKeys (infoVersions info) survivors
        , infoDistTags = Map.filter ((`Set.member` survivors) . renderVersion) (infoDistTags info)
        , infoPublishedAt = Map.restrictKeys (infoPublishedAt info) survivors
        }
  where
    survivors :: Set Text
    survivors =
        maybe mempty (Set.fromList . map Key.toText . KeyMap.keys) (filtered ^? key "versions" . _Object)

{- Project each excluded version's 'Decision' to a 'ServeDecision' for the
no-survivors status. 'applyFilterPlan' carries the plan's decisions in
@versions@-key order ('Data.Map.elems'), so they zip back onto the same-ordered
'PackageDetails' to recover the package\/version each denial is about. -}
projectDecisions :: PackageInfo -> [Decision] -> [ServeDecision]
projectDecisions info =
    zipWith serveDecisionOf (Map.elems (infoVersions info))

-- ── decision-surface replay ───────────────────────────────────────────────────

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
the precedence-winning source's raw @Value@ — the first trusted source if any,
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
    rewriteTarballUrls (pdMountBaseUrl deps) (Object rebuilt)
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
    -- than fabricated — coherence with the plan is preserved by construction.
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

-- ── status when nothing survives ──────────────────────────────────────────────

{- The per-version serve decisions weighed for the no-survivors status: the
public-set exclusions, plus a needed-but-unavailable signal for a private upstream
that did not resolve. A private upstream that failed to resolve contributes a
transient 'ServeDecision' (it may resolve on retry), so a private outage with no
public survivors is a @503@ rather than a @403@ — a needed upstream was
unavailable. -}
collectDecisions :: Maybe (PackageInfo, Value) -> [ServeDecision] -> [ServeDecision]
collectDecisions privResult publicExclusions =
    privateUnavailable <> publicExclusions
  where
    privateUnavailable :: [ServeDecision]
    privateUnavailable = case privResult of
        Just _ -> []
        Nothing -> [Reject (Rejection (Unavailable (WillResolve Nothing)) "a needed upstream was unavailable")]

-- ── response rendering ────────────────────────────────────────────────────────

{- Render the served packument body: @200@ with our own ETag over the served
bytes, or a @304@ when the client's conditional validator already matches. The
ETag is computed over exactly the bytes served, so it changes iff the served
document changes. -}
servePackumentBody :: Request -> ServedBody -> Response
servePackumentBody request body =
    case evaluateOwnETag (requestHeaders request) encoded of
        NotModified etag ->
            jsonResponse (mkStatus 304 "Not Modified") [etagHeader etag] ""
        Modified etag ->
            jsonResponse status200 [etagHeader etag] encoded
  where
    encoded :: LByteString
    encoded = Aeson.encode (servedValue body)

{- Render the no-survivors outcome: the status 'packumentStatus' chose over the
exclusions, with a denial body collecting the reasons. Never a @404@ — the package
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

-- ── response helper ───────────────────────────────────────────────────────────

-- A JSON response with the given status, extra headers, and body. Used for the
-- served packument document itself, which is npm JSON.
jsonResponse :: Status -> ResponseHeaders -> LByteString -> Response
jsonResponse status extra =
    responseLBS status ((hContentType, "application/json") : extra)

-- A response built from a renderer's 'RenderedBody': its content type, then any
-- extra headers, then the rendered bytes.
renderedResponse :: Status -> ResponseHeaders -> RenderedBody -> Response
renderedResponse status extra (RenderedBody contentType body) =
    responseLBS status ((hContentType, contentType) : extra) body

-- ── the tarball handler ───────────────────────────────────────────────────────

{- | Serve a @GET \/{pkg}\/-\/{file}.tgz@ artifact request end to end, over the
request's 'RequestCtx'.

The mount's 'PackumentDeps' and error renderer are read from the matched
'MountBinding'; an unwired mount is the recognised-but-unserved @501@ stub (as for
'servePackument'). With dependencies wired and the edge token (if any) validated,
the artifact is fetched __by the preserved 'Filename'__ — never a name rebuilt from
the coordinate:

* the __private__ upstream is tried first, __uncached__, forwarding the client's
  credential; a @2xx@ streams the bytes through with bounded memory, any other
  status falls through;
* on a private miss the __public__ version metadata is fetched anonymously and
  __that one version__ gated against the rules; an admit streams the public bytes
  __and enqueues a 'MirrorJob'__ (serve-then-enqueue, the enqueue best-effort and
  non-blocking), a reject renders the serve error model
  (@403@\/@503@\/@500@\/@404@) through the mount's renderer.

The public-upstream fetch is always anonymous (the client credential is never sent to the
public upstream); the mirror job carries no credential. The serve path does not
verify @dist.integrity@ (see the module header → "Artifact path").
-}
serveTarball ::
    PackageName ->
    Version ->
    Filename ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
serveTarball name version filename request respond = do
    renderer <- asks (bindingRenderer . ctxMount)
    asks (bindingPackumentDeps . ctxMount) >>= \case
        Nothing -> liftIO (respond (recognisedButUnserved renderer))
        Just deps -> serveTarballWithDeps renderer deps name version filename request respond

-- Serve a tarball once the mount's dependencies are known: edge auth, then the
-- private-hit / public-miss fetches the module header describes. The composition-root
-- 'Env' is read from the request context.
serveTarballWithDeps ::
    MountRenderer ->
    PackumentDeps ->
    PackageName ->
    Version ->
    Filename ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
serveTarballWithDeps renderer deps name version (Filename file) request respond
    | not (edgeAuthorised deps request) = liftIO (respond (edgeUnauthorised renderer))
    | otherwise = do
        env <- asks ctxEnv
        liftIO $ do
            let clientToken = forwardedToken request
            privateHit <- streamPrivateArtifact env deps clientToken name version file respond
            case privateHit of
                Just received -> pure received
                Nothing -> servePublicArtifact env renderer deps name version file respond

{- Stream the artifact from the __trusted__ private upstream at its __authoritative
location__: fetch the private packument (uncached, forwarding the client's credential
— the @passthrough@ private-origin posture), select the requested version's 'Artifact'
by its preserved filename, and fetch that artifact's 'artUrl' directly, rather than
reconstructing @{base}\/{pkg}\/-\/{file}@. Honouring the preserved location is what
lets the private upstream serve a tarball wherever it chooses (a separate files host,
a path the npm convention cannot rebuild). A @2xx@ is streamed through with bounded
memory and yields 'Just'; anything that does not produce a served body — the
packument not resolving, no artifact matching the filename, the host policy refusing
the @artUrl@, an unformable URL, a non-@2xx@ status, or a failure opening the
connection — yields 'Nothing' so the caller falls through to the public origin, the
upstream artifact body never read.

The private upstream is operator-configured and trusted, so its fetch carries no
resolved-IP recheck ('envPrivateManager') and its tarball-host gate is the
'Ecluse.Security.TrustedOrigin' one — __exempt from the internal-range block__, the
serve-path mirror of that unguarded manager (security.md invariant 3): a private
registry on an internal address (e.g. @http:\/\/10.0.0.5\/@) serves its same-host
@dist.tarball@ rather than having it refused while same-host metadata succeeds. The
configured tarball-host policy still gates the @artUrl@ host against the upstream
allowlist and (under the secure default) to the private base URL's own host, so a
same-host private tarball is admitted by default and a cross-host one refused.

A failure that strikes __after__ a @2xx@ has begun streaming is unrecoverable — the
response is already on the wire — so 'streamUpstreamWhen' lets it propagate rather
than reporting a miss: the request fails internally (the connection is torn down)
instead of responding a second time over a half-sent artifact. -}
streamPrivateArtifact ::
    Env ->
    PackumentDeps ->
    Maybe Secret ->
    PackageName ->
    Version ->
    Text ->
    (Response -> IO ResponseReceived) ->
    IO (Maybe ResponseReceived)
streamPrivateArtifact env deps token name version file respond = do
    selected <- selectPrivateArtifact env deps token name version file
    case privateRequestFor =<< selected of
        Just req -> streamUpstreamWhen (envPrivateManager env) req statusIsSuccessful relayArtifact respond
        Nothing -> pure Nothing
  where
    -- Form the honoured-URL request for a selected private artifact, when its host
    -- passes the tarball-host policy and its URL parses. 'Nothing' on either refusal —
    -- a private miss the caller falls through on, never a fabricated reconstruction.
    privateRequestFor :: Artifact -> Maybe HTTP.Request
    privateRequestFor artifact =
        if tarballHostHonoured TrustedOrigin deps (pdPrivateBaseUrl deps) (artUrl artifact)
            then rightToMaybe (artifactRequestByUrl (clientConfig (pdLimits deps) (envPrivateManager env) (pdPrivateBaseUrl deps) token) (artUrl artifact))
            else Nothing

{- Fetch the private packument (uncached, with the client's credential) and select
the requested version's 'Artifact' by its preserved filename. 'Nothing' when the
private origin does not resolve, the version is absent, or no artifact matches the
filename — each a clean private miss the caller falls through on. -}
selectPrivateArtifact :: Env -> PackumentDeps -> Maybe Secret -> PackageName -> Version -> Text -> IO (Maybe Artifact)
selectPrivateArtifact env deps token name version file = do
    resolved <- fetchPrivateOrigin (pdLimits deps) env (pdPrivateBaseUrl deps) token name
    pure $ do
        (info, _value) <- resolved
        details <- Map.lookup (renderVersion version) (infoVersions info)
        artifactFor file details

{- Serve the artifact from the public upstream after a private miss: gate the
single requested version against the rules, and on an admit stream the public bytes
(anonymously) and enqueue a mirror job; on a reject render the serve error model.
The public version metadata is fetched anonymously to decide. -}
servePublicArtifact ::
    Env ->
    MountRenderer ->
    PackumentDeps ->
    PackageName ->
    Version ->
    Text ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
servePublicArtifact env renderer deps name version file respond = do
    gated <- gatePublicVersion env deps name version file
    case gated of
        Admitted artifact -> streamPublicArtifact env renderer deps name version artifact respond
        Refused decision -> respond (artifactError renderer deps (artifactStatus decision) decision)

{- The outcome of gating a single requested artifact on the public path: either the
chosen 'Artifact' to fetch, or the serve decision the error model renders. The
admit carries the artifact so the stream step honours its 'artUrl' rather than
re-deciding or reconstructing the location. -}
data PublicArtifactGate
    = -- | The version was admitted; carries the artifact selected by filename.
      Admitted Artifact
    | -- | The version was refused (policy denial, upstream outage, or absence).
      Refused ServeDecision

{- Gate the single requested version against both rule tiers and select its
artifact, returning the gate outcome. The public packument is fetched anonymously
and parsed; the requested version's 'PackageDetails' is evaluated through
'evalRulesEffectful' (the same engine the packument path gates with). On an admit the
artifact matching the requested filename is selected ('artifactFor'); a filename
absent from an otherwise-admitted version is a forwarded miss, the same @404@ as an
absent version.

The refusal causes the error model maps: a version (or file) absent from the public
metadata is a genuine miss (a @404@ forwarded absence, projected as 'Unavailable'
'WontResolve' only to carry a non-admit — the status is overridden to @404@ in
'artifactError'); a metadata fetch that fails is a transient upstream outage (@503@);
a present version is decided by the rules, where a needed effectful rule that cannot
be consulted fail-closes to an 'Unavailable' @503@\/@500@. -}
gatePublicVersion :: Env -> PackumentDeps -> PackageName -> Version -> Text -> IO PublicArtifactGate
gatePublicVersion env deps name version file = do
    evalCtx <- EvalContext <$> pdNow deps
    fetchPublicOrigin (pdLimits deps) env (pdPublicBaseUrl deps) name >>= \case
        Nothing -> pure (Refused upstreamUnavailable)
        Just (info, _value) -> case Map.lookup (renderVersion version) (infoVersions info) of
            Nothing -> pure (Refused versionAbsent)
            Just details -> gateVersion evalCtx deps file details

{- Project a single version's two-tier rule decision to a gate outcome, selecting the
artifact by filename on an admit. A denied version is 'Refused' with its decision; an
admitted version whose requested filename matches no artifact is a forwarded miss
('versionAbsent', rendered @404@).

The __integrity-presence admission policy__ is enforced here, after the rules admit
and the artifact is selected: a public version whose selected artifact carries no
integrity digest of any kind is inadmissible ('integrityMissing', rendered @403@) and
refused outright, never fetched. This is the public path; the trusted private upstream
('selectPrivateArtifact') is exempt and never reaches this gate. -}
gateVersion :: EvalContext -> PackumentDeps -> Text -> PackageDetails -> IO PublicArtifactGate
gateVersion ctx deps file details = do
    decision <- serveDecisionOf details <$> evalRulesEffectful ctx (pdRules deps) (pdEffectfulRules deps) details
    pure $ case decision of
        Admit -> maybe (Refused versionAbsent) admitWithIntegrity (artifactFor file details)
        Reject _ -> Refused decision
  where
    -- A rule-admitted artifact is served only if it carries an integrity digest;
    -- one with empty 'artHashes' is refused by the integrity-presence policy.
    admitWithIntegrity :: Artifact -> PublicArtifactGate
    admitWithIntegrity artifact
        | null (artHashes artifact) = Refused integrityMissing
        | otherwise = Admitted artifact

-- A transient public-upstream outage: a 'WillResolve' rejection (→ @503@).
upstreamUnavailable :: ServeDecision
upstreamUnavailable =
    Reject (Rejection (Unavailable (WillResolve Nothing)) "the upstream registry was unavailable")

{- A version not present in the public metadata: a non-admit carrying a
'WontResolve' cause, whose status 'artifactError' overrides to a @404@ forwarded
miss (the package may exist, this version does not). -}
versionAbsent :: ServeDecision
versionAbsent =
    Reject (Rejection (Unavailable WontResolve) "the requested version was not found upstream")

{- A public version refused by the integrity-presence admission policy: its selected
artifact carries no integrity digest of any kind, so it cannot be tied to a
tamper-evident fingerprint. A deliberate deny-by-default policy refusal ('MissingIntegrity',
rendered @403@), not a rule denial and not a retryable outage. The trusted private
upstream is exempt, so this never arises on the private path. -}
integrityMissing :: ServeDecision
integrityMissing =
    Reject (Rejection MissingIntegrity "this version carries no integrity digest and cannot be served from a public upstream")

{- Stream the artifact from the public upstream at its __authoritative location__,
__anonymously__ (the client credential is never sent to the public upstream), and —
__after__ the response is begun — enqueue a best-effort mirror job. The chosen
'Artifact''s 'artUrl' is honoured directly rather than reconstructed; the
tarball-host policy gates whether that location may be fetched (the public packument
host is the reference), and the resolved-IP recheck on 'envManager' is the
defence-in-depth backstop. A host the policy refuses is the @403@ policy-denial path;
an unformable URL is the internal-error path.

The fetch keeps the open phase distinct from the committed stream, the same split the
private origin uses: opening the connection is the recoverable phase, so a transient
network failure or a connection-time 'Ecluse.Security.BlockedTarget' (the resolved-IP
recheck refusing an allowlisted name that resolves internal) yields no committed
response and is rendered as the transient upstream-unavailable @503@ through the
mount's renderer — not left to escape as a bare @500@. Any upstream status is relayed
verbatim (the @accept@ predicate is total); only a failure __after__ the stream is
committed propagates, the connection torn down as it unwinds, so a half-sent artifact
is never followed by a second response. The mirror enqueue runs only on the committed
path, after the response is begun. -}
streamPublicArtifact ::
    Env ->
    MountRenderer ->
    PackumentDeps ->
    PackageName ->
    Version ->
    Artifact ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
streamPublicArtifact env renderer deps name version artifact respond =
    if tarballHostHonoured UntrustedOrigin deps (pdPublicBaseUrl deps) (artUrl artifact)
        then case artifactRequestByUrl (clientConfig (pdLimits deps) (envManager env) (pdPublicBaseUrl deps) Nothing) (artUrl artifact) of
            Right req ->
                streamUpstreamWhen (envManager env) req (const True) relayArtifact respond >>= \case
                    Just received -> do
                        enqueueMirror env deps name version (artUrl artifact)
                        pure received
                    Nothing -> respond (artifactError renderer deps (artifactStatus upstreamUnavailable) upstreamUnavailable)
            Left _ -> respond internalArtifactError
        else respond crossHostRefused

{- Enqueue a demand-driven mirror job for an admitted artifact, __best-effort__: it
runs after the client response is begun and any failure is swallowed, so a queue
outage never fails or delays the serve. The job names the artifact's authoritative
URL (the same location the public fetch targeted) and the mount's mirror target; it
carries no credential (the worker mints its own). -}
enqueueMirror :: Env -> PackumentDeps -> PackageName -> Version -> Text -> IO ()
enqueueMirror env deps name version artifactUrl =
    void . tryAny . enqueue (envQueue env) $
        MirrorJob
            { jobPackage = name
            , jobVersion = version
            , jobArtifactUrl = artifactUrl
            , jobMirrorTarget = pdMirrorTarget deps
            }

-- ── the egress gate at the serve seam ─────────────────────────────────────────

{- Whether an artifact's authoritative @url@ may be fetched, given the origin's trust,
the mount's tarball-host policy, and the host that served the packument it came from.
Connects the pure 'tarballHostAllowed' at the serve seam: the @url@'s host must be on
the upstream allowlist and — under the secure-default
'Ecluse.Security.SameHostAsPackument' — equal to the packument host; the opt-in
'Ecluse.Security.AnyAllowlistedHost' relaxes that last clause to any allowlisted host.
This is the policy half of the @dist.tarball@ defence; for an
'Ecluse.Security.UntrustedOrigin' the resolved-IP recheck on the guarded manager is
its connection-time backstop (an allowlisted name that resolves to an internal address
is still refused there).

The internal-range block is __origin-aware__, mirroring the connection layer's trusted
vs guarded manager split: an 'Ecluse.Security.UntrustedOrigin' (the public path) is
gated against it (subject to the empty opt-in here, the composition root's secure
default), while an 'Ecluse.Security.TrustedOrigin' (the operator-configured private
upstream) is exempt — a private registry may legitimately live on an internal address,
just as its connections use the unguarded 'Ecluse.Security.Egress.newTrustedTlsManager'
(security.md invariant 3). The allowlist and same-host clauses still gate the trusted
origin identically. -}
tarballHostHonoured :: Origin -> PackumentDeps -> Text -> Text -> Bool
tarballHostHonoured origin deps packumentBaseUrl artifactUrl =
    tarballHostAllowed
        origin
        (pdTarballHostPolicy deps)
        (upstreamAllowlist deps)
        (pdAllowedInternalHosts deps)
        (hostAddress packumentBaseUrl)
        (hostAddress artifactUrl)

{- The host allowlist on the serve path: the bare hosts of the mount's configured
upstreams — the public and private upstream base URLs and the mirror target. These
are exactly the hosts the proxy is configured to talk to, so an artifact @url@ on any
other host is off the allowlist regardless of policy (security.md invariant 2: a
@dist.tarball@ host off the configured upstreams is refused). -}
upstreamAllowlist :: PackumentDeps -> LoweredHostSet
upstreamAllowlist deps =
    lowerCaseHosts . fromList $
        map hostAddress [pdPublicBaseUrl deps, pdPrivateBaseUrl deps, pdMirrorTarget deps]

{- Select the artifact a request's filename names from a version's distribution
files. npm has exactly one artifact per version, so the match is the single file; a
many-per-version ecosystem (PyPI) would select the wheel\/sdist whose filename the
client requested. 'Nothing' when no artifact carries the requested filename — a
forwarded miss, never a fabricated location. -}
artifactFor :: Text -> PackageDetails -> Maybe Artifact
artifactFor file details =
    find ((== file) . artFilename) (pkgArtifacts details)

{- A @403@ for an artifact whose authoritative @url@ the tarball-host policy refuses:
a cross-host @dist.tarball@ under the secure-default 'Ecluse.Security.SameHostAsPackument',
or a host off the upstream allowlist. A policy denial, not a serve outcome the rules
produced — the same @403@ surface a rule denial renders, with a fixed reason. -}
crossHostRefused :: Response
crossHostRefused =
    responseLBS (mkStatus 403 "Forbidden") [(hContentType, "application/json")] "{\"error\":\"the upstream artifact host is not permitted by the tarball-host policy\"}"

{- The relay for an artifact stream: forward the upstream status and headers,
dropping only the hop-by-hop framing headers (@Transfer-Encoding@, @Connection@)
whose values describe the upstream hop, not the artifact. The body is opaque binary
streamed verbatim, so the content headers (type, length, encoding) and the
upstream's @ETag@ pass through unchanged — the client verifies the artifact's own
@dist.integrity@ over exactly these bytes. -}
relayArtifact :: Status -> ResponseHeaders -> (Status, ResponseHeaders)
relayArtifact status headers =
    (status, filter (not . isHopByHop . fst) headers)
  where
    isHopByHop name = name == "Transfer-Encoding" || name == "Connection"

{- Render a non-admit artifact outcome as the serve error model: @403@ for a policy
denial, @503@ for a transient upstream unavailability, @404@ for a forwarded
upstream miss (the requested version is absent), @500@ otherwise. The body is shaped
by the mount's renderer; a transient status carries no suggested delay here (the
single-artifact path has none to offer). A @404@ is the version-absent miss, which
'gatePublicVersion' flags as a 'WontResolve' rejection — the only such cause on this
path — so it is mapped to @404@ rather than the @500@ a 'WontResolve' would
otherwise render. -}
artifactError :: MountRenderer -> PackumentDeps -> ArtifactStatus -> ServeDecision -> Response
artifactError renderer deps status decision =
    renderedResponse (toStatus actualStatus) [] (renderError renderer (pdHelp deps) message)
  where
    -- The version-absent miss is carried as a 'WontResolve' rejection but rendered
    -- as a forwarded @404@, not the @500@ a generic 'WontResolve' maps to.
    actualStatus :: ArtifactStatus
    actualStatus = if isVersionAbsent then NotFound else status

    isVersionAbsent :: Bool
    isVersionAbsent = case decision of
        Reject (Rejection (Unavailable WontResolve) _) -> True
        _ -> False

    toStatus :: ArtifactStatus -> Status
    toStatus s = mkStatus (artifactStatusCode s) (statusReason s)

    statusReason :: ArtifactStatus -> ByteString
    statusReason = \case
        Ok -> "OK"
        Forbidden -> "Forbidden"
        Unavailable'{} -> "Service Unavailable"
        ServerError -> "Internal Server Error"
        NotFound -> "Not Found"

    message :: Text
    message = case decision of
        Admit -> "the artifact is available"
        Reject rej -> rejectionMessage rej

{- A @500@ for an unformable upstream artifact URL — a configuration fault, not a
serve decision. The package segment and filename are already known-safe, so this is
reachable only on a misconfigured base URL; it is the internal-error tier, distinct
from the rule\/upstream outcomes 'artifactError' renders. -}
internalArtifactError :: Response
internalArtifactError =
    responseLBS (mkStatus 500 "Internal Server Error") [(hContentType, "application/json")] "{\"error\":\"could not form the upstream artifact URL\"}"
