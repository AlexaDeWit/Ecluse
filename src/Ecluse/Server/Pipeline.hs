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
public registry would be a credential disclosure, so the public leg is built with no
token at all. Under @passthrough@ the client's own credential is additionally
__forwarded verbatim to the private upstream__, which is the authority for who may
read what. The two legs are fetched concurrently, each with its own credential
posture; nothing shares a token across the trust split.

Because @passthrough@ makes the private upstream the __per-client authority__, its
metadata is __not cached across clients__ here: the private leg fetches and parses on
every request with that client's own credential, so the upstream re-authorises each
client itself, and only the anonymous public leg is cached (one shared document, no
per-client authority to preserve). Caching the private leg keyed by base URL alone
would let one client's cached entry serve another client's private document within the
TTL, bypassing the upstream's authorisation — a cross-client disclosure. (Other
strategies make the private leg shareable by authorising each serve differently; the
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

This is, for now, the __npm__ packument pipeline: it reaches for the npm registry
client, projection, and structural filter directly, so it is the one
@Ecluse.Server.*@ module that depends on a concrete adapter. The coupling is
expedient, not intended — the agnostic handles that would let it dispatch through an
adapter (a per-adapter router, and an ecosystem-neutral filter\/projection) are
tracked as separate work, after which a second ecosystem would reuse this
orchestration unchanged.

== Artifact path

The tarball handler ('serveTarball') is the demand-driven artifact relay. It
fetches __by the preserved on-the-wire filename__ the route parsed (not a name
rebuilt from the coordinate), so the bytes are addressed exactly as the client
requested them. The private leg is tried first, __uncached__, forwarding the
client's credential; a hit streams the artifact through with __bounded memory__
(the @withResponse@\/@responseStream@ relay, never a buffering fetch), a miss falls
through. The public leg is anonymous: it gates __that one version__ against the
rules (the same machinery the packument path gates the whole set with), and on an
admit __streams the public bytes and enqueues a 'Ecluse.Queue.MirrorJob'__ for the
worker to back-fill the mirror target; on a reject it renders the serve error model
(@403@\/@503@\/@500@) through the mount's renderer. The enqueue is
__serve-then-enqueue, best-effort and non-blocking__: the artifact reaches the
client first, and an enqueue failure is swallowed rather than failing or delaying
the response. Mirroring is __demand-driven__ — a job is enqueued only here, on a
tarball-path admit, never when a packument is filtered. The serve path does __not__
verify @dist.integrity@; the client checks the artifact's own hash and the worker
re-verifies before publishing.
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
import Network.HTTP.Types (ResponseHeaders, Status, hAuthorization, hContentType, mkStatus, status200, status401, status501, statusIsSuccessful)
import Network.Wai (Request, Response, ResponseReceived, requestHeaders, responseLBS)
import UnliftIO (concurrently)
import UnliftIO.Exception (throwString, tryAny)

import Ecluse.Credential (Secret, mkSecret)
import Ecluse.Env (Env (envManager, envMetadataCache, envQueue))
import Ecluse.Package (PackageDetails, PackageInfo (infoDistTags, infoPublishedAt, infoVersions), PackageName)
import Ecluse.Package.Filter (filterPlan)
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
    artifactFileUrl,
    artifactRequestByFile,
    fetchMetadataForm,
    noValidators,
 )
import Ecluse.Registry.Npm.Filter (FilterResult (Filtered, NoSurvivors), applyFilterPlan, rewriteTarballUrls)
import Ecluse.Registry.Npm.Project (parsePackageInfo)
import Ecluse.Rules (evalRules)
import Ecluse.Rules.Types (Decision, EvalContext (EvalContext), PrecededRule)
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
    RejectReason (Unavailable),
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
import Ecluse.Server.Stream (streamUpstream, streamUpstreamWhen)
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
__concurrently__ — the client's credential forwarded to the private leg, the public
leg anonymous — each parse failure or unavailable upstream degrading to a missing
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
                    (fetchPrivateLeg env (pdPrivateBaseUrl deps) clientToken name)
                    (fetchPublicLeg env (pdPublicBaseUrl deps) name)
            let private = trustedSource <$> privResult
                (public, publicExclusions) = gatePublic deps evalCtx pubResult
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

-- ── per-upstream fetch ────────────────────────────────────────────────────────

{- A successfully resolved upstream contribution: the parsed packument used to
decide, alongside the raw @Value@ that is edited in place to serve. Pairing them
is the decision-surface\/served-surface contract — every stage carries the raw
@Value@ next to the typed view so losslessness survives the pipeline. -}
data Contribution = Contribution
    { srcProvenance :: Provenance
    , srcInfo :: PackageInfo
    , srcValue :: Value
    }

{- Resolve the private (trusted) upstream leg, __uncached__, forwarding the client's
own credential (the default @passthrough@ posture). Returns its coherent (parsed
packument, raw @Value@) pair — or 'Nothing' when the leg is unavailable or its body
does not parse. A failed leg is a degraded contribution, not an error: the merge
serves the best-effort union of whatever resolved (partial-upstream availability).

Under @passthrough@ the private upstream is the per-client authority for who may read
what, so its metadata is __not__ shared across clients: this leg fetches and parses on
__every__ request with that client's own forwarded token, so the upstream re-authorises
each client itself. Caching it would key on the base URL alone (no credential
dimension), so within the TTL one client's cache hit would skip the fetch and serve
another client's private document — bypassing the upstream's authorisation. The leg is
therefore deliberately kept out of the metadata cache; only the anonymous public leg
is cached. (How a non-@passthrough@ strategy can instead share this leg safely is the
serve-time authorisation it adds — see @docs\/architecture\/access-model.md@.) -}
fetchPrivateLeg :: Env -> Text -> Maybe Secret -> PackageName -> IO (Maybe (PackageInfo, Value))
fetchPrivateLeg env baseUrl token name = do
    resolved <- tryAny (fetchEntry env baseUrl token name)
    pure (either (const Nothing) (Just . unpair) resolved)

{- Resolve the public (gated, anonymous) upstream leg through the metadata cache,
keyed by the leg's base URL as its 'Source', returning its coherent (parsed
packument, raw @Value@) pair — or 'Nothing' when the leg is unavailable or its body
does not parse. A failed leg is a degraded contribution, not an error.

The public leg is anonymous (no client credential), so a single cached entry serves
every client without crossing any trust boundary — there is no per-client authority
to preserve, only one shared anonymous document. A hit returns the cached pair
(typed view and the exact bytes it was decoded from), so the served document and the
decision over it stay coherent across the TTL, and concurrent resolutions of a
popular package __collapse to one upstream call__. -}
fetchPublicLeg :: Env -> Text -> PackageName -> IO (Maybe (PackageInfo, Value))
fetchPublicLeg env baseUrl name = do
    resolved <- tryAny (resolveMetadata (envMetadataCache env) (Source baseUrl) name (fetchEntry env baseUrl Nothing name))
    pure (either (const Nothing) (Just . unpair) resolved)

{- Fetch one upstream leg's full packument (the @Full@ form, for the @time@ map a
publish-age rule needs) and decode it once into both the typed 'PackageInfo' used to
decide and the raw @Value@ edited in place to serve. The two come from the /same/
fetch, so the decision is taken over exactly the bytes served. A body that does not
decode into both throws, so the leg degrades to a missing contribution rather than
failing the whole request. The injected token is the leg's credential posture (the
client's for the private leg, 'Nothing' for the anonymous public leg). -}
fetchEntry :: Env -> Text -> Maybe Secret -> PackageName -> IO CacheEntry
fetchEntry env baseUrl token name = do
    response <- fetchMetadataForm (clientConfig env baseUrl token) Full noValidators name
    case (parsePackageInfo response, Aeson.eitherDecodeStrict (responseBody response)) of
        (Right info, Right value) -> pure (CacheEntry{entryInfo = info, entryRaw = value})
        _ -> throwString "packument did not decode into both a typed view and a raw document"

unpair :: CacheEntry -> (PackageInfo, Value)
unpair entry = (entryInfo entry, entryRaw entry)

{- The npm client config for one leg: the shared 'Manager', the leg's base URL,
and the leg's injected token (the client's credential for the private leg,
'Nothing' for the anonymous public leg). The client never originates a token; the
authority model is decided here. -}
clientConfig :: Env -> Text -> Maybe Secret -> NpmClientConfig
clientConfig env baseUrl token =
    NpmClientConfig
        { npmBaseUrl = baseUrl
        , npmManager = envManager env
        , npmToken = token
        }

-- A trusted (private) source enters the merge unfiltered, its raw @Value@ kept as
-- fetched (tarball URLs are rewritten at assembly, uniformly across sources).
trustedSource :: (PackageInfo, Value) -> Contribution
trustedSource (info, value) = Contribution TrustedSource info value

{- Gate a public-upstream contribution through the rules and the structural
filter, returning the surviving 'Contribution' (if any survived) and the per-version
exclusion outcomes (for the no-survivors status when nothing survives anywhere).

A public leg that did not resolve contributes nothing and no exclusions. A
resolved leg is decided by the agnostic 'filterPlan' (over the typed
'PackageInfo') and that plan replayed by 'applyFilterPlan' onto its raw @Value@:
'Filtered' yields a gated 'Contribution' over the surviving versions; 'NoSurvivors'
yields no contribution and the per-version 'ServeDecision's (each excluded
version's decision projected, paired with its 'PackageDetails' for the denial
message).

The gated contribution's typed 'PackageInfo' is __restricted to the survivors__ to
match its filtered @Value@: 'mergePackuments' treats a 'GatedSource' as the
already-filtered set and never re-filters, so feeding it the unfiltered view would
let a denied version reach the merge plan (and skew the reconciled @latest@\/@time@). -}
gatePublic :: PackumentDeps -> EvalContext -> Maybe (PackageInfo, Value) -> (Maybe Contribution, [ServeDecision])
gatePublic deps ctx = \case
    Nothing -> (Nothing, [])
    Just (info, value) ->
        case applyFilterPlan (pdMountBaseUrl deps) (filterPlan ctx (pdRules deps) info) value of
            Filtered filtered ->
                (Just (Contribution GatedSource (restrictToSurvivors filtered info) filtered), [])
            NoSurvivors decisions -> (Nothing, projectDecisions info decisions)

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
    survivors = case objectAt "versions" filtered of
        Just versionsObject -> Set.fromList (map Key.toText (KeyMap.keys versionsObject))
        Nothing -> mempty

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

    versionObjectFrom :: SourceId -> Text -> Maybe Value
    versionObjectFrom sid version = do
        source <- Map.lookup sid bySource
        versionsObject <- objectAt "versions" (srcValue source)
        KeyMap.lookup (Key.fromText version) versionsObject

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

    bookkeepingTime :: KeyMap Value
    bookkeepingTime =
        case objectAt "time" base of
            Just timeObject -> KeyMap.filterWithKey (\k _ -> Key.toText k `elem` timeBookkeepingKeys) timeObject
            Nothing -> mempty

-- The non-version keys an npm @time@ object carries that must be relayed unchanged.
timeBookkeepingKeys :: [Text]
timeBookkeepingKeys = ["created", "modified"]

-- The object at @key@ within a JSON object, when it is itself an object.
objectAt :: Text -> Value -> Maybe (KeyMap Value)
objectAt key = \case
    Object o -> case KeyMap.lookup (Key.fromText key) o of
        Just (Object inner) -> Just inner
        _ -> Nothing
    _ -> Nothing

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

The public leg is always anonymous (the client credential is never sent to the
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
-- private-hit / public-miss legs the module header describes. The composition-root
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
            privateHit <- streamPrivateArtifact env deps clientToken name file respond
            case privateHit of
                Just received -> pure received
                Nothing -> servePublicArtifact env renderer deps name version file respond

{- Stream the artifact from the private upstream by its preserved filename,
forwarding the client's credential (the @passthrough@ private leg, uncached). A
@2xx@ is streamed through with bounded memory and yields 'Just'; a non-@2xx@ status,
an unformable URL, or a failure opening the connection yields 'Nothing' so the
caller falls through to the public leg, the upstream body never read.

A failure that strikes __after__ a @2xx@ has begun streaming is unrecoverable — the
response is already on the wire — so 'streamUpstreamWhen' lets it propagate rather
than reporting a miss: the request fails internally (the connection is torn down)
instead of responding a second time over a half-sent artifact. -}
streamPrivateArtifact ::
    Env ->
    PackumentDeps ->
    Maybe Secret ->
    PackageName ->
    Text ->
    (Response -> IO ResponseReceived) ->
    IO (Maybe ResponseReceived)
streamPrivateArtifact env deps token name file respond =
    case artifactRequestByFile (clientConfig env (pdPrivateBaseUrl deps) token) name file of
        Left _ -> pure Nothing
        Right req -> streamUpstreamWhen (envManager env) req statusIsSuccessful relayArtifact respond

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
    decision <- gatePublicVersion env deps name version
    case artifactStatus decision of
        Ok -> streamPublicArtifact env deps name version file respond
        status -> respond (artifactError renderer deps status decision)

{- Gate the single requested version against the mount's rules, returning its
serve outcome. The public packument is fetched anonymously and parsed; the
requested version's 'PackageDetails' is evaluated against the rule set
('evalRules', the same engine the packument path gates with) and projected to a
'ServeDecision'.

The outcome distinguishes the refusal causes the error model maps: a version
absent from the public metadata is a genuine miss (a @404@ forwarded upstream
absence, projected as 'Unavailable' 'WontResolve' only to carry a non-admit — the
status is overridden to @404@ in 'artifactError'); a metadata fetch that fails is a
transient upstream outage (@503@); a present version is decided by the rules. -}
gatePublicVersion :: Env -> PackumentDeps -> PackageName -> Version -> IO ServeDecision
gatePublicVersion env deps name version = do
    evalCtx <- EvalContext <$> pdNow deps
    fetched <- tryAny (fetchPublicLeg env (pdPublicBaseUrl deps) name)
    pure $ case fetched of
        Left _ -> upstreamUnavailable
        Right Nothing -> upstreamUnavailable
        Right (Just (info, _value)) -> case Map.lookup (renderVersion version) (infoVersions info) of
            Nothing -> versionAbsent
            Just details -> gateVersion evalCtx (pdRules deps) details

-- Project a single version's rule decision to a serve outcome.
gateVersion :: EvalContext -> [PrecededRule] -> PackageDetails -> ServeDecision
gateVersion ctx rules details = serveDecisionOf details (evalRules ctx rules details)

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

{- Stream the artifact from the public upstream by its preserved filename,
__anonymously__ (the client credential is never sent to the public upstream), and —
__after__ the response is begun — enqueue a best-effort mirror job. An unformable
URL is the internal-error path. -}
streamPublicArtifact ::
    Env ->
    PackumentDeps ->
    PackageName ->
    Version ->
    Text ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
streamPublicArtifact env deps name version file respond =
    case artifactRequestByFile (clientConfig env (pdPublicBaseUrl deps) Nothing) name file of
        Left _ -> respond internalArtifactError
        Right req -> do
            received <- streamUpstream (envManager env) req relayArtifact respond
            enqueueMirror env deps name version file
            pure received

{- Enqueue a demand-driven mirror job for an admitted artifact, __best-effort__: it
runs after the client response is begun and any failure is swallowed, so a queue
outage never fails or delays the serve. The job names the public artifact URL (the
same location the public fetch targeted) and the mount's mirror target; it carries
no credential (the worker mints its own). An unformable artifact URL skips the
enqueue rather than failing the served response. -}
enqueueMirror :: Env -> PackumentDeps -> PackageName -> Version -> Text -> IO ()
enqueueMirror env deps name version file =
    case artifactFileUrl (pdPublicBaseUrl deps) name file of
        Left _ -> pure ()
        Right artifactUrl ->
            void . tryAny . enqueue (envQueue env) $
                MirrorJob
                    { jobPackage = name
                    , jobVersion = version
                    , jobArtifactUrl = artifactUrl
                    , jobMirrorTarget = pdMirrorTarget deps
                    }

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
