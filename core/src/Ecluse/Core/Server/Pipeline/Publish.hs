{- | The serve paths behind the package routes: the packument merge behind
@GET \/{pkg}@ and the artifact relay behind @GET \/{pkg}\/-\/{file}.tgz@.

This is the data-plane handler module. It composes the
slices that decide /what/ to serve — the registry client
("Ecluse.Core.Registry.Npm"), the per-version rules ("Ecluse.Core.Rules"), the structural
filter ("Ecluse.Core.Registry.Npm.Filter"), the cross-upstream merge
("Ecluse.Core.Package.Merge"), the metadata cache ("Ecluse.Core.Server.Cache"), the
own-ETag conditional ("Ecluse.Core.Server.Conditional"), and the serve-outcome status
("Ecluse.Core.Server.Response") — into one action in the
'Ecluse.Core.Server.Context.Handler' reader, reading its mount's serve dependencies and
the request runtime 'Ecluse.Core.Server.Context.ServeRuntime' from the request's
'Ecluse.Core.Server.Context.RequestCtx'.

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
serve-path module that depends on a concrete adapter. The coupling is
expedient, not intended — the agnostic handles that would let it dispatch through an
adapter (a per-adapter router, and an ecosystem-neutral filter\/projection) would
let a second ecosystem reuse this orchestration unchanged.

== Artifact path

The tarball handler ('serveTarball') is the demand-driven artifact relay. Its two legs
locate the tarball differently, by the trust of their origin.

The __private__ leg is a __conventional stable read__: it fetches the tarball at
@{pdPrivateBaseUrl}\/{pkg}\/-\/{file}@ ('artifactRequestByFile'), addressed by the
client's requested filename, __without a private-packument fetch__ — the stable,
cacheable shape an @npm ci@ install issues, so a worst-case lockfile fan-out pays one
artifact round-trip per tarball rather than a packument fetch+decode per tarball it
would only discard. The request __forwards the client's credential__ over the
__trusted__ manager, attached at the single bearer-attach point
('Ecluse.Core.Registry.Npm.withToken'), which pins @redirectCount = 0@: this
credential-bearing read __never follows a redirect__ (a private CDN @302@ is returned to
the serve path, not chased with the bearer). The constructed URL is on the private base
host, so the 'Ecluse.Core.Security.TrustedOrigin' tarball-host gate is satisfied
__same-host__, and the trusted origin is exempt from the internal-range block (a private
registry on an internal address still serves). A @2xx@ streams the artifact through with
__bounded memory__ (the @withResponse@\/@responseStream@ relay, never a buffering fetch)
and __answers the request__; a non-@2xx@ status or a connection failure is a __clean
miss__ that falls through to the public leg.

The private leg applies __no serve-time integrity floor__. An established version pinned
in a consumer's lockfile and served from an operator-__trusted__ private registry is
fast-tracked: its bytes are still verified __client-side by @npm@__ (against the
@dist.integrity@ it resolved over the packument route) and by the __mirror worker__ on
ingestion, so fast-tracking gives up only the proactive "refuse weak-integrity" stance,
not tamper-evidence. A consequence of the conventional read: a private upstream that
serves its tarball __off the conventional @\/-\/@ path__ (a separate files host, a signed
CDN URL the convention cannot rebuild) is not reached by this leg, so it is a private
miss that falls through to the public origin.

The __public__ leg honours the __authoritative upstream location__ — the
@Artifact.artUrl@ the projection preserved from the gated version's @dist.tarball@,
selected by the requested filename — rather than reconstructing the conventional path,
so the proxy can front a public registry that serves its artifacts from a separate host
or an off-convention path (a CDN\/files host, a signed URL). That location is gated, not
trusted: it is fetched only when the tarball-host policy
('Ecluse.Core.Security.tarballHostAllowed', per @ECLUSE_RESPECT_UPSTREAM_TARBALL_HOST@)
admits its host (the default refuses a cross-host @dist.tarball@), and the untrusted
egress is https-only with certificate validation. The public leg is anonymous: it
gates __that one version__ against the rules (the same machinery the packument path
gates the whole set with) and selects the artifact, and on an admit __streams the public
bytes from @artUrl@ and enqueues a 'Ecluse.Core.Queue.MirrorJob'__ (naming that
authoritative URL) for the worker to back-fill the mirror target; on a reject —
including a host the tarball-host policy refuses — it renders the serve error model
(@403@\/@503@\/@500@\/@404@) through the mount's renderer. The enqueue is
__serve-then-enqueue, best-effort and non-blocking__: the artifact reaches the client
first, and an enqueue failure is swallowed rather than failing or delaying the response.
Mirroring is __demand-driven__ — a job is enqueued only here, on a tarball-path admit,
never when a packument is filtered. The serve path does __not__ verify @dist.integrity@;
the client checks the artifact's own hash and the worker re-verifies before publishing.

An artifact is a __pass-through__ body — served byte-identical to upstream's — so its
conditional-GET handling __relays__ rather than computing an own ETag (see
@docs\/architecture\/web-layer.md@ → "Middleware and helper libraries", and contrast
the merged-packument own-ETag path): the client's @If-None-Match@\/@If-Modified-Since@
are forwarded onto the upstream artifact request on __both__ legs ('forwardValidators'),
and an upstream @304 Not Modified@ is relayed straight back to the client as a bodiless
@304@ ('isNotModified' via the relay's accept predicate) rather than re-downloading the
tarball — the cheap freshness check on the hot artifact path.
-}
module Ecluse.Core.Server.Pipeline.Publish (
    -- * The first-party publish handler
    servePublish,
) where

import Data.Aeson (Value (String))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS

import Lens.Micro ((^?))
import Lens.Micro.Aeson (key, _Object)
import Network.HTTP.Types (mkStatus, status403, status405, status500, status502)
import Network.Wai (Request, Response, ResponseReceived, consumeRequestBodyStrict)
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Package (
    PackageName,
    Scope,
    pkgNamespace,
    renderPackageName,
 )
import Ecluse.Core.Registry (PublishRelayResponse (PublishRelayResponse), UrlFormationError)
import Ecluse.Core.Server.Context (
    Handler,
    MountBinding (bindingPublishDeps, bindingRenderer),
    PublishDeps (..),
    ServeRuntime (srPrivateManager),
    ctxMount,
    ctxRuntime,
 )
import Ecluse.Core.Server.Pipeline.Shared

import Ecluse.Core.Server.Response (
    MountRenderer,
    renderError,
 )

servePublish ::
    PackageName ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
servePublish name request respond = do
    renderer <- asks (bindingRenderer . ctxMount)
    asks (bindingPublishDeps . ctxMount) >>= \case
        Nothing -> liftIO (respond (publishDisabled renderer))
        Just deps -> publishWithDeps renderer deps name request respond

-- Serve a publish once the mount's publication target is known: the edge gate, the
-- anti-shadowing scope guard, then the body-name agreement check (all before any write),
-- then the relay to the publication target with the publisher's forwarded credential.
publishWithDeps ::
    MountRenderer ->
    PublishDeps ->
    PackageName ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
publishWithDeps renderer deps name request respond
    | not (edgeTokenAuthorised (pubInboundToken deps) request) =
        liftIO (respond (edgeUnauthorised renderer))
    | not (inPublishScope (pubScopes deps) name) =
        liftIO (respond (outOfScope renderer deps name))
    | otherwise = do
        rt <- asks ctxRuntime
        -- The body is bounded by the client→proxy request-size cap (the size-limit
        -- middleware), read here only after the scope guard has admitted the name, so a
        -- refused publish never even buffers its (potentially large, base64-tarball)
        -- body.
        body <- liftIO (consumeRequestBodyStrict request)
        -- The body-name agreement leg of the anti-shadowing guard (issue #391): the scope
        -- guard authorised the URL-path name, but the publish document carries its own
        -- declared identity, so a crafted body could otherwise write a name the guard never
        -- saw. Refuse — before the relay — any present declared name that disagrees with the
        -- URL-path name, so the identity authorised is provably the identity written.
        case bodyNameDisagreement (pubCanonicaliseName deps) name body of
            Just declared -> liftIO (respond (bodyNameMismatch renderer deps name declared))
            -- @consumeRequestBodyStrict@ reads the whole body but returns it lazy; the
            -- publish builder ('relayPublishDocument') puts it on the wire as a strict
            -- @RequestBodyBS@, so materialise it strict here. The body is already bounded by
            -- the client→proxy request-size cap.
            Nothing -> do
                outcome <- tryAny (liftIO (pubRelayPublish deps (pubLimits deps) (srPrivateManager rt) (pubTargetUrl deps) (forwardedToken request <|> pubStaticToken deps) name (LBS.toStrict body)))
                liftIO (respond (renderRelay renderer deps outcome))

{- Whether a package name falls within the configured publish-scope allow-list — the
anti-shadowing guard. A __scoped__ name is admitted iff its scope is one of the
configured scopes; an __unscoped__ name is never in any scope, so it is refused (the
MVP allow-list is scope-based, e.g. @\@acme@). The scope equality is exact, so
@\@acme-evil@ does not match an @\@acme@ allow-list entry. -}
inPublishScope :: [Scope] -> PackageName -> Bool
inPublishScope scopes name = case pkgNamespace name of
    Just scope -> scope `elem` scopes
    Nothing -> False

{- Render the relay outcome: the publication target's own status and body forwarded to
the client on success (so the publisher sees the registry's real answer — a success
shape, a @409@, a @403@ the registry's own authorisation produced); a @502@ when the
target could not be reached; a @500@ when its URL is unformable (misconfiguration). -}
renderRelay ::
    MountRenderer ->
    PublishDeps ->
    Either SomeException (Either UrlFormationError PublishRelayResponse) ->
    Response
renderRelay renderer deps = \case
    Right (Right (PublishRelayResponse code relayed)) ->
        jsonResponse (mkStatus code "") [] relayed
    Right (Left _urlErr) ->
        renderedResponse status500 [] (renderError renderer (pubHelp deps) "the publication target URL is misconfigured")
    Left _exc ->
        renderedResponse status502 [] (renderError renderer (pubHelp deps) "the publication target could not be reached")

-- A @405@ for a publish on a mount with no publication target configured: the
-- opt-in path is off, so a @PUT \/{pkg}@ is not an allowed method here. The @Allow@
-- header advertises the read methods the package route does serve.
publishDisabled :: MountRenderer -> Response
publishDisabled renderer =
    renderedResponse status405 [("Allow", "GET, HEAD")] (renderError renderer Nothing "publishing is not enabled on this proxy (no publication target is configured)")

-- A @403@ for a publish whose name is outside the configured publish-scope
-- allow-list — the anti-shadowing guard, refused before any upstream write.
outOfScope :: MountRenderer -> PublishDeps -> PackageName -> Response
outOfScope renderer deps name =
    renderedResponse status403 [] (renderError renderer (pubHelp deps) message)
  where
    message :: Text
    message =
        "refusing to publish '"
            <> renderPackageName name
            <> "': its name is outside the configured publish-scope allow-list (the anti-shadowing guard against publishing a name that shadows a public package)"

-- A @403@ for a publish whose document body declares a package name — its @_id@,
-- top-level @name@, or a @versions[].name@ — that disagrees with the scope-guarded
-- URL-path name. The body-name agreement leg of the anti-shadowing guard (issue #391),
-- refused before any upstream write so the identity the guard authorises is the
-- identity written.
bodyNameMismatch :: MountRenderer -> PublishDeps -> PackageName -> Text -> Response
bodyNameMismatch renderer deps name declared =
    renderedResponse status403 [] (renderError renderer (pubHelp deps) message)
  where
    message :: Text
    message =
        "refusing to publish '"
            <> renderPackageName name
            <> "': the document body declares the name '"
            <> declared
            <> "', which disagrees with the URL-path package name the scope guard authorised (the anti-shadowing guard against publishing a name the allow-list never saw)"

{- The first declared body name that disagrees with the URL-path name, or 'Nothing'
when the body declares no disagreeing name. The publish document carries its own
identity — a top-level @_id@ and @name@, and a @name@ per entry in @versions@ — so a
relay that keyed the write off the body could otherwise write a name the scope guard
never authorised. Each __present__ declared name is canonicalised the same way the
route builds its 'PackageName' ('projectName') and compared by 'PackageName' equality
(ecosystem-aware, so an encoding variant of the same name cannot disagree silently); a
present name that does not equal the URL-path name is a disagreement. Only the names
are read — the base64 @_attachments@ are never decoded. An __absent__ name is not a
claim, so it is not a disagreement (a legitimate npm client always sends matching
names); a body that does not decode to a JSON object likewise declares no readable
name and raises none, leaving the relay to meet the target's own validation. -}
bodyNameDisagreement :: (Text -> Maybe PackageName) -> PackageName -> LByteString -> Maybe Text
bodyNameDisagreement canonicalise name body =
    case Aeson.decode body of
        Nothing -> Nothing
        Just document -> find disagrees (declaredNames document)
  where
    disagrees :: Text -> Bool
    disagrees declared = case canonicalise declared of
        Just declaredName -> declaredName /= name
        Nothing -> True

-- Every package-name string a publish document declares as its own identity: the
-- top-level @_id@ and @name@, and each @versions.<v>.name@. Only string-valued name
-- slots are read (a non-string slot is no name claim); the base64 @_attachments@ are
-- never touched.
declaredNames :: Value -> [Text]
declaredNames document =
    [ declared
    | slot <-
        [document ^? key "_id", document ^? key "name"]
            <> [ versionDoc ^? key "name"
               | versions <- toList (document ^? key "versions" . _Object)
               , versionDoc <- KeyMap.elems versions
               ]
    , Just (String declared) <- [slot]
    ]
