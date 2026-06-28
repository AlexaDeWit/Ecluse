{- | The npm __data plane__: the effectful "Ecluse.Core.Registry" fields over
@http-client@.

This module is the network half of the npm protocol boundary. Where
"Ecluse.Core.Registry.Npm.Wire" and "Ecluse.Core.Registry.Npm.Project" are the pure decode
and projection, this is the side-effecting fetch and publish: 'newNpmClient'
assembles a "Ecluse.Core.Registry.RegistryClient" whose effectful fields talk to a
registry over plain HTTP, and whose @parse*@ fields are the pure projection
re-exported through the handle.

It speaks the npm registry protocol directly with @http-client@, __never__
@amazonka@: the control plane (the @GetAuthorizationToken@ mint, the mirror
queue) is @amazonka@'s job behind separate handles, but the data plane — fetch
metadata, stream a tarball, publish — is ordinary HTTPS+JSON, identical across
every npm-speaking backend (npmjs.org, CodeArtifact, Artifact Registry, a
self-hosted Verdaccio). Keeping the streaming path off @amazonka@'s
@conduit@\/@ResourceT@ machinery is exactly what makes bounded-memory artifact
proxying tractable (see @docs\/architecture\/web-layer.md@ → "Control plane vs.
data plane").

== Request shaping

Three details of the wire protocol are load-bearing and handled here (see
@docs\/research\/reverse-engineering\/npm.md@ → "Transport & conventions"):

* __Content negotiation.__ Metadata comes in two forms selected by @Accept@: the
  __abbreviated__ install view (@application\/vnd.npm.install-v1+json@), which the
  proxy treats as primary, and the __full__ packument (@application\/json@),
  needed when a rule reasons over publish age (the abbreviated form drops the
  @time@ map). 'MetadataForm' selects between them; both request
  @Accept-Encoding: gzip@, since popular packuments are megabytes.
* __Scoped-name path encoding.__ A scoped name @\@scope\/name@ is encoded on the
  wire as @\@scope%2Fname@ — the scope separator is percent-encoded but the
  leading @\@@ is not. 'metadataRequest' builds this from an
  __already-parsed__ 'PackageName', never from raw client path segments.
* __Idempotent publish.__ A @PUT \/{pkg}@ that re-publishes an existing version
  returns @409 Conflict@; because versions are immutable, that conflict is
  __success-equivalent__ for a redelivered mirror job (the artifact is already
  present), so 'publishArtifact' treats it as 'Right', not an error.

== Streaming and buffering

'artifactRequest' marks its request __non-decompressing__ ('decompress' returns
'False'): a tarball is opaque binary that must reach the client byte-for-byte, so
the @.tgz@ is never gunzipped in flight (and its @dist.integrity@ stays valid).
The request is exposed so the web layer (see
@docs\/architecture\/web-layer.md@ → "Streaming and resource lifetime") can
bracket it with @withResponse@\/@responseStream@ and relay the open body
__without buffering the whole artifact in memory__. The handle's
'Ecluse.Core.Registry.fetchArtifact' field, by contrast, buffers (its 'RegistryResponse'
return is whole bytes) and is for the mirror worker, which must read the entire
artifact to verify its integrity before publishing.

== Authentication

The client accepts an __injected__ bearer token and attaches it to every
request; it never originates credential policy. Which token to send on which request is
the request pipeline's authority model, decided upstream of this module: always
__strip__ the client's token before any public fetch, and use the minted mirror
token only to write. Whether the client's own token is forwarded to the private
upstream (the default @passthrough@) or Écluse's own read token is used instead is
the mount's access strategy (see @docs\/architecture\/access-model.md@). A client
with no token sends none.
-}
module Ecluse.Core.Registry.Npm (
    -- * Construction
    NpmClientConfig (..),
    defaultNpmConfig,
    publicRegistryBaseUrl,
    newNpmClient,

    -- * Content negotiation
    MetadataForm (..),
    metadataAccept,

    -- * Conditional-GET validators
    Validators (..),
    noValidators,

    -- * Request building
    metadataRequest,
    artifactRequest,
    artifactRequestByFile,
    artifactRequestByUrl,
    artifactFileUrl,
    publishRequest,

    -- * First-party publish relay
    PublishRelayResponse (..),
    relayPublishDocument,

    -- * Publish-document assembly
    npmPublishDocument,

    -- * Lower-level fetch (form- and validator-aware)
    fetchMetadataForm,

    -- * Response-bound breach
    ResponseBoundExceeded (..),
) where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Network.HTTP.Client (
    BodyReader,
    Manager,
    Request (decompress, method, redirectCount, requestBody, requestHeaders),
    RequestBody (RequestBodyBS),
    Response (responseStatus),
    applyBearerAuth,
    brRead,
    httpLbs,
    parseRequest,
    responseBody,
    withResponse,
 )
import Network.HTTP.Types.Header (
    hAccept,
    hAcceptEncoding,
    hContentType,
    hIfModifiedSince,
    hIfNoneMatch,
 )
import Network.HTTP.Types.Status (statusCode)
import UnliftIO (throwIO)

import Ecluse.Core.Credential (Secret, unSecret)
import Ecluse.Core.Package (
    PackageName,
    pkgNamespace,
    renderPackageName,
    unScope,
    unscopedName,
 )
import Ecluse.Core.Registry (
    PublishError (..),
    PublishFault (PublishRejected, PublishUrlUnformable),
    RegistryClient (..),
    RegistryResponse (RegistryResponse),
    UrlFormationError (EmptyBaseUrl, UnparseableUrl),
 )
import Ecluse.Core.Registry.Npm.Project qualified as Project
import Ecluse.Core.Security (
    LimitError,
    Limits,
    boundedRead,
    defaultLimits,
 )
import Ecluse.Core.Server.Route (encodeComponent)
import Ecluse.Core.Text (joinUrlPath)
import Ecluse.Core.Version (Version, renderVersion)

-- ── configuration ────────────────────────────────────────────────────────────

{- | Everything 'newNpmClient' needs to talk to one npm-speaking registry: the
base URL, the shared HTTP 'Manager', and an optional injected bearer token.

The 'Manager' is shared (it owns the connection pool), so it is taken rather than
built here — the same one the composition root reuses across requests. The token
is whatever the request pipeline decided this client should present; this module
never chooses it (see the module header → "Authentication").
-}
data NpmClientConfig = NpmClientConfig
    { npmBaseUrl :: Text
    {- ^ The registry base URL (e.g. the public registry, or a CodeArtifact npm
    endpoint). The package path is appended to it.
    -}
    , npmManager :: Manager
    -- ^ The shared @http-client@ 'Manager' to issue requests through.
    , npmToken :: Maybe Secret
    -- ^ An injected bearer token to attach, or 'Nothing' for anonymous requests.
    , npmLimits :: Limits
    {- ^ The response-bound budget enforced on a metadata fetch: 'fetchMetadataForm'
    reads the body through 'Ecluse.Core.Security.boundedRead' against
    'Ecluse.Core.Security.maxBodyBytes', aborting fail-closed past the cap rather than
    buffering an unbounded body. The other 'Ecluse.Core.Security.Limits' ceilings (version
    count, nesting depth) are enforced by the decode\/projection layer, not here.
    -}
    }

{- | The canonical public npm registry base URL, @https:\/\/registry.npmjs.org@.
The default target when no managed backend is configured.
-}
publicRegistryBaseUrl :: Text
publicRegistryBaseUrl = "https://registry.npmjs.org"

{- | An anonymous client config against the public registry ('publicRegistryBaseUrl'),
using the given shared 'Manager' and the secure-default response bounds
('Ecluse.Core.Security.defaultLimits'). Override 'npmBaseUrl'\/'npmToken'\/'npmLimits' for
a managed backend or a per-deployment budget.
-}
defaultNpmConfig :: Manager -> NpmClientConfig
defaultNpmConfig manager =
    NpmClientConfig
        { npmBaseUrl = publicRegistryBaseUrl
        , npmManager = manager
        , npmToken = Nothing
        , npmLimits = defaultLimits
        }

-- ── content negotiation ──────────────────────────────────────────────────────

{- | Which of npm's two metadata documents to request, selected by the @Accept@
header (see 'metadataAccept').
-}
data MetadataForm
    = {- | The install-optimised __abbreviated__ packument
      (@application\/vnd.npm.install-v1+json@). Smaller and the proxy's primary
      view, but it drops the @time@ map.
      -}
      Abbreviated
    | {- | The __full__ packument (@application\/json@). Larger, but the only form
      carrying the @time@ map a publish-age rule needs.
      -}
      Full
    deriving stock (Eq, Show)

{- | The @Accept@ header value selecting a 'MetadataForm'.

>>> metadataAccept Abbreviated
"application/vnd.npm.install-v1+json"

>>> metadataAccept Full
"application/json"
-}
metadataAccept :: MetadataForm -> ByteString
metadataAccept = \case
    Abbreviated -> "application/vnd.npm.install-v1+json"
    Full -> "application/json"

-- ── conditional-GET validators ───────────────────────────────────────────────

{- | The conditional-GET validators to relay on a metadata fetch. Replaying an
upstream's @ETag@ as @If-None-Match@ (or its @Last-Modified@ as
@If-Modified-Since@) lets the upstream answer @304 Not Modified@ with no body —
the cheap freshness check the proxy uses on a cache revalidation. Both are
forwarded only when present.
-}
data Validators = Validators
    { validatorIfNoneMatch :: Maybe ByteString
    -- ^ An entity tag to send as @If-None-Match@ (an upstream @ETag@).
    , validatorIfModifiedSince :: Maybe ByteString
    {- ^ An RFC-1123 date to send as @If-Modified-Since@ (an upstream
    @Last-Modified@).
    -}
    }
    deriving stock (Eq, Show)

-- | No conditional-GET validators — an unconditional fetch.
noValidators :: Validators
noValidators = Validators{validatorIfNoneMatch = Nothing, validatorIfModifiedSince = Nothing}

-- ── request building ──────────────────────────────────────────────────────────

{- | Build the metadata @GET@ request for a package: the URL is
@{baseUrl}\/{encoded-name}@ with the @Accept@ header for the chosen
'MetadataForm', @Accept-Encoding: gzip@, an optional bearer token, and any
relayed conditional-GET 'Validators'.

The package path is derived from an __already-parsed__ 'PackageName', then the
scope separator is percent-encoded (@\@scope\/name@ → @\@scope%2Fname@). Fails
with a 'UrlFormationError' only when the URL cannot be formed (an empty base URL).
-}
metadataRequest ::
    NpmClientConfig ->
    MetadataForm ->
    Validators ->
    PackageName ->
    Either UrlFormationError Request
metadataRequest config form validators name = do
    url <- packageUrl (npmBaseUrl config) name
    base <- parseRequestEither url
    pure
        . withToken (npmToken config)
        . addValidators validators
        $ base
            { requestHeaders =
                (hAccept, metadataAccept form)
                    : (hAcceptEncoding, "gzip")
                    : requestHeaders base
            }

{- | Build the artifact @GET@ request for one version's tarball.

The request is marked __non-decompressing__ ('decompress' returns 'False') so the
@.tgz@ bytes are streamed through verbatim — a tarball is opaque binary and must
reach the client byte-for-byte for its @dist.integrity@ to verify. The artifact
URL is the registry-served tarball location, derived like 'metadataRequest' but
addressing the version's artifact path. Exposed so the web layer can bracket it
for bounded-memory streaming (see the module header).

Fails with a 'UrlFormationError' only when the URL cannot be formed.
-}
artifactRequest ::
    NpmClientConfig ->
    PackageName ->
    Version ->
    Either UrlFormationError Request
artifactRequest config name version = do
    url <- artifactUrl (npmBaseUrl config) name version
    base <- parseRequestEither url
    pure
        . withToken (npmToken config)
        $ base
            { -- A tarball must never be gunzipped in flight: it is opaque binary
              -- whose integrity the client verifies, so stream the raw bytes. We
              -- deliberately advertise no @Accept-Encoding@ here: a @.tgz@ is
              -- already-compressed application data, and requesting a transport
              -- encoding we then refuse to decode ('decompress' is 'False') would
              -- risk a doubly-gzipped body that fails its @dist.integrity@.
              decompress = const False
            }

{- | Build the artifact @GET@ request addressing a tarball by its __preserved
on-the-wire filename__, at @{baseUrl}\/{encoded-pkg}\/-\/{filename}@.

The serve path fetches an artifact by the exact filename the client requested —
the authoritative name for the bytes — rather than reconstructing it from
@(package, version)@ as 'artifactRequest' does, so a registry whose tarball naming
differs from the proxy's own convention still resolves. The @filename@ is taken
verbatim (the classifier has already passed it through the component-safety gate),
and the package segment is the same scope-percent-encoded path 'artifactRequest'
uses. The request is marked __non-decompressing__ for the same reason: a @.tgz@ is
opaque binary streamed byte-for-byte so its @dist.integrity@ verifies. Exposed so
the web layer can bracket it for bounded-memory streaming (see the module header).

Fails with a 'UrlFormationError' only when the URL cannot be formed.
-}
artifactRequestByFile ::
    NpmClientConfig ->
    PackageName ->
    Text ->
    Either UrlFormationError Request
artifactRequestByFile config name filename = do
    url <- artifactFileUrl (npmBaseUrl config) name filename
    base <- parseRequestEither url
    pure
        . withToken (npmToken config)
        $ base
            { -- A tarball must never be gunzipped in flight (see 'artifactRequest').
              decompress = const False
            }

{- | Build the artifact @GET@ request addressing a tarball at its __authoritative
upstream location__ — the absolute @url@ the projection preserved from the
upstream's @dist.tarball@ — rather than reconstructing it from @(base, package,
file)@.

The artifact location is server-chosen data, not a derivable fact: a registry may
serve a version's tarball from a different host or a path the npm @\/-\/@ convention
cannot rebuild (a separate CDN\/files host, server-generated segments, a signed
query string). Honouring the preserved location is what lets Écluse front those
registries; the URL it fetches is the same one the served packument's
@dist.integrity@ is paired with, so the bytes still verify. The egress gate
('Ecluse.Core.Security.tarballHostAllowed' plus the resolved-IP recheck) decides
__whether__ that location may be fetched; this builder only forms the request once
it is permitted. The 'NpmClientConfig''s @npmBaseUrl@ is unused here (the URL is
absolute) but its 'Manager' and token are not — the manager carries the trust
context and the token the credential posture.

The request is marked __non-decompressing__ for the same reason as 'artifactRequest':
a @.tgz@ is opaque binary streamed byte-for-byte. Fails with a 'UrlFormationError'
only when the @url@ cannot be parsed into a request.
-}
artifactRequestByUrl ::
    NpmClientConfig ->
    Text ->
    Either UrlFormationError Request
artifactRequestByUrl config url = do
    base <- parseRequestEither url
    pure
        . withToken (npmToken config)
        $ base
            { -- A tarball must never be gunzipped in flight (see 'artifactRequest').
              decompress = const False
            }

{- | Build the publish @PUT \/{pkg}@ request: the body is the npm publish
document (a packument carrying the version manifest and the base64 tarball under
@_attachments@), already serialised by the caller. Carries the bearer token and a
@Content-Type: application\/json@ header.

Fails with a 'UrlFormationError' only when the URL cannot be formed; a genuine
write fault (a non-2xx, non-409 status) is the 'PublishError' that
'Ecluse.Core.Registry.publishArtifact' reports.
-}
publishRequest ::
    NpmClientConfig ->
    PackageName ->
    ByteString ->
    Either UrlFormationError Request
publishRequest config name document = do
    url <- packageUrl (npmBaseUrl config) name
    base <- parseRequestEither url
    pure
        . withToken (npmToken config)
        $ base
            { method = "PUT"
            , requestBody = RequestBodyBS document
            , -- A spec-compliant registry (e.g. Verdaccio) rejects a publish whose
              -- body is not declared @application/json@ with a 415; the npm publish
              -- protocol requires it. Accept is set too, for the registry's response.
              requestHeaders =
                (hContentType, "application/json")
                    : (hAccept, "application/json")
                    : requestHeaders base
            }

-- ── first-party publish relay ─────────────────────────────────────────────────

{- | The publication target's response to a relayed first-party publish: the HTTP
status __code__ and the response __body__, so the front door can forward the
registry's own answer to the @npm@ client verbatim.

This is deliberately distinct from the mirror worker's
'Ecluse.Core.Registry.publishArtifact', which collapses the registry's reply into a
success-or-'Ecluse.Core.Registry.PublishFault' verdict (the worker only needs to know
whether to ack or retry the job). A first-party publish is a __relay__: the client
publishes through the proxy and must see exactly what the registry said — a success
shape, a @409@ "version exists", a @403@ the registry's own authorisation produced —
so the status and body are carried through rather than reduced to a verdict.
-}
data PublishRelayResponse = PublishRelayResponse
    { relayStatus :: Int
    -- ^ The HTTP status code the publication target returned.
    , relayBody :: LByteString
    -- ^ The publication target's response body, relayed to the client unchanged.
    }
    deriving stock (Eq, Show)

{- | Relay a client's npm publish document to the publication target and return the
target's own response — the first-party publish primitive behind the @PUT \/{pkg}@
serve path.

The @document@ is the publisher's own @PUT@ body, relayed __verbatim__ (the proxy
does not re-assemble it the way the mirror worker assembles 'npmPublishDocument' from
verified bytes). The request is built by 'publishRequest', so it carries the
config's injected bearer — for this path the publisher's __own forwarded token__
(passthrough), put on the per-request 'NpmClientConfig' by the serve layer — and the
@Content-Type: application\/json@ the npm publish protocol requires. The package URL
is formed from the route's 'PackageName', never the document's self-reported name.

The response body is read __bounded__ through 'Ecluse.Core.Security.boundedRead' against
the config's 'npmLimits' (the same @maxBodyBytes@ budget the metadata path enforces), via
'withResponse' rather than an unbounded 'httpLbs' — so a hostile or compromised
publication target cannot exhaust the proxy with a multi-gigabyte response. A body past
the cap aborts fail-closed as a 'ResponseBoundExceeded' throw, which the serve layer's
@tryAny@ turns into a gateway error.

Returns the publication target's status and bounded body ('PublishRelayResponse'), or a
'UrlFormationError' when the request URL cannot be formed (a misconfigured base URL). A
transport failure (the target unreachable) throws, as the serve layer's @tryAny@ expects
— it renders a gateway error rather than a relayed status. Unlike
'Ecluse.Core.Registry.publishArtifact', a @409@ is __not__ folded into success here: a
first-party publisher re-publishing an existing version should see the registry's @409@,
not a fabricated @200@.
-}
relayPublishDocument ::
    NpmClientConfig ->
    PackageName ->
    ByteString ->
    IO (Either UrlFormationError PublishRelayResponse)
relayPublishDocument config name document =
    case publishRequest config name document of
        Left urlErr -> pure (Left urlErr)
        Right request ->
            withResponse request (npmManager config) $ \response -> do
                RegistryResponse body <- readBoundedBody (npmLimits config) (responseBody response)
                pure
                    ( Right
                        PublishRelayResponse
                            { relayStatus = statusCode (responseStatus response)
                            , relayBody = LBS.fromStrict body
                            }
                    )

-- ── publish-document assembly ─────────────────────────────────────────────────

{- | Assemble the npm publish document for one version from its verified tarball
bytes — the serialised body 'publishRequest' (hence
'Ecluse.Core.Registry.publishArtifact') @PUT@s to @\/{pkg}@.

The document is the npm @PUT \/{pkg}@ shape: the package name and a single-version
@versions@ map carrying the version manifest (@name@, @version@, and a @dist@ with
the integrity digests), @dist-tags.latest@ pointed at that version, and the tarball
itself base64-encoded under @_attachments@ with its byte @length@. A managed npm
registry (CodeArtifact, Artifact Registry, Verdaccio) recomputes the served
@dist.tarball@ location from the attachment, so the location is not carried.

The integrity digests written into @dist@ are the __caller's__ — the worker passes
the serve-time-admitted digests it has already verified the bytes against — so the
published manifest's integrity matches exactly the bytes attached. The tarball
@length@ is taken from the actual byte count, never a caller-declared size, so the
attachment can never disagree with its own bytes.

This is the inverse of the read-side decode in "Ecluse.Core.Registry.Npm.Wire", which
deliberately does not model @_attachments@: it is constructed only here, for the
write.
-}
npmPublishDocument ::
    -- | The package being published.
    PackageName ->
    -- | The version being published.
    Version ->
    -- | The tarball's filename — the @_attachments@ key and tarball file segment.
    Text ->
    -- | The @dist.integrity@ SRI string, if known (e.g. @"sha512-…"@).
    Maybe Text ->
    -- | The @dist.shasum@ (SHA-1, hex), if known.
    Maybe Text ->
    -- | The verified tarball bytes.
    ByteString ->
    ByteString
npmPublishDocument name version filename integrity shasum tarball =
    toStrict . Aeson.encode $
        object
            [ "_id" .= rendered
            , "name" .= rendered
            , "dist-tags" .= object ["latest" .= versionText]
            , "versions"
                .= object
                    [ Key.fromText versionText
                        .= object
                            [ "name" .= rendered
                            , "version" .= versionText
                            , "dist"
                                .= object
                                    ( ["tarball" .= filename]
                                        <> maybe [] (\i -> ["integrity" .= i]) integrity
                                        <> maybe [] (\s -> ["shasum" .= s]) shasum
                                    )
                            ]
                    ]
            , "_attachments"
                .= object
                    [ Key.fromText filename
                        .= object
                            [ "content_type" .= ("application/octet-stream" :: Text)
                            , "data" .= encodedTarball
                            , "length" .= BS.length tarball
                            ]
                    ]
            ]
  where
    versionText = renderVersion version
    rendered = renderPackageName name
    -- The npm attachment carries the raw tarball bytes, standard-base64-encoded.
    encodedTarball :: Text
    encodedTarball = decodeUtf8 (convertToBase Base64 tarball :: ByteString)

-- ── handle assembly ───────────────────────────────────────────────────────────

{- | Assemble a "Ecluse.Core.Registry.RegistryClient" for the npm protocol over the
given configuration.

The effectful fields close over the config's 'Manager' and token and speak npm
over HTTP; the @parse*@ fields are the pure projection from
"Ecluse.Core.Registry.Npm.Project", re-exported through the handle. The handle's
'Ecluse.Core.Registry.fetchMetadata' requests the 'Abbreviated' form
unconditionally; the richer 'fetchMetadataForm' (for the full packument and
relayed validators) is exposed separately for the request pipeline.
-}
newNpmClient :: NpmClientConfig -> IO RegistryClient
newNpmClient config =
    pure
        RegistryClient
            { fetchMetadata = fetchMetadataForm config Abbreviated noValidators
            , fetchArtifact = fetchArtifact' config
            , publishArtifact = publishArtifact' config
            , parsePackageInfo = Project.parsePackageInfo
            , parseVersionDetails = Project.parseVersionDetails
            , parseVersionList = Project.parseVersionList
            }

{- | Fetch a package's metadata in the requested 'MetadataForm', relaying any
conditional-GET 'Validators'. The bounded-read fetch used by the handle's
'Ecluse.Core.Registry.fetchMetadata'; the request pipeline calls this directly when it
needs the full packument or wants to revalidate against an @ETag@.

The body is read __chunk-by-chunk through 'Ecluse.Core.Security.boundedRead'__ against
the config's 'npmLimits', not buffered whole: a hostile or compromised upstream
returning a body larger than 'Ecluse.Core.Security.maxBodyBytes' is aborted
__fail-closed__ rather than exhausting memory (security.md invariant 4). A body
within budget is returned whole (the metadata path projects the entire document);
artifacts are the separate streaming concern, not bounded here. The request's
@Accept-Encoding: gzip@ still applies — @http-client@ decompresses transparently
under 'withResponse' exactly as under @httpLbs@, so the cap bounds the
__decompressed__ bytes the proxy actually retains.

A body-size breach surfaces as a typed 'ResponseBoundExceeded' exception carrying
the 'Ecluse.Core.Security.LimitError', so the request pipeline's @tryAny@ degrades the
contribution to nothing — the fail-closed parse-failure path — rather than the
projection layer ever seeing a truncated body. A request-building failure (an
unformable URL) likewise surfaces as a typed 'UrlFormationError' exception rather
than a silent success: a misconfigured base URL is a programming\/config fault on
the read path, not a per-response condition the projection layer reports. (The write
path instead returns an unformable URL as a 'Ecluse.Core.Registry.PublishFault' value,
where the worker must choose retry vs. drop.)
-}
fetchMetadataForm ::
    NpmClientConfig ->
    MetadataForm ->
    Validators ->
    PackageName ->
    IO RegistryResponse
fetchMetadataForm config form validators name = do
    request <- orThrow (metadataRequest config form validators name)
    withResponse request (npmManager config) $ \response ->
        readBoundedBody (npmLimits config) (responseBody response)

{- | Raised when an upstream metadata body breaches a 'Ecluse.Core.Security.Limits'
ceiling: the body-size guard here, or — surfaced through the same type by the serve
pipeline — the version-count or nesting-depth guard.

Carries the 'Ecluse.Core.Security.LimitError' (which ceiling, the observed value, and the
cap), so the breach is __diagnosable__ rather than collapsing into an opaque failure:
the serve path logs it at the breach point before degrading the contribution to
nothing. It is thrown fail-closed (never a truncated or partial body), so it surfaces
to the fetch caller exactly as a parse failure would — the request pipeline's @tryAny@
treats it as a degraded (missing) contribution.
-}
newtype ResponseBoundExceeded = ResponseBoundExceeded LimitError
    deriving stock (Eq, Show)

instance Exception ResponseBoundExceeded

{- Read a response body chunk-by-chunk through 'boundedRead' against the budget,
returning the whole body as a 'RegistryResponse' when within the cap. A body past
'Ecluse.Core.Security.maxBodyBytes' aborts the read fail-closed and is raised as a typed
'ResponseBoundExceeded' (never a truncated body), so the caller can log the breach
and its @tryAny@ degrades it to a missing contribution — the same handling a parse
failure gets. -}
readBoundedBody :: Limits -> BodyReader -> IO RegistryResponse
readBoundedBody limits bodyReader =
    boundedRead limits (brRead bodyReader) >>= \case
        Right body -> pure (RegistryResponse body)
        -- 'boundedRead' only ever yields 'BodyTooLarge'; the other 'LimitError's
        -- come from the decode\/projection layer. Either way a bound breach is a
        -- fail-closed typed exception, never a truncated body.
        Left err -> throwIO (ResponseBoundExceeded err)

-- Fetch and __buffer__ a version's artifact bytes (the handle's 'fetchArtifact').
fetchArtifact' :: NpmClientConfig -> PackageName -> Version -> IO RegistryResponse
fetchArtifact' config name version = do
    request <- orThrow (artifactRequest config name version)
    response <- httpLbs request (npmManager config)
    pure (RegistryResponse (toStrict (responseBody response)))

{- Publish a version's artifact, treating a @409 Conflict@ (the version is
already present) as idempotent success.

A published @name\@version@ is immutable, so a conflict means the bytes are
already there — exactly the success a redelivered mirror job wants, not an error
to retry forever. Any other non-2xx status is reported as a 'PublishRejected' so
the mirror job is left un-acked and retried.

A request-building failure (an unformable URL) is reported as a
'PublishUrlUnformable' __value__, distinct from 'PublishRejected': it is a
config\/programming fault the worker must __drop__, not a transient rejection it
should retry forever. Surfacing it as a value (rather than throwing, as the read
paths do) keeps that retry-vs-drop decision total at the call site.
-}
publishArtifact' ::
    NpmClientConfig ->
    PackageName ->
    Version ->
    ByteString ->
    IO (Either PublishFault ())
publishArtifact' config name _version document =
    case publishRequest config name document of
        Left urlErr -> pure (Left (PublishUrlUnformable urlErr))
        Right request -> do
            response <- httpLbs request (npmManager config)
            let code = statusCode (responseStatus response)
            pure (classifyPublish code)

{- Map a publish response status onto success or a 'PublishFault'. A 2xx or a
@409@ (already present, immutable) is success; anything else is a retryable
'PublishRejected' naming the status the job saw.
-}
classifyPublish :: Int -> Either PublishFault ()
classifyPublish code
    | code >= 200 && code < 300 = Right ()
    | code == 409 = Right () -- version already present; immutable, so success-equivalent
    | otherwise =
        Left (PublishRejected (PublishError ("publish failed with HTTP status " <> show code)))

-- ── helpers ───────────────────────────────────────────────────────────────────

{- The metadata\/publish URL for a package: @{baseUrl}\/{encoded-name}@, with
the scoped-name separator percent-encoded (@\@scope\/name@ → @\@scope%2Fname@).
-}
packageUrl :: Text -> PackageName -> Either UrlFormationError Text
packageUrl baseUrl name =
    joinPath baseUrl (encodePackagePath name)

{- The artifact (tarball) URL for one version:
@{baseUrl}\/{encoded-name}\/-\/{tarball-file}@. npm serves a version's tarball
under the package's @\/-\/@ path; the filename is @{base}-{version}.tgz@ (scope
dropped from the file segment, as npm names it).
-}
artifactUrl :: Text -> PackageName -> Version -> Either UrlFormationError Text
artifactUrl baseUrl name version =
    joinPath baseUrl (encodePackagePath name <> "/-/" <> tarballFile name version)

{- | The artifact (tarball) URL addressing a __preserved filename__:
@{baseUrl}\/{encoded-name}\/-\/{encoded-filename}@. The filename is the exact
on-the-wire name (not @{base}-{version}.tgz@ rebuilt from the coordinate), so the
bytes are fetched by the name the client requested; it is percent-encoded as a
single component ('Ecluse.Core.Server.Route.encodeComponent') so a once-decoded escape
in it cannot reach the upstream raw. Exposed so the serve path can record the
public artifact location on a mirror job (the same URL its public fetch targets).

Fails with a 'UrlFormationError' only when the URL cannot be formed.
-}
artifactFileUrl :: Text -> PackageName -> Text -> Either UrlFormationError Text
artifactFileUrl baseUrl name filename =
    joinPath baseUrl (encodePackagePath name <> "/-/" <> encodeComponent filename)

{- Join a base URL and an already-encoded path, tolerating one trailing slash
on the base so the join never doubles it. An empty base URL is refused with a
'UrlFormationError' — the read- and write-path builders share this report, so an
unformable URL is never mislabelled as a publish failure.
-}
joinPath :: Text -> Text -> Either UrlFormationError Text
joinPath baseUrl path
    | T.null baseUrl = Left EmptyBaseUrl
    | otherwise = Right (joinUrlPath baseUrl path)

{- Encode a package name as its on-the-wire path segment. Each name component
(scope, base name) is percent-encoded ('Ecluse.Core.Server.Route.encodeComponent')
around the structural delimiters this builder writes: a scoped @\@scope\/name@
becomes @\@{enc-scope}%2F{enc-base}@ — the leading @\@@ and the @%2F@ separator
are written here, never derived from a component, so a legitimate scoped name
yields exactly one @%2F@ — and an unscoped name is its single encoded component.
Encoding each component is the defence in depth that keeps a @\'%\'@, @\'\/\'@, or
other reserved byte inside a decoded name from reaching the upstream URL raw (a
once-decoded @%2e%2e%2f@ is re-encoded to @%252e%252e%252f@), without
double-encoding the structural separator.
-}
encodePackagePath :: PackageName -> Text
encodePackagePath name = case pkgNamespace name of
    Just scope -> "@" <> encodeComponent (unScope scope) <> "%2F" <> encodeComponent (unscopedName name)
    Nothing -> encodeComponent (renderPackageName name)

{- The conventional npm tarball filename for a version: @{base}-{version}.tgz@.
The base name and version are percent-encoded as components around the structural
@\'-\'@ and @.tgz@ this builder writes, so a reserved byte in either cannot reach
the upstream URL raw. -}
tarballFile :: PackageName -> Version -> Text
tarballFile name version =
    encodeComponent (unscopedName name) <> "-" <> encodeComponent (renderVersion version) <> ".tgz"

{- Finalize an npm data-plane request: __disable redirect following__ ('redirectCount'
= 0) on __every__ request, and attach a bearer token when one is injected.

This is the single request-finalization point for the whole npm data plane — every
builder and call site funnels through it (it is also the only 'applyBearerAuth') — so
pinning @redirectCount = 0@ here makes one invariant universal: __Écluse never follows an
upstream redirect__, on the credentialed and the anonymous plane alike.

Two dangers it forecloses, one per plane:

\* __Credential leakage__ (credentialed plane). http-client's default ('redirectCount' =
  10) re-sends the @Authorization@ header to the redirect's @Location@, and its
  @shouldStripHeaderOnRedirect@ does not strip it cross-host — so a hostile or
  misconfigured upstream could @302@ a forwarded\/minted credential to an attacker-chosen
  host. That is especially dangerous on the __trusted private manager__, which carries no
  resolved-IP SSRF recheck (it may legitimately resolve to an internal address), so a
  redirect there could exfiltrate the credential to an internal target with no egress
  guard at all.

\* __SSRF via redirect__ (anonymous plane). The host allowlist is enforced when the URL is
  built, not per redirect hop, so following a @302@ would let an allowlisted upstream
  steer an anonymous fetch to __any__ host — an internal\/cloud-metadata address or any
  off-allowlist host — re-gated by nothing. Not following the redirect removes the hop
  there is to gate.

The accepted consequence, symmetric across both planes: a read no longer follows an
upstream's CDN @302@ — it returns the @3xx@ to the serve path rather than chasing it. That
is the safer posture, and the proxy already honours the __packument's__ @dist.tarball@
location explicitly, gated by the egress policy, rather than relying on redirects.
Redirect-following for a nonstandard upstream (a presigned\/redirecting object store) is an
explicit, per-upstream opt-in, never the default.

(Out of scope here: amazonka — CodeArtifact\/SQS — and the OTLP exporter build their own
requests outside this function, so this invariant does not reach them; that is a separate
follow-up.) -}
-- The anonymous no-redirect-follow posture closes the SSRF leg of #397; the
-- redirect-following opt-in for nonstandard upstreams is tracked by #395.
withToken :: Maybe Secret -> Request -> Request
withToken Nothing request = request{redirectCount = 0}
withToken (Just secret) request =
    applyBearerAuth (encodeUtf8 (unSecret secret)) request{redirectCount = 0}

-- Add the present conditional-GET validators as request headers.
addValidators :: Validators -> Request -> Request
addValidators validators request =
    request{requestHeaders = newHeaders <> requestHeaders request}
  where
    newHeaders =
        catMaybes
            [ (,) hIfNoneMatch <$> validatorIfNoneMatch validators
            , (,) hIfModifiedSince <$> validatorIfModifiedSince validators
            ]

{- Parse a built URL into a 'Request', mapping a parse failure into a
'UrlFormationError'. The URL is derived from configuration and an already-safe
name, so a failure here is a configuration fault, reported uniformly with the
other URL-formation errors.
-}
parseRequestEither :: Text -> Either UrlFormationError Request
parseRequestEither url =
    case parseRequest (toString url) of
        Just request -> Right request
        Nothing -> Left (UnparseableUrl url)

{- Run a request-building 'Either' from a __read__ path, raising its
'UrlFormationError' as the typed exception it is (no stringly @stringException@).
Used by the metadata and artifact fetches, where an unformable URL is a config
fault rather than a per-response condition; the write path instead returns it as
a 'PublishUrlUnformable' value.
-}
orThrow :: Either UrlFormationError Request -> IO Request
orThrow = \case
    Left err -> throwIO err
    Right request -> pure request
