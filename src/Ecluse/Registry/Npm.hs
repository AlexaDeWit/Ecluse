{- | The npm __data plane__: the effectful "Ecluse.Registry" fields over
@http-client@.

This module is the network half of the npm protocol boundary. Where
"Ecluse.Registry.Npm.Wire" and "Ecluse.Registry.Npm.Project" are the pure decode
and projection, this is the side-effecting fetch and publish: 'newNpmClient'
assembles a "Ecluse.Registry.RegistryClient" whose effectful fields talk to a
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
'Ecluse.Registry.fetchArtifact' field, by contrast, buffers (its 'RegistryResponse'
return is whole bytes) and is for the mirror worker, which must read the entire
artifact to verify its integrity before publishing.

== Authentication

The client accepts an __injected__ bearer token and attaches it to every
request; it never originates credential policy. Which token to send on which leg
(forward the client's to the private upstream, strip it before any public fetch,
use the minted mirror token only to write) is the request pipeline's authority
model, decided upstream of this module (see
@docs\/architecture\/registry-model.md@ → "Credential flow and authority"). A
client with no token sends none.
-}
module Ecluse.Registry.Npm (
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
    publishRequest,

    -- * Lower-level fetch (form- and validator-aware)
    fetchMetadataForm,
) where

import Data.Text qualified as T
import Network.HTTP.Client (
    Manager,
    Request (decompress, method, requestBody, requestHeaders),
    RequestBody (RequestBodyBS),
    Response (responseStatus),
    applyBearerAuth,
    httpLbs,
    parseRequest,
    responseBody,
 )
import Network.HTTP.Types.Header (
    hAccept,
    hAcceptEncoding,
    hIfModifiedSince,
    hIfNoneMatch,
 )
import Network.HTTP.Types.Status (statusCode)
import UnliftIO (throwIO)
import UnliftIO.Exception (stringException)

import Ecluse.Credential (Secret, unSecret)
import Ecluse.Package (
    PackageName,
    pkgNamespace,
    renderPackageName,
    unScope,
 )
import Ecluse.Registry (
    PublishError (..),
    RegistryClient (..),
    RegistryResponse (RegistryResponse),
 )
import Ecluse.Registry.Npm.Project qualified as Project
import Ecluse.Security (UrlError (EmptyBaseUrl))
import Ecluse.Version (Version, renderVersion)

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
    -- ^ The registry base URL (e.g. the public registry, or a CodeArtifact npm
    -- endpoint). The package path is appended to it.
    , npmManager :: Manager
    -- ^ The shared @http-client@ 'Manager' to issue requests through.
    , npmToken :: Maybe Secret
    -- ^ An injected bearer token to attach, or 'Nothing' for anonymous requests.
    }

{- | The canonical public npm registry base URL, @https:\/\/registry.npmjs.org@.
The default target when no managed backend is configured.
-}
publicRegistryBaseUrl :: Text
publicRegistryBaseUrl = "https://registry.npmjs.org"

{- | An anonymous client config against the public registry ('publicRegistryBaseUrl'),
using the given shared 'Manager'. Override 'npmBaseUrl'\/'npmToken' for a managed
backend.
-}
defaultNpmConfig :: Manager -> NpmClientConfig
defaultNpmConfig manager =
    NpmClientConfig
        { npmBaseUrl = publicRegistryBaseUrl
        , npmManager = manager
        , npmToken = Nothing
        }

-- ── content negotiation ──────────────────────────────────────────────────────

{- | Which of npm's two metadata documents to request, selected by the @Accept@
header (see 'metadataAccept').
-}
data MetadataForm
    = -- | The install-optimised __abbreviated__ packument
      -- (@application\/vnd.npm.install-v1+json@). Smaller and the proxy's primary
      -- view, but it drops the @time@ map.
      Abbreviated
    | -- | The __full__ packument (@application\/json@). Larger, but the only form
      -- carrying the @time@ map a publish-age rule needs.
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
    -- ^ An RFC-1123 date to send as @If-Modified-Since@ (an upstream
    -- @Last-Modified@).
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
with a 'PublishError' only when the URL cannot be formed (an empty base URL).
-}
metadataRequest ::
    NpmClientConfig ->
    MetadataForm ->
    Validators ->
    PackageName ->
    Either PublishError Request
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

Fails with a 'PublishError' only when the URL cannot be formed.
-}
artifactRequest ::
    NpmClientConfig ->
    PackageName ->
    Version ->
    Either PublishError Request
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

{- | Build the publish @PUT \/{pkg}@ request: the body is the npm publish
document (a packument carrying the version manifest and the base64 tarball under
@_attachments@), already serialised by the caller. Carries the bearer token and a
@Content-Type: application\/json@ header.

Fails with a 'PublishError' only when the URL cannot be formed.
-}
publishRequest ::
    NpmClientConfig ->
    PackageName ->
    ByteString ->
    Either PublishError Request
publishRequest config name document = do
    url <- packageUrl (npmBaseUrl config) name
    base <- parseRequestEither url
    pure
        . withToken (npmToken config)
        $ base
            { method = "PUT"
            , requestBody = RequestBodyBS document
            , requestHeaders = (hAccept, "application/json") : requestHeaders base
            }

-- ── handle assembly ───────────────────────────────────────────────────────────

{- | Assemble a "Ecluse.Registry.RegistryClient" for the npm protocol over the
given configuration.

The effectful fields close over the config's 'Manager' and token and speak npm
over HTTP; the @parse*@ fields are the pure projection from
"Ecluse.Registry.Npm.Project", re-exported through the handle. The handle's
'Ecluse.Registry.fetchMetadata' requests the 'Abbreviated' form
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
conditional-GET 'Validators'. The buffered fetch used by the handle's
'Ecluse.Registry.fetchMetadata'; the request pipeline calls this directly when it
needs the full packument or wants to revalidate against an @ETag@.

A request-building failure (an unformable URL) surfaces as an 'IO' exception
rather than a silent success: a misconfigured base URL is a programming\/config
fault, not a per-response condition the projection layer reports.
-}
fetchMetadataForm ::
    NpmClientConfig ->
    MetadataForm ->
    Validators ->
    PackageName ->
    IO RegistryResponse
fetchMetadataForm config form validators name = do
    request <- orThrow (metadataRequest config form validators name)
    response <- httpLbs request (npmManager config)
    pure (RegistryResponse (toStrict (responseBody response)))

-- | Fetch and __buffer__ a version's artifact bytes (the handle's 'fetchArtifact').
fetchArtifact' :: NpmClientConfig -> PackageName -> Version -> IO RegistryResponse
fetchArtifact' config name version = do
    request <- orThrow (artifactRequest config name version)
    response <- httpLbs request (npmManager config)
    pure (RegistryResponse (toStrict (responseBody response)))

{- | Publish a version's artifact, treating a @409 Conflict@ (the version is
already present) as idempotent success.

A published @name\@version@ is immutable, so a conflict means the bytes are
already there — exactly the success a redelivered mirror job wants, not an error
to retry forever. Any other non-2xx status is reported as a 'PublishError' so the
mirror job is left un-acked and retried.
-}
publishArtifact' ::
    NpmClientConfig ->
    PackageName ->
    Version ->
    ByteString ->
    IO (Either PublishError ())
publishArtifact' config name _version document =
    case publishRequest config name document of
        Left err -> pure (Left err)
        Right request -> do
            response <- httpLbs request (npmManager config)
            let code = statusCode (responseStatus response)
            pure (classifyPublish code)

{- | Map a publish response status onto success or a 'PublishError'. A 2xx or a
@409@ (already present, immutable) is success; anything else is reported so the
job is retried.
-}
classifyPublish :: Int -> Either PublishError ()
classifyPublish code
    | code >= 200 && code < 300 = Right ()
    | code == 409 = Right () -- version already present; immutable, so success-equivalent
    | otherwise =
        Left (PublishError ("publish failed with HTTP status " <> show code))

-- ── helpers ───────────────────────────────────────────────────────────────────

{- | The metadata\/publish URL for a package: @{baseUrl}\/{encoded-name}@, with
the scoped-name separator percent-encoded (@\@scope\/name@ → @\@scope%2Fname@).
-}
packageUrl :: Text -> PackageName -> Either PublishError Text
packageUrl baseUrl name =
    joinPath baseUrl (encodePackagePath name)

{- | The artifact (tarball) URL for one version:
@{baseUrl}\/{encoded-name}\/-\/{tarball-file}@. npm serves a version's tarball
under the package's @\/-\/@ path; the filename is @{base}-{version}.tgz@ (scope
dropped from the file segment, as npm names it).
-}
artifactUrl :: Text -> PackageName -> Version -> Either PublishError Text
artifactUrl baseUrl name version =
    joinPath baseUrl (encodePackagePath name <> "/-/" <> tarballFile name version)

{- | Join a base URL and an already-encoded path, tolerating one trailing slash
on the base so the join never doubles it. An empty base URL is refused with the
shared 'Ecluse.Security.UrlError' vocabulary so an unformable URL reads
consistently across the codebase.
-}
joinPath :: Text -> Text -> Either PublishError Text
joinPath baseUrl path
    | T.null baseUrl = Left (urlError EmptyBaseUrl)
    | otherwise = Right (stripTrailingSlash baseUrl <> "/" <> path)
  where
    stripTrailingSlash b = fromMaybe b (T.stripSuffix "/" b)

{- | Encode a package name as its on-the-wire path segment: the rendered name
with the scope separator percent-encoded. A scoped @\@scope\/name@ becomes
@\@scope%2Fname@ (the leading @\@@ is left as-is, per npm); an unscoped name is
unchanged.
-}
encodePackagePath :: PackageName -> Text
encodePackagePath name = case pkgNamespace name of
    Just scope -> "@" <> unScope scope <> "%2F" <> baseName name
    Nothing -> renderPackageName name

{- | The bare (unscoped) package name — the path segment after the scope, or the
whole rendered name when unscoped. Used both for the @%2F@-encoded path and the
tarball filename.
-}
baseName :: PackageName -> Text
baseName name =
    let rendered = renderPackageName name
     in case pkgNamespace name of
            Just _ -> T.drop 1 (snd (T.breakOn "/" rendered))
            Nothing -> rendered

-- | The conventional npm tarball filename for a version: @{base}-{version}.tgz@.
tarballFile :: PackageName -> Version -> Text
tarballFile name version = baseName name <> "-" <> renderVersion version <> ".tgz"

-- | Attach a bearer token to a request when one is injected; otherwise leave it.
withToken :: Maybe Secret -> Request -> Request
withToken Nothing request = request
withToken (Just secret) request =
    applyBearerAuth (encodeUtf8 (unSecret secret)) request

-- | Add the present conditional-GET validators as request headers.
addValidators :: Validators -> Request -> Request
addValidators validators request =
    request{requestHeaders = newHeaders <> requestHeaders request}
  where
    newHeaders =
        catMaybes
            [ (,) hIfNoneMatch <$> validatorIfNoneMatch validators
            , (,) hIfModifiedSince <$> validatorIfModifiedSince validators
            ]

{- | Parse a built URL into a 'Request', mapping a parse failure into a
'PublishError'. The URL is derived from configuration and an already-safe name,
so a failure here is a configuration fault, reported uniformly with the other
URL-formation errors.
-}
parseRequestEither :: Text -> Either PublishError Request
parseRequestEither url =
    case parseRequest (toString url) of
        Just request -> Right request
        Nothing -> Left (PublishError ("could not parse upstream URL: " <> url))

-- | Adapt a 'UrlError' into the 'PublishError' the request builders report.
urlError :: UrlError -> PublishError
urlError err = PublishError ("could not form upstream URL: " <> show err)

{- | Run a request-building 'Either', throwing its 'PublishError' as an 'IO'
exception. Used by the effectful fetch paths, where an unformable URL is a
config fault rather than a per-response condition.
-}
orThrow :: Either PublishError Request -> IO Request
orThrow = \case
    Left err -> throwIO (stringException (toString (publishErrorMessage err)))
    Right request -> pure request
