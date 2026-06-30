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
queue) is @amazonka@'s job behind separate handles, but the data plane: fetch
metadata, stream a tarball, publish: is ordinary HTTPS+JSON, identical across
every npm-speaking backend. Keeping the streaming path off @amazonka@'s
@conduit@/@ResourceT@ machinery is exactly what makes bounded-memory artifact
proxying tractable.

== Streaming and buffering

'Ecluse.Core.Registry.Npm.Request.artifactRequest' marks its request __non-decompressing__
so a tarball is opaque binary that must reach the client byte-for-byte. The
request is exposed so the web layer can relay the open body __without buffering
the whole artifact in memory__. The handle's 'Ecluse.Core.Registry.fetchArtifact'
field, by contrast, buffers (its 'RegistryResponse' return is whole bytes) and
is for the mirror worker, which must read the entire artifact to verify its
integrity before publishing.

== Authentication

The client accepts an __injected__ bearer token and attaches it to every
request; it never originates credential policy. Which token to send on which request is
the request pipeline's authority model, decided upstream of this module.
-}
module Ecluse.Core.Registry.Npm (
    -- * Construction
    NpmClientConfig (..),
    defaultNpmConfig,
    publicRegistryBaseUrl,
    publicRegistryUrl,
    newNpmClient,
    newNpmPublishClient,

    -- * Lower-level fetch
    fetchMetadataForm,

    -- * First-party publish relay
    relayPublishDocument,

    -- * Response-bound breach
    ResponseBoundExceeded (..),
) where

import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NE
import Network.HTTP.Client (
    BodyReader,
    Manager,
    Request,
    Response (responseStatus),
    brRead,
    httpLbs,
    responseBody,
    withResponse,
 )
import Network.HTTP.Types.Status (statusCode)
import UnliftIO (throwIO)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Package (Hash (hashAlg, hashValue), HashAlg (SHA1, SRI), PackageName)
import Ecluse.Core.Queue (MirrorArtifact (maFilename, maHashes))
import Ecluse.Core.Registry (
    ParseError (ParseError),
    PublishError (..),
    PublishFault (PublishRejected, PublishUrlUnformable),
    PublishRelayResponse (..),
    RegistryClient (..),
    RegistryResponse (RegistryResponse),
    UrlFormationError,
 )

import Ecluse.Core.Registry.Npm.Project qualified as Project
import Ecluse.Core.Registry.Npm.Publish (npmPublishDocument, publishRequest)
import Ecluse.Core.Registry.Npm.Request (
    MetadataForm (Abbreviated),
    Validators,
    artifactRequest,
    metadataRequest,
    noValidators,
 )
import Ecluse.Core.Security (
    LimitError,
    Limits,
    boundedRead,
    defaultLimits,
 )
import Ecluse.Core.Security.Egress.Internal (RegistryUrl (RegistryUrl))
import Ecluse.Core.Version (Version)

{- | Everything 'newNpmClient' needs to talk to one npm-speaking registry: the
base URL, the shared HTTP 'Manager', and an optional injected bearer token.

The 'Manager' is shared (it owns the connection pool), so it is taken rather than
built here: the same one the composition root reuses across requests. The token
is whatever the request pipeline decided this client should present; this module
never chooses it.
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
    buffering an unbounded body.
    -}
    }

{- | The canonical public npm registry base URL, @https://registry.npmjs.org@.
The default target when no managed backend is configured.
-}
publicRegistryBaseUrl :: Text
publicRegistryBaseUrl = "https://registry.npmjs.org"

{- | The canonical public npm registry as an https 'RegistryUrl': the
'publicRegistryBaseUrl' text, https by construction. The default @ECLUSE_PUBLIC_UPSTREAM@
when none is configured.
-}
publicRegistryUrl :: RegistryUrl
publicRegistryUrl = RegistryUrl publicRegistryBaseUrl

{- | An anonymous client config against the public registry ('publicRegistryBaseUrl'),
using the given shared 'Manager' and the secure-default response bounds
('Ecluse.Core.Security.defaultLimits'). Override 'npmBaseUrl'/'npmToken'/'npmLimits' for
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
newNpmClient config = newNpmPublishClient config (pure (npmToken config))

{- | Build an npm RegistryClient whose publishArtifact field mints a fresh token
per call via the provided IO action. Other fields use the token in the config.
-}
newNpmPublishClient :: NpmClientConfig -> IO (Maybe Secret) -> IO RegistryClient
newNpmPublishClient config mintToken =
    pure
        RegistryClient
            { fetchMetadata = fetchMetadataForm config Abbreviated noValidators
            , fetchArtifact = fetchArtifact' config
            , publishArtifact = publishArtifact' config mintToken
            , -- Each version's @dist.tarball@ scheme is normalised against the host this
              -- client reads from (same-host http upgraded, foreign-host http dropped) as
              -- a projection post-step; the Handle field types are unchanged.
              parsePackageInfo = \name resp -> Project.enforceTarballScheme upstreamBaseUrl <$> Project.parsePackageInfo name resp
            , parseVersionDetails = \resp version ->
                Project.parseVersionDetails resp version
                    >>= maybe (Left tarballNotHttps) Right . Project.enforceTarballSchemeDetails upstreamBaseUrl
            , parseVersionList = Project.parseVersionList
            }
  where
    upstreamBaseUrl = npmBaseUrl config
    tarballNotHttps =
        ParseError "the requested version's dist.tarball is not an https URL on the upstream host"

{- | Fetch a package's metadata in the requested 'MetadataForm', relaying any
conditional-GET 'Validators'. The bounded-read fetch used by the handle's
'Ecluse.Core.Registry.fetchMetadata'; the request pipeline calls this directly when it
needs the full packument or wants to revalidate against an @ETag@.

The body is read __chunk-by-chunk through 'Ecluse.Core.Security.boundedRead'__ against
the config's 'npmLimits', not buffered whole: a hostile or compromised upstream
returning a body larger than 'Ecluse.Core.Security.maxBodyBytes' is aborted
__fail-closed__ rather than exhausting memory.
-}
fetchMetadataForm ::
    NpmClientConfig ->
    MetadataForm ->
    Validators ->
    PackageName ->
    IO RegistryResponse
fetchMetadataForm config form validators name = do
    request <- orThrow (metadataRequest (npmBaseUrl config) (npmToken config) form validators name)
    withResponse request (npmManager config) $ \response ->
        readBoundedBody (npmLimits config) (responseBody response)

{- | Raised when an upstream metadata body breaches a 'Ecluse.Core.Security.Limits'
ceiling: the body-size guard here, or: surfaced through the same type by the serve
pipeline: the version-count or nesting-depth guard.
-}
newtype ResponseBoundExceeded = ResponseBoundExceeded LimitError
    deriving stock (Eq, Show)

instance Exception ResponseBoundExceeded

{- Read a response body chunk-by-chunk through 'boundedRead' against the budget,
returning the whole body as a 'RegistryResponse' when within the cap. A body past
'Ecluse.Core.Security.maxBodyBytes' aborts the read fail-closed and is raised as a typed
'ResponseBoundExceeded' (never a truncated body). -}
readBoundedBody :: Limits -> BodyReader -> IO RegistryResponse
readBoundedBody limits bodyReader =
    boundedRead limits (brRead bodyReader) >>= \case
        Right body -> pure (RegistryResponse body)
        Left err -> throwIO (ResponseBoundExceeded err)

-- Fetch and __buffer__ a version's artifact bytes (the handle's 'fetchArtifact').
fetchArtifact' :: NpmClientConfig -> PackageName -> Version -> IO RegistryResponse
fetchArtifact' config name version = do
    request <- orThrow (artifactRequest (npmBaseUrl config) (npmToken config) name version)
    response <- httpLbs request (npmManager config)
    pure (RegistryResponse (toStrict (responseBody response)))

{- Publish a version's artifact: assemble the ecosystem-specific publish document
from the artifact metadata and raw tarball bytes, then PUT it, treating a
@409 Conflict@ (the version is already present) as idempotent success.
-}
publishArtifact' ::
    NpmClientConfig ->
    IO (Maybe Secret) ->
    PackageName ->
    Version ->
    MirrorArtifact ->
    ByteString ->
    IO (Either PublishFault ())
publishArtifact' config mintToken name version artifact tarball = do
    token <- mintToken
    let document = npmPublishDocument name version (maFilename artifact) (sriOf artifact) (sha1Of artifact) tarball
    case publishRequest (npmBaseUrl config) token name document of
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

{- | Relay a client's npm publish document to the publication target and return the
target's own response: the first-party publish primitive behind the @PUT /{pkg}@
serve path.
-}
relayPublishDocument ::
    NpmClientConfig ->
    PackageName ->
    ByteString ->
    IO (Either UrlFormationError PublishRelayResponse)
relayPublishDocument config name document =
    case publishRequest (npmBaseUrl config) (npmToken config) name document of
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

-- Pick the SRI (@dist.integrity@) string from the admitted digests, if present.
sriOf :: MirrorArtifact -> Maybe Text
sriOf = firstHashValue SRI

-- Pick the SHA-1 shasum from the admitted digests, if present.
sha1Of :: MirrorArtifact -> Maybe Text
sha1Of = firstHashValue SHA1

firstHashValue :: HashAlg -> MirrorArtifact -> Maybe Text
firstHashValue alg artifact =
    fmap hashValue (find ((== alg) . hashAlg) (NE.toList (maHashes artifact)))
