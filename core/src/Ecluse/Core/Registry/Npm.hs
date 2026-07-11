-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

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
the whole artifact in memory__. The mirror worker, which must read the whole
artifact to verify its integrity before publishing, buffers it (bounded) through
'Ecluse.Core.Worker.Fetch.fetchArtifactBytes' instead.

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
    fetchMetadataFormBounded,

    -- * First-party publish relay
    relayPublishDocument,

    -- * Response-bound breach
    ResponseBoundExceeded (..),
) where

import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NE
import Network.HTTP.Client (
    BodyReader,
    HttpException,
    Manager,
    Response (responseStatus),
    brRead,
    httpLbs,
    responseBody,
    withResponse,
 )
import Network.HTTP.Types.Status (statusCode)
import UnliftIO (try)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Package (Hash (hashAlg, hashValue), HashAlg (SHA1, SRI), PackageName)
import Ecluse.Core.Queue (MirrorArtifact (maFilename, maHashes))
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable),
    ParseError (ParseError),
    PublishError (..),
    PublishFault (PublishRejected, PublishTransport, PublishUrlUnformable),
    PublishRelayFault (RelayBoundExceeded, RelayTransport, RelayUrlUnformable),
    PublishRelayResponse (..),
    RegistryClient (..),
    RegistryResponse (RegistryResponse),
 )

import Ecluse.Core.Registry.Npm.Project qualified as Project
import Ecluse.Core.Registry.Npm.Publish (npmPublishDocument, publishRequest)
import Ecluse.Core.Registry.Npm.Request (
    MetadataForm (Abbreviated),
    Validators,
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
    {- ^ The response-bound budget enforced on a metadata fetch:
    'fetchMetadataFormBounded' reads the body through
    'Ecluse.Core.Security.boundedRead' against 'Ecluse.Core.Security.maxBodyBytes',
    aborting fail-closed past the cap rather than buffering an unbounded body.
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
unconditionally; the request pipeline reaches the richer forms (the full packument,
relayed validators) through 'fetchMetadataFormBounded' directly.
-}
newNpmClient :: NpmClientConfig -> IO RegistryClient
newNpmClient config = newNpmPublishClient config (pure (npmToken config))

{- | Build an npm RegistryClient whose 'Ecluse.Core.Registry.publishArtifact' and
'Ecluse.Core.Registry.fetchMetadata' fields mint a fresh token per call via the provided
IO action; the remaining fields use the token in the config. The metadata read mints
because the worker's mirror-presence probe reads the mirror target through this handle,
and a managed mirror (CodeArtifact) requires auth on reads as on writes -- an anonymous
probe would be refused and the dedup would never confirm anything. For 'newNpmClient'
the mint is the configured token, so its behaviour is unchanged.
-}
newNpmPublishClient :: NpmClientConfig -> IO (Maybe Secret) -> IO RegistryClient
newNpmPublishClient config mintToken =
    pure
        RegistryClient
            { fetchMetadata = \name -> do
                token <- mintToken
                fetchMetadataFormBounded config{npmToken = token} Abbreviated noValidators name
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
conditional-GET 'Validators', reporting __every__ fetch failure as a
'Ecluse.Core.Registry.FetchFault' value: an unformable request URL, a response-bound
breach, or a transport fault ('classifyTransport' folds the @http-client@ exception
into the typed channel at this edge). Total: no fetch failure escapes as an
exception, so the serve read adapter ("Ecluse.Core.Registry.Npm.Metadata") and the
handle's 'Ecluse.Core.Registry.fetchMetadata' thread it straight into their own
typed channels with no throw-then-catch round-trip.

The body is read __chunk-by-chunk through 'Ecluse.Core.Security.boundedRead'__ against
the config's 'npmLimits', not buffered whole: a hostile or compromised upstream returning
a body larger than 'Ecluse.Core.Security.maxBodyBytes' is refused __fail-closed__ as a
'FetchBoundExceeded' rather than exhausting memory. The transport wrap covers the
__whole__ exchange, the body read included: metadata is buffered before anything is
served, so a connection lost mid-body is still a pre-commit fault with a value
representation, not a half-delivered response.
-}
fetchMetadataFormBounded ::
    NpmClientConfig ->
    MetadataForm ->
    Validators ->
    PackageName ->
    IO (Either FetchFault RegistryResponse)
fetchMetadataFormBounded config form validators name =
    case metadataRequest (npmBaseUrl config) (npmToken config) form validators name of
        Left urlErr -> pure (Left (FetchUrlUnformable urlErr))
        Right request ->
            try (withResponse request (npmManager config) $ \response -> readBoundedBody (npmLimits config) (responseBody response))
                <&> \case
                    Left httpErr -> Left (FetchTransport (classifyTransport httpErr))
                    Right (Left limitErr) -> Left (FetchBoundExceeded limitErr)
                    Right (Right response) -> Right response

{- | The thrown form of a response-bound breach: a body that crossed the
'Ecluse.Core.Security.maxBodyBytes' ceiling, carried as its 'LimitError'. The bounded
read itself reports the breach as a __value__ ('readBoundedBody' returns an
'Either'); this exception is what the deliberately-throwing consumers re-raise at their
own boundary -- the publish relay ('readRelayResponse') and the worker's bounded
artifact fetch ("Ecluse.Core.Worker.Fetch") -- so a @tryAny@ caller sees a typed
breach rather than a truncated body.
-}
newtype ResponseBoundExceeded = ResponseBoundExceeded LimitError
    deriving stock (Eq, Show)

instance Exception ResponseBoundExceeded

{- Read a response body chunk-by-chunk through 'boundedRead' against the budget,
returning the whole body as a 'RegistryResponse' when within the cap, or the 'LimitError'
as a __value__ when the body crosses 'Ecluse.Core.Security.maxBodyBytes' (never a
truncated body). Returning the breach lets the serve read path thread it as a value; the
throwing callers re-raise it as a 'ResponseBoundExceeded' at their own boundary. -}
readBoundedBody :: Limits -> BodyReader -> IO (Either LimitError RegistryResponse)
readBoundedBody limits bodyReader =
    fmap RegistryResponse <$> boundedRead limits (brRead bodyReader)

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
        Right request ->
            try (httpLbs request (npmManager config)) <&> \case
                Left (err :: HttpException) ->
                    Left (PublishTransport ("publish transport failure: " <> show err))
                Right response -> classifyPublish (statusCode (responseStatus response))

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
    IO (Either PublishRelayFault PublishRelayResponse)
relayPublishDocument config name document =
    case publishRequest (npmBaseUrl config) (npmToken config) name document of
        Left urlErr -> pure (Left (RelayUrlUnformable urlErr))
        Right request ->
            -- The transport wrap covers the whole exchange, the bounded body
            -- read included: the relay buffers the target's response before
            -- anything is answered, so a mid-body reset is still a pre-commit
            -- fault with a value representation.
            try (withResponse request (npmManager config) (readRelayResponse (npmLimits config)))
                <&> \case
                    Left httpErr -> Left (RelayTransport (classifyTransport httpErr))
                    Right relayed -> relayed

{- Buffer the publication target's response to a relayed publish: the body read
bounded against the budget (an overstep is the typed 'RelayBoundExceeded'),
paired with the status the target answered. -}
readRelayResponse :: Limits -> Response BodyReader -> IO (Either PublishRelayFault PublishRelayResponse)
readRelayResponse limits response =
    readBoundedBody limits (responseBody response) <&> \case
        Left limitErr -> Left (RelayBoundExceeded limitErr)
        Right (RegistryResponse body) ->
            Right
                PublishRelayResponse
                    { relayStatus = statusCode (responseStatus response)
                    , relayBody = LBS.fromStrict body
                    }

-- Pick the SRI (@dist.integrity@) string from the admitted digests, if present.
sriOf :: MirrorArtifact -> Maybe Text
sriOf = firstHashValue SRI

-- Pick the SHA-1 shasum from the admitted digests, if present.
sha1Of :: MirrorArtifact -> Maybe Text
sha1Of = firstHashValue SHA1

firstHashValue :: HashAlg -> MirrorArtifact -> Maybe Text
firstHashValue alg artifact =
    fmap hashValue (find ((== alg) . hashAlg) (NE.toList (maHashes artifact)))
