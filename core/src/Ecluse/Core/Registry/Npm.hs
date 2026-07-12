-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The npm __read and relay data plane__: the effectful metadata fetch and the
first-party publish relay over @http-client@.

This module is the network half of the npm read-side protocol boundary. Where
"Ecluse.Core.Registry.Npm.Wire" and "Ecluse.Core.Registry.Npm.Project" are the pure decode
and projection, this is the side-effecting exchange: 'fetchMetadataFormBounded'
reads a metadata document bounded with every failure in its typed channel, and
'relayPublishDocument' forwards a client's own publish to the publication target.
The mirror write is not here: its protocol codec lives in
"Ecluse.Core.Registry.Npm.Publish" and executes through the shared transport
("Ecluse.Core.Registry.Publish").

It speaks the npm registry protocol directly with @http-client@, __never__
@amazonka@: the control plane (the @GetAuthorizationToken@ mint, the mirror
queue) is @amazonka@'s job behind separate handles, but the data plane: fetch
metadata, stream a tarball, publish: is ordinary HTTPS+JSON, identical across
every npm-speaking backend. Keeping the streaming path off @amazonka@'s
@conduit@/@ResourceT@ machinery is exactly what makes bounded-memory artifact
proxying tractable.

== Streaming and buffering

The artifact request builders ('Ecluse.Core.Registry.Npm.Request.artifactRequestByFile'
and 'Ecluse.Core.Registry.Npm.Request.artifactRequestByUrl') mark their requests
__non-decompressing__ so a tarball is opaque binary that must reach the client
byte-for-byte, and are exposed so the web layer can relay the open body
__without buffering the whole artifact in memory__. The mirror worker, which must
read the whole artifact to verify its integrity before publishing, buffers it
(bounded) through 'Ecluse.Core.Worker.Fetch.fetchArtifactBytes' instead.

== Authentication

Every request here carries an __injected__ bearer token (or none); this module
never originates credential policy. Which token to send on which request is
the request pipeline's authority model, decided upstream of this module.
-}
module Ecluse.Core.Registry.Npm (
    -- * Construction
    NpmClientConfig (..),

    -- * Bounded metadata fetch
    fetchMetadataFormBounded,

    -- * First-party publish relay
    relayPublishDocument,
) where

import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client (
    BodyReader,
    Manager,
    Response (responseStatus),
    brRead,
    responseBody,
    withResponse,
 )
import Network.HTTP.Types.Status (statusCode)
import UnliftIO (try)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable),
    PublishRelayFault (RelayBoundExceeded, RelayTransport, RelayUrlUnformable),
    PublishRelayResponse (..),
    RegistryResponse (RegistryResponse),
 )

import Ecluse.Core.Registry.Npm.Publish (publishRequest)
import Ecluse.Core.Registry.Npm.Request (
    MetadataForm,
    Validators,
    metadataRequest,
 )
import Ecluse.Core.Security (
    LimitError,
    Limits,
    boundedRead,
 )

{- | Everything this data plane needs to talk to one npm-speaking registry: the
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

{- | Fetch a package's metadata in the requested 'MetadataForm', relaying any
conditional-GET 'Validators', reporting __every__ fetch failure as a
'Ecluse.Core.Registry.FetchFault' value: an unformable request URL, a response-bound
breach, or a transport fault ('classifyTransport' folds the @http-client@ exception
into the typed channel at this edge). Total: no fetch failure escapes as an
exception, so the serve read adapter ("Ecluse.Core.Registry.Npm.Metadata") threads
it straight into its own typed channel with no throw-then-catch round-trip.

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

{- Read a response body chunk-by-chunk through 'boundedRead' against the budget,
returning the whole body as a 'RegistryResponse' when within the cap, or the 'LimitError'
as a __value__ when the body crosses 'Ecluse.Core.Security.maxBodyBytes' (never a
truncated body). Returning the breach lets the serve read path thread it as a value; a
consumer behind an exception-shaped boundary wraps it in the agnostic
'Ecluse.Core.Registry.Fault.ResponseBoundExceeded' there. -}
readBoundedBody :: Limits -> BodyReader -> IO (Either LimitError RegistryResponse)
readBoundedBody limits bodyReader =
    fmap RegistryResponse <$> boundedRead limits (brRead bodyReader)

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
