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

import Network.HTTP.Client (Manager)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (
    FetchFault (FetchUrlUnformable),
    PublishRelayFault (RelayUrlUnformable),
    PublishRelayResponse,
    RegistryResponse,
 )

import Ecluse.Core.Registry.Exchange (boundedFetch, boundedRelay)
import Ecluse.Core.Registry.Npm.Publish (publishRequest)
import Ecluse.Core.Registry.Npm.Request (
    MetadataForm,
    Validators,
    metadataRequest,
 )
import Ecluse.Core.Security (Limits)

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
            boundedFetch (npmManager config) (npmLimits config) request

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
            boundedRelay (npmManager config) (npmLimits config) request
