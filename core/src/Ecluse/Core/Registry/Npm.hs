-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The npm __read and relay data plane__: the effectful metadata fetch and the
first-party publish relay over @http-client@.

This module is the network half of the npm read-side protocol boundary.
"Ecluse.Core.Registry.Npm.Wire" and "Ecluse.Core.Registry.Npm.Project" are the pure
decode and projection. This module is the side-effecting exchange.
'fetchMetadataFormBounded' reads a metadata document bounded, with every failure in
its typed channel. 'relayPublishDocument' forwards a client's own publish to the
publication target. The mirror write is not here. Its protocol codec lives in
"Ecluse.Core.Registry.Npm.Publish" and executes through the shared transport
("Ecluse.Core.Registry.Publish").

It speaks the npm registry protocol directly with @http-client@, __never__
@amazonka@. The control plane (the @GetAuthorizationToken@ mint, the mirror queue)
is @amazonka@'s job behind separate handles. The data plane is ordinary HTTPS and
JSON, identical across every npm-speaking backend: fetch metadata, stream a tarball,
publish. Keeping the streaming path off @amazonka@'s @conduit@/@ResourceT@ machinery
is what makes bounded-memory artifact proxying tractable.

== Streaming and buffering

The artifact request builders ('Ecluse.Core.Registry.Npm.Request.artifactRequestByFile'
and 'Ecluse.Core.Registry.Npm.Request.artifactRequestByUrl') mark their requests
__non-decompressing__, because a tarball is opaque binary that must reach the client
byte-for-byte. The module exports them so the web layer can relay the open body
__without buffering the whole artifact in memory__. The mirror worker must read the whole
artifact to verify its integrity before publishing, so it buffers the artifact
(bounded) through 'Ecluse.Core.Worker.Fetch.fetchArtifactBytes' instead.

== Authentication

Every request here carries an __injected__ bearer token, or none. This module never
originates credential policy. Which token to send on which request is the request
pipeline's authority model, decided upstream of this module.
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
    PublishRelayResponse,
    RegistryResponse,
 )

import Ecluse.Core.Registry.Exchange (boundedFetch, boundedRelay, formThen)
import Ecluse.Core.Registry.Npm.Publish (publishRequest)
import Ecluse.Core.Registry.Npm.Request (
    MetadataForm,
    Validators,
    metadataRequest,
 )
import Ecluse.Core.Security (Limits)

{- | Everything this data plane needs to reach one npm-speaking registry. The composition
root supplies the pooled 'Manager', and the request pipeline picks the token.
-}
data NpmClientConfig = NpmClientConfig
    { npmBaseUrl :: Text
    {- ^ The registry base URL (e.g. the public registry, or a CodeArtifact npm
    endpoint). The proxy appends the package path to it.
    -}
    , npmManager :: Manager
    -- ^ The shared @http-client@ 'Manager' to issue requests through.
    , npmToken :: Maybe Secret
    -- ^ An injected bearer token to attach, or 'Nothing' for anonymous requests.
    , npmLimits :: Limits
    {- ^ The response-bound budget 'fetchMetadataFormBounded' enforces on a metadata fetch,
    fail-closed past 'Ecluse.Core.Security.maxBodyBytes'.
    -}
    }

{- | Fetch a package's metadata in the requested 'MetadataForm', relaying any conditional-GET
'Validators'. Every failure, the body read included, comes back as a 'FetchFault' value, never
an exception. It reads the body through 'Ecluse.Core.Security.boundedRead' and refuses one past
'Ecluse.Core.Security.maxBodyBytes' fail-closed, so a hostile upstream cannot exhaust memory.
-}
fetchMetadataFormBounded ::
    NpmClientConfig ->
    MetadataForm ->
    Validators ->
    PackageName ->
    IO (Either FetchFault RegistryResponse)
fetchMetadataFormBounded config form validators name =
    formThen
        FetchUrlUnformable
        (boundedFetch (npmManager config) (npmLimits config))
        (metadataRequest (npmBaseUrl config) (npmToken config) form validators name)

{- | Relay a client's npm publish document to the publication target and return the
target's own response. It is the first-party publish primitive behind the
@PUT /{pkg}@ serve path.
-}
relayPublishDocument ::
    NpmClientConfig ->
    PackageName ->
    ByteString ->
    IO (Either FetchFault PublishRelayResponse)
relayPublishDocument config name document =
    formThen
        FetchUrlUnformable
        (boundedRelay (npmManager config) (npmLimits config))
        (publishRequest (npmBaseUrl config) (npmToken config) name document)
