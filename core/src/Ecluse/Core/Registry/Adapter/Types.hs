-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The vocabulary of the ecosystem adapter registry: the capability record an
ecosystem registers ('RegistryAdapter') and its four cohesive slices.

A 'RegistryAdapter' captures what an ecosystem __is__ -- a static fact of the
build, independent of anything an operator configures: how its request paths
classify and its errors render (the serve surface), how its metadata is read and
assembled, how its artifact requests are formed, and how a publish reaches a
registry. Which ecosystems are __active__ is configuration's fact, not this
record's: nothing here holds a URL, a credential, a limit, or a policy. Those
arrive as arguments when the composition root projects a consuming pipeline's
dependency record ('Ecluse.Core.Server.Context.PackumentDeps',
'Ecluse.Core.Server.Context.PublishDeps', the worker runtime's fetch wiring) from
an adapter's fields. The pipelines keep their own records and never read this one,
so an adapter is resolved at boot and never rides the hot path.

The record vocabulary lives apart from the registration
("Ecluse.Core.Registry.Adapter") so an ecosystem's adapter module can type its
record without importing the registry, which must import every adapter -- the
cycle-breaking @.Types@ extraction STYLE.md sanctions.
-}
module Ecluse.Core.Registry.Adapter.Types (
    -- * The capability record
    RegistryAdapter (..),

    -- * The serve surface
    AdapterServe (..),

    -- * Metadata
    AdapterMetadata (..),

    -- * Artifact requests
    AdapterArtifact (..),

    -- * Publish
    AdapterPublish (..),
) where

import Data.Aeson (Value)
import Network.HTTP.Client (Manager, Request)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (InvalidEntry, PackageName)
import Ecluse.Core.Package.Merge (MergePlan, SourceId)
import Ecluse.Core.Registry (
    PublishRelayFault,
    PublishRelayResponse,
    UrlFormationError,
 )
import Ecluse.Core.Registry.Metadata (MetadataClient, MetadataError)
import Ecluse.Core.Registry.Publish (PublishCodec)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Server.Context (MountRouter)
import Ecluse.Core.Server.Metadata (ManifestCaching)
import Ecluse.Core.Server.Response (MountRenderer)
import Ecluse.Core.Server.RouteSpec (RouteSpec)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Telemetry.Span (TracingPort)

{- | One ecosystem's complete capability record: the interfaces every consuming
pipeline's wiring is projected from. Assembled once per ecosystem (npm's is
'Ecluse.Core.Registry.Npm.Adapter.npmAdapter') and resolved through
'Ecluse.Core.Registry.Adapter.adapterFor'.
-}
data RegistryAdapter = RegistryAdapter
    { adapterEcosystem :: Ecosystem
    {- ^ The ecosystem this record serves. The registry's key must agree with it
    (pinned by the adapter spec), so a record can never be registered under a
    foreign ecosystem unnoticed.
    -}
    , adapterServe :: AdapterServe
    -- ^ The web-facing serve surface: the path grammar and the error renderer.
    , adapterMetadata :: AdapterMetadata
    -- ^ The metadata capability: the read-handle constructor and the packument assembly.
    , adapterArtifact :: AdapterArtifact
    -- ^ The artifact request formation, by filename and by authoritative URL.
    , adapterPublish :: AdapterPublish
    {- ^ The publish capability: the first-party relay, the name canonicaliser, and
    the mirror write's protocol codec.
    -}
    }

{- | The ecosystem's web-facing serve surface: the one slice of the record that is
about HTTP shape rather than registry protocol, typed against the agnostic action
and response vocabulary ("Ecluse.Core.Server.Context", "Ecluse.Core.Server.Response")
because it is web-facing by definition (the registry-to-server import direction is
deliberate here). Serve surfaces are where ecosystems diverge most, so this slice
stands alone rather than sharing shape with the protocol slices.

Both routing fields are __derived by the adapter from one declarative route table__
(npm's is "Ecluse.Core.Registry.Npm.Route"), so the surface the server routes and the
surface the manifest documents are two interpretations of a single declaration and
cannot drift apart.
-}
data AdapterServe = AdapterServe
    { serveRouter :: MountRouter
    {- ^ The ecosystem's __whole routing decision__: which of its paths a
    mount-relative request names, and what serving that amounts to (an
    'Ecluse.Core.Server.Context.RouteAction'). The authoritative router the server
    dispatches through. An unrecognised path yields the deny-by-default @404@.
    -}
    , serveRoutes :: NonEmpty RouteSpec
    {- ^ The same route table as data: the declarative 'RouteSpec' projection of the
    patterns 'serveRouter' routes on, one per served route. The capability manifest
    ("Ecluse.Manifest") renders this rather than re-describing the path grammar, so the
    documented surface cannot drift from what is routed.
    -}
    , serveRenderer :: MountRenderer
    -- ^ The ecosystem body shape an in-mount denial or error renders through.
    }

{- | The ecosystem's metadata capability: how a package's metadata is read from an
origin and how a served document is assembled. The fields have exactly the shapes
the consuming dependency records carry
('Ecluse.Core.Server.Context.pdNewMetadataClient' and
'Ecluse.Core.Server.Context.pdAssemble'), so the composition root projects them
unchanged and registering an adapter cannot reshape a pipeline.
-}
data AdapterMetadata = AdapterMetadata
    { metadataNewClient ::
        TracingPort ->
        MetricsPort ->
        Metric.Upstream ->
        ManifestCaching ->
        (PackageName -> MetadataError -> IO ()) ->
        (PackageName -> [InvalidEntry] -> IO ()) ->
        (PackageName -> IO ()) ->
        Limits ->
        Manager ->
        Text ->
        Maybe Secret ->
        MetadataClient
    {- ^ Build a per-request metadata client for one origin, given the per-fetch
    runtime parameters; the adapter closes over the ecosystem's raw fetch
    primitives.
    -}
    , metadataAssemble :: Text -> Map SourceId Value -> MergePlan -> Value -> Value
    {- ^ Assemble the served document from a merge plan and the raw source
    documents, rewriting each surviving version's artifact URL under the given
    mount base.
    -}
    }

{- | The ecosystem's artifact request formation: the two ways an artifact is
addressed, by conventional filename under a registry base and by its authoritative
upstream URL. The fields have exactly the shapes the consuming dependency records
carry ('Ecluse.Core.Server.Context.pdBuildArtifactRequestByFile' and
'Ecluse.Core.Server.Context.pdBuildArtifactRequestByUrl').
-}
data AdapterArtifact = AdapterArtifact
    { artifactByFile :: Limits -> Manager -> Text -> Maybe Secret -> PackageName -> Text -> Either UrlFormationError Request
    {- ^ Build an artifact request by conventional filename path under a base URL:
    how a trusted origin is addressed.
    -}
    , artifactByUrl :: Limits -> Manager -> Text -> Maybe Secret -> Text -> Either UrlFormationError Request
    {- ^ Build an artifact request at its authoritative upstream URL: how a
    location the upstream chose is honoured. The URL is complete on its own, so an
    implementation must form the request from it alone: a caller may pass an empty
    base URL (there is no base to resolve against) and an anonymous credential
    ('Nothing'), as the mirror worker's fetch does.
    -}
    }

{- | The ecosystem's publish capability: relaying a client's own publish document,
canonicalising a raw package name, and the mirror-write protocol codec. The relay
and canonicaliser have exactly the shapes the consuming dependency record carries
('Ecluse.Core.Server.Context.pubRelayPublish' and
'Ecluse.Core.Server.Context.pubCanonicaliseName'); the codec is the protocol half
of the mirror write, which the composition root marries to the shared publish
transport per mounted ecosystem ('Ecluse.Core.Registry.Publish.newMirrorPublish').
-}
data AdapterPublish = AdapterPublish
    { publishRelay :: Limits -> Manager -> Text -> Maybe Secret -> PackageName -> ByteString -> IO (Either PublishRelayFault PublishRelayResponse)
    -- ^ Relay a client's publish document to the publication target, returning its response.
    , publishCanonicaliseName :: Text -> Maybe PackageName
    -- ^ Canonicalise a raw package-name string, or 'Nothing' when it cannot be parsed.
    , publishCodec :: PublishCodec
    {- ^ The mirror write's protocol codec: publish document assembly and request
    formation, the probe's request and version-list projection, and the status
    semantics -- protocol only. The manager, credential mint, and fault
    classification are the shared transport's, supplied at the marriage.
    -}
    }
