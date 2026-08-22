-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The vocabulary of the ecosystem adapter registry: the capability record an
ecosystem registers ('RegistryAdapter') and its four cohesive slices.

A 'RegistryAdapter' captures what an ecosystem __is__: a static fact of the build,
independent of anything an operator configures. It holds the serve surface, the metadata
read and assembly, the artifact request formation, and the publish path. Which
ecosystems are __active__ is configuration's fact, not this record's: nothing here holds
a URL, a credential, a limit, or a policy. Those arrive as arguments when the
composition root projects a consuming pipeline's dependency record
('Ecluse.Core.Server.Context.PackumentDeps', 'Ecluse.Core.Server.Context.PublishDeps',
the worker runtime's fetch wiring) from an adapter's fields. The pipelines keep their
own records and never read this one. The root resolves an adapter at boot, so it never
rides the hot path.

The record vocabulary lives apart from the registration
("Ecluse.Core.Registry.Adapter"). An ecosystem's adapter module can then type its
record without importing the registry, which must import every adapter. That is the
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
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (MetadataClient, MetadataError)
import Ecluse.Core.Registry.Publish (PublishCodec)
import Ecluse.Core.Registry.Request (CredentialMapping)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Server.Context (MountRouter)
import Ecluse.Core.Server.Metadata (ManifestCaching)
import Ecluse.Core.Server.RouteSpec (RouteSpec)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Telemetry.Span (TracingPort)

{- | One ecosystem's complete capability record: the interfaces the composition root
projects every consuming pipeline's wiring from. Each ecosystem assembles it once
(npm's is 'Ecluse.Core.Registry.Npm.Adapter.npmAdapter'), and
'Ecluse.Core.Registry.Adapter.adapterFor' resolves it.
-}
data RegistryAdapter = RegistryAdapter
    { adapterEcosystem :: Ecosystem
    {- ^ The ecosystem this record serves. The registry's key must agree with it
    (pinned by the adapter spec), so no record can register under a foreign
    ecosystem unnoticed.
    -}
    , adapterServe :: AdapterServe
    -- ^ The web-facing serve surface: the route grammar and response contracts.
    , adapterMetadata :: AdapterMetadata
    -- ^ The metadata capability: the read-handle constructor and the packument assembly.
    , adapterArtifact :: AdapterArtifact
    -- ^ The artifact request formation, by filename and by authoritative URL.
    , adapterPublish :: AdapterPublish
    {- ^ The publish capability: the first-party relay, the name canonicaliser, the
    mirror write's protocol codec, and the declared-name extractor. The anti-shadowing
    guard reads a publish body through that extractor.
    -}
    }

{- | The ecosystem's web-facing serve surface: the one slice of the record about HTTP
shape rather than registry protocol. Its types come from the agnostic action and
response vocabulary ("Ecluse.Core.Server.Context", "Ecluse.Core.Server.Response"),
because it is web-facing by definition. The registry-to-server import direction is
deliberate here. Serve surfaces are where ecosystems diverge most, so this slice
stands alone rather than sharing shape with the protocol slices.

The adapter __derives both routing fields from one declarative route table__ (npm's is
"Ecluse.Core.Registry.Npm.Route"). The surface the server routes and the surface the
manifest documents are therefore two readings of a single declaration, and cannot
drift apart. The credential presentation joins them here because it states the same
kind of fact: what this ecosystem's clients put on the wire.
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
    documented surface cannot drift from the routed one.
    -}
    , serveCredential :: CredentialMapping
    {- ^ The ecosystem's __credential presentation__: how the mount recovers a client's
    credential from the headers the client presents. The same presentation carries a
    credential on a request Écluse makes upstream. A mount accepts exactly the form its
    ecosystem presents. The neutral pipeline therefore spells no scheme of its own, and
    still keeps the constant-time edge compare and the deny-by-default refusal.
    -}
    }

{- | The ecosystem's metadata capability: how the proxy reads a package's metadata
from an origin and how it assembles a served document. The fields have exactly the
shapes the consuming dependency records carry
('Ecluse.Core.Server.Context.pdNewMetadataClient' and
'Ecluse.Core.Server.Context.pdAssemble'). The composition root therefore projects
them unchanged, and registering an adapter cannot reshape a pipeline.
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
    runtime parameters. The adapter closes over the ecosystem's raw fetch
    primitives.
    -}
    , metadataAssemble :: Text -> Map SourceId CachedDoc -> MergePlan -> Maybe CachedDoc -> CachedDoc
    {- ^ Assemble the served document ('CachedDoc') from a merge plan and the raw source
    documents, rewriting each surviving version's artifact URL under the given mount
    base. The adapter projects each source and the precedence-winning base document
    ('Nothing' when there is none) into its own representation. It merges them and
    injects the result back, so the neutral pipeline threads the documents without
    reading them.
    -}
    , metadataSerialise :: CachedDoc -> LByteString
    {- ^ Encode an assembled served document ('CachedDoc') to its wire bytes. The
    adapter projects the document to its own representation and serialises it. The
    neutral serve tail turns a served document into bytes without knowing its shape.
    -}
    }

{- | The ecosystem's artifact request formation: the two ways the proxy addresses an
artifact. One is a conventional filename under a registry base, the other its
authoritative upstream URL. The fields have exactly the shapes the consuming dependency
records carry ('Ecluse.Core.Server.Context.pdBuildArtifactRequestByFile' and
'Ecluse.Core.Server.Context.pdBuildArtifactRequestByUrl').
-}
data AdapterArtifact = AdapterArtifact
    { artifactByFile :: Limits -> Manager -> Text -> Maybe Secret -> PackageName -> Text -> Either UrlFormationError Request
    {- ^ Build an artifact request by conventional filename path under a base URL:
    how the proxy addresses a trusted origin.
    -}
    , artifactByUrl :: Limits -> Manager -> Text -> Maybe Secret -> Text -> Either UrlFormationError Request
    {- ^ Build an artifact request at its authoritative upstream URL: how the proxy
    honours a location the upstream chose. The URL is complete on its own, so an
    implementation must form the request from it alone. A caller may pass an empty
    base URL and an anonymous credential ('Nothing'), as the mirror worker's fetch
    does. There is no base to resolve against.
    -}
    , artifactHosts :: [Text]
    {- ^ The ecosystem's canonical artifact hosts, as URLs whose authorities feed
    the tarball-host gate. These are hosts the public registry serves artifact bytes
    from __by design__ (PyPI's @https://files.pythonhosted.org@). The secure-default
    same-host policy admits them without the operator naming hostnames. Empty for
    an ecosystem (npm) whose artifacts ride the registry host.
    -}
    }

{- | The ecosystem's publish capability: relaying a client's own publish document and
canonicalising a raw package name. It also extracts the names a publish body declares,
and holds the mirror-write protocol codec. The relay, canonicaliser, and declared-name
extractor have exactly the shapes the consuming dependency record carries
('Ecluse.Core.Server.Context.pubRelayPublish',
'Ecluse.Core.Server.Context.pubCanonicaliseName', and
'Ecluse.Core.Server.Context.pubDeclaredNames'). The codec is the protocol half of the
mirror write. The composition root marries it to the shared publish transport per
mounted ecosystem ('Ecluse.Core.Registry.Publish.newMirrorPublish').
-}
data AdapterPublish = AdapterPublish
    { publishRelay :: Limits -> Manager -> Text -> Maybe Secret -> PackageName -> ByteString -> IO (Either PublishRelayFault PublishRelayResponse)
    -- ^ Relay a client's publish document to the publication target, returning its response.
    , publishCanonicaliseName :: Text -> Maybe PackageName
    -- ^ Canonicalise a raw package-name string, or 'Nothing' when it cannot be parsed.
    , publishDeclaredNames :: LByteString -> [Text]
    {- ^ Extract every package name a publish body declares as its own identity, read
    from the ecosystem's own publish-document schema. The neutral publish pipeline's
    anti-shadowing body-name guard injects this and refuses any declared name that
    disagrees with the URL-path name. The npm document shape therefore stays
    adapter-side instead of coupling the pipeline. A body that declares no readable
    name yields @[]@.
    -}
    , publishCodec :: PublishCodec
    {- ^ The mirror write's protocol codec: publish document assembly and request
    formation, the probe's request and version-list projection, and the status
    semantics. Protocol only: the manager, credential mint, and fault classification
    belong to the shared transport, supplied at the marriage.
    -}
    }
