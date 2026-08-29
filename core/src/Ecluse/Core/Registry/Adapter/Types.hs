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

import Network.HTTP.Client (Request)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (InvalidEntry, PackageName)
import Ecluse.Core.Package.Merge (MergePlan, SourceId)
import Ecluse.Core.Registry (
    FetchFault,
    PublishRelayResponse,
    UrlFormationError,
 )
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (MetadataClient, MetadataError)
import Ecluse.Core.Registry.Origin (OriginClient)
import Ecluse.Core.Registry.Publish (PublishCodec)
import Ecluse.Core.Registry.Request (CredentialMapping)
import Ecluse.Core.Server.Context (MountRouter)
import Ecluse.Core.Server.Metadata (ManifestCaching)
import Ecluse.Core.Server.RouteSpec (RouteSpec)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Telemetry.Span (TracingPort)

{- | One ecosystem's complete capability record, which the composition root wires every
consuming pipeline from. 'Ecluse.Core.Registry.Adapter.adapterFor' resolves it (npm's is
'Ecluse.Core.Registry.Npm.Adapter.npmAdapter').
-}
data RegistryAdapter = RegistryAdapter
    { adapterEcosystem :: Ecosystem
    {- ^ The ecosystem this record serves. The registry key must agree with it, so no record can
    register under a foreign ecosystem.
    -}
    , adapterServe :: AdapterServe
    -- ^ The web-facing serve surface: the route grammar and response contracts.
    , adapterMetadata :: AdapterMetadata
    -- ^ The metadata capability: the read-handle constructor and the packument assembly.
    , adapterArtifact :: AdapterArtifact
    -- ^ The artifact request formation, by filename and by authoritative URL.
    , adapterPublish :: AdapterPublish
    {- ^ The publish capability: the first-party relay, the name canonicaliser, the declared-name
    extractor, and the mirror write's protocol codec.
    -}
    }

{- | The ecosystem's web-facing serve surface. The adapter derives both routing fields from one
declarative route table (npm's is "Ecluse.Core.Registry.Npm.Route"), so the routed surface and
the documented one cannot drift apart.
-}
data AdapterServe = AdapterServe
    { serveRouter :: MountRouter
    {- ^ The ecosystem's whole routing decision: which path a mount-relative request names and
    what serving it amounts to (an 'Ecluse.Core.Server.Context.RouteAction'). An unrecognised
    path yields the deny-by-default @404@.
    -}
    , serveRoutes :: NonEmpty RouteSpec
    {- ^ The same route table as data, one 'RouteSpec' per route 'serveRouter' serves. The
    OpenAPI spec ("Ecluse.Manifest") renders this instead of re-describing the grammar.
    -}
    , serveCredential :: CredentialMapping
    {- ^ The ecosystem's credential presentation: how the mount recovers a client's credential
    from its headers, and how Écluse carries one upstream. The neutral pipeline spells no scheme
    of its own and keeps the constant-time edge compare and the deny-by-default refusal.
    -}
    }

{- | The ecosystem's metadata capability: reading a package's metadata from an origin,
assembling the served document, and encoding it. The fields match the consuming dependency
record ('Ecluse.Core.Server.Context.pdNewMetadataClient' and
'Ecluse.Core.Server.Context.pdAssemble'), so the composition root projects them unchanged.
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
        OriginClient ->
        MetadataClient
    {- ^ Build a per-request metadata client for one origin, given the serve-path
    observation callbacks and the origin to read through. The adapter closes over the
    ecosystem's raw fetch primitives.
    -}
    , metadataAssemble :: Text -> Map SourceId CachedDoc -> MergePlan -> Maybe CachedDoc -> CachedDoc
    {- ^ Assemble the served document from a merge plan, the raw source documents, and the
    precedence-winning base document ('Nothing' when there is none), rewriting each surviving
    version's artifact URL under the given mount base.
    -}
    , metadataSerialise :: CachedDoc -> LByteString
    -- ^ Encode an assembled served document ('CachedDoc') to its wire bytes.
    }

{- | The ecosystem's artifact request formation: by conventional filename under a registry base,
or at the artifact's authoritative upstream URL. The request fields match the consuming
dependency record ('Ecluse.Core.Server.Context.pdBuildArtifactRequestByFile' and
'Ecluse.Core.Server.Context.pdBuildArtifactRequestByUrl').
-}
data AdapterArtifact = AdapterArtifact
    { artifactByFile :: OriginClient -> PackageName -> Text -> Either UrlFormationError Request
    {- ^ Build an artifact request by conventional filename path under the origin's base URL:
    how the proxy addresses a trusted origin.
    -}
    , artifactByUrl :: Maybe Secret -> Text -> Either UrlFormationError Request
    {- ^ Build an artifact request at its authoritative upstream URL. The URL is complete on
    its own, so an implementation forms the request from it and the credential alone. It takes
    no origin, because the mirror worker's fetch has none to name.
    -}
    , artifactHosts :: [Text]
    {- ^ The ecosystem's canonical artifact hosts, whose authorities feed the tarball-host gate.
    The secure-default same-host policy admits them (PyPI's is @https://files.pythonhosted.org@)
    without the operator naming hostnames. Empty for npm, whose artifacts ride the registry host.
    -}
    }

{- | The ecosystem's publish capability: the first-party relay, the name canonicaliser, the
declared-name extractor, and the mirror write's protocol codec. The composition root marries
the codec to the shared publish transport per mounted ecosystem
('Ecluse.Core.Registry.Publish.newMirrorPublish').
-}
data AdapterPublish = AdapterPublish
    { publishRelay :: OriginClient -> PackageName -> ByteString -> IO (Either FetchFault PublishRelayResponse)
    {- ^ Relay a client's publish document to the publication target, named as the origin to
    write through, and return the target's own response.
    -}
    , publishCanonicaliseName :: Text -> Maybe PackageName
    -- ^ Canonicalise a raw package-name string, or 'Nothing' when it cannot be parsed.
    , publishDeclaredNames :: LByteString -> [Text]
    {- ^ Extract every package name a publish body declares as its own identity. The
    anti-shadowing guard refuses any declared name that disagrees with the URL-path name. A body
    that declares no readable name yields @[]@.
    -}
    , publishCodec :: PublishCodec
    {- ^ The mirror write's protocol codec: publish document assembly, request formation, the
    probe's request and version-list projection, and the status semantics. Protocol only: the
    manager, credential mint, and fault classification belong to the shared transport.
    -}
    }
