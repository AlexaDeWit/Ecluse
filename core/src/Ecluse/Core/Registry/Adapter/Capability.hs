-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The three ecosystem capability slices a consuming pipeline's dependency record
__embeds__: the metadata read and assembly, the artifact request formation, and the publish
path. Nothing here holds a URL, a credential, a limit, or a policy.

They sit below the serve surface because the deps records carry them as fields, while the
fourth slice ('Ecluse.Core.Registry.Adapter.Types.AdapterServe') names the routing knot
defined over those same records. The split is what makes both directions typeable.
-}
module Ecluse.Core.Registry.Adapter.Capability (
    -- * Metadata
    AdapterMetadata (..),

    -- * Artifact requests
    AdapterArtifact (..),

    -- * Publish
    AdapterPublish (..),
) where

import Network.HTTP.Client (Request)

import Ecluse.Core.Credential (Secret)
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
import Ecluse.Core.Server.Metadata (ManifestCaching)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Telemetry.Span (TracingPort)

{- | The ecosystem's metadata capability: reading a package's metadata from an origin,
assembling the served document, and encoding it ('Ecluse.Core.Server.Context.pdMetadata').
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
    {- ^ Build a per-request metadata client for one origin. The adapter closes over the
    ecosystem's raw fetch primitives, and the caller names the origin and the observers.
    -}
    , metadataAssemble :: Text -> Map SourceId CachedDoc -> MergePlan -> Maybe CachedDoc -> CachedDoc
    {- ^ Assemble the served document from a merge plan, the raw source documents, and the
    precedence-winning base document ('Nothing' when there is none), rewriting each surviving
    version's artifact URL under the given mount base.
    -}
    , metadataSerialise :: CachedDoc -> LByteString
    -- ^ Encode an assembled served document ('CachedDoc') to its wire bytes.
    }

{- | The ecosystem's artifact request formation, by conventional filename or authoritative URL.
The serve deps and the worker bundle share it ('Ecluse.Core.Server.Context.pdArtifact').
-}
data AdapterArtifact = AdapterArtifact
    { artifactByFile :: OriginClient -> PackageName -> Text -> Either UrlFormationError Request
    {- ^ Build an artifact request by conventional filename path under the origin's base URL:
    how the proxy addresses a trusted origin.
    -}
    , artifactByUrl :: Maybe Secret -> Text -> Either UrlFormationError Request
    {- ^ Build an artifact request at its authoritative upstream URL. It names no origin: the
    URL is complete on its own, and the mirror worker's fetch has none to give.
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
