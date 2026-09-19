-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Registry capabilities used by the ecosystem-neutral pipeline.
Adapters own wire formats and share exact source-entry selection for served metadata.
-}
module Ecluse.Core.Registry.Adapter.Capability (
    -- * Metadata
    AdapterMetadata (..),
    ManifestFetch,

    -- * Artifact requests
    AdapterArtifact (..),

    -- * Publish
    AdapterPublish (..),

    -- * Names
    ProjectName,

    -- * Store maintenance
    AdapterMaintenance (..),
    StoreListing (..),
    VersionDelete (..),
) where

import Network.HTTP.Client (Request)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (InvalidEntry, PackageName)
import Ecluse.Core.Package.Merge (MergePlan, SourceId)
import Ecluse.Core.Registry (
    FetchFault,
    ParseError,
    PublishRelayResponse,
    RegistryResponse,
    UrlFormationError,
 )
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Maintenance (NameAlphabet, StoreRefusal)
import Ecluse.Core.Registry.Metadata (Manifest, MetadataError)
import Ecluse.Core.Registry.Origin (OriginClient, OriginFor)
import Ecluse.Core.Registry.Publish (PublishCodec)
import Ecluse.Core.Server.Metadata (MetadataReads)
import Ecluse.Core.Snapshot (Snapshot)
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Telemetry.Span (TracingPort)
import Ecluse.Core.Version (Version)

{- | Canonicalise a raw package-name string under one ecosystem's own grammar, 'Nothing' for
a string that grammar refuses. It is the one parser every caller reaches a 'PackageName' through.
-}
type ProjectName = Text -> Maybe PackageName

{- | The ecosystem's metadata capability: reading a package's metadata from an origin,
assembling the served document, and encoding it ('Ecluse.Core.Server.Context.pdMetadata').
-}
data AdapterMetadata = AdapterMetadata
    { metadataNewReads ::
        forall posture.
        TracingPort ->
        MetricsPort ->
        (PackageName -> MetadataError -> IO ()) ->
        (PackageName -> [InvalidEntry] -> IO ()) ->
        (PackageName -> IO ()) ->
        OriginFor posture ->
        MetadataReads posture
    -- ^ Bind one origin's metadata reads to their observers, carrying its posture.
    , metadataAssemble :: Text -> Map SourceId (Snapshot CachedDoc) -> MergePlan -> Maybe CachedDoc -> CachedDoc
    -- ^ Select exact admitted entries from the supplied snapshots before rendering their wire shape.
    , metadataSerialise :: CachedDoc -> LByteString
    -- ^ Encode an assembled served document ('CachedDoc') to its wire bytes.
    , metadataFetchManifest :: ManifestFetch
    -- ^ The raw read under 'metadataNewReads', without its caching and metrics, for a store sweep.
    }

{- | Fetching and projecting one package's full manifest from an origin. Every failure is a
'MetadataError' value, as it is through the client built over it.
-}
type ManifestFetch = TracingPort -> OriginClient -> PackageName -> IO (Either MetadataError Manifest)

{- | The ecosystem's artifact request formation, by conventional filename or authoritative URL.
The serve deps and the worker bundle share it ('Ecluse.Core.Server.Context.pdArtifact').
-}
data AdapterArtifact = AdapterArtifact
    { artifactByFile :: OriginClient -> PackageName -> Text -> Either UrlFormationError Request
    -- ^ Address a trusted origin by the conventional filename path under its base URL.
    , artifactByUrl :: Maybe ClientCredential -> Text -> Either UrlFormationError Request
    {- ^ Build the request at the authoritative upstream URL, which names no origin because the
    mirror worker's fetch has none to give.
    -}
    , artifactHosts :: [Text]
    {- ^ The hosts the same-host tarball gate admits without the operator naming them. Empty for
    npm, whose artifacts ride the registry host.
    -}
    }

{- | The ecosystem's publish capability. The composition root marries its codec to the shared
publish transport per mounted ecosystem ('Ecluse.Core.Registry.Publish.newMirrorPublish').
-}
data AdapterPublish = AdapterPublish
    { publishRelay :: OriginClient -> PackageName -> ByteString -> IO (Either FetchFault PublishRelayResponse)
    -- ^ Relay a client's publish document to the target named as the origin, and return its answer.
    , publishDeclaredNames :: LByteString -> [Text]
    {- ^ Every name a publish body claims, @[]@ when none is readable. The anti-shadowing guard
    refuses one that disagrees with the URL-path name.
    -}
    , publishCodec :: PublishCodec
    {- ^ Document assembly, request formation, the probe, and the status semantics. The manager,
    credential mint, and fault classification are the transport's.
    -}
    }

{- | The ecosystem's store maintenance verbs, which "Ecluse.Core.Registry.Maintenance.Protocol"
drives. Each is 'Nothing' for a protocol that spells no such verb, and the Dredger then refuses.
-}
data AdapterMaintenance = AdapterMaintenance
    { maintenanceListing :: Maybe StoreListing
    -- ^ How the protocol enumerates a store's packages, where it can.
    , maintenanceVersionDelete :: Maybe VersionDelete
    -- ^ How the protocol deletes one version, where it can.
    , maintenanceAlphabet :: NameAlphabet
    {- ^ The characters a name may begin with, which partition the store's name space into the
    buckets a full walk covers one at a time.
    -}
    }

{- | Reading every package a store holds. The protocol's own listing endpoint, which a public
registry may well refuse: a listing that does not answer @200@ is the caller's fault to report.
-}
data StoreListing = StoreListing
    { listingRequest :: OriginClient -> Either UrlFormationError Request
    -- ^ Form the listing read against the store.
    , listingParse :: ByteString -> Either ParseError [PackageName]
    {- ^ Project a listing body onto the names it holds. An unparseable entry is dropped, because
    Écluse could serve it no better than it can sweep it.
    -}
    }

{- | Deleting one version, as the request sequence the protocol spells it with. It names its own
document read, because an install-optimised metadata read may omit the revision the edit needs.
-}
data VersionDelete = VersionDelete
    { deleteDocumentRequest :: OriginClient -> PackageName -> Either UrlFormationError Request
    -- ^ Form the read of the document the delete requests are built from.
    , deleteRequests ::
        OriginClient ->
        PackageName ->
        Version ->
        RegistryResponse ->
        Either StoreRefusal (NonEmpty Request)
    {- ^ Form the ordered requests that remove one version, or say why the document admits none.
    Every one must be sent, in this order, for the version to be gone.
    -}
    }
