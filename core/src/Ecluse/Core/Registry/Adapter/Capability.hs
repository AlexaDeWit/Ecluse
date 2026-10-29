-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Registry capabilities used by the ecosystem-neutral pipeline.
Adapters own wire formats and share exact source-entry selection for served metadata.
-}
module Ecluse.Core.Registry.Adapter.Capability (
    -- * Metadata
    AdapterMetadata (..),

    -- * Artifact requests
    AdapterArtifact (..),
    PrivateArtifactAddressing (..),

    -- * Publish
    AdapterPublish (..),
    PublishRequestBody (..),
    PublishExtras (..),
    MirrorRequirements (..),
    WorkerMetadataRead (..),
    MirrorStateRequirement (..),
    UploadEndpointRequirement (..),
    UploadIdentityRequirement (..),

    -- * Names
    ProjectName,

    -- * Store maintenance
    AdapterMaintenance (..),
    StoreListing (..),
    VersionDelete (..),
) where

import Data.JsonStream.Lexer (Cursor)
import Network.HTTP.Client (Request)
import Network.HTTP.Types.Header (RequestHeaders)

import Ecluse.Core.Credential (ClientCredential, Secret)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Package.Merge (MergePlan, SourceId)
import Ecluse.Core.Registry (
    FetchFault,
    PublishBodyFault,
    PublishRelayResponse,
    RegistryResponse,
    UrlFormationError,
 )
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Json.Walk (Step)
import Ecluse.Core.Registry.Maintenance (StoreRefusal)
import Ecluse.Core.Registry.Maintenance.NameSpace (NameAlphabet)
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead)
import Ecluse.Core.Registry.Origin (OriginClient)
import Ecluse.Core.Registry.Publish (PublishCodec)
import Ecluse.Core.Registry.Request (CredentialMapping)
import Ecluse.Core.Registry.ServedDocument (RenderRefused)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Server.Admission.Types (ChargeFactors)
import Ecluse.Core.Snapshot (Snapshot)
import Ecluse.Core.Version (Version)

-- | Canonicalise a raw name under one ecosystem's grammar: the one parser every caller reaches a 'PackageName' through.
type ProjectName = Text -> Maybe PackageName

-- | The ecosystem's metadata read, served-document assembly, and encoding ('Ecluse.Core.Server.Context.pdMetadata').
data AdapterMetadata = AdapterMetadata
    { metadataRead :: PrepareRead
    -- ^ Prepare under the selected origin before reading its metadata.
    , metadataAssemble :: Text -> Map SourceId (Snapshot CachedDoc) -> MergePlan -> Maybe CachedDoc -> CachedDoc
    -- ^ Select exact admitted entries from the supplied snapshots before rendering their wire shape.
    , metadataSerialise :: CachedDoc -> Either RenderRefused LByteString
    -- ^ Encode an assembled served document ('CachedDoc') to its wire bytes, or refuse a damaged render.
    , metadataChargeFactors :: ChargeFactors
    -- ^ What this ecosystem's full reads and listings charge the memory budget per source byte.
    }

-- | Private artifact locations come from a conventional path or authoritative index metadata.
data PrivateArtifactAddressing
    = ConventionalArtifactPath
    | IndexedArtifactLocation
    | -- | Requires metadata admission before discovery or index workspace allocation.
      AdmittedIndexedArtifactLocation
    deriving stock (Eq, Show)

-- | Artifact request formation by filename or authoritative URL, shared by serve and worker ('Ecluse.Core.Server.Context.pdArtifact').
data AdapterArtifact = AdapterArtifact
    { artifactByFile :: OriginClient -> PackageName -> Text -> Either UrlFormationError Request
    -- ^ Address a trusted origin by the conventional filename path under its base URL.
    , artifactByUrl :: Maybe ClientCredential -> Text -> Either UrlFormationError Request
    -- ^ Build the request at the authoritative upstream URL. It names no origin, because the mirror worker has none.
    , artifactHosts :: [Text]
    -- ^ Hosts the same-host tarball gate admits unnamed. Empty for npm, whose artifacts ride the registry host.
    , artifactPrivateAddressing :: PrivateArtifactAddressing
    -- ^ How private requests find their artifact location, independent of the admitted public hosts.
    }

-- | Original request bytes and their declared media type, retained under byte admission.
data PublishRequestBody = PublishRequestBody
    { publishBodyBytes :: ByteString
    , publishBodyContentType :: Maybe ByteString
    , publishBodyExtras :: PublishExtras
    }

-- | Checked native publication headers. Secrets never enter durable mirror state.
data PublishExtras = NoPublishExtras | RubyPublishOtp Secret
    deriving stock (Eq, Show)

-- | Worker reads use cached facts, charged chunks, or native workspace reservations.
data WorkerMetadataRead
    = CachedWorkerRead
    | ChargedWorkerRead PrepareRead
    | NativeChargedWorkerRead PrepareRead

-- | Completion uses native inventory or requires durable version state.
data MirrorStateRequirement
    = NativeVersionState
    | DurableVersionState
    deriving stock (Eq, Show)

-- | Mirror uploads use the read root or require a separate endpoint.
data UploadEndpointRequirement
    = ReadRootUpload
    | ExplicitUploadEndpoint
    deriving stock (Eq, Show)

-- | Named protocols require a username when using a static token.
data UploadIdentityRequirement
    = TokenIdentity
    | NamedIdentity
    deriving stock (Eq, Show)

-- | Static mirror obligations, separate from runtime completion observations.
data MirrorRequirements = MirrorRequirements
    { mirrorWorkerRead :: WorkerMetadataRead
    -- ^ Cache and charging policy for worker metadata.
    , mirrorStateRequirement :: MirrorStateRequirement
    -- ^ State the configured mirror requires at boot.
    , mirrorUploadEndpoint :: UploadEndpointRequirement
    -- ^ Whether the writer requires a separate endpoint.
    }

-- | Native relay, identity validation, and mirror publication for one ecosystem.
data AdapterPublish = AdapterPublish
    { publishCredential :: CredentialMapping
    -- ^ Recover publication credentials independently from read credentials.
    , publishRequestExtras :: RequestHeaders -> Either PublishBodyFault PublishExtras
    -- ^ Admit only native publication headers before buffering the body.
    , publishRelay :: OriginClient -> PackageName -> PublishRequestBody -> IO (Either FetchFault PublishRelayResponse)
    -- ^ Relay a client's publish document to the target named as the origin, and return its answer.
    , publishUploadClaims :: Limits -> (Int -> IO ()) -> Text -> PublishRequestBody -> IO (Either PublishBodyFault [Text])
    -- ^ Check claims before target IO using the selected logical view base. Charge extra workspace as an absolute total.
    , publishCodec :: PublishCodec
    -- ^ The mirror write's document, requests, probe, and status semantics. The transport owns the manager, mint, and fault classification.
    , publishMirrorRequirements :: MirrorRequirements
    -- ^ The worker read and boot obligations for configured mirrors.
    , publishIdentity :: UploadIdentityRequirement
    -- ^ Static credential requirements for mirror and client uploads.
    }

-- | Store maintenance verbs for "Ecluse.Core.Registry.Maintenance.Protocol". A missing verb makes the Dredger refuse.
data AdapterMaintenance = AdapterMaintenance
    { maintenanceListing :: Maybe StoreListing
    -- ^ How the protocol enumerates a store's packages, where it can.
    , maintenanceVersionDelete :: Maybe VersionDelete
    -- ^ How the protocol deletes one version, where it can.
    , maintenanceAlphabet :: NameAlphabet
    -- ^ The characters a name may begin with: the buckets a full walk covers one at a time.
    }

-- | The protocol's own listing of a store. A public registry may refuse it, and the caller reports a non-@200@.
data StoreListing = StoreListing
    { listingRequest :: OriginClient -> Either UrlFormationError Request
    -- ^ Form the listing read against the store.
    , listingWalk :: Cursor -> Step [PackageName]
    -- ^ Stream names while skipping unrelated per-package metadata.
    }

-- | One version's delete requests and their own document read, since an install-optimised read may omit the revision.
data VersionDelete = VersionDelete
    { deleteDocumentRequest :: OriginClient -> PackageName -> Either UrlFormationError Request
    -- ^ Form the read of the document the delete requests are built from.
    , deleteRequests ::
        OriginClient ->
        PackageName ->
        Version ->
        RegistryResponse ->
        Either StoreRefusal (NonEmpty Request)
    -- ^ The ordered requests that remove one version, all of them needed, or why the document admits none.
    }
