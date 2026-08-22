-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The serve-path metadata handle: the read boundary between the request pipeline
and a registry mount, expressed as two __implementation-agnostic intent operations__.

This is the read counterpart to the publish-side "Ecluse.Core.Registry" handle, and
deliberately distinct from it. The publish handle is the write and worker side: one
client minted once at the composition root. This handle is the serve path's read
boundary, constructed __per request__. It captures what a serve fetch needs: the
per-origin manager, credential posture, base URL, response budget, and the shared
metadata cache. The pipeline never reaches for a registry's wire format. It asks a
mount for one of two things, and the mount owns both fetch and parse behind the
answer.

The two operations are asymmetric __by design__:

* 'fetchFullManifest' yields the packument-level 'Ecluse.Core.Package.PackageInfo'
  /and/ the raw document the mount decoded it from. The serve path needs the raw
  document. It edits the packument in place, dropping filtered versions and rewriting
  artifact locations, then re-serialises it to the client.
  'Ecluse.Core.Package.PackageInfo' is a lossy projection that cannot reconstruct the
  document.

* 'fetchVersionMetadata' yields only one version's
  'Ecluse.Core.Package.PackageDetails'. It never re-serialises, so it need not carry
  the raw document. That is what lets a mount make it the cheap path (a smaller
  endpoint, or a selective parse) without changing this boundary.

Both operations are total. A failure comes back as a 'MetadataError' __value__, never
a throw, so the caller decides how each maps onto a served response. A transport fault
is in the same channel ('MetadataUnreachable'). Unobtainable metadata therefore arrives
typed whatever the cause: a parse failure, a policy refusal, or an unreachable
upstream.
-}
module Ecluse.Core.Registry.Metadata (
    -- * The read handle
    MetadataClient (..),

    -- * The full-manifest result
    Manifest (..),
    ContentDigest,
    digestOf,
    digestBytes,

    -- * Errors
    MetadataError (..),
    fetchFaultError,

    -- * Single-version resolution
    VersionEvaluation (..),
    fetchVersionDetails,
) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray qualified as BA

import Ecluse.Core.Fault (TransportFault)
import Ecluse.Core.Package (PackageDetails, PackageInfo, PackageName)
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable),
    UrlFormationError,
 )
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Security (LimitError)
import Ecluse.Core.Version (Version)

{- | A SHA-256 digest of one origin's wire body: the exact bytes the mount decoded a
manifest from. Opaque: built only by 'digestOf', read only by 'digestBytes'.
-}
newtype ContentDigest = ContentDigest ByteString
    deriving stock (Eq, Show)

-- | Digest a strict body: one @O(body)@ pass, paid at fetch time, never per serve.
digestOf :: ByteString -> ContentDigest
digestOf body = ContentDigest (BA.convert (hash body :: Digest SHA256))

-- | The digest's raw 32 bytes, for feeding into a wider fingerprint.
digestBytes :: ContentDigest -> ByteString
digestBytes (ContentDigest bytes) = bytes

{- | A resolved full manifest. The serve path edits the raw document, re-serialises it, and
builds its derived ETag over 'manifestDigest' ('Ecluse.Core.Server.Conditional').
-}
data Manifest = Manifest
    { manifestInfo :: PackageInfo
    -- ^ The typed packument view the rules and merge reason over.
    , manifestRaw :: CachedDoc
    -- ^ The raw upstream document ('CachedDoc') the served body is built from.
    , manifestDigest :: ContentDigest
    -- ^ Digest of the wire bytes behind 'manifestInfo' and 'manifestRaw'.
    }

{- | The serve-path read handle over one registry mount. Its closures capture the
per-origin fetch configuration and the shared cache, keeping a backend out of the core.
-}
data MetadataClient = MetadataClient
    { fetchFullManifest :: PackageName -> IO (Either MetadataError Manifest)
    {- ^ Fetch and project a package's full manifest, every version included. Every failure
    comes back as a 'MetadataError' value: fetch, transport, parse, or policy.
    -}
    , fetchVersionMetadata :: PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))
    {- ^ Fetch one @(package, version)@'s metadata. 'Nothing' means the package resolved
    without that version (a forwarded @404@), unlike a 'MetadataError' (a transient @503@).
    -}
    }

{- | Why a metadata fetch could not yield a usable result. Each cause is a value the serve
path maps onto its own response, so a name mismatch (the anti-shadowing defence) never
degrades like a transient outage.
-}
data MetadataError
    = {- | The upstream body breached a response bound (its size, version count, or
      nesting depth). Carries the 'LimitError' so the breach is diagnosable.
      -}
      MetadataBoundExceeded LimitError
    | {- | The upstream answered, but its body did not decode into a usable manifest
      (malformed JSON, or an absent\/undecodable top-level name).
      -}
      MetadataUndecodable
    | {- | The upstream answered with a manifest that self-reported a /different/
      package's name (carried verbatim for the audit log). The origin is untrusted for
      this request, so the proxy drops it and never serves it as the requested package.
      -}
      MetadataNameMismatch Text
    | {- | The upstream request URL could not be formed from configuration (an empty or
      unparseable base URL). A __config fault__, held distinct from a decode failure or
      a transient outage, mirroring the write path's
      'Ecluse.Core.Registry.PublishUrlUnformable'. A misconfigured base URL therefore
      stays what it is, never laundered into a retryable degrade. Carries the
      'Ecluse.Core.Registry.UrlFormationError'.
      -}
      MetadataUrlUnformable UrlFormationError
    | {- | The upstream could not be reached at all: the transport failed before a
      usable body returned (a timeout, a refused connection, a TLS refusal). Carried
      as the adapter-classified 'TransportFault'. The __outage__ cause, held distinct
      from a decode failure or a config fault, so the serve path degrades it as the
      transient it is.
      -}
      MetadataUnreachable TransportFault
    deriving stock (Eq, Show)

{- | Fold the shared upstream fault channel ('FetchFault') onto the serve-path error
vocabulary. Every adapter's metadata layer threads its bounded fetch's fault through it.
-}
fetchFaultError :: FetchFault -> MetadataError
fetchFaultError = \case
    FetchBoundExceeded err -> MetadataBoundExceeded err
    FetchUrlUnformable urlErr -> MetadataUrlUnformable urlErr
    FetchTransport transport -> MetadataUnreachable transport

{- | The outcome of resolving one version's metadata for a policy decision. The serve-time
gate and the mirror worker share it, so both reach the same outcome from one fetch.
-}
data VersionEvaluation
    = -- | The version resolved and projected. Its 'PackageDetails' is ready for the rules engine.
      VersionPresent PackageDetails
    | {- | The package resolved but does not carry the requested version (a withdrawn or
      never-published version), a genuine absence distinct from unobtainable metadata.
      -}
      VersionMissing
    | {- | The metadata could not be obtained at all: any 'MetadataError' (an
      unreachable upstream, a decode failure, a bound breach, or a self-reported name
      mismatch). Transient: the one retryable outcome every unobtainable-metadata
      cause collapses to.
      -}
      VersionMetadataUnavailable
    deriving stock (Eq, Show)

{- | Resolve a single version's metadata through a 'MetadataClient' and classify it. Both
the serve-time tarball gate and the mirror worker run this step before the rules engine.
-}
fetchVersionDetails :: MetadataClient -> PackageName -> Version -> IO VersionEvaluation
fetchVersionDetails client name version =
    fetchVersionMetadata client name version <&> \case
        Left _ -> VersionMetadataUnavailable
        Right Nothing -> VersionMissing
        Right (Just details) -> VersionPresent details
