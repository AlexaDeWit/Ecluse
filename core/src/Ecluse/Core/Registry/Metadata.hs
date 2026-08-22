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

{- | A SHA-256 digest of one origin's __wire body__: the exact bytes the mount decoded
a manifest from. The read boundary hashes them once, where the strict body already
exists, so downstream consumers can fingerprint a document without re-encoding or
re-hashing it. Opaque: built only by 'digestOf', read only by 'digestBytes'.
-}
newtype ContentDigest = ContentDigest ByteString
    deriving stock (Eq, Show)

-- | Digest a strict body: one @O(body)@ pass, paid at fetch time, never per serve.
digestOf :: ByteString -> ContentDigest
digestOf body = ContentDigest (BA.convert (hash body :: Digest SHA256))

-- | The digest's raw 32 bytes, for feeding into a wider fingerprint.
digestBytes :: ContentDigest -> ByteString
digestBytes (ContentDigest bytes) = bytes

{- | A resolved full manifest. It carries the typed packument-level view, the raw
document the mount decoded it from, and the 'ContentDigest' of the wire bytes both came
from. The serve path edits the raw document and re-serialises it. That digest is the
input fingerprint the serve path builds its derived ETag over
('Ecluse.Core.Server.Conditional').
-}
data Manifest = Manifest
    { manifestInfo :: PackageInfo
    -- ^ The typed packument view the rules and merge reason over.
    , manifestRaw :: CachedDoc
    -- ^ The raw upstream document ('CachedDoc') the served body is built from.
    , manifestDigest :: ContentDigest
    -- ^ Digest of the wire bytes behind 'manifestInfo' and 'manifestRaw'.
    }

{- | The serve-path read handle: a record of two intent operations over a registry
mount. The closures capture its private state, the per-origin fetch configuration and
the shared cache. Both fields return 'IO' so a backend stays decoupled from the proxy
core, exactly as the publish-side handle does.
-}
data MetadataClient = MetadataClient
    { fetchFullManifest :: PackageName -> IO (Either MetadataError Manifest)
    {- ^ Fetch and project a package's __full manifest__. It carries the
    packument-level 'PackageInfo' (every version), the raw document ('CachedDoc') the
    mount decoded it from, and the wire bytes' 'ContentDigest'. The raw document lets
    the serve path rebuild and re-serialise the document through injected adapter
    capabilities. The digest lets it fingerprint the document without re-hashing it.
    Every failure is a 'MetadataError' value: fetch, transport, parse, or policy.
    -}
    , fetchVersionMetadata :: PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))
    {- ^ Fetch the __single-version metadata__ for one @(package, version)@: that
    version's 'PackageDetails', or 'Nothing' when the package resolved but does not
    carry the requested version. That absence is genuine, and the caller renders it as
    a miss. A 'MetadataError' means the metadata itself was unobtainable. Never carries
    the raw document, because it does not re-serialise.

    The 'Maybe' is the one deliberate departure from a bare
    @Either MetadataError PackageDetails@. A serve path must distinguish a version that
    is absent (a forwarded @404@) from metadata it could not obtain at all (a transient
    @503@). An absent version is a normal outcome over sound metadata, not a parse
    error.
    -}
    }

{- | Why a metadata fetch could not yield a usable result. It is a value, so the serve
path maps each cause onto the response it renders.

Each constructor preserves a distinction the serve path acts on. A name mismatch stays
apart from a plain decode failure. A mount answering for a /different/ package is an
untrusted, misreporting origin: the anti-shadowing defence. The proxy
drops that origin and surfaces the mismatch distinctly, rather than degrading it like
an outage.
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
vocabulary ('MetadataError'), preserving the distinction each cause carries. A
response-bound breach is 'MetadataBoundExceeded'. An unformable upstream URL is
'MetadataUrlUnformable', a config fault held distinct from a decode or an outage. A
transport fault is 'MetadataUnreachable', the outage, kept transient.
Ecosystem-agnostic: every adapter's metadata layer threads its bounded fetch's fault
through this same fold.
-}
fetchFaultError :: FetchFault -> MetadataError
fetchFaultError = \case
    FetchBoundExceeded err -> MetadataBoundExceeded err
    FetchUrlUnformable urlErr -> MetadataUrlUnformable urlErr
    FetchTransport transport -> MetadataUnreachable transport

{- | The outcome of resolving one version's metadata for a policy decision. It is the
projected version snapshot when present, or the degrade both the serve-time gate and the
mirror worker map onto their own response. It is the shared classification of a
single-version fetch. The gate that decides what to serve and the worker that decides
what to mirror reach the same three outcomes from one fetch and projection.
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

{- | Resolve a single version's metadata through a 'MetadataClient' and classify the
outcome into a 'VersionEvaluation'. This is the one fetch-and-project step both the
serve-time tarball gate and the mirror worker run before the rules engine. A future
rule that reads a new field therefore sees the same projected 'PackageDetails' in
either context.

Any 'MetadataError' classifies as 'VersionMetadataUnavailable', collapsing every
unobtainable-metadata cause, an unreachable upstream included, to the one transient
outcome. A resolved-but-absent version is 'VersionMissing'. A resolved version is
'VersionPresent'. Total by type: the fetch reports every failure in its 'Either', so
this classification is a pure fold with nothing to catch.
-}
fetchVersionDetails :: MetadataClient -> PackageName -> Version -> IO VersionEvaluation
fetchVersionDetails client name version =
    fetchVersionMetadata client name version <&> \case
        Left _ -> VersionMetadataUnavailable
        Right Nothing -> VersionMissing
        Right (Just details) -> VersionPresent details
