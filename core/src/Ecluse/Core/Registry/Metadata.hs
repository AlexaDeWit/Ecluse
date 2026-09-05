-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The serve-path metadata handle: the read boundary between the request pipeline and a
registry mount, constructed per request. The pipeline asks a mount for one of two things, and
the mount owns both fetch and parse behind the answer. The two are asymmetric by design.
'fetchFullManifest' also yields the raw document, because the serve path edits the packument in
place and re-serialises it. 'fetchVersionMetadata' never re-serialises, which is what lets a
mount make it the cheap path (a smaller endpoint, or a selective parse) without changing this
boundary. Both are total: a failure comes back as a 'MetadataError' __value__, the upstream
exchange's own faults included ('MetadataFetch').

'fetchThenProject' is the shape both operations take on every mount: fetch the document under
the fetch span, then run a pure projection over its bytes under the decode span. A mount
supplies the fetch action and the projection, and never the span or the fault fold.
-}
module Ecluse.Core.Registry.Metadata (
    -- * The read handle
    MetadataClient (..),

    -- * The full-manifest result
    Manifest (..),
    ContentDigest,
    digestOf,
    digestBytes,

    -- * The fetch-then-project step
    fetchThenProject,

    -- * Errors
    MetadataError (..),

    -- * Single-version resolution
    VersionEvaluation (..),
    fetchVersionDetails,
    versionTransience,
) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray qualified as BA

import Ecluse.Core.Package (PackageDetails, PackageInfo, PackageName)
import Ecluse.Core.Registry (FetchFault, RegistryResponse (responseBody))
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Rules.Types (Transience (WillResolve, WontResolve))
import Ecluse.Core.Security (LimitError)
import Ecluse.Core.Telemetry.Span (TracingPort (spanMetadataDecode, spanMetadataFetch))
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

{- | Fetch one package's metadata document under the fetch span, then run a pure projection
over its wire bytes under the decode span. An exchange fault folds to 'MetadataFetch', so a
mount's projection only ever sees a body.

Every mount's read operations differ in the fetch action and the projection alone, so each
passes those two and inherits the spans and the fault fold from here.
-}
fetchThenProject ::
    TracingPort ->
    (PackageName -> IO (Either FetchFault RegistryResponse)) ->
    PackageName ->
    (ByteString -> Either MetadataError a) ->
    IO (Either MetadataError a)
fetchThenProject tracing fetch name project =
    spanMetadataFetch tracing name (fetch name) >>= \case
        Left fault -> pure (Left (MetadataFetch fault))
        Right response -> spanMetadataDecode tracing name (pure (project (responseBody response)))

{- | Why a metadata fetch could not yield a usable result. Each cause is a value the serve
path maps onto its own response, so a name mismatch (the anti-shadowing defence) never
degrades like a transient outage.
-}
data MetadataError
    = {- | The upstream exchange never delivered a body, carried as the shared
      'Ecluse.Core.Registry.FetchFault' so a config fault and an outage stay distinct.
      -}
      MetadataFetch FetchFault
    | {- | The decoded document breached a structural bound (version count, nesting depth),
      distinct from the exchange's response-size bound, which arrives as 'MetadataFetch'.
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
    deriving stock (Eq, Show)

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

{- | The transience of a lookup that yielded no details, and 'Nothing' for a resolved one. The
serve gate and the mirror worker both read it, so neither classifies a lookup on its own.
-}
versionTransience :: VersionEvaluation -> Maybe Transience
versionTransience = \case
    VersionMetadataUnavailable -> Just (WillResolve Nothing)
    -- A withdrawn version is gone for good, so no consumer waits for it to come back.
    VersionMissing -> Just WontResolve
    VersionPresent{} -> Nothing
