{- | The serve-path metadata handle: the read boundary between the request pipeline
and a registry mount, expressed as two __implementation-agnostic intent operations__.

This is the read counterpart to the publish-side "Ecluse.Core.Registry" handle, and
deliberately distinct from it. The publish handle is the write\/worker side (one
client minted once at the composition root); this handle is the serve path's read
boundary, constructed __per request__ so it can capture the per-origin manager,
credential posture, base URL, and response budget — and the shared metadata cache —
that a serve fetch needs. The pipeline never reaches for a registry's wire format: it
asks a mount for one of two things and the mount owns both fetch and parse behind the
answer.

The two operations are asymmetric __by design__:

* 'fetchFullManifest' yields the packument-level 'Ecluse.Core.Package.PackageInfo'
  /and/ the raw document it was decoded from. The serve path needs the raw document
  because it edits the packument in place (dropping filtered versions, rewriting
  artifact locations) and re-serializes it to the client — and
  'Ecluse.Core.Package.PackageInfo' is a lossy projection that cannot reconstruct the
  document.

* 'fetchVersionMetadata' yields only one version's
  'Ecluse.Core.Package.PackageDetails'. It never re-serializes, so it need not carry
  the raw document — which is what lets a mount make it the cheap path (a smaller
  endpoint, or a selective parse) without changing this boundary.

Both operations are total: a failure is reported as a 'MetadataError' __value__, not
thrown, so the caller decides how each maps onto a served response. A genuine
transport fault (an unreachable upstream) is the exception channel the caller already
brackets; this 'Either' is the /parse and policy/ channel.
-}
module Ecluse.Core.Registry.Metadata (
    -- * The read handle
    MetadataClient (..),

    -- * Errors
    MetadataError (..),
) where

import Data.Aeson (Value)

import Ecluse.Core.Package (PackageDetails, PackageInfo, PackageName)
import Ecluse.Core.Security (LimitError)
import Ecluse.Core.Version (Version)

{- | The serve-path read handle — a record of two intent operations over a registry
mount, whose private state (the per-origin fetch configuration and the shared cache)
the closures capture. Both fields return 'IO' so a backend stays decoupled from the
proxy core, exactly as the publish-side handle does.
-}
data MetadataClient = MetadataClient
    { fetchFullManifest :: PackageName -> IO (Either MetadataError (PackageInfo, Value))
    {- ^ Fetch and project a package's __full manifest__: the packument-level
    'PackageInfo' (every version) paired with the raw 'Value' it was decoded from, so
    the serve path can edit and re-serialize the document. A failed fetch\/parse is a
    'MetadataError'; a transport fault is thrown (the caller already brackets it).
    -}
    , fetchVersionMetadata :: PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))
    {- ^ Fetch the __single-version metadata__ for one @(package, version)@: that
    version's 'PackageDetails', or 'Nothing' when the package resolved but does not
    carry the requested version (a genuine absence the caller renders as a miss,
    distinct from a 'MetadataError' that means the metadata itself was unobtainable).
    Never carries the raw document — it does not re-serialize.

    The 'Maybe' is the one deliberate departure from a bare
    @Either MetadataError PackageDetails@: a serve path must distinguish a version that
    is simply absent (a forwarded @404@) from metadata it could not obtain at all (a
    transient @503@), and an absent version is a normal outcome over sound metadata,
    not a parse error.
    -}
    }

{- | Why a metadata fetch could not yield a usable result — reported as a value so the
serve path maps each cause onto the response it renders, byte-for-byte as before.

Each constructor preserves a distinction the serve path acts on: a name mismatch is
held apart from a plain decode failure because a mount answering for a /different/
package is an untrusted, misreporting origin (the anti-shadowing defence), dropped and
surfaced distinctly rather than degraded like an outage.
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
      this request and dropped — never served as the requested package.
      -}
      MetadataNameMismatch Text
    deriving stock (Eq, Show)
