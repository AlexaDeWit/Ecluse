-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The registry-protocol vocabulary: the payload and typed-fault types every
registry-facing capability shares.

This is the __ecosystem (protocol) axis__' common ground (see
@docs\/architecture\/registry-model.md@ → "Registry Abstraction"): the raw fetched
document ('RegistryResponse'), the mirror-write descriptor ('MirrorArtifact'),
and one typed fault channel per exchange -- 'FetchFault' for a metadata read,
'PublishFault' for the mirror write, 'PublishRelayFault' for the first-party
relay, with 'UrlFormationError' the protocol-independent request-formation
failure they all share. The capabilities that speak a protocol over these types
live beside them: the metadata read handle in "Ecluse.Core.Registry.Metadata",
the mirror write's codec-over-transport split in "Ecluse.Core.Registry.Publish",
and each ecosystem's capability record in "Ecluse.Core.Registry.Adapter".

Two design points are load-bearing:

* __Failures are values.__ Each exchange reports every failure -- transport
  included -- in its typed channel, never a throw, so a consumer's fall-through
  or retry-vs-drop decision is total at the call site
  (/parse, don't validate/; see @docs\/architecture\/fault-model.md@).

* __The vocabulary carries no authentication.__ Protocol and auth are orthogonal
  axes: every managed npm registry (AWS CodeArtifact, GCP Artifact Registry, a
  self-hosted Verdaccio) speaks the same npm protocol and differs only in how a
  bearer token is minted, which lives behind the separate
  "Ecluse.Core.Credential" handle. So one protocol implementation is reused
  across every cloud rather than near-duplicated per provider.
-}
module Ecluse.Core.Registry (
    -- * Fetch payload
    RegistryResponse (..),

    -- * Publish descriptor
    MirrorArtifact (..),

    -- * Errors
    ParseError (..),
    FetchFault (..),
    PublishError (..),
    PublishFault (..),
    UrlFormationError (..),
    PublishRelayResponse (..),
    PublishRelayFault (..),
) where

import Ecluse.Core.Fault (TransportFault)
import Ecluse.Core.Package (Hash)
import Ecluse.Core.Security (LimitError)

{- | A raw response fetched from a registry -- the unparsed bytes of a metadata
document, as returned by 'fetchMetadata'. It is kept opaque-of-bytes here so the
protocol\/data plane (fetch) is separate from parsing: a @parse*@ field turns a
'RegistryResponse' into a domain type.
-}
newtype RegistryResponse = RegistryResponse
    { responseBody :: ByteString
    -- ^ The raw response body (a metadata JSON document, or artifact bytes).
    }
    deriving stock (Eq, Show)

{- | The artifact descriptor a mirror publish is assembled from: the filename,
integrity digests, and declared size of the artifact the worker's ingest
re-evaluation __re-admitted__ under current policy. The worker derives it entirely
from current metadata -- the queue payload carries no digest or size -- so the
published document can only ever name what the shared admission gate
floor-checked (see "Ecluse.Core.Worker.Job").

'maHashes' is a 'NonEmpty' because admission refuses a digest-less version (the
integrity-presence policy), so a descriptor with nothing to verify or publish is
unrepresentable by construction.
-}
data MirrorArtifact = MirrorArtifact
    { maFilename :: Text
    {- ^ The artifact's on-the-wire filename, the @_attachments@ key in the publish
    document.
    -}
    , maHashes :: NonEmpty Hash
    {- ^ The integrity digests (at least one): the floor-checked set the tamper
    gate verified the fetched bytes against, and the set the publish document's
    npm @dist.integrity@ \/ @shasum@ fields are picked from.
    -}
    , maSize :: Maybe Int
    {- ^ The registry-declared size, if reported. Not guaranteed to be the tarball byte
    count: for npm it is the unpacked-tree size (@dist.unpackedSize@).
    -}
    }
    deriving stock (Eq, Show)

{- | Why parsing a 'RegistryResponse' into a domain type failed. Parsing is the
boundary that turns untrusted wire data into the proxy's precise types, so a
failure is reported (not thrown): the caller decides how to respond.
-}
newtype ParseError = ParseError
    { parseErrorMessage :: Text
    -- ^ A human-readable description of what could not be parsed.
    }
    deriving stock (Eq, Show)

{- | Why publishing an artifact to a registry failed -- a genuine write fault
reported by the mirror write ('Ecluse.Core.Registry.Publish.mpPublishArtifact';
an 'Ecluse.Core.Queue' job is then left un-acked and retried; see
@docs\/architecture\/cloud-backends.md@).

This is the __write-path__ fault and nothing more: forming the request URL is a
separate concern (a 'UrlFormationError'), so a read-path fetch can no longer
surface a failure mislabelled as a publish.
-}
newtype PublishError = PublishError
    { publishErrorMessage :: Text
    -- ^ A human-readable description of why the publish failed.
    }
    deriving stock (Eq, Show)

{- | Why an upstream request URL could not be formed from configuration and an
already-parsed 'Ecluse.Core.Package.PackageName'.

This is a __protocol-independent__ fault shared by every request an adapter
builds -- metadata fetch, artifact fetch, and publish alike -- so a read-path
failure is reported as what it is rather than borrowing the write-path's
'PublishError'. It reports that the configured base URL is empty or the URL the
adapter formed could not be parsed.
-}
data UrlFormationError
    = -- | The configured base URL is empty, so no request URL can be formed.
      EmptyBaseUrl
    | {- | The formed URL string could not be parsed into a request. Carries the
      offending URL.
      -}
      UnparseableUrl Text
    deriving stock (Eq, Show)

{- | Why a metadata fetch could not produce a response body, reported as a __value__
so a read-path consumer maps each cause onto its own outcome -- the serve read
adapter onto the response it renders, the worker's mirror-presence probe onto its
fall-through -- rather than catching a typed throw two calls away. Total over the
read fetch: an unformable request URL, a response-bound breach, and a transport
fault are all in this channel, so no fetch failure rides up outside the declared
type.
-}
data FetchFault
    = -- | The request URL could not be formed from configuration (an empty or unparseable base URL).
      FetchUrlUnformable UrlFormationError
    | -- | The upstream body crossed the response-size bound and was refused fail-closed.
      FetchBoundExceeded LimitError
    | {- | The request never completed: the transport failed before a usable body
      returned (a timeout, an unreachable peer, a TLS refusal), carried as the
      'TransportFault' the adapter edge classified out of its client library's
      exception.
      -}
      FetchTransport TransportFault
    deriving stock (Eq, Show)

{- | Why a first-party publish relay produced no response from the publication
target, reported as a __value__ so the serve path renders each cause directly:
an unformable target URL is the operator-misconfiguration @500@, and a transport
fault or an overstepped response bound is the target-unreachable @502@ (in both,
the target's own answer never arrived whole). Total over the relay: no relay
failure rides up outside the declared type.
-}
data PublishRelayFault
    = -- | The publication target URL could not be formed from configuration.
      RelayUrlUnformable UrlFormationError
    | {- | The write never produced a usable response: the transport failed,
      carried as the 'TransportFault' the adapter edge classified.
      -}
      RelayTransport TransportFault
    | -- | The target's response body overstepped the response-size bound.
      RelayBoundExceeded LimitError
    deriving stock (Eq, Show)

{- | The response from the publication target after relaying a publish document.
Kept in memory (no streaming) -- the relayed body is small (typically a JSON
envelope and a tarball under the target's size limit), and buffering it whole lets the
proxy catch and log an exception before starting a chunked response it would otherwise
abandon mid-stream.
-}
data PublishRelayResponse = PublishRelayResponse
    { relayStatus :: Int
    -- ^ The HTTP status code the publication target returned.
    , relayBody :: LByteString
    -- ^ The publication target's response body, relayed to the client unchanged.
    }
    deriving stock (Eq, Show)

{- | Why a publish could not complete, surfaced as a __value__ rather than thrown
so the mirror worker decides retry vs. drop by an exhaustive pattern match rather
than by catching (and re-classifying) an exception. The cases differ in exactly
that -- retryability -- which is the whole reason this is a value: a registry
rejection or a transport fault is worth redelivering, an unformable URL never is.
-}
data PublishFault
    = {- | The request URL could not be formed (e.g. an empty base URL) -- a
      configuration fault carried as its 'UrlFormationError'. __Not retryable__:
      redelivering the job cannot change a misconfigured base URL, so the worker
      drops (and alerts) rather than re-enqueueing forever.
      -}
      PublishUrlUnformable UrlFormationError
    | {- | The registry rejected the write (a non-2xx, non-@409@ status), carried
      as a 'PublishError'. __Retryable__: the job is left un-acked and redelivered.
      -}
      PublishRejected PublishError
    | {- | The write never reached the registry: the HTTP request threw before any
      status returned (a connection failure, a TLS error, a timeout), carried as its
      rendered detail. __Retryable__: the transport may recover, so the job is left
      un-acked and redelivered, exactly as a 'PublishRejected'. Surfacing it as a value
      is what lets the mirror write ('Ecluse.Core.Registry.Publish.mpPublishArtifact')
      honour its total, never-thrown contract.
      -}
      PublishTransport Text
    deriving stock (Eq, Show)
