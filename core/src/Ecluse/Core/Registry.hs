-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The registry-protocol vocabulary: the payload and typed-fault types every
registry-facing capability shares.

This is the __ecosystem (protocol) axis__' common ground (see
@docs\/architecture\/registry-model.md@ → "Registry Abstraction"). It holds the raw
fetched document ('RegistryResponse'), the mirror-write descriptor
('MirrorArtifact'), and the digest selector ('firstHashValue') that reads it. It
also holds one typed fault channel per exchange: 'FetchFault' for a metadata read,
'PublishFault' for the mirror write, and 'PublishRelayFault' for the first-party
relay. 'UrlFormationError' is the protocol-independent request-formation failure
they all share. The capabilities that speak a protocol over these types live
beside them. The metadata read handle is in "Ecluse.Core.Registry.Metadata", the
mirror write's codec-over-transport split in "Ecluse.Core.Registry.Publish", and
each ecosystem's capability record in "Ecluse.Core.Registry.Adapter".

Two design points are load-bearing:

* __Failures are values.__ Each exchange reports every failure, transport
  included, in its typed channel, never a throw. A consumer's fall-through or
  retry-vs-drop decision is therefore total at the call site
  (/parse, don't validate/, see @docs\/architecture\/fault-model.md@).

* __The vocabulary carries no authentication.__ Protocol and authentication are
  orthogonal axes. Every managed npm registry (AWS CodeArtifact, GCP Artifact
  Registry, a self-hosted Verdaccio) speaks the same npm protocol. They differ
  only in how a credential provider mints a bearer token, which lives behind the
  separate "Ecluse.Core.Credential" handle. One protocol implementation therefore
  serves every cloud instead of a near-duplicate per provider.
-}
module Ecluse.Core.Registry (
    -- * Fetch payload
    RegistryResponse (..),

    -- * Publish descriptor
    MirrorArtifact (..),
    firstHashValue,

    -- * Errors
    ParseError (..),
    FetchFault (..),
    PublishError (..),
    PublishFault (..),
    UrlFormationError (..),
    renderUrlFormationError,
    PublishRelayResponse (..),
    PublishRelayFault (..),
) where

import Ecluse.Core.Fault (TransportFault)
import Ecluse.Core.Package (Hash, HashAlg, hashAlg, hashValue)
import Ecluse.Core.Security (LimitError, authorityLabel)

{- | A raw response fetched from a registry, the unparsed bytes 'fetchMetadata' returned.
The bytes stay opaque here to keep fetching separate from parsing.
-}
newtype RegistryResponse = RegistryResponse
    { responseBody :: ByteString
    -- ^ The raw response body (a metadata JSON document, or artifact bytes).
    }
    deriving stock (Eq, Show)

{- | The artifact descriptor the mirror publish uses. The worker derives it from current metadata,
not the queue payload, so a publish names only what the admission gate floor-checked (see
"Ecluse.Core.Worker.Job"). 'maHashes' is 'NonEmpty' because admission refuses a digest-less version.
-}
data MirrorArtifact = MirrorArtifact
    { maFilename :: Text
    {- ^ The artifact's on-the-wire filename, the @_attachments@ key in the publish
    document.
    -}
    , maHashes :: NonEmpty Hash
    {- ^ The integrity digests, at least one. The tamper gate verified the fetched bytes against
    this floor-checked set.
    -}
    , maSize :: Maybe Int
    {- ^ The registry-declared size, if reported. Not guaranteed to be the tarball byte
    count: for npm it is the unpacked-tree size (@dist.unpackedSize@).
    -}
    }
    deriving stock (Eq, Show)

{- | The digest value of the first 'Hash' with the given 'HashAlg', or 'Nothing' when the
artifact carries none.
-}
firstHashValue :: HashAlg -> MirrorArtifact -> Maybe Text
firstHashValue alg artifact =
    fmap hashValue (find ((== alg) . hashAlg) (maHashes artifact))

{- | Why parsing a 'RegistryResponse' into a domain type failed. The parser reports this
value rather than throwing, so the caller decides how to respond to untrusted wire data.
-}
newtype ParseError = ParseError
    { parseErrorMessage :: Text
    -- ^ A human-readable description of what could not be parsed.
    }
    deriving stock (Eq, Show)

{- | Why publishing an artifact to a registry failed, the fault
'Ecluse.Core.Registry.Publish.mpPublishArtifact' reports. Forming the request URL is a separate
concern ('UrlFormationError'), so a read-path failure is never mislabelled as a publish.
-}
newtype PublishError = PublishError
    { publishErrorMessage :: Text
    -- ^ A human-readable description of why the publish failed.
    }
    deriving stock (Eq, Show)

{- | Why an upstream request URL could not be formed from configuration and a parsed
'Ecluse.Core.Package.PackageName'. Every request an adapter builds shares this fault, whether
it fetches metadata, fetches an artifact, or publishes.
-}
data UrlFormationError
    = -- | The configured base URL is empty, so no request URL can be formed.
      EmptyBaseUrl
    | {- | The formed URL string could not be parsed into a request. Carries the
      offending URL.
      -}
      UnparseableUrl Text
    deriving stock (Eq, Show)

{- | Render a 'UrlFormationError' for an operator log line, with any URL it carries reduced to
its authority ('authorityLabel').

A carried URL can hold a credential in its userinfo or a signed query string, and the 'Show'
instance prints it whole.

>>> renderUrlFormationError (UnparseableUrl "https://deploy:hunter2@upstream.test/base?token=abc")
"UnparseableUrl upstream.test:443"

>>> renderUrlFormationError EmptyBaseUrl
"EmptyBaseUrl"
-}
renderUrlFormationError :: UrlFormationError -> Text
renderUrlFormationError = \case
    EmptyBaseUrl -> "EmptyBaseUrl"
    UnparseableUrl url -> "UnparseableUrl " <> authorityLabel url

{- | Why a metadata fetch could not produce a response body, reported as a value rather than
thrown. Total over the read fetch: no fetch failure rides up outside this type.
-}
data FetchFault
    = -- | The request URL could not be formed from configuration (an empty or unparseable base URL).
      FetchUrlUnformable UrlFormationError
    | -- | The upstream body crossed the response-size bound, and the read refused it fail-closed.
      FetchBoundExceeded LimitError
    | {- | The request never completed: the transport failed before a usable body
      returned (a timeout, an unreachable peer, a TLS refusal). Carried as the
      'TransportFault' the adapter edge classified out of its client library's
      exception.
      -}
      FetchTransport TransportFault
    deriving stock (Eq, Show)

{- | Why a first-party publish relay produced no response from the publication target,
reported as a value. The serve path renders an unformable target URL as @500@, and a transport
fault or an overstepped response bound as @502@. Total over the relay: no fault escapes this type.
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

{- | The response from the publication target after relaying a publish document. The proxy
buffers the body whole rather than streaming it, so it can catch and log an exception before
it starts a chunked response it would otherwise abandon mid-stream.
-}
data PublishRelayResponse = PublishRelayResponse
    { relayStatus :: Int
    -- ^ The HTTP status code the publication target returned.
    , relayBody :: LByteString
    -- ^ The publication target's response body, relayed to the client unchanged.
    }
    deriving stock (Eq, Show)

{- | Why a publish could not complete, surfaced as a value rather than thrown. The cases
differ in retryability, so the mirror worker decides retry against drop by an exhaustive
pattern match.
-}
data PublishFault
    = {- | The request URL could not be formed (e.g. an empty base URL): a
      configuration fault carried as its 'UrlFormationError'. __Not retryable__:
      redelivering the job cannot change a misconfigured base URL, so the worker
      drops the job and alerts rather than re-enqueueing forever.
      -}
      PublishUrlUnformable UrlFormationError
    | {- | The registry rejected the write (a non-2xx, non-@409@ status), carried
      as a 'PublishError'. __Retryable__: the worker leaves the job un-acked for
      redelivery.
      -}
      PublishRejected PublishError
    | {- | The write never reached the registry: the HTTP request threw before any
      status returned (a connection failure, a TLS error, a timeout). Carried as the
      'TransportFault' the adapter edge classified out of its client library's
      exception, exactly as 'FetchTransport' and 'RelayTransport' carry theirs.
      __Retryable__: the transport may recover, so the worker leaves the job un-acked
      for redelivery, exactly as for a 'PublishRejected'. Surfacing it as a value is
      what lets the mirror write ('Ecluse.Core.Registry.Publish.mpPublishArtifact')
      honour its total, never-thrown contract.
      -}
      PublishTransport TransportFault
    deriving stock (Eq, Show)
