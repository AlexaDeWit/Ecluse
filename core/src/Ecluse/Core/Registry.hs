-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The registry-protocol vocabulary: the payload and typed-fault types every registry-facing
capability shares. Failures are __values__ here, never throws, so a consumer's fall-through or
retry-versus-drop decision stays total at the call site. The vocabulary carries no
authentication, because protocol and credential are orthogonal axes, and one protocol
implementation therefore serves every managed npm registry. The capabilities that speak a
protocol over these types live beside it: "Ecluse.Core.Registry.Metadata" for the read handle,
"Ecluse.Core.Registry.Publish" for the mirror write, and "Ecluse.Core.Registry.Adapter" for
each ecosystem's capability record.
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
) where

import Ecluse.Core.Fault (TransportFault)
import Ecluse.Core.Package (Hash, HashAlg, hashAlg, hashValue)
import Ecluse.Core.Security (LimitError, authorityLabel)
import Ecluse.Core.Server.Path (Filename)

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
    { maFilename :: Filename
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

{- | Why a bounded exchange could not produce a usable response, reported as a value.
Total over every exchange the proxy runs: no failure rides up outside this type.
-}
data FetchFault
    = -- | The request URL could not be formed from the base URL and the package identity.
      FetchUrlUnformable UrlFormationError
    | -- | The peer's body crossed the response-size bound, and the read refused it fail-closed.
      FetchBoundExceeded LimitError
    | {- | The request never completed (a timeout, an unreachable peer, a TLS refusal),
      carried as the 'TransportFault' the adapter edge classified.
      -}
      FetchTransport TransportFault
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

{- | Why a publish could not complete, surfaced as a value. The cases differ in
retryability, so the worker decides retry against drop by an exhaustive match.
-}
data PublishFault
    = {- | The exchange never produced a status to read, carried as the shared 'FetchFault'
      so the worker reads one retry-versus-drop table for the write and the artifact fetch.
      -}
      PublishFetch FetchFault
    | -- | The registry answered and rejected the write (a non-2xx, non-@409@ status). Retryable.
      PublishRejected PublishError
    deriving stock (Eq, Show)
