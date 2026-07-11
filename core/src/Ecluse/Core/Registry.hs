-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The registry-protocol handle: the sole interface between the proxy core and
any specific registry's wire protocol.

This is the __ecosystem (protocol) axis__ -- fetch, publish, and parse -- and
nothing more (see @docs\/architecture\/registry-model.md@ → "Registry
Abstraction"). It is a __record of functions__ (the Handle pattern): a backend's
smart constructor returns a 'RegistryClient' whose closures capture that
backend's private state (an HTTP manager). The proxy core operates only on
'Ecluse.Core.Package.PackageInfo' (the packument-level view) and
'Ecluse.Core.Package.PackageDetails' (the per-version snapshot the rules engine
evaluates); an adapter projects its wire format into those, and nothing above
the registry layer sees registry-specific structures.

Two design points are load-bearing:

* __The effectful fields return 'IO', not @App@.__ An adapter closes over its
  own state (HTTP manager, credentials) and never imports the proxy's
  @Env@\/@App@, so backends stay decoupled from the core (no import cycle) -- see
  @docs\/architecture\/technology-stack.md@ → "Key Decisions". The @parse*@
  fields are __pure__ ('Either'): parsing a fetched response is a total,
  side-effect-free projection (/parse, don't validate/).

* __'RegistryClient' deliberately carries no authentication.__ Protocol and auth
  are orthogonal axes: every managed npm registry (AWS CodeArtifact, GCP Artifact
  Registry, a self-hosted Verdaccio) speaks the same npm protocol and differs
  only in how a bearer token is minted, which lives behind the separate
  "Ecluse.Core.Credential" handle. So one npm 'RegistryClient' is reused across every
  cloud rather than near-duplicated per provider.

The abstraction is the sole interface, so a new ecosystem backend (PyPI,
RubyGems, …) is an additive constructor behind this record rather than a
structural change.
-}
module Ecluse.Core.Registry (
    -- * Protocol handle
    RegistryClient (..),

    -- * Fetch payload
    RegistryResponse (..),

    -- * Errors
    ParseError (..),
    FetchFault (..),
    PublishError (..),
    PublishFault (..),
    UrlFormationError (..),
    PublishRelayResponse (..),
    PublishRelayFault (..),
    RegistryUnconfigured (..),
) where

import Ecluse.Core.Fault (TransportFault)
import Ecluse.Core.Package (PackageDetails, PackageInfo, PackageName)
import Ecluse.Core.Queue (MirrorArtifact)
import Ecluse.Core.Security (LimitError)
import Ecluse.Core.Version (Version)

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
reported by 'publishArtifact' (an 'Ecluse.Core.Queue' job is then left un-acked and
retried; see @docs\/architecture\/cloud-backends.md@).

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

{- | The handle slot is filled but no backend is wired behind it: every effectful
field of 'Ecluse.Proxy.unconfiguredRegistry' refuses with this typed throw. A
composition fault with no per-request decision -- a justified typed exception
rather than a value a caller might fall through -- recognised by the worker's
supervision policy (it fails the process up rather than retrying forever) and by
the request perimeter's escape classification.
-}
data RegistryUnconfigured = RegistryUnconfigured
    deriving stock (Eq, Show)

instance Exception RegistryUnconfigured

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
      is what lets 'publishArtifact' honour its total, never-thrown contract.
      -}
      PublishTransport Text
    deriving stock (Eq, Show)

{- | The registry-protocol handle -- a record of functions over a backend whose
private state the closures capture. The effectful fields return 'IO' (decoupled
from the core); the @parse*@ fields are pure. See the module header.
-}
data RegistryClient = RegistryClient
    { fetchMetadata :: PackageName -> IO (Either FetchFault RegistryResponse)
    {- ^ Fetch a package's metadata document (its packument) from the registry. A
    failure is reported as a 'FetchFault' __value__ -- an unformable URL, a bound
    breach, or a transport fault -- never thrown, so a consumer's fall-through
    decision is total at the call site.
    -}
    , publishArtifact :: PackageName -> Version -> MirrorArtifact -> ByteString -> IO (Either PublishFault ())
    {- ^ Publish one version's artifact to the registry, given its metadata
    ('MirrorArtifact': filename, integrity hashes, declared size) and the raw
    tarball bytes. The adapter is responsible for assembling the
    ecosystem-specific publish document from these inputs. Idempotent at the
    protocol level (versions are immutable), so a redelivered mirror job's
    re-publish is safe. A failure is reported as a 'PublishFault' __value__ --
    'PublishRejected' or 'PublishTransport' (retry) or 'PublishUrlUnformable' (drop)
    -- never thrown, so the worker's retry-vs-drop decision is total at the call site.
    -}
    , parsePackageInfo :: PackageName -> RegistryResponse -> Either ParseError PackageInfo
    {- ^ Project a fetched metadata response into the packument-level
    'PackageInfo' for the requested package. The 'PackageName' is the identity the
    request is for -- the proxy always knows it from the route -- supplied so the
    projection has the requested identity available alongside the upstream
    document's self-reported @name@.
    -}
    , parseVersionDetails :: RegistryResponse -> Version -> Either ParseError PackageDetails
    {- ^ Project a fetched metadata response into the per-version
    'PackageDetails' for a specific version.
    -}
    , parseVersionList :: RegistryResponse -> Either ParseError [Version]
    -- ^ Extract the list of available versions from a fetched metadata response.
    }
