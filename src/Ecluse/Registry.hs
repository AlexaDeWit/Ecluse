{- | The registry-protocol seam: the sole interface between the proxy core and
any specific registry's wire protocol.

This is the __ecosystem (protocol) axis__ — fetch, publish, and parse — and
nothing more (see @docs\/architecture\/registry-model.md@ → "Registry
Abstraction"). It is a __record of functions__ (the Handle pattern): a backend's
smart constructor returns a 'RegistryClient' whose closures capture that
backend's private state (an HTTP manager). The proxy core operates only on
'Ecluse.Package.PackageInfo' (the packument-level view) and
'Ecluse.Package.PackageDetails' (the per-version snapshot the rules engine
evaluates); an adapter projects its wire format into those, and nothing above
the registry layer sees registry-specific structures.

Two design points are load-bearing:

* __The effectful fields return 'IO', not @App@.__ An adapter closes over its
  own state (HTTP manager, credentials) and never imports the proxy's
  @Env@\/@App@, so backends stay decoupled from the core (no import cycle) — see
  @docs\/architecture\/technology-stack.md@ → "Key Decisions". The @parse*@
  fields are __pure__ ('Either'): parsing a fetched response is a total,
  side-effect-free projection (/parse, don't validate/).

* __'RegistryClient' deliberately carries no authentication.__ Protocol and auth
  are orthogonal axes: every managed npm registry (AWS CodeArtifact, GCP Artifact
  Registry, a self-hosted Verdaccio) speaks the same npm protocol and differs
  only in how a bearer token is minted, which lives behind the separate
  "Ecluse.Credential" seam. So one npm 'RegistryClient' is reused across every
  cloud rather than near-duplicated per provider.

At launch the only implementation is the npm registry protocol; the abstraction
exists from day one so further backends (PyPI, RubyGems) are additive rather than
structural. The concrete adapters and in-memory\/fixture doubles are layered on
later behind this same record.
-}
module Ecluse.Registry (
    -- * Protocol seam
    RegistryClient (..),

    -- * Fetch payload
    RegistryResponse (..),

    -- * Errors
    ParseError (..),
    PublishError (..),
) where

import Ecluse.Package (PackageDetails, PackageInfo, PackageName)
import Ecluse.Version (Version)

{- | A raw response fetched from a registry — the unparsed bytes of a metadata
document or an artifact, as returned by 'fetchMetadata' \/ 'fetchArtifact'. It is
kept opaque-of-bytes here so the protocol\/data plane (fetch) is separate from
parsing: a @parse*@ field turns a 'RegistryResponse' into a domain type.
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

{- | Why publishing an artifact to a registry failed (an 'Ecluse.Queue' job is
then left un-acked and retried; see @docs\/architecture\/cloud-backends.md@).
-}
newtype PublishError = PublishError
    { publishErrorMessage :: Text
    -- ^ A human-readable description of why the publish failed.
    }
    deriving stock (Eq, Show)

{- | The registry-protocol seam — a record of functions over a backend whose
private state the closures capture. The effectful fields return 'IO' (decoupled
from the core); the @parse*@ fields are pure. See the module header.
-}
data RegistryClient = RegistryClient
    { fetchMetadata :: PackageName -> IO RegistryResponse
    -- ^ Fetch a package's metadata document (its packument) from the registry.
    , fetchArtifact :: PackageName -> Version -> IO RegistryResponse
    -- ^ Fetch the artifact bytes for one version.
    , publishArtifact :: PackageName -> Version -> ByteString -> IO (Either PublishError ())
    -- ^ Publish an artifact's bytes for one version to the registry. Idempotent
    -- at the protocol level (versions are immutable), so a redelivered mirror
    -- job's re-publish is safe.
    , parsePackageInfo :: RegistryResponse -> Either ParseError PackageInfo
    -- ^ Project a fetched metadata response into the packument-level
    -- 'PackageInfo'. Pure and total.
    , parseVersionDetails :: RegistryResponse -> Version -> Either ParseError PackageDetails
    -- ^ Project a fetched metadata response into the per-version
    -- 'PackageDetails' for a specific version. Pure and total.
    , parseVersionList :: RegistryResponse -> Either ParseError [Version]
    -- ^ Extract the list of available versions from a fetched metadata response.
    -- Pure and total.
    }
