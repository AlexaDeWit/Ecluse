# Registry Model

> Part of the [Écluse architecture overview](../architecture.md).

## Three-Registry Model

The proxy is configured with three registry endpoints:

| Role | Purpose |
|------|---------|
| **Private upstream** | Primary fetch target. If a package is found here, it is served immediately with no rules applied — it has already been vetted. |
| **Public upstream** | Fallback. Queried only when the private upstream does not have the package. Security rules are applied to all responses from here. |
| **Mirror target** | Where approved public packages are written after passing rules. May be the same registry as the private upstream (most common) or a different one (e.g. separate internal/public stores). |

## Registry Abstraction

The proxy core is registry-agnostic. The `RegistryClient` record is the sole
interface between the proxy logic and any specific registry protocol:

```haskell
data RegistryClient = RegistryClient
  { fetchMetadata    :: PackageName -> IO RegistryResponse
  , fetchArtifact    :: PackageName -> Version -> IO RegistryResponse
  , publishArtifact  :: PackageName -> Version -> ByteString -> IO (Either PublishError ())
  , parsePackageInfo :: RegistryResponse -> Either ParseError PackageInfo
  , parseVersionDetails :: RegistryResponse -> Version -> Either ParseError PackageDetails
  , parseVersionList :: RegistryResponse -> Either ParseError [Version]
  }
```

The effectful fields return plain `IO`, not `App`: an adapter closes over its own
state (HTTP manager, credentials) and never imports the proxy's `Env`/`App`, so
backends stay decoupled from the core. The `parse*` fields are pure. See
[Technology Stack → the effect model](technology-stack.md#key-decisions).

Nothing above the registry layer imports registry-specific types. The proxy core
operates only on `PackageInfo` (the packument-level view) and `PackageDetails`
(the per-version snapshot the rules engine evaluates — see
[`src/Ecluse/Package.hs`](../../src/Ecluse/Package.hs)). A registry
adapter is responsible for projecting its wire format into these types.

**Supported implementations at launch:** npm registry protocol only. The
`RegistryClient` abstraction exists from day one to make future backends
(PyPI, RubyGems, …) additive rather than structural changes.

`RegistryClient` is the **ecosystem (protocol) seam** — fetch, publish, and parse
— and nothing more. It deliberately does **not** carry authentication, because
protocol and auth are **orthogonal axes**: AWS **CodeArtifact**, GCP **Artifact
Registry**, and a self-hosted Verdaccio/Nexus all speak the *same* npm protocol
and differ only in how a bearer token is obtained. Folding "CodeArtifact-ness"
into the npm adapter would force a near-duplicate adapter per cloud; instead the
npm `RegistryClient` is used **unchanged** and paired with a
[`CredentialProvider`](cloud-backends.md#credential-provider) that mints the
token. The backend matrix is therefore *ecosystem × credential provider*, and the
cells compose freely (npm-on-CodeArtifact, npm-on-Artifact-Registry,
pypi-on-static, …). See [Cloud Backends](cloud-backends.md#cloud-backends).
