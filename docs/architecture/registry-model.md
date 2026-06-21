# Registry Model

> Part of the [Écluse architecture overview](../architecture.md).

## Three-Registry Model

The proxy is configured with three registry endpoints:

| Role | Purpose |
|------|---------|
| **Private upstream** | Authoritative, already-vetted source. A **tarball** found here is served immediately, unfiltered. A **packument**'s versions are trusted and **merged** with the gated public set (see [Packument merge](#packument-merge-across-upstreams)) rather than short-circuiting the public fetch. |
| **Public upstream** | Source of versions not (yet) in the private upstream; rules are applied to everything from here. For a **tarball** it is the fallback on a private miss; for a **packument** it is fetched **alongside** the private upstream and merged in. |
| **Mirror target** | Where approved public packages are written after passing rules. May be the same registry as the private upstream (most common) or a different one (e.g. separate internal/public stores). |

### Credential flow and authority

Écluse is **not** an access-granting authority. Read access is decided entirely by
the upstreams; Écluse holds a credential only to *write* mirrored packages. Each
role has a distinct credential behaviour:

- **Private upstream (read)** — Écluse **forwards the client's own credential**
  (`Authorization` / `_authToken`) verbatim, and the private upstream authorizes.
  The upstream is the authority for who may read what; Écluse adjudicates nothing
  on reads and never substitutes its own identity.
- **Public upstream (read/fallback)** — queried **anonymously**. The client's
  credential is **never** forwarded here; sending an internal token to the public
  registry would be a credential disclosure. (If a public mirror itself needs
  auth, that is Écluse's *own* configured credential — never the client's.)
- **Mirror target (write)** — the **only** place Écluse uses its own credential:
  the [`CredentialProvider`](cloud-backends.md#credential-provider) mints the token
  to publish approved packages. Often the same registry as the private upstream,
  but a different identity on it: the *client* reads it, *Écluse* writes it.

The non-negotiable invariant: **the client's credential reaches the private
upstream and nothing else — and never the public upstream.**

Outbound requests are further constrained by the
[security invariants](security.md): an **outbound host allowlist**,
**internal-range blocking**, **identifier canonicalisation**, and **bounded
responses**, so a crafted identifier or a hostile upstream can neither steer a
fetch to an unintended target nor exhaust the proxy (issue #11).

## Packument merge across upstreams

A **packument** (package metadata) is not served by first-hit short-circuit the
way a tarball is. A tarball is one concrete version from one source, so a
private-upstream hit is streamed and we are done. A packument is the *set of
available versions*, and that set is spread across upstreams: the private upstream
holds what has been vetted/mirrored, the public upstream holds the full history —
including **new versions not yet mirrored**. Serving only the private packument
would hide those new versions, so a client never requests them, so **demand-driven
mirroring never fires for them** (see
[Cloud Backends → Mirror Queue](cloud-backends.md#mirror-queue)). The packument is
therefore **merged**, not short-circuited.

**The merge is a fold over upstream packuments**, with the single-source case as
the degenerate identity:

- **Fetch upstreams in parallel.** For a packument, the private and public
  upstreams are fetched concurrently (the credential rules above still hold:
  client token to private, anonymous to public).
- **Trust split by provenance.** Private-upstream versions are **trusted** and
  enter the merged document **unfiltered** (already vetted). Public-upstream
  versions are **gated** — the rules engine filters them (see
  [Applying verdicts to a packument](rules-engine.md#applying-verdicts-to-a-packument))
  — before they enter. The merged packument is deliberately **mixed-provenance**:
  `trusted(private) ∪ filtered(public)`.
- **Collision → private wins; divergence is a signal.** When the same version key
  appears in both upstreams, the private copy wins (it is the authority). But if
  the public copy's **integrity differs** from the private one of the same version,
  that divergence is exactly the supply-chain tampering Écluse exists to catch: it
  is **detected, logged, and metered** (and may fail-closed on that version), never
  silently reconciled.
- **Reconcile over the union.** `dist-tags.latest` is repointed to the highest
  *surviving* version across **all** sources; tags pointing at an absent version
  are dropped; `time` is the union restricted to surviving versions. The
  cross-field coherence this requires (every `dist-tags` target is a present
  `versions` key) is an invariant held by tests, not the type.
- **Partial-upstream availability.** If one upstream fails while another succeeds,
  the merge serves the **best-effort union** of what resolved, with a degraded
  signal — readiness stays
  [lenient about public-upstream reachability](web-layer.md#meta-routes-ping-health-and-search).
  Only when *nothing* resolves does the request error (per the
  [serve error model](web-layer.md#error-model)).

**Where the merge lives.** Above the [`RegistryClient`](#registry-abstraction)
seam, as a **pure, ecosystem-agnostic** operation over `PackageInfo` — *not* inside
an adapter. The seam stays single-registry (`fetchMetadata` fetches one registry;
`parsePackageInfo` parses one document); the core fans out across the configured
upstreams, parses each, and folds the results. So a new ecosystem's adapter does
not re-implement merging, and the merge is unit-tested over hand-built
`PackageInfo` with no network. The merged document is something **no single
upstream produces** — Écluse authors it — which is why its served schema is owned
(see [API Surface](api-surface.md#the-synthesized-packument-schema--the-trust-boundary)).

### Registry-level composition (optional, never required)

The merge is an **Écluse-level capability**, so a correct deployment needs nothing
more than the three endpoints above. Some operators will *additionally* compose at
the **registry** level — e.g. AWS CodeArtifact upstream relationships, where
`PRIVATE_UPSTREAM_URL` points at an aggregating repository that itself draws from a
mirror-target repo and a first-party "published-by-us" repo. The private upstream
then behaves as an aggregator and returns a richer trusted set in one fetch.

This is a supported topology, **not a requirement**: Écluse's own fold gives the
same correctness to operators who cannot or do not compose at the registry level.
One caveat makes it safe — registry composition aggregates the **trusted** sources
only (mirror + first-party); it must **not** include a direct external connection
to the public registry, because that would let unvetted public packages reach
clients *through the private upstream, bypassing the gate*. The public upstream is
always fetched and gated by Écluse itself.

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
