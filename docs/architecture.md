# Architecture & Requirements

This document captures the **systems design**: how the proxy is structured and
why. Development practices — codebase layout, testing strategy, and CI / repo
requirements — live in [`../CONTRIBUTING.md`](../CONTRIBUTING.md).

## Vision

Supply chain attacks through malicious or hijacked package publications are an
increasing threat in high-volume ecosystems like npm. **Écluse** (package
`ecluse`) is a lightweight proxy that sits between consumers (developers, CI)
and the npm registry, applying a configurable resilience policy before any
package reaches a build — without taking on the cost or complexity of hosting
packages itself.

The name is French for a canal lock — a chamber whose gates never open at once.
That is the posture: not a wall that blocks, but a controlled passage every
dependency is held in and cleared through before it is admitted to a build. The
goal is resilience — mitigating the blast radius of a bad publish — rather than
malware detection.

The proxy is not a registry. It delegates storage to whatever backend the
operator chooses (e.g. AWS CodeArtifact), and enforces a configurable policy on
what may be fetched and mirrored from the public registry.

---

## Three-Registry Model

The proxy is configured with three registry endpoints:

| Role | Purpose |
|------|---------|
| **Private upstream** | Primary fetch target. If a package is found here, it is served immediately with no rules applied — it has already been vetted. |
| **Public upstream** | Fallback. Queried only when the private upstream does not have the package. Security rules are applied to all responses from here. |
| **Mirror target** | Where approved public packages are written after passing rules. May be the same registry as the private upstream (most common) or a different one (e.g. separate internal/public stores). |

---

## Registry Abstraction

The proxy core is registry-agnostic. The `RegistryClient` record is the sole
interface between the proxy logic and any specific registry protocol:

```haskell
data RegistryClient = RegistryClient
  { fetchMetadata    :: PackageId -> App RegistryResponse
  , fetchArtifact    :: PackageId -> Version -> App RegistryResponse
  , publishArtifact  :: PackageId -> Version -> ByteString -> App (Either PublishError ())
  , parsePackageInfo :: RegistryResponse -> Either ParseError PackageInfo
  , parseVersionDetails :: RegistryResponse -> Version -> Either ParseError PackageDetails
  , parseVersionList :: RegistryResponse -> Either ParseError [Version]
  }
```

Nothing above the registry layer imports registry-specific types. The proxy core
operates only on `PackageInfo` (the packument-level view) and `PackageDetails`
(the per-version snapshot the rules engine evaluates — see
[`src/Ecluse/Package.hs`](../src/Ecluse/Package.hs)). A registry
adapter is responsible for projecting its wire format into these types.

**Supported implementations at launch:** npm registry protocol only. The
`RegistryClient` abstraction exists from day one to make future backends
(PyPI, etc.) additive rather than structural changes.

**CodeArtifact** is a first-class backend for the private upstream and mirror
target. It speaks the npm protocol but requires IAM-based authentication rather
than a static credential. The npm `RegistryClient` implementation will include a
CodeArtifact variant that handles token refresh via the AWS SDK (`amazonka`).

---

## Multi-Ecosystem Hosting

A single Écluse process serves one or more ecosystems from one listener, by
**mounting** each registry under a path prefix on a shared base URL:

```
https://registry.internal.example.com/npm    → npm mount
https://registry.internal.example.com/pypi   → PyPI mount
```

No second instance, host, or port is needed per ecosystem. This is the "virtual
repository" model proven by Artifactory and Nexus: one host, many repositories
under paths, several ecosystems side by side.

### Mounts

A **mount** binds a path prefix to:

- a **registry adapter** — the `RegistryClient` for that ecosystem (see
  [Registry Abstraction](#registry-abstraction));
- a **three-registry tuple** — its own private upstream, public upstream, and
  mirror target (see [Three-Registry Model](#three-registry-model));
- a **rule set**.

Mounts are independent, so one process can host several mounts of the *same*
ecosystem under different policies (e.g. `/npm-prod` vs `/npm-canary`), not
merely one mount per ecosystem. The single-registry setup described under
[Configuration](#configuration) is the degenerate case — one mount — and
generalizes to a map of `prefix → mount`.

### Why path prefixes work

Both npm and pip treat their configured endpoint as a **base URL and derive
every request path relative to it**; neither assumes the registry sits at the
root of a host. A client is pointed once at a mount's base and the rest follows:

- **npm** — the `registry` (and `@scope:registry`) setting is a base URL that may
  include a path; auth tokens are keyed by that base *including its path*, so
  credentials scope cleanly to a mount.
- **pip** — the index URL points at wherever the Simple API root lives, and file
  URLs are resolved relative to the index page, so a prefix is transparent.

The per-ecosystem request shapes and base-relative behaviour are documented in
[`research/reverse-engineering/npm.md`](research/reverse-engineering/npm.md) and
[`research/reverse-engineering/pypi.md`](research/reverse-engineering/pypi.md).

### The load-bearing requirement: URL rewriting

Registry responses embed **absolute artifact locations** — npm's `dist.tarball`,
and on public PyPI the file URLs point at a *separate artifact host* entirely. If
Écluse forwards these unchanged, a client resolves metadata *through* the proxy
but downloads bytes *directly from upstream*, bypassing the gate.

So a mount must **rewrite embedded artifact URLs to stay under its own prefix**
before serving metadata:

- **npm** — rewrite `dist.tarball` to `{mount-base}/{pkg}/-/{file}`.
- **PyPI** — emit artifact URLs relative to the Simple index (cleanest — the
  client resolves them under the mount automatically), or absolute under the
  mount.

Keeping artifacts on the **same host, under the prefix** has a second benefit:
npm attaches credentials only to requests on the registry host, so same-host
artifact URLs keep auth flowing on tarball fetches that a separate artifact host
would silently drop.

Because rewriting must emit correct absolute URLs, **a mount must know its own
externally-visible base URL.** Inferring this from request headers is unreliable
behind load balancers and TLS terminators, so the public base is explicit
configuration, per mount.

### Dispatch

Routing is a thin layer above the registry adapters: match the request path's
**leading segment** to a mount, strip the prefix, and hand the remainder — now an
ordinary ecosystem-native path — to that mount's adapter and the standard
[Request Lifecycle](#request-lifecycle). The proxy core and the adapters are
unchanged by the presence of multiple mounts; only the front door learns to fan
out. A mount prefix should be accepted with or without a trailing slash, since
the base-URL join behaviour differs subtly between clients — an area to validate
against the real `npm` and `pip` clients during implementation.

### Alternative: host-based routing

The same single process could instead distinguish ecosystems by **hostname**
(`npm.registry.example.com`, `pypi.registry.example.com`), dispatching on the
request's host rather than a path prefix. This yields root-path URLs (rewriting
only swaps the host, never injects a prefix) at the cost of a DNS name and TLS
coverage per ecosystem. It is still one instance — the choice is routing *style*,
not instance count. **Path-prefix mounting is the default** (one name, one
certificate, no DNS choreography); host-based routing is available where
per-ecosystem hostnames are specifically wanted.

---

## Request Lifecycle

```
Client request
    │
    ▼
[1] Fetch from private upstream
    │
    ├─ 2xx ──────────────────────────────────────────► Serve to client. Done.
    │
    └─ non-2xx (miss)
        │
        ▼
    [2] Fetch from public upstream
        │
        ├─ non-2xx ──────────────────────────────────► Forward error to client.
        │
        └─ 2xx
            │
            ▼
        [3] Parse into PackageInfo / PackageDetails
            │
            ▼
        [4] Evaluate RuleSet (deny by default)
            │
            ├─ Pure rules first (no IO, fast)
            ├─ Effectful rules if undecided (CVE lookups, etc.)
            │
            ├─ Denied ──────────────────────────────► 403 + denial message. Done.
            │
            └─ Allowed
                │
                ▼
            [5] Enqueue mirror job (SQS) — non-blocking
                │
                ▼
            [6] Serve response to client immediately
```

Tarball/artifact requests follow the same lifecycle via `fetchArtifact`.

---

## Web Layer

The front door is a raw `wai` `Application` served by `warp`. It does three
jobs: route an incoming request, stream artifacts through with bounded memory,
and apply cross-cutting concerns as middleware. Three decisions shape it.

### Raw WAI, not a web framework

A proxy is fundamentally a passthrough over a small, irregular URL surface — npm
paths carry URL-encoded slashes (`/@scope%2Fpkg`, `/pkg/-/pkg-1.0.0.tgz`) and
reserved meta-routes (`/-/npm/v1/security/advisories/bulk`). Matching on
`pathInfo` in a raw `Application` is simpler and more flexible than encoding that
shape at the type level (servant) or adopting a framework whose response
handling hides the streaming control we depend on (see
[Streaming](#streaming-and-resource-lifetime)).

Routing sits in two layers. **Mount dispatch** (see [Dispatch](#dispatch))
matches the leading path segment to a mount and strips the prefix. What remains
is an ecosystem-native path, classified by a **pure** function into a small route
type:

```haskell
data Route = Packument PackageName | Tarball PackageName Text | Ping | Search | Unsupported

classify :: [Text] -> Route
```

Keeping `classify` pure makes the whole routing table unit-testable with no
server — feed it `pathInfo`, assert the `Route`. Two npm-specific facts it must
encode: `pathInfo` splits on `/` *before* percent-decoding, so an encoded scoped
name (`/@scope%2Fpkg`) arrives as a single segment; and reserved meta-routes
(`/-/…`) are matched first, since a real package name can never begin with `-`.
Anything unrecognized is `Unsupported` → 404, so deny-by-default holds at the
routing layer too.

### Control plane vs. data plane

The single most important split in the HTTP code:

- **Data plane** — streaming artifacts and fetching metadata — goes through
  `http-client`.
- **Control plane** — SQS (mirror queue), STS, and CodeArtifact's
  `GetAuthorizationToken` — goes through `amazonka`.

This matters most for CodeArtifact. Its npm repository is a **standard HTTPS npm
endpoint**: obtain a bearer token from `GetAuthorizationToken` (control plane,
`amazonka`), then fetch packuments and tarballs with ordinary `http-client` (data
plane). The streaming path therefore never touches `amazonka`'s
conduit/`ResourceT` machinery — which is exactly where naive
streaming-through-a-proxy goes wrong.

### Streaming and resource lifetime

A WAI streaming response body **runs after the handler returns** — Warp
serializes it while writing to the socket. So a resource with lexical scope
(`bracket`, `withResponse`, `runResourceT`) released when the handler returns is
already gone by the time the body streams: a use-after-free / GC race. This is
the classic trap, and it is why frameworks that hide the response continuation
make memory-bounded artifact streaming awkward.

Raw WAI avoids it by construction. `Application` is continuation-passing — *you*
call `respond` — so the resource acquisition can bracket the `respond` call
itself:

```haskell
serveArtifact mgr upstreamReq respond =
  withResponse upstreamReq mgr $ \up ->            -- upstream connection acquired
    respond $ responseStream status200 (relayHeaders up) $ \write flush -> do
      let pump = do
            chunk <- brRead (responseBody up)
            unless (BS.null chunk) (write (byteString chunk) >> flush >> pump)
      pump                                          -- closed only after Warp returns
```

The upstream connection lives for exactly the duration of the streamed body and
is closed only when Warp returns `ResponseReceived`. `write` blocks on the socket
send buffer, so we pull from upstream only as fast as the client drains —
**constant memory regardless of artifact size**, with backpressure for free. No
`ResourceT`, no conduit on the hot path.

### Middleware and helper libraries

The dividing principle: **adopt libraries for cross-cutting infrastructure that
is identical for every service; hand-roll anything that encodes our domain or
wire contract.**

- **Adopt — `wai-extra` middleware** (already a dependency): `RequestSizeLimit`
  (defensive body cap), `RealIp`/`ForwardedFor` (correct client IP behind a load
  balancer), and `Timeout`, composed around the `Application`. Two it
  deliberately does *not* use: `Autohead` — it answers HEAD by running the GET
  handler and discarding the body, which on a tarball route would open the
  upstream and stream a whole artifact to nowhere (HEAD on artifacts is handled
  explicitly instead); and `Gzip` — artifacts are already compressed, and
  re-compressing the stream would fight the backpressure above.
- **Adopt — `unliftio`** for the worker/service layer, where `ReaderT Env IO`
  runs: it lifts `bracket`/`finally`/`async` into the reader so resource-safety
  stays ergonomic. Request handlers stay in plain `IO` taking `Env`, so the hot
  path carries no transformer lifting.
- **Hand-roll** — the router (`classify`), response/error helpers (the npm
  `{"error": …}` shape lives in an `Ecluse.Server.Response` module, grown as
  repetition surfaces), a thin `katip` logging middleware (so request logs join
  the same structured stream as everything else, rather than `wai-extra`'s stock
  logger), and conditional-GET / ETag relay (forwarding the client's validators
  upstream and relaying `304`s is domain behaviour).
- **Decline** — routing libraries (`wai-routes`, `wai-routing`, …): largely
  dormant and segment-based, so they fight the encoded-slash handling that a
  small pure `classify` gets right.
- **Defer** — `http-reverse-proxy` (revisit only if the hand-rolled core starts
  reinventing it; our need to intercept and *synthesize* denial responses argues
  against a transparent proxy), metrics middleware (`wai-middleware-prometheus`
  or `katip` + `ekg`, when observability lands), and `warp-tls` (only if TLS is
  not terminated upstream).

---

## Rules Engine

**Deny by default.** A package is blocked unless at least one rule explicitly
allows it.

Rules evaluate a single `PackageDetails` snapshot — the ecosystem-agnostic
per-version view produced by a registry adapter. A rule never sees registry wire
formats.

Rules are evaluated in two tiers:

1. **Pure rules** — evaluated against `PackageDetails` with no IO. Fast and
   deterministic. Evaluated first. This is the tier implemented today
   ([`src/Ecluse/Rules.hs`](../src/Ecluse/Rules.hs)).
2. **Effectful rules** — may perform IO (advisory lookups, external policy
   checks). Only evaluated if no pure rule has produced a decision. A later
   phase, layered on top of the pure tier.

### Evaluation model

Each rule, applied to a `PackageDetails`, yields a `RuleOutcome`:

- **`Allow reason`** — the rule explicitly allows the package.
- **`Deny reason`** — the rule explicitly denies it (reserved for future deny
  rules; the initial allow-rules never deny).
- **`Abstain reason`** — the rule has no opinion. The reason is retained for the
  audit trail.

`evalRules` folds a rule set in order: the **first decisive outcome** (`Allow` or
`Deny`) wins, producing `Approved rule reason` or `Denied rule reason`. If every
rule abstains, the result is `DeniedByDefault reasons` — deny-by-default, with
each rule's reason collected (in order) so the denial response can explain what
was considered.

Crucially, an allow-rule that does not match **abstains rather than denies**, so
that a later rule still gets the chance to allow the package.

### Initial Rule Set

| Rule | Type | Description |
|------|------|-------------|
| `AllowIfPublishedBefore ageSeconds` | Pure | Allows a package version if it was published more than `ageSeconds` seconds ago. Default: 604800 (7 days). Guards against typosquatting and dependency confusion attacks where attackers race to publish before detection. |
| `AllowScope scope` | Pure | Unconditionally allows all packages under a given npm scope (e.g. `@myorg`). Use for internal scopes that bypass public-registry rules. |

Additional rules (e.g. `DenyHasInstallScript`, `DenyIfCVE`) are added as
subsequent phases.

---

## Mirror Queue

When a package passes rules, the proxy:

1. Enqueues a mirror job to **SQS** (the mirror target URL, package ID, version,
   and artifact location).
2. Returns the response to the client **immediately** — no blocking on mirror
   completion.

The SQS consumer (a separate worker process) reads jobs from the queue, fetches
the artifact from the public upstream, and publishes it to the mirror target via
`publishArtifact`. Failed jobs are retried with SQS's built-in retry and
dead-letter queue support.

This means there is a window between a package being approved and it appearing
in the private upstream. Subsequent requests for the same package during this
window will fall through to the public upstream again and re-run rules — this is
acceptable; the rules are deterministic for a given package version.

---

## CVE Subsystem

The CVE subsystem provides an interface for effectful rules to query advisory
databases. The `CVELookup` abstraction allows handlers to be backed by different
sources or caching layers.

**Recommended sources at launch:**

- **npm security advisory endpoint** (`registry.npmjs.org/-/npm/v1/security/advisories/bulk`)
  — the most direct source for npm, no API key required, returns advisories for
  requested packages in bulk.
- **OSV.dev API** — secondary source; broader coverage, also free, useful for
  cross-referencing.

Results should be cached locally in memory (with a configurable TTL) to avoid
per-request latency on advisory lookups.

---

## Configuration

Runtime configuration is provided entirely via environment variables. Rule sets
and other structured config are supplied as JSON strings.

The variables below configure a **single mount**. A multi-mount deployment (see
[Multi-Ecosystem Hosting](#multi-ecosystem-hosting)) supplies the equivalent set
per mount via structured config, keyed by path prefix; the single-mount
variables are the one-entry degenerate form.

| Variable | Required | Description |
|----------|----------|-------------|
| `PROXY_PORT` | No (default: 4873) | Port the proxy listens on. |
| `PRIVATE_UPSTREAM_URL` | Yes | URL of the private upstream registry. |
| `PUBLIC_UPSTREAM_URL` | No (default: `https://registry.npmjs.org`) | URL of the public upstream. |
| `MIRROR_TARGET_URL` | Yes | URL of the registry to mirror approved packages to. |
| `MIRROR_QUEUE_URL` | Yes | SQS queue URL for mirror jobs. |
| `AWS_REGION` | CodeArtifact only | AWS region for CodeArtifact and SQS. |
| `PROXY_AUTH_TOKEN` | No | If set, clients must supply this token as `Bearer` or `_authToken`. Omit for open/network-secured deployments. |
| `PROXY_RULES` | Yes | JSON array of rule objects defining the allow policy (see below). |
| `PROXY_HELP_MESSAGE` | No | Custom string appended to all denial messages (e.g. `"Contact #platform-eng on Slack for assistance."`). |
| `CVE_CACHE_TTL_SECONDS` | No (default: 3600) | How long to cache advisory lookup results. |

### Rule Configuration Format

```json
[
  { "type": "AllowScope",              "scope": "@myorg" },
  { "type": "AllowIfPublishedBefore",  "ageSeconds": 604800 }
]
```

Rules are evaluated in order; first match wins.

---

## Client Authentication

Authentication to the proxy is **optional**. Three modes:

1. **Open** — `PROXY_AUTH_TOKEN` is unset. Any client can reach the proxy.
   Access control is delegated entirely to the network layer (VPC, service mesh,
   etc.).
2. **Static token** — `PROXY_AUTH_TOKEN` is set. Clients must include it as
   `Bearer <token>` in the `Authorization` header or as `_authToken` in
   `.npmrc`. Standard npm tooling supports this out of the box.
3. **AWS IAM (future)** — Validating AWS identity at the proxy edge is deferred
   as a gateway concern. CodeArtifact can be used as the mirror target with IAM
   controlling writes independently.

---

## Denial Responses

When a request is denied (no allow rule matched, or a deny rule fired):

- HTTP status follows npm protocol conventions (403 for policy denials).
- The response body is a JSON object matching the npm error format:
  ```json
  {
    "error": "Package @evil/pkg@1.0.0 was denied: AllowIfPublishedBefore — published 3 hours ago, minimum age is 7 days. Contact #platform-eng on Slack for assistance."
  }
  ```
- The denial reason (which rule decided, and why) is always included.
- `PROXY_HELP_MESSAGE`, if configured, is appended to every denial.

---

## Technology Stack

| Concern | Choice | Rationale |
|---------|--------|-----------|
| Language | Haskell (GHC 9.6) | Type safety, strong concurrency, fits the rule engine well. |
| Prelude | `relude` | Safer defaults: `Text` over `String`, partial functions hidden, re-exports `containers`/`text`/`bytestring`/`stm`. Wired in as the implicit prelude (see below). |
| Effect style | `ReaderT Env IO` (+ `unliftio`) | Simple, standard, testable without exotic dependencies. `unliftio` lifts `bracket`/`async` into the reader for the worker/service layer; request handlers stay in plain `IO` taking `Env`. See [Web Layer](#web-layer). |
| HTTP server | `warp` + `wai` (+ `wai-extra`) | Fast, battle-tested. Raw WAI routing rather than a framework — see [Web Layer](#web-layer). `wai-extra` supplies cross-cutting middleware (size limits, real-IP, timeouts). |
| HTTP client | `http-client` + `http-client-tls` | The data plane: streams artifacts and fetches metadata, including from CodeArtifact's npm endpoint. Kept off `amazonka`'s `ResourceT` streaming path — see [Web Layer](#web-layer). |
| JSON | `aeson` | Metadata parsing, rule config, SQS payloads, denial bodies. |
| AWS | `amazonka` | Split packages: `amazonka-sqs` (mirror queue), `amazonka-codeartifact` (npm auth token), `amazonka-sts` (IAM). |
| Logging | `katip` | Structured, contextual JSON logging. Denials are an audit trail — package/version/rule context attaches to every event. |
| Config | `envparse` | Applicative env-var parser; aggregates all missing/invalid vars into one error rather than failing on the first. |
| Caching | `cache` | STM-backed TTL cache for advisory lookups; handles expiry/eviction for us. |
| Concurrency | `async` + `stm` | Non-blocking mirror enqueue; shared cache/state. |
| Time | `time` | `AllowIfPublishedBefore` age calculations. |
| Unit tests | `hspec` (+ `hspec-wai`) | `hspec-wai` drives the proxy `Application` end-to-end. |
| Property tests | `hedgehog` (+ `hspec-hedgehog`) | Integrated shrinking; used heavily against the pure rules engine. |
| Integration tests | `testcontainers` | Launches ephemeral Docker containers from the test suite (lifecycle + readiness). GHC 9.6-compatible, actively maintained. |
| AWS emulation (tests) | `ministack` | Local AWS emulator (image `ministackorg/ministack`, port 4566) for SQS/STS in integration tests — no real AWS or credentials. |
| Dev environment | Nix flakes + `direnv` | Fully reproducible; all tooling from `nix develop`. |
| Build | Cabal | Natural Nix pairing; `flake.lock` provides reproducibility. |

### Key Decisions

**`relude` as the implicit prelude.** Rather than `NoImplicitPrelude` plus a manual
`import Relude` in every module, it is wired through cabal mixins in the shared
`common` stanza so it replaces the default prelude transparently:

```cabal
build-depends: base, relude
mixins:
    base hiding (Prelude)
  , relude (Relude as Prelude)
```

Note: this rules out `-Wunused-packages`. GHC cannot attribute prelude usage
through the mixin rename, so it reports `base` and `relude` as unused in every
component — a false positive. The flag is therefore omitted; reach for `weeder`
if dependency-hygiene checking is wanted later.

**Raw WAI, not a web framework.** A proxy is a passthrough over an irregular URL
surface (URL-encoded slashes, reserved meta-routes), and memory-bounded artifact
streaming needs direct control over the response body's lifetime. Both point away
from servant/Scotty/Yesod and toward a raw `Application`. The full rationale —
routing, the control/data-plane split, streaming, and the middleware stance — is
in [Web Layer](#web-layer).

---

## Out of Scope (for now)

- Package hosting / storage (delegated to the configured registries).
- Web UI or admin API.
- PyPI and other non-npm **adapters** — the hosting model and `RegistryClient`
  seam are designed to accommodate them (see
  [Multi-Ecosystem Hosting](#multi-ecosystem-hosting)), but only the npm adapter
  ships at launch.
- AWS IAM validation at the proxy edge (gateway concern).
- Local on-disk caching of artifacts (the SQS retry window is acceptable).
