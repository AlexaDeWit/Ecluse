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
operator chooses (e.g. AWS CodeArtifact or GCP Artifact Registry), and enforces a configurable policy on
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
(PyPI, RubyGems, …) additive rather than structural changes.

`RegistryClient` is the **ecosystem (protocol) seam** — fetch, publish, and parse
— and nothing more. It deliberately does **not** carry authentication, because
protocol and auth are **orthogonal axes**: AWS **CodeArtifact**, GCP **Artifact
Registry**, and a self-hosted Verdaccio/Nexus all speak the *same* npm protocol
and differ only in how a bearer token is obtained. Folding "CodeArtifact-ness"
into the npm adapter would force a near-duplicate adapter per cloud; instead the
npm `RegistryClient` is used **unchanged** and paired with a
[`CredentialProvider`](#credential-provider) that mints the token. The backend
matrix is therefore *ecosystem × credential provider*, and the cells compose
freely (npm-on-CodeArtifact, npm-on-Artifact-Registry, pypi-on-static, …). See
[Cloud Backends](#cloud-backends).

---

## Internal Domain Model

`PackageDetails` ([`src/Ecluse/Package.hs`](../src/Ecluse/Package.hs)) is the
ecosystem-agnostic per-version snapshot every adapter produces and the rules
engine consumes. Its shape is the synthesis of the npm/PyPI/RubyGems protocol
studies ([`research/synthesis.md`](research/synthesis.md)); two principles
govern it:

- **The rules engine is ecosystem-blind.** It never branches on npm vs PyPI vs
  RubyGems. Adapters project each ecosystem's wire format into *normalised
  signals*; a rule sees `CodeExecSignal`, `Trust`, `Availability` — never
  `hasInstallScript`, `packagetype`, or `extensions`.
- **Signal availability is explicit.** A signal the adapter has not (or cannot
  cheaply) determined is represented as such (`CodeExecUnknown`, `TrustUnknown`,
  `Nothing`), so a pure rule abstains rather than guessing and the effectful tier
  can resolve it later (see [Rules Engine](#rules-engine)).

### The shared vocabulary

| Concern | Representation | Why |
|---|---|---|
| **Identity** | `PackageName`: ecosystem tag + optional namespace (npm scope) + a normalised `canonical` key + a `display` form; equality is on the canonical key only. | npm is case-sensitive with scopes, PyPI normalises (PEP 503), RubyGems is verbatim — matching must use one canonical key while rendering stays faithful. |
| **Version** | opaque text, **no derived `Ord`**; ordering is `compareVersion ecosystem` (semver / PEP 440 / `Gem::Version`). | Lexicographic ordering is wrong for every grammar (`"10.0.0" < "9.0.0"`). |
| **Install-time code execution** | `CodeExecSignal = NoCodeOnInstall \| RunsCodeOnInstall reason \| CodeExecUnknown`. | Unifies npm install scripts, PyPI sdist builds, and RubyGems native extensions; `Unknown` carries the gemspec-fetch case. |
| **Trust / provenance** | `Trust = Trusted (NonEmpty TrustEvidence) \| Untrusted \| TrustUnknown`; `TrustEvidence = Signed \| Attested \| MfaPublished \| OtherEvidence text`. | Signing/attestation/MFA differ per ecosystem but reduce to one signal; the evidence captures the *how* without leaking the ecosystem. |
| **Availability** | `Availability = Available \| Deprecated msg \| Yanked (Maybe reason)`, plus a per-artifact `artYanked`. | npm deprecates (advisory) and RubyGems yanks whole versions; PyPI yanks individual *files* — the per-file flag preserves PyPI's "listed-but-yanked" so exact pins still resolve. |
| **Artifacts** | a version owns `NonEmpty Artifact`; each carries algorithm-tagged `Hash`es, kind/platform, size, interpreter constraint, and a provenance URL. | npm has one tarball; PyPI has an sdist + many wheels; RubyGems has one gem per platform. |
| **Dependencies** | `[Dependency]` with the constraint kept as **raw text** + kind + optional marker. | Lossless and agnostic across semver / PEP 508 / `Gem::Requirement`; parsed only when a rule needs to compare. |

### Decisions captured

The model resolves the open questions from the synthesis (worked through one at a
time):

1. **Yank/availability granularity** — version-level `Availability` **and** a
   per-artifact `artYanked` flag (faithful to all three ecosystems).
2. **Version ordering** — implemented now, per-ecosystem (`compareVersion`),
   rather than deferred; `Version`'s misleading derived `Ord` was dropped.
3. **Ecosystem-specific signals** — folded into ecosystem-blind normalised
   signals (notably `Trust`, with a `TrustEvidence` vocabulary and an
   `OtherEvidence` escape hatch) rather than an ecosystem-tagged sum. Raw residue
   needed only for faithful *serving* stays in the adapter, below the rules
   layer.
4. **Dependencies** — a lossless structured list with raw constraints (no
   constraint parsing yet).
5. **Rollout** — landed as one coherent revision of `Ecluse.Package` and the
   rule inputs; the `evalRules` fold is unchanged.

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
            [5] Enqueue mirror job — non-blocking
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
  `GetAuthorizationToken` (the AWS [`CredentialProvider`](#credential-provider)'s
  `mintToken`) — goes through `amazonka`.

This matters most for CodeArtifact. Its npm repository is a **standard HTTPS npm
endpoint**: obtain a bearer token from `GetAuthorizationToken` (control plane,
`amazonka`), then fetch packuments and tarballs with ordinary `http-client` (data
plane). The streaming path therefore never touches `amazonka`'s
conduit/`ResourceT` machinery — which is exactly where naive
streaming-through-a-proxy goes wrong.

The same split holds on GCP — Pub/Sub and the Artifact Registry token are
control-plane work, while the npm data plane is unchanged `http-client` (see
[Cloud Backends](#cloud-backends)).

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

**Deny by default, and deny wins.** A package is admitted only if some rule
explicitly allows it *and* no rule denies it. A single matching deny rule
overrides every allow.

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

**A rule's tier is determined by where its signal lives, not only by whether it
"feels" like IO.** Many inputs are already present in the metadata an adapter
fetches for resolution — publish age, declared scope, npm's `hasInstallScript`,
a PyPI file's `packagetype == sdist` — and support **pure** rules. Others are
*not* exposed in any metadata response and must be fetched and parsed per
version. RubyGems is the motivating case: a gem's native `extensions` — its
install-time code-execution signal, the analog of npm's install scripts — appears
only in the gemspec inside the `.gem` (or the legacy `quick` Marshal spec), never
in the Compact Index or the JSON API (see
[`research/reverse-engineering/rubygems.md`](research/reverse-engineering/rubygems.md)).
A rule over such a signal is necessarily **effectful**, even though it is
conceptually a simple per-version predicate. Guidance: `parseVersionDetails`
populates `PackageDetails` from the cheap metadata path; a signal that needs an
extra fetch belongs in the effectful tier alongside advisory lookups, and the
same logical rule (e.g. `DenyHasInstallScripts`) may therefore land in different
tiers for different ecosystems.

### Evaluation model

Each rule, applied to a `PackageDetails`, yields a `RuleOutcome`:

- **`Allow reason`** — the rule explicitly allows the package.
- **`Deny reason`** — the rule explicitly denies it. A single `Deny` overrides
  any `Allow`.
- **`Abstain reason`** — the rule has no opinion. The reason is retained for the
  audit trail.

`evalRules` evaluates the whole rule set with **deny precedence**: the first rule
to `Deny` wins outright — producing `Denied rule reason` even if an earlier rule
allowed — and ends evaluation. Absent any deny, the **first `Allow`** wins,
producing `Approved rule reason`. If no rule is decisive, the result is
`DeniedByDefault reasons` — deny-by-default, with each abstaining rule's reason
collected (in order) so the denial response can explain what was considered.

Crucially, a rule that does not fire **abstains rather than deciding**: an
allow-rule that does not match abstains (so a later rule may still allow), and a
deny-rule whose condition is absent abstains (so it never forces a denial on its
own). Only an actual `Deny` blocks — and it does so regardless of its position in
the set.

### Initial Rule Set

| Rule | Type | Description |
|------|------|-------------|
| `AllowIfPublishedBefore ageSeconds` | Pure | Allows a package version if it was published more than `ageSeconds` seconds ago. Default: 604800 (7 days). Guards against typosquatting and dependency confusion attacks where attackers race to publish before detection. |
| `AllowScope scope` | Pure | Unconditionally allows all packages under a given npm scope (e.g. `@myorg`). Use for internal scopes that bypass public-registry rules. |
| `DenyHasInstallScripts` | Pure | Denies any version whose metadata flags install scripts (npm's `hasInstallScript`) — a common arbitrary-code-execution vector at install time. Abstains otherwise. As a deny rule it overrides any allow. |

Further rules — e.g. `DenyIfCVE`, or effectful per-version checks like RubyGems
native `extensions` (see [above](#rules-engine)) — are added as subsequent
phases.

---

## Mirror Queue

When a package passes rules, the proxy:

1. Enqueues a mirror job (the mirror target URL, package ID, version, and
   artifact location) to the configured **mirror queue**.
2. Returns the response to the client **immediately** — no blocking on mirror
   completion.

The queue is a cloud-agnostic seam with backends for AWS SQS and GCP Pub/Sub
(see [Cloud Backends](#cloud-backends)). A consumer (a separate worker process)
receives jobs, fetches the artifact from the public upstream, publishes it to the
mirror target via `publishArtifact`, and acknowledges the job. The worker thus
touches both cloud seams — [`MirrorQueue`](#queue-abstraction) to receive and
[`CredentialProvider`](#credential-provider) to authenticate the write — while the
publish itself is **plain npm protocol plus a bearer token**: pushing to a managed
registry is no different from pushing to any npm registry, so there is no
per-cloud publish path. Both backends give at-least-once delivery with retry and a
dead-letter path for jobs that keep failing — the semantics the worker needs,
regardless of cloud. At-least-once is safe here because the worker is idempotent: a
redelivered job re-runs the deterministic rules and re-publishes the same artifact.

This means there is a window between a package being approved and it appearing
in the private upstream. Subsequent requests for the same package during this
window will fall through to the public upstream again and re-run rules — this is
acceptable; the rules are deterministic for a given package version.

---

## Cloud Backends

Écluse couples to a cloud provider in exactly **two seams**, both records of
functions (the Handle pattern — see [Seams](#seams-records-of-functions)) so that
a provider is an additive backend rather than a structural change, the same
posture as [`RegistryClient`](#registry-abstraction):

1. **`MirrorQueue`** — the durable hand-off from the request path to the mirror
   worker (see [Mirror Queue](#mirror-queue)).
2. **`CredentialProvider`** — mints the short-lived bearer token for any registry
   endpoint (private upstream or mirror target) that is a cloud-managed registry
   rather than a static-credential one (see
   [Credential Provider](#credential-provider)).

These two are the **cloud axis**. The **ecosystem axis** is
[`RegistryClient`](#registry-abstraction), which is cloud-agnostic — so the npm
protocol/data plane, **including publish**, is written once and reused across
every cloud (a managed registry is just an npm endpoint plus a token; there is no
per-cloud publish path and no object-store seam). Everything else — the proxy
core, rules engine, web layer, CVE subsystem — is cloud-agnostic too. **AWS and
GCP are both first-class targets**; the design admits a third provider by adding
backends behind these two seams.

### Seams: records of functions

Every seam — `RegistryClient`, `MirrorQueue`, `CredentialProvider` — is a
**record whose fields are functions** (the *Handle pattern*), constructed by a
per-backend smart constructor (`newSqsQueue :: SqsConfig -> IO MirrorQueue`). This
is Haskell's idiomatic equivalent of an interface with swappable implementations:
the record type is the interface, a smart constructor is a concrete
implementation, and the closure it returns captures that backend's private state
(an `amazonka` env, an HTTP manager) exactly as an object's fields would.

Backend choice is **runtime, config-driven, single-binary**: all adapters are
compiled in, and one **composition root** reads the configured provider, calls the
matching smart constructor, and stores the resulting record in `Env`. Nothing
downstream knows which backend it holds — it just applies the field. This keeps
the cloud SDKs' selection in one place rather than smeared across the code, and
leaves the door open to split adapters into separate libraries later without
disturbing the seam.

*Alternatives considered.* A **free monad** (operations reified as data, AWS/GCP
as interpreters) and **tagless-final** both abstract the backend too, but they buy
*program-as-data* / compile-time dispatch we do not need: selection here is at
runtime by config, the per-op work lives in the interpreter either way, and both
would mean a heavier dependency than the `ReaderT Env IO` baseline. Records of
functions give the same swappability and trivial test doubles (an in-memory
record) with none of that. The free monad would earn its keep only if we needed to
inspect/rewrite mirror programs (e.g. batch enqueues) — and that has a contained
answer behind the existing seam if it ever arises.

### Service mapping

| Concern | AWS | GCP |
|---------|-----|-----|
| Mirror queue | SQS | Cloud Pub/Sub |
| Managed npm registry | CodeArtifact | Artifact Registry |
| Workload identity / token source | STS / instance role | Workload Identity / ADC |
| Local emulator (tests) | `ministack` (LocalStack-style) | Google's official Pub/Sub emulator |

Both managed registries speak the **npm protocol over HTTPS** and differ only in
how the bearer token is obtained and refreshed, so they sit behind the
[`CredentialProvider`](#credential-provider) seam while the `RegistryClient`
protocol/data plane (`http-client`) is identical across them (see
[Web Layer](#web-layer)).

### Credential Provider

Outbound auth (proxy → registry) is its own seam, separate from
[`RegistryClient`](#registry-abstraction). A `CredentialProvider` yields the
current bearer token for a registry endpoint, refreshing it before expiry:

```haskell
newtype CredentialProvider = CredentialProvider
  { currentToken :: IO AuthToken }            -- refreshes-before-expiry internally

data AuthToken = AuthToken { secret :: Secret, expiresAt :: Maybe UTCTime }
```

A provider attaches **per registry endpoint**, not globally: the three-registry
tuple (private upstream, public upstream, mirror target) may need up to three,
though they commonly collapse — the private upstream and mirror target are often
the same CodeArtifact repo behind one provider, and the public upstream is usually
anonymous.

**The sub-seam that matters.** The interesting logic is the refresh / cache /
expiry / concurrency policy, *not* the cloud call. So a single generic wrapper
holds that policy, parameterised over a tiny per-cloud `mintToken` leaf:

```
CredentialProvider
  └─ generic refresh/cache wrapper      -- deterministic: injected clock + fake mint
       └─ mintToken :: IO AuthToken     -- the only per-cloud, un-emulable part
```

Adapters supply only the leaf: `static` (a fixed token, no expiry), **CodeArtifact**
(`GetAuthorizationToken` via `amazonka`, TTL up to 12h), **ADC** (an OAuth2 access
token, TTL ~1h). The wide TTL spread is exactly why the wrapper refreshes off the
token's own `expiresAt` rather than a fixed interval — the same policy then fits
either cloud, and each cloud contributes ~10 lines. This isolation also bounds the
test gap (see [Testing](#testing)): everything but `mintToken` is unit-testable.

### Queue abstraction

The queue is the one piece with materially different APIs per cloud, so it is its
own seam — a `MirrorQueue` with `enqueue` / `receive` / `ack` operations. SQS
(`SendMessage` / `ReceiveMessage` + visibility timeout / `DeleteMessage`) and
Pub/Sub (`Publish` / `Pull` + ack deadline / `Acknowledge`) both fit this
receive → process → ack shape; the differences (visibility timeout vs ack
deadline, dead-letter configuration) stay behind the seam. The provider is chosen
by configuration (see [Configuration](#configuration)).

### Haskell client maturity — a design risk to retire early

This is the one place GCP is **not** a free addition. `amazonka` is comprehensive
and well-maintained; the GCP side is weaker, and the design names that risk
rather than assuming it away:

- **`gogol`** (the amazonka-equivalent GCP SDK, by the same author) covers
  Pub/Sub but has historically trailed `amazonka` in coverage and release
  cadence — its current state must be verified before it is relied on.
- `gogol` is **REST/JSON**-generated, whereas the official Pub/Sub **emulator is
  gRPC-first**, so "does our chosen client work against the emulator?" is not a
  given. Native Haskell gRPC (`grpc-haskell`) is itself immature and is avoided.
- The hedge that fits our philosophy — adopt for big infrastructure, hand-roll
  the small domain surface (see [Web Layer](#web-layer)) — is a thin REST client:
  Pub/Sub's `publish` / `pull` / `acknowledge` is a handful of JSON-over-HTTPS
  calls, and we already run `http-client` + `aeson` + a bearer-token pattern. A
  small client behind the `MirrorQueue` seam keeps us off a possibly-stale SDK,
  **provided** the emulator serves those REST calls.

**Design requirement.** GCP is *designed for* from day one (the two seams above),
but shipping it is **gated on a de-risking spike**: stand up the Pub/Sub emulator
via `testcontainers` and prove one client path can `publish → pull → ack` against
it. That single experiment resolves both the client-maturity and
emulator-compatibility questions before GCP is committed to a release. AWS
(`amazonka` + `ministack`) carries no such risk and ships first.

### Testing

`testcontainers` is a generic container manager, not an AWS-specific one — it
runs `ministack` today and the Pub/Sub emulator the same way. Each cloud's queue
backend is exercised in the integration tier against its own emulator (no real
cloud account or credentials; the Pub/Sub emulator ignores auth entirely), so the
`MirrorQueue` seam is verified per provider.

The managed-registry backends need no emulator — neither CodeArtifact nor
Artifact Registry has a usable one — and the seam split is what makes that a
non-problem. The npm **protocol** is just HTTPS+JSON, so it is exercised **once**
against a real npm-speaking registry (e.g. Verdaccio) or an in-process WAI stub,
and that single suite covers every managed registry because they share the
protocol. The only genuinely un-emulable surface is the per-cloud token *mint*,
isolated in the [`CredentialProvider`](#credential-provider)'s `mintToken` leaf:
the refresh/cache/expiry policy around it is unit-tested deterministically with an
injected clock and a fake mint, and the real cloud mint runs end-to-end only in
the (non-gating) smoke tier. The split shrinks the un-testable surface to one
small function per cloud — an explicit, accepted residual risk, consistent with
how `ecluse-smoke` is already treated.

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
| `MIRROR_QUEUE_PROVIDER` | No (default: `sqs`) | Mirror-queue backend: `sqs` (AWS) or `pubsub` (GCP). See [Cloud Backends](#cloud-backends). |
| `MIRROR_QUEUE_URL` | Yes | Queue identifier for mirror jobs: an SQS queue URL, or a Pub/Sub `projects/<project>/topics/<topic>` resource, per provider. |
| `AWS_REGION` | AWS backends only | Region for SQS and CodeArtifact. |
| `GOOGLE_CLOUD_PROJECT` | GCP backends only | Project for Pub/Sub and Artifact Registry. Credentials come from Application Default Credentials (ADC). |
| `PROXY_AUTH_TOKEN` | No | If set, clients must supply this token as `Bearer` or `_authToken`. Omit for open/network-secured deployments. |
| `PROXY_RULES` | Yes | JSON array of rule objects defining the allow policy (see below). |
| `PROXY_HELP_MESSAGE` | No | Custom string appended to all denial messages (e.g. `"Contact #platform-eng on Slack for assistance."`). |
| `CVE_CACHE_TTL_SECONDS` | No (default: 3600) | How long to cache advisory lookup results. |

### Outbound Registry Credentials

Each registry endpoint selects a [`CredentialProvider`](#credential-provider). A
**cloud-managed** endpoint (its URL host identifies CodeArtifact or Artifact
Registry) derives its token from the ambient cloud credentials already configured
above (`AWS_REGION` / instance role, or ADC / `GOOGLE_CLOUD_PROJECT`) — no secret
is placed in Écluse's own config. A **plain** registry takes an optional static
token per endpoint (e.g. `PRIVATE_UPSTREAM_TOKEN`, `MIRROR_TARGET_TOKEN`); absent
one, the endpoint is treated as anonymous. The public upstream is anonymous by
default. This keeps long-lived registry secrets out of config wherever a cloud
identity can mint a short-lived token instead.

### Rule Configuration Format

```json
[
  { "type": "AllowScope",              "scope": "@myorg" },
  { "type": "AllowIfPublishedBefore",  "ageSeconds": 604800 },
  { "type": "DenyHasInstallScripts" }
]
```

The whole set is evaluated with deny precedence: any matching deny rule blocks
the package, otherwise the first matching allow rule wins; if none is decisive,
the package is denied by default.

---

## Client Authentication

This section covers **inbound** auth (client → proxy). **Outbound** auth
(proxy → registry) is a separate concern, handled by the
[`CredentialProvider`](#credential-provider) seam.

Authentication to the proxy is **optional**. Three modes:

1. **Open** — `PROXY_AUTH_TOKEN` is unset. Any client can reach the proxy.
   Access control is delegated entirely to the network layer (VPC, service mesh,
   etc.).
2. **Static token** — `PROXY_AUTH_TOKEN` is set. Clients must include it as
   `Bearer <token>` in the `Authorization` header or as `_authToken` in
   `.npmrc`. Standard npm tooling supports this out of the box.
3. **Cloud IAM (future)** — Validating cloud identity (AWS IAM / GCP IAM) at the
   proxy edge is deferred as a gateway concern. A managed registry (CodeArtifact /
   Artifact Registry) can be the mirror target with cloud IAM controlling writes
   independently.

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
| HTTP client | `http-client` + `http-client-tls` | The data plane: streams artifacts and fetches metadata, including the CodeArtifact / Artifact Registry npm endpoints. Kept off `amazonka`'s `ResourceT` streaming path — see [Web Layer](#web-layer). |
| JSON | `aeson` | Metadata parsing, rule config, queue payloads, denial bodies. |
| Cloud — AWS | `amazonka` | Split packages: `amazonka-sqs` (mirror queue), `amazonka-codeartifact` (registry token), `amazonka-sts` (workload identity). Mature and comprehensive. |
| Cloud — GCP | `gogol` *or* a hand-rolled REST client (TBD) | Pub/Sub mirror queue + Artifact Registry token. GCP's Haskell story is weaker than AWS's, so the choice is gated on a spike — see [Cloud Backends](#cloud-backends). |
| Logging | `katip` | Structured, contextual JSON logging. Denials are an audit trail — package/version/rule context attaches to every event. |
| Config | `envparse` | Applicative env-var parser; aggregates all missing/invalid vars into one error rather than failing on the first. |
| Caching | `cache` | STM-backed TTL cache for advisory lookups; handles expiry/eviction for us. |
| Concurrency | `async` + `stm` | Non-blocking mirror enqueue; shared cache/state. |
| Time | `time` | `AllowIfPublishedBefore` age calculations. |
| Unit tests | `hspec` (+ `hspec-wai`) | `hspec-wai` drives the proxy `Application` end-to-end. |
| Property tests | `hedgehog` (+ `hspec-hedgehog`) | Integrated shrinking; used heavily against the pure rules engine. |
| Integration tests | `testcontainers` | Launches ephemeral Docker containers from the test suite (lifecycle + readiness). GHC 9.6-compatible, actively maintained. |
| Cloud emulation (tests) | `ministack` · Pub/Sub emulator | AWS via `ministack` (image `ministackorg/ministack`, port 4566, SQS/STS); GCP via Google's official Pub/Sub emulator. Both run as containers through `testcontainers` — no real cloud or credentials. |
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
- Mirroring to raw object storage (S3 / GCS). The mirror target is a registry and
  writes go through `publishArtifact`, so no blob-store seam is introduced;
  revisit only if a non-registry mirror target is ever wanted.
- Web UI or admin API.
- PyPI and other non-npm **adapters** — the hosting model and `RegistryClient`
  seam are designed to accommodate them (see
  [Multi-Ecosystem Hosting](#multi-ecosystem-hosting)), but only the npm adapter
  ships at launch.
- Cloud IAM validation at the proxy edge (gateway concern).
- Local on-disk caching of artifacts (the mirror retry window is acceptable).
- **GCP backends at launch** — the cloud seams (mirror queue, managed-registry
  token) are designed for GCP from day one, but shipping a GCP backend is gated on
  the client-viability spike; AWS ships first (see
  [Cloud Backends](#cloud-backends)).
