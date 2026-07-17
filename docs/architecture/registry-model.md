# Registry model

> Part of the [Écluse architecture overview](../architecture.md).

## Registry roles

The proxy is configured with **up to four registry roles**, two reads and two writes, configured
separately. Several may map to one physical registry; collapsing them is the simplest setup, but
the recommended topology keeps first-party and public-derived stores separate and unions them at
the registry level (see
[Registry-level composition](#registry-level-composition-the-recommended-topology)). A single
shared registry is the degenerate floor, not the goal. A **serve-only** mount (one that
declares no mirror target; see
[Configuration](configuration.md#configuration)) runs on the read roles alone: the mirror
target is absent, the private upstream optional (absent too on the pure public gate), and
the full rules gate applies unchanged; the trade is that every artifact stays on the gated
public leg rather than retiring onto the private read.

| Role | Purpose |
|------|---------|
| **Private upstream** | Authoritative, already-vetted source. A tarball is served by a conventional stable read at `{base}/{pkg}/-/{file}`, no packument fetch, no serve-time integrity floor (see [Serving a tarball](#serving-a-tarball-a-conventional-private-read-an-honoured-public-location)). A packument's versions are trusted and merged with the gated public set (see [Packument merge](#packument-merge-across-upstreams)). Optional on a serve-only mount. |
| **Public upstream** | Source of versions not yet in the private upstream; everything from here is rules-gated. For a tarball it is the fallback on a private miss; for a packument it is fetched alongside the private upstream and merged in. The only required role: the pure public gate serves from it alone. |
| **Mirror target** | Where approved public packages are written after passing rules. Declaring one is what makes a mount mirrored; absent, the mount is serve-only and never writes. May be the private upstream, but is recommended to be a distinct store unioned into the private-upstream read path, so public-derived inventory stays separable from first-party. |
| **Publication target** | Where client-published first-party packages are written (`npm publish` through the proxy). The write counterpart to the private read role; recommended to be a distinct first-party store unioned into the read path. Distinct from the mirror target: *client*-driven first-party content vs *proxy*-driven approved-public content. See [Publishing first-party packages](#publishing-first-party-packages-the-publication-target). |

### Credential flow and authority

How reads are credentialled is the mount's [credential strategy](access-model.md); the per-role
behaviour:

- **Private upstream (read)**: under the default `passthrough` Écluse forwards the client's own
  credential and the upstream authorises each request; under `service` Écluse reads with its own
  [`CredentialProvider`](cloud-backends.md#credential-provider) token and authority moves to the
  edge. Either way the read is per-request and uncached.
- **Public upstream (read/fallback)**: queried anonymously under every strategy; the client's
  credential is never forwarded here. If a public mirror itself needs auth, that is Écluse's own
  configured credential, never the client's.
- **Mirror target (write)**: always Écluse's own `CredentialProvider` token, derived from the
  mirror-target URL (see
  [Configuration](configuration.md#outbound-registry-credentials)). Often the same registry as
  the private upstream, declared under its own key even then (the write's destination is never
  implied from another endpoint, and declaring it is what makes the mount mirrored): the client
  reads it, Écluse writes it.
- **Publication target (write)**: the client's own forwarded credential (`passthrough`); Écluse
  substitutes no identity and mints no token here.

The non-negotiable invariant, under every strategy: the client's credential is never sent to the
public upstream.

#### The private upstream's metadata is never cached across clients

The private upstream is the per-client authority for who may read what, so its metadata is read
per request and never entered into the shared cache: a credential-blind cache key would let one
client warm a private entry a differently-authorised client then gets as a hit (the cross-client
disclosure hazard, [threat #9](https://ecluse-proxy.com/threat-model.html#threat-9)). Only the
anonymous public (gated) origin is cached. See
[access model → why Écluse never caches the private origin](access-model.md#why-écluse-never-caches-the-private-origin)
for the full argument.

Outbound requests are further constrained by the [security invariants](security.md): the host
allowlist, internal-range blocking, identifier canonicalisation, and bounded responses.

## Publishing first-party packages (the publication target)

The publication target adds the one client-driven write path. A client's `npm publish`
(`PUT /{pkg}`) is accepted at the mount and relayed to the publication target, so the proxy
mediates the publish the same way it mediates reads.

- **What it writes.** First-party / internal packages the client publishes, relayed as the
  client's own document with a different trigger,
  content, and credential from the mirror write (the mirror target is *proxy*-written with
  approved public packages through the worker's per-ecosystem publish capability).
- **Anti-shadowing guard (the load-bearing control).** A publish is refused unless its package
  name falls within the operator's configured publish allow-list (`publishAllow`, in the
  ecosystem's native form; for npm, scopes such as `@acme`). This stops a client publishing a name that shadows an existing public package, a
  dependency-confusion vector. The guard holds a guard-name ≡ write-name ≡ body-name invariant: the
  scope check keys on the URL-path name, and because the npm publish document carries its own
  declared identity (`_id`, top-level `name`, every `versions[].name`) that a target may key the
  write off, the body's declared names are validated too. Any present declared name disagreeing
  with the URL-path name is a `403` before any relay, compared under the same ecosystem-aware
  `PackageName` equality the route uses, so a crafted body cannot publish a name the allow-list
  never authorised. An absent declared name is no claim and is not refused; only names are read,
  the base64 `_attachments` are never decoded.
- **Credential.** Passthrough: the publisher's own token is forwarded (see
  [Credential flow and authority](#credential-flow-and-authority)); Écluse mints no token here.
- **No read-back role.** The publication target is write-only from the proxy's view. Published
  packages read back through the private upstream, so to serve what was published the operator
  configures the publication target to be the same registry as the private upstream (or has the
  private upstream aggregate it), keeping the read model at two sources.
- **Opt-in.** The path exists only when `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET` is configured;
  otherwise a `PUT /{pkg}` is rejected with `405 Method Not Allowed`.

## Serving a tarball: a conventional private read, an honoured public location

A tarball is one concrete version from one source, so a private-upstream hit is streamed straight
through. The two serve legs locate the bytes differently, by the trust of their origin.

The **private leg is a conventional stable read**: it fetches the tarball directly at
`{private-base}/{pkg}/-/{file}` by the client's requested filename, the same stable URL an `npm ci`
install issues, without first fetching the private packument. On a lockfile fan-out this is the
hot-path win: a tarball request pays one artifact round-trip, not a per-tarball packument
fetch+decode it would only discard. The client's credential is forwarded (`passthrough`), so the
private upstream authorises each read; redirect-following is disabled, so the forwarded credential
never follows a `3xx`
([credential-redirect invariant](security.md#egress-scope-what-the-outbound-controls-guard-and-what-they-do-not)).
A `2xx` streams the bytes; a non-`2xx` or connection failure is a clean private miss that falls
through to the public leg.

The private leg applies **no serve-time integrity floor**. A version already pinned in a
consumer's lockfile and served from an operator-trusted private registry is fast-tracked: its bytes
are still verified client-side by npm (against the `dist.integrity` resolved over the packument
route) and by the mirror worker on ingestion, so fast-tracking gives up only the proactive "refuse
weak-integrity" stance, not tamper-evidence. The packument route's listing-side trusted floor
([invariant 5](security.md#invariants)) is unchanged; an operator who wants the floor back on the
tarball leg uses the opt-in metadata-resolution mode. One accepted limitation rides with the
conventional read: a nonstandard private upstream that serves its tarball off-convention (a
separate files host, a CDN or presigned URL the `/-/` path cannot rebuild) is not reached by the
conventional URL, becoming a private miss that falls through to public; restoring it is the same
opt-in mode.

The **public leg** honours the authoritative upstream location, the `dist.tarball` the gated
version declares, fetched at exactly that URL rather than a reconstructed `/-/` path, so Écluse can
front a public registry serving artifacts from a separate host (the PyPI-files-host shape) or a
signed CDN URL. An ecosystem whose registry serves artifact bytes from a canonical separate host
__by design__ declares those hosts on its adapter, and the secure-default same-host policy admits
them (still allowlist- and internal-range-gated); the operator's tarball-host knob stays a policy
choice, never a hostname list. That location is gated, not trusted: the host allowlist and tarball-host policy
bound where it may be fetched, https-only egress with certificate validation authenticates the
host, and a legacy `http` tarball is upgraded (same host) or dropped (see
[Why `dist.tarball` is honoured](security.md#why-disttarball-is-honoured-and-what-bounds-it)).

## Packument merge across upstreams

A packument is the *set of available versions*, spread across upstreams: the private upstream holds
what has been vetted/mirrored, the public upstream holds the full history including new versions not
yet mirrored. Serving only the private packument would hide those new versions, so a client never
requests them, so demand-driven mirroring never fires for them (see
[Mirror Queue](cloud-backends.md#mirror-queue)). The packument is therefore merged, not
short-circuited.

The merge is a fold over upstream packuments, realised as a lawful `Monoid`: the identity is the
empty merge, `<>` is the trusted-wins union with order-independent divergence detection, and the
fold is `foldMap` over each input. It is associative and identity-respecting but intentionally
**not commutative**: each survivor is labelled with the *position* of the input that won it, so the
serve layer can index back to the right raw `Value`. Every decision the merge owns is
order-independent; only the positional labels track input order.

- **Fetch in parallel.** Private and public upstreams are fetched concurrently (the private fetch
  follows the mount's [credential strategy](access-model.md); the public fetch is anonymous).
- **Trust split by provenance.** Private versions are trusted and enter unfiltered; public versions
  are gated by the rules engine (see
  [Applying verdicts](rules-engine.md#applying-verdicts-to-a-packument)) before entering. The
  merged packument is deliberately mixed-provenance: `trusted(private) ∪ filtered(public)`.
- **Collision → private wins; divergence is a signal.** On a shared version key the private copy
  wins. But if the public copy contradicts the private one on a shared artifact's shared
  integrity algorithm (a file both carry, under an algorithm both assert for it, whose digests
  disagree), that is the supply-chain tampering Écluse exists to catch
  ([threat #11](https://ecluse-proxy.com/threat-model.html#threat-11)): detected, logged (a
  `WARNING` naming the package, the contradicting versions, and their digests) and metered
  (`ecluse.registry.merge.divergence`), never silently reconciled. Whether the contested version is
  additionally withheld from the served listing is the operator's `ECLUSE_INTEGRITY__DIVERGENCE_POLICY`:
  `warn` (the default) serves the trusted copy and relies on the alarm; `fail-closed` drops the
  contested version from the listing (dropping any `dist-tag`, including `latest`, that pointed at
  it). The algorithm compared
  is the one each digest asserts (an SRI is resolved to its embedded algorithm, not bucketed under
  an opaque `SRI` tag), so the same algorithm as hex or SRI is cross-checked together. An asymmetric
  set is not a divergence: one upstream carrying a digest the other omits, or a file the other
  does not serve (a multi-artifact ecosystem's mirror holding fewer files than the index),
  describes the same bytes with no disagreement on any shared key.
- **Below-floor versions are inadmissible (at admission, not merge).** A version whose strongest
  digest is too weak or absent is a divergence blind spot (two differing-byte copies can
  fingerprint-collide). Écluse refuses it before the merge: the served listing drops it in both
  trust contexts, and on the public artifact path the gate `403`s it as `MissingIntegrity` or
  `BelowIntegrityFloor`. The exception is the private tarball serve leg, a conventional stable read
  with no serve-time floor (its bytes stay client- and worker-verified). The floors
  (`ECLUSE_INTEGRITY__MIN_PUBLIC`, hard-floored at SHA-256; `ECLUSE_INTEGRITY__MIN_TRUSTED`,
  loosenable below it) are detailed under
  [Configuration](configuration.md#public-integrity-floor); this is
  [security invariant 5](security.md#invariants).
- **Reconcile over the union.** `dist-tags.latest` follows the keep-unless-denied, stable-preferring
  rule (see [Applying verdicts](rules-engine.md#applying-verdicts-to-a-packument)): kept as
  published when it survives, else repointed to the highest stable survivor. Tags pointing at an
  absent version are dropped; `time` is the union restricted to surviving versions but retains
  non-version bookkeeping keys (`created`/`modified`). Precedence is resolved by provenance (trusted
  wins), not input order. Cross-field coherence (every `dist-tags` target is a present `versions`
  key) is held by tests, not the type.
- **Partial availability.** If one upstream fails while another succeeds, the merge serves the
  best-effort union of what resolved, with a degraded signal (readiness stays
  [lenient about public reachability](web-layer.md#meta-routes-ping-health-and-search)). Only when
  nothing resolves does the request error (per the [serve error model](web-layer.md#error-model)).

The merge lives above the [protocol boundary](#registry-abstraction), as a pure,
ecosystem-agnostic operation over `PackageInfo`, not inside an adapter: each read handle stays
single-registry, and the core fans out across the configured upstreams, parses each, and folds the
results. So a new ecosystem does not re-implement merging, and the merge is unit-tested over
hand-built `PackageInfo` with no network. The merged document is one no single upstream produces,
which is why its served schema is
[owned](api-surface.md#the-synthesised-packument-schema--the-trust-boundary).

### The route name is the served name's validation authority

The proxy always knows the requested name from the route, so an upstream packument's self-reported
top-level `name` is at most a cross-check, never the served authority. The route name validates, it
does not rewrite: the served `name` is always a value an upstream genuinely reported (which, having
passed validation, equals the route name), never a value the proxy invented. The check is per
origin, at the serve boundary:

- An origin whose self-reported `name` **agrees** with the route name is admitted into the merge
  normally (trusted-private unfiltered, gated-public rule-filtered).
- An origin whose `name` **disagrees** is treated as untrusted for this request: its contribution
  is dropped from the merge, degraded like an undecodable packument, and the mismatch is logged (a
  katip warning with the requested name, the reported name, and the origin). An absent or
  undecodable name stays an undecodable-packument degrade; only a present-but-different name is a
  mismatch.

This preserves graceful degradation: a single misreporting upstream drops out while any other
origin returning a valid packument still serves `200`. When no origin yields a valid packument
*because the responding origins mismatched*, the request is a `502 Bad Gateway` (a responding
upstream returned an invalid response), the `PackumentBadGateway` variant of `packumentStatus` (see
[Web Layer → Error model](web-layer.md#error-model)), deliberately distinct from a genuine absence.
It also forecloses a cache-poisoning hazard: the served name can only ever be one an upstream
genuinely reported for the route, so a misreporting upstream can neither shadow a real package nor
win the cross-upstream union with a divergent `name`.

### Decision surface vs served surface

The merge reasons over the typed `PackageInfo` domain model, but the document Écluse serves is the
raw upstream JSON (`Value`), edited in place. The boundary is load-bearing:

- The typed `PackageInfo` is the **decision** surface: which versions survive, which source wins a
  collision, what `latest` resolves to, which integrity divergences exist.
- The raw `Value` is the **served** surface: those decisions are applied structurally to the raw
  bytes (only surviving versions taken, their tarball URLs rewritten in place, `latest` carried
  from the plan), so every unmodeled wire key relays unchanged. The served body is never
  re-serialised from the lossy typed model (see
  [API Surface](api-surface.md#the-synthesised-packument-schema--the-trust-boundary)).

So the merge is a producer→consumer pipeline: the rules and structural filter decide over the typed
model (emitting a merge plan, the surviving set, the per-version precedence winner, the resolved
`latest`, the detected divergences); the ecosystem-agnostic core merges; the adapter assembles the
outcome onto the raw `Value`(s) in one pass. Each stage carries its raw `Value` alongside the typed
view so losslessness survives the pipeline.

### Graceful degradation: per-version, not per-package

Écluse degrades gracefully: a hostile or malformed upstream must not take a healthy package
offline. Decoding the packument into the decision surface is deliberately lenient at version
granularity, with a clear fail-closed boundary:

- **Advisory fields degrade, the version survives.** A version's non-decisive `dist` sub-fields
  (`unpackedSize`, `fileCount`, `signatures`) decode leniently: a present-but-undecodable value
  reads as absent/empty rather than failing the version.
- **A version broken in a required/security-decisive field is dropped.** If a version cannot be
  decoded in a load-bearing field (no `dist` or `tarball`, an unusable `version`), that single
  version is dropped from the decision surface and so from the served body. This is fail-closed for
  that version: it cannot be evaluated for integrity, CVEs, or rules, so it is never served
  unverifiable, while every healthy sibling keeps serving.
- **The package is denied wholesale only if the top-level document is unusable.** A body that is not
  a JSON object, an absent/empty top-level `name`, or a non-object `versions` leaves nothing to
  serve and degrades like an undecodable packument.

This turns the "one poisoned version denies the whole package" class into a per-version drop.
Per-version drops are currently silent; surfacing them as telemetry is a noted follow-up.

### Registry-level composition (the recommended topology)

The recommended deployment keeps the first-party store and the public-derived mirror store
physically separate and unions them at the registry level into the private-upstream read path, e.g.
AWS CodeArtifact upstream relationships where `ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM` points at an
aggregating repository drawing from a mirror-target repo and a first-party repo. The private
upstream then behaves as a read-only union of two trusted stores, returning the full trusted set in
one fetch while each store stays independently governable (distinct storage-level scanning and
policy per provenance, clean post-disclosure scoping). Managed registries provide exactly this
aggregation primitive.

#### Traffic shape over time: the V, and why the public leg is transient

The topology is a **V**: Écluse fans a read to the public origin and to the private pull-through,
and the pull-through unions the mirror and first-party stores. The dynamic consequence is the
design intent: because every admitted public tarball is back-filled into the mirror by the worker,
and the mirror feeds the private read path, the private conventional read comes to serve nearly all
tarball traffic once a fleet has warmed. The public tarball leg is a transient, per-artifact
fail-over, the onboarding ramp a new package or version transits until the worker promotes it,
after which that artifact never takes the public leg again. So the public leg's throughput matters
for onboarding experience, not steady-state capacity, and optimisations must respect this ordering:
trading private-hit (hot-path) work to speed the public fail-over is a regression against the
design.

A **serve-only** mount opts out of the V's back-fill: with no mirror target there is no
worker promotion, so the public leg is permanent rather than transient. That is the openly
accepted trade of the low-effort onboarding shape: slower installs at scale, egress that
never retires, availability coupled to the public registry, and no mirrored copy surviving
an upstream yank; the security gate itself is identical. Declaring a `mirrorTarget` later
upgrades the mount in place; clients change nothing.

Registry-level composition is the recommended way to get that separation but not the only one:
Écluse's own merge gives the same correctness to operators who cannot compose at the registry level,
and collapsing the roles onto one store remains supported as the degenerate floor (it trades away
auditability and defence-in-depth, not the perimeter; register
[threat #10](https://ecluse-proxy.com/threat-model.html#threat-10)).

#### The one rule of registry composition: Écluse is the only path from public

Écluse exists to apply ingestion-time policy (freshness gating, integrity floors, the rule algebra)
that managed registries do not provide. That value holds only if public packages enter your
ecosystem through Écluse and nowhere else. So the aggregating read endpoint (the private upstream)
must union trusted stores only, your first-party publications and Écluse's sanitised mirror, and
must not carry a direct upstream connection to the public registry. Such a connection would let raw,
ungated public packages reach clients behind Écluse's gate rather than through it, the one
configuration that silently nullifies the protection. Écluse cannot detect this from the outside
(the private upstream is trusted by construction, its wiring invisible to the proxy), so keeping the
internal registry disconnected from public is an operator-architecture invariant (register
[threat #15](https://ecluse-proxy.com/threat-model.html#threat-15)).

## Registry abstraction

The proxy core is registry-agnostic. An ecosystem registers one capability record
(`RegistryAdapter`, resolved through the adapter registry at the composition root and nowhere
else), whose slices are the sole interface between the proxy logic and that registry's protocol:
the web-facing serve surface (path grammar and denial renderer), the metadata capability (the
read-handle constructor and packument assembly), the artifact request formation, and the publish
capability. The mirror write inside the publish capability splits along what genuinely varies per
ecosystem: the adapter contributes a **protocol codec** (`PublishCodec`: publish document assembly
and request formation, the presence probe's request and version-list projection, and the status
semantics), and the environment supplies a **shared publish transport** (the trusted-path
connection manager, the credential mint, the response bound, and the fault classification). The
composition root marries the two per mounted ecosystem into the `MirrorPublish` handle each worker
bundle carries, so a new ecosystem contributes protocol and never transport.

The effectful operations return plain `IO`, not `App`: an implementation closes over its own state
(HTTP manager, credentials) and never imports the proxy's `Env`/`App`, so backends stay decoupled
from the core. Each reports its failures as a typed value (`FetchFault` on a read, `PublishFault`
on the mirror write; both carry a transport arm classified at the boundary), so no fetch or publish
fault rides up as an exception and a caller's fall-through or retry-vs-drop decision is total at
the call site. The projections are pure. See
[Technology Stack → the effect model](technology-stack.md#key-decisions). The packument projection
takes the route-requested `PackageName` as a validation input, so the adapter validates the
upstream's self-reported name against it rather than trusting it (see
[route name validation](#the-route-name-is-the-served-names-validation-authority)).

Nothing above the registry layer imports registry-specific types: the proxy core operates only on
`PackageInfo` (the packument view) and `PackageDetails` (the per-version snapshot the rules engine
evaluates; see [`core/src/Ecluse/Core/Package.hs`](../../core/src/Ecluse/Core/Package.hs)), and an
adapter projects its wire format into these types. Only the npm registry protocol ships at launch;
the abstraction exists from day one to make future backends (PyPI, RubyGems) additive rather than
structural.

The protocol vocabulary deliberately carries no authentication, because protocol and auth are
orthogonal:
AWS CodeArtifact, GCP Artifact Registry, and a self-hosted Verdaccio/Nexus all speak the same npm
protocol and differ only in how a bearer token is obtained. Folding "CodeArtifact-ness" into the
npm
adapter would force a near-duplicate adapter per cloud; instead the npm protocol implementation is
used
unchanged and paired with a [`CredentialProvider`](cloud-backends.md#credential-provider) that
mints
the token. The backend matrix is therefore *ecosystem × credential provider*, composing freely
(npm-on-CodeArtifact, npm-on-Artifact-Registry, pypi-on-static). See
[Cloud Backends](cloud-backends.md#cloud-backends).
