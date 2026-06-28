# Registry Model

> Part of the [Écluse architecture overview](../architecture.md).

## Registry roles

The proxy is configured with **four registry roles**, two reads and two writes. They
are distinct roles, configured separately. Several *may* map to the same physical
registry, collapsing them onto one store is the simplest setup, but the
**recommended** topology keeps the first-party and public-derived stores separate and
unions them at the registry level (see [Registry-level
composition](#registry-level-composition-the-recommended-topology)). A single shared
registry is the degenerate floor, not the goal:

| Role | Purpose |
|------|---------|
| **Private upstream** | Authoritative, already-vetted source. A **tarball** is served by a **conventional stable read** at `{base}/{pkg}/-/{file}`, no packument fetch, no serve-time integrity floor (see [Serving a tarball](#serving-a-tarball-a-conventional-private-read-an-honoured-public-location)). A **packument**'s versions are trusted and **merged** with the gated public set (see [Packument merge](#packument-merge-across-upstreams)) rather than short-circuiting the public fetch. |
| **Public upstream** | Source of versions not (yet) in the private upstream; rules are applied to everything from here. For a **tarball** it is the fallback on a private miss; for a **packument** it is fetched **alongside** the private upstream and merged in. |
| **Mirror target** | Where approved public packages are written after passing rules. May be the same registry as the private upstream (the simplest, degenerate setup), but is **recommended** to be a distinct store unioned into the private-upstream read path at the registry level, so public-derived inventory stays separable from first-party. |
| **Publication target** | Where **client-published first-party packages** are written (`npm publish` through the proxy). The write counterpart to the private read role; may be the same registry as the private upstream, but is **recommended** to be a distinct first-party store unioned into the private-upstream read path (so first-party content stays separable from approved-public). Distinct from the mirror target: *client*-driven first-party content vs *proxy*-driven approved-public content. See [Publishing first-party packages](#publishing-first-party-packages-the-publication-target). |

### Credential flow and authority

How reads are credentialled is a **per-mount choice**, the
[credential strategy](access-model.md), which separates *who is calling* (edge
authentication), *what they may retrieve* (authorisation), and *what token an
upstream wants on the wire* (credential supply). The strategies are detailed in the
[Access & Credential Model](access-model.md); the per-role behaviour is:

- **Private upstream (read)**, depends on the mount's strategy. Under the default
  **`passthrough`**, Écluse **forwards the client's own credential**
  (`Authorization` / `_authToken`) verbatim and the upstream authorises each request
  (Écluse substitutes no identity). Under **`service`**, Écluse reads with its **own**
  [`CredentialProvider`](cloud-backends.md#credential-provider) token and authority
  moves to the edge, still **per-request and uncached**, like `passthrough` (see
  [credential strategies](access-model.md#credential-strategies-per-mount)).
- **Public upstream (read/fallback)**, queried **anonymously** under every strategy.
  The client's credential is **never** forwarded here; sending an internal token to
  the public registry would be a credential disclosure. (If a public mirror itself
  needs auth, that is Écluse's *own* configured credential, never the client's.)
- **Mirror target (write)**, always Écluse's own credential: the
  [`CredentialProvider`](cloud-backends.md#credential-provider) mints the token to
  publish approved packages. Often the same registry as the private upstream, so its
  URL **defaults to `PRIVATE_UPSTREAM_URL`** when unset, but a different identity on
  it: the *client* reads it, *Écluse* writes it, and the write credential is selected
  explicitly (it does not fold with the URL).
- **Publication target (write)**, the **client's own forwarded credential**
  (`passthrough`): a `npm publish` is relayed to the publication target, which
  authorises the publisher. Symmetric with the private-upstream read under
  `passthrough`, Écluse substitutes no identity and mints no token of its own here
  (unlike the mirror target). The client's publish token is forwarded **only** to the
  publication target.

The non-negotiable invariant, under **every** strategy: **the client's credential is
never sent to the public upstream.** (Whether it reaches the private upstream is
strategy-specific, it does under `passthrough`, not under `service`.)

#### The private upstream's metadata is never cached across clients

The private upstream is the **per-client authority** for who may read what, so its
packument metadata is **never cached across clients** under any strategy: it is
re-consulted on **every request**, with the client's **own** forwarded credential
under `passthrough`, or Écluse's own identity behind the edge under `service`, so the
read is freshly authorised each time. Only the **anonymous public (gated) origin** is
held in the [metadata cache](web-layer.md#metadata-cache). Écluse
[forbids a shared private cache](access-model.md#why-écluse-never-caches-the-private-origin)
outright, it is a thin broker and leaves caching to the upstreams, so no strategy
shares the private origin.

The reason is a **cross-client disclosure hazard**, a credential-blind cache key would
let one client warm a private entry that a differently-authorised client then gets as a
hit, bypassing the upstream's per-client authorisation. The full mechanics live in
[access model → why Écluse never caches the private origin](access-model.md#why-écluse-never-caches-the-private-origin),
and it is catalogued as [threat #9](https://alexadewit.github.io/Ecluse/threat-model.html).
The public origin has no such hazard: it is fetched anonymously, so one shared entry
serves every client with nothing per-client to preserve.

Outbound requests are further constrained by the
[security invariants](security.md): an **outbound host allowlist**,
**internal-range blocking**, **identifier canonicalisation**, and **bounded
responses**, so a crafted identifier or a hostile upstream can neither steer a
fetch to an unintended target nor exhaust the proxy.

## Publishing first-party packages (the publication target)

The roles above are read-plus-mirror; the **publication target** adds the one
client-driven *write* path. A client's `npm publish` (`PUT /{pkg}`) is accepted at the
mount and relayed to the publication target, so the proxy mediates the publish the same
way it mediates reads, rather than forcing first-party publishers into a separate,
out-of-band flow.

- **What it writes.** First-party / internal packages the client publishes, the write
  counterpart to the private read role. Contrast the mirror target, which the *proxy*
  writes with *approved public* packages after the rules gate. Same
  `RegistryClient.publishArtifact` primitive; different trigger, content, and credential.
- **Anti-shadowing guard (the load-bearing control).** A publish is **refused** unless
  its package name falls within the operator's configured **publish scope allow-list**
  (the MVP mechanism; e.g. `@acme/*`). This is what stops a client publishing a name
  that shadows an existing public package, a dependency-confusion vector the proxy must
  not enable. (Future work may add richer name grammars or live collision resolution;
  the allow-list is the MVP.) The guard holds the **guard-name ≡ write-name ≡ body-name**
  invariant: the scope check keys on the **URL-path** name, and, because the npm publish
  document carries its own declared identity (`_id`, top-level `name`, every
  `versions[].name`) that a publication target may key the write off, the body's
  **declared names are validated too**. Any present declared name that disagrees with the
  URL-path name is a **`403` before any relay**, compared with the same name
  canonicalisation the route applies (`PackageName` equality, ecosystem-aware, never a
  byte-for-byte string compare), so a crafted body cannot publish a name the allow-list
  never authorised. An **absent** declared name is no claim and is not refused (a
  legitimate npm client always sends matching names); only the names are read, the
  base64 `_attachments` are never decoded.
- **Credential, passthrough.** The publisher's own token is forwarded to the
  publication target (see [Credential flow and authority](#credential-flow-and-authority));
  Écluse authorises nothing itself and mints no token here.
- **No read-back role.** The publication target is **write-only** from the proxy's view.
  Published packages are read back through the **private upstream**, so to serve what
  was published, the operator configures the publication target to be the *same
  registry* as the private upstream (or has the private upstream aggregate it). This
  keeps the read model at two sources.
- **Opt-in.** The path exists only when `PUBLICATION_TARGET_URL` is configured; with no
  publication target a `PUT /{pkg}` is rejected with **`405 Method Not Allowed`**.

## Serving a tarball: a conventional private read, an honoured public location

A tarball is one concrete version from one source, so, unlike a packument, a
private-upstream hit is streamed straight through and we are done. The two serve legs
locate the bytes differently, by the trust of their origin.

The **private leg is a conventional stable read.** It fetches the tarball directly at
`{private-base}/{pkg}/-/{file}` by the client's requested filename, the same stable,
cacheable URL an `npm ci` install issues, **without first fetching the private
packument**. On a worst-case lockfile fan-out this is the hot-path win: a tarball request
pays one artifact round-trip, not a per-tarball private-packument fetch+decode it would
only discard. The client's credential is forwarded (the `passthrough` posture), so the
private upstream still authorises each artifact read; the request is built with
redirect-following disabled, so the forwarded credential never follows a `3xx`
([credential-redirect invariant](security.md#egress-scope-what-the-outbound-controls-guard-and-what-they-do-not)).
A `2xx` streams the bytes through; a non-`2xx` or a connection failure is a clean
**private miss** that falls through to the public leg.

The private leg applies **no serve-time integrity floor.** An established version already
pinned in a consumer's lockfile and served from an operator-**trusted** private registry
is fast-tracked: its bytes are still verified **client-side by npm** (against the
`dist.integrity` it resolved over the packument route, unchanged) and by the **mirror
worker** on ingestion, so fast-tracking gives up only the proactive "refuse
weak-integrity" stance, not tamper-evidence. The packument route's listing-side trusted
floor ([invariant 5](security.md#invariants)) is unchanged; an operator who wants the
floor back on the tarball leg uses the opt-in metadata-resolution mode.

One **accepted limitation** rides with the conventional read: a **nonstandard** private
upstream that serves its tarball **off-convention**, a separate files host, a CDN or
presigned URL the `/-/` path cannot rebuild, is **not reached** by the conventional URL,
so it becomes a private miss that falls through to the public origin. Restoring such an
upstream is the opt-in metadata-resolution mode.

The **public leg** instead honours the **authoritative upstream location**, the
`dist.tarball` the gated version declares, fetched at exactly that URL rather than a
reconstructed `/-/` path, so Écluse can front a public registry that serves artifacts
from a separate host (the PyPI-files-host shape) or a signed CDN URL. That location is
gated, not trusted: the tarball-host policy and the resolved-IP recheck bound *where* it
may be fetched (see [Why `dist.tarball` is honoured](security.md#why-disttarball-is-honoured-and-what-bounds-it)).

## Packument merge across upstreams

A **packument** (package metadata) is not served by first-hit short-circuit the
way a tarball is. A tarball is one concrete version from one source, so a
private-upstream hit is streamed and we are done. A packument is the *set of
available versions*, and that set is spread across upstreams: the private upstream
holds what has been vetted/mirrored, the public upstream holds the full history,including **new versions not yet mirrored**. Serving only the private packument
would hide those new versions, so a client never requests them, so **demand-driven
mirroring never fires for them** (see
[Cloud Backends → Mirror Queue](cloud-backends.md#mirror-queue)). The packument is
therefore **merged**, not short-circuited.

**The merge is a fold over upstream packuments**, with the single-source case as
the degenerate identity. It is realised as a lawful **`Monoid`**: the identity is
the empty merge (zero inputs), `<>` is the trusted-wins union with
order-independent divergence detection, and the fold is `foldMap` over each input's
contribution. The fold is **associative and identity-respecting but intentionally
not commutative**, each survivor is labelled with the *position* of the input that
won it, so the serve layer can index back to the right raw `Value`, and swapping
inputs swaps those labels. Every *decision* the merge owns is order-independent
(see the precedence rule below); only the positional labels track input order. The
detected divergences are a **set**: a version key's distinct integrity fingerprints
are compared and a divergence is flagged when two copies **contradict on a shared
digest algorithm** (one both carry, whose digests disagree), a property of the set of
fingerprints offered rather than of any one fold step, so it is order-independent and
deduplicating.

- **Fetch upstreams in parallel.** For a packument, the private and public
  upstreams are fetched concurrently (the credential rules above still hold: the
  private fetch follows the mount's [credential strategy](access-model.md), the
  public fetch is always anonymous).
- **Trust split by provenance.** Private-upstream versions are **trusted** and
  enter the merged document **unfiltered** (already vetted). Public-upstream
  versions are **gated**, the rules engine filters them (see
  [Applying verdicts to a packument](rules-engine.md#applying-verdicts-to-a-packument))
 , before they enter. The merged packument is deliberately **mixed-provenance**:
  `trusted(private) ∪ filtered(public)`.
- **Collision → private wins; divergence is a signal.** When the same version key
  appears in both upstreams, the private copy wins (it is the authority). But if
  the public copy **contradicts the private one on a shared integrity algorithm**
  (an algorithm both carry whose digests disagree) for that version, that divergence
  is exactly the supply-chain tampering Écluse exists to catch
  ([threat #11](https://alexadewit.github.io/Ecluse/threat-model.html)): it is
  **detected, logged, and metered** (and may fail-closed on that version), never
  silently reconciled. The algorithm compared is the one each digest **asserts**, an SRI is
  resolved to its embedded algorithm, never bucketed under an opaque `SRI` tag, so the
  same algorithm expressed as a hex digest or as an SRI is cross-checked **together**,
  while two *different* algorithms over the same bytes (e.g. a recomputing mirror's
  `sha256` vs npm's `sha512`) form an asymmetric set, **not** a divergence. An
  **asymmetric** digest set, one upstream also carrying a digest the other omits, with no
  disagreement on any *shared* algorithm, describes the same bytes and is **not** a
  divergence: a weak digest agreeing never suppresses a contradicting strong one, and a
  strong digest agreeing makes the asymmetric weak one irrelevant.
- **A below-floor version is inadmissible (admission, not merge), by default in both
  trust contexts.** Divergence detection compares a version's integrity fingerprint across
  upstreams, so a version whose strongest digest is too weak (or absent) is a blind spot:
  two differing-byte copies that carry no digest, or only a collision-broken one, can
  fingerprint-collide so a divergence goes undetected. Écluse resolves this **at admission,
  not in the merge**, and **by default in both contexts**: a version whose strongest digest
  does not meet its **integrity floor** is **refused before it reaches the merge**, the
  served listing drops it (in both trust contexts) and, on the **public** artifact path,
  the gate `403`s it, as `MissingIntegrity` (no digest) or `BelowIntegrityFloor` (a
  too-weak one). The **private tarball serve leg** is the exception: it is a
  [conventional stable read](#serving-a-tarball-a-conventional-private-read-an-honoured-public-location)
  that skips the packument and applies **no serve-time floor**, so a below-floor private
  *artifact* is still served from the private origin (the listing-side trusted floor on
  the packument route is unchanged; the bytes stay client- and worker-verified, and the
  opt-in metadata-resolution mode restores the floor here). The **public
  floor** (`PROXY_MIN_PUBLIC_INTEGRITY`, default SHA-256) is **hard-floored** and never
  lowerable; the **trusted floor** (`PROXY_MIN_TRUSTED_INTEGRITY`, default SHA-256) shares
  that default but is **operator-loosenable below SHA-256** for a legacy private mirror.
  With the floors enforced at admission, the cross-upstream divergence reasons over
  fingerprints each anchored on a strong digest **by default**; the **private-weak /
  public-strong** cross-check on a shared weak digest arises **only when an operator
  explicitly loosens the trusted floor** below SHA-256 (the public copy still independently
  meets its own hard floor on its strong digest). This is
  [security invariant 5](security.md#invariants).
- **Reconcile over the union.** `dist-tags.latest` follows the **keep-unless-denied,
  stable-preferring** rule (see
  [Applying verdicts to a packument](rules-engine.md#applying-verdicts-to-a-packument)):
  kept as the precedence-winning source published it when it survives, else
  repointed to the highest stable survivor. Tags pointing at an absent version are
  dropped; `time` is the union restricted to surviving versions but **retains
  non-version bookkeeping keys** (`created`/`modified`). Precedence, for versions,
  tags, and `time` alike, is resolved **by provenance (trusted wins), not input
  order**. The cross-field coherence this requires (every `dist-tags` target is a
  present `versions` key) is an invariant held by tests, not the type.
- **Partial-upstream availability.** If one upstream fails while another succeeds,
  the merge serves the **best-effort union** of what resolved, with a degraded
  signal, readiness stays
  [lenient about public-upstream reachability](web-layer.md#meta-routes-ping-health-and-search).
  Only when *nothing* resolves does the request error (per the
  [serve error model](web-layer.md#error-model)).

**Where the merge lives.** Above the [`RegistryClient`](#registry-abstraction)
handle, as a **pure, ecosystem-agnostic** operation over `PackageInfo`, *not* inside
an adapter. The handle stays single-registry (`fetchMetadata` fetches one registry;
`parsePackageInfo` parses one document); the core fans out across the configured
upstreams, parses each, and folds the results. So a new ecosystem's adapter does
not re-implement merging, and the merge is unit-tested over hand-built
`PackageInfo` with no network. The merged document is something **no single
upstream produces**, Écluse authors it, which is why its served schema is owned
(see [API Surface](api-surface.md#the-synthesized-packument-schema--the-trust-boundary)).

### The route name is the served name's validation authority

The proxy always knows the requested package name from the **route**, so an upstream
packument's self-reported top-level `name` is at most a **cross-check**, never the
served authority. The route name is the authority for **validation, not rewriting**:
the served packument's `name` is always a value an upstream *genuinely reported*
(which, having passed validation, equals the route name), never a substituted,
manufactured, or empty value the proxy invented.

The check is applied **per origin, at the serve boundary**, as the upstream packument is
projected:

- If an origin's self-reported `name` **agrees** with the route name, its
  contribution is admitted into the merge as normal (trusted-private unfiltered,
  gated-public rule-filtered).
- If an origin's self-reported `name` **disagrees**, that origin is treated as
  **untrusted for this request**: its contribution is **dropped from the merge**,  degraded exactly like the existing undecodable-packument path, and the mismatch is
  **logged** (a katip warning carrying the requested name, the upstream's reported
  name, and the origin). An *absent* or otherwise undecodable name remains an
  undecodable-packument degrade, as before; only a *present-but-different* name is a
  mismatch.

This preserves **graceful degradation**: a single misreporting upstream just drops
out, and any *other* origin that returned a valid packument for the route name still
serves `200`. A bad upstream never denies a package another upstream serves.

When **no** origin yields a valid packument *because the responding origins
mismatched*, the request is a **`502 Bad Gateway`**, a responding upstream returned
an invalid response. This is deliberately **distinct from a genuine absence** (no such
package at all, which keeps its existing status): a mismatch is "upstream returned an
invalid response", not "package not found". The status surface is the
`PackumentBadGateway` variant of `packumentStatus` (see
[Web Layer → Error model](web-layer.md#error-model)).

This also forecloses a cache-poisoning-adjacent hazard: because the served name can
only ever be a name an upstream genuinely reported for the requested route, a
misreporting upstream can neither shadow a real package under the served name nor have
its divergent `name` chosen over the correct one in the cross-upstream union.

### Decision surface vs served surface

The merge reasons over the **typed** `PackageInfo` domain model, but the document
Écluse **serves** is the raw upstream JSON (`Value`), edited in place. These are two
distinct surfaces, and the boundary between them is load-bearing:

- The **typed `PackageInfo`** is the *decision* surface, which versions survive,
  which source wins a collision, what `latest` resolves to, which integrity
  divergences exist.
- The **raw `Value`** is the *served* surface. Those decisions are computed on the
  typed model but **applied structurally to the raw bytes**, denied versions
  removed, tarball URLs rewritten, `latest` repointed, so every unmodeled wire key
  is relayed unchanged. The served body is **never re-serialised from the lossy
  typed model** (see
  [API Surface → synthesized-packument schema](api-surface.md#the-synthesized-packument-schema--the-trust-boundary)).

So the merge is a **producer→consumer pipeline** across slices: the adapter filters
and rewrites a single public packument (deciding over `PackageInfo`, editing the
`Value`); the ecosystem-agnostic core merges; the serve layer applies the outcome
to the raw `Value`(s). The merge stage therefore emits a **merge plan**, the
surviving set, the per-version precedence winner, the resolved `latest`, and the
detected divergences, that the serve layer **replays onto the raw bytes**, rather
than a finished typed document the serve layer would have to re-encode (which would
drop unmodeled fields). Each stage carries its raw `Value` alongside the typed view
so losslessness survives the whole pipeline.

### Graceful degradation: per-version, not per-package

Écluse is a resilience proxy: a hostile or malformed upstream must not be able to
take a healthy package **offline**. Decoding the upstream packument into the
decision surface is therefore deliberately **lenient at the version granularity**,
with a clear fail-closed boundary:

- **Advisory fields degrade, the version survives.** A version's non-rule-decisive,
  non-serving-decisive `dist` sub-fields (`unpackedSize`, `fileCount`,
  `signatures`) are decoded leniently in the wire layer: a present-but-undecodable
  value (a fractional/huge/`Int`-overflowing number, a wrong-typed field, a
  malformed or non-array `signatures`) reads as absent/empty rather than failing
  the version.
- **A version broken in a required/security-decisive field is dropped.** If a
  version's manifest cannot be decoded in a load-bearing field (no `dist` or
  `tarball`, an unusable `version`), that **single version** is dropped from the
  decision surface. Because presence in the decision surface is what makes a
  version a serve-candidate, a dropped version is automatically excluded from the
  served body (`applyFilterPlan` restricts `versions`/`time` to the survivors). This
  is **fail-closed for that version**: a version that cannot be decoded cannot be
  evaluated for integrity, CVEs, or rules, so it is never served unverifiable,  while every healthy sibling version keeps serving.
- **The package is denied wholesale only if the top-level document is unusable.**
  A body that is not a JSON object, an absent/empty top-level `name`, or a
  `versions` that is not an object at all leaves nothing identifiable to serve, so
  it degrades exactly like the existing undecodable-packument path (and, for a
  *present-but-different* name, the name-mismatch degrade above).

This turns the general "one poisoned version denies the whole package"
denial-of-service class into a per-version drop. Per-version drops are currently
**silent**; surfacing them as telemetry (comparing the raw versus decoded version
count) is a noted follow-up.

### Registry-level composition (the recommended topology)

The **recommended** deployment keeps the first-party store and the public-derived
mirror store **physically separate** and unions them at the **registry** level into the
private-upstream read path; e.g. AWS CodeArtifact upstream relationships, where
`PRIVATE_UPSTREAM_URL` points at an aggregating repository that itself draws from a
mirror-target repo and a first-party "published-by-us" repo. The private upstream then
behaves as a read-only union of two trusted stores and returns the full trusted set in
one fetch, while each store stays independently governable, distinct storage-level
scanning and policy per provenance, and clean post-disclosure scoping. Managed
registries (CodeArtifact, Artifact Registry, …) provide exactly this aggregation
primitive; Écluse is designed to lean on it.

Composing at the registry level is the recommended way to get that separation, but it is
**not the only one**: Écluse's own merge gives the same *correctness* to operators who
cannot compose at the registry level, and collapsing the roles onto a single store
remains supported as the **degenerate floor**, it trades away auditability and
defence-in-depth, not the perimeter (register
[threat #10](https://alexadewit.github.io/Ecluse/threat-model.html)). What is **not**
optional is the rule below.

#### The one rule of registry composition: Écluse is the only path from public

Écluse exists to apply ingestion-time policy, freshness / time-gating, integrity
floors, and the rule algebra, that managed registries do not themselves provide. That
value holds only if **public packages enter your ecosystem through Écluse and nowhere
else.**

So the aggregating read endpoint (the private upstream) must union **trusted stores
only**, your first-party publications and Écluse's sanitized mirror, and must **not**
carry a direct upstream connection to the public registry. Such a connection would let
raw, ungated public packages reach clients through the trusted path, *behind* Écluse's
gate rather than *through* it, the one configuration that silently nullifies the
protection Écluse is there to provide. Écluse cannot detect this from the outside (the
private upstream is trusted by construction, and its upstream wiring is invisible to the
proxy), so keeping the internal registry disconnected from public is an
**operator-architecture invariant** (register
[threat #15](https://alexadewit.github.io/Ecluse/threat-model.html)). The public
upstream is always fetched and gated by Écluse itself.

## Registry Abstraction

The proxy core is registry-agnostic. The `RegistryClient` record is the sole
interface between the proxy logic and any specific registry protocol:

```haskell
data RegistryClient = RegistryClient
  { fetchMetadata    :: PackageName -> IO RegistryResponse
  , fetchArtifact    :: PackageName -> Version -> IO RegistryResponse
  , publishArtifact  :: PackageName -> Version -> ByteString -> IO (Either PublishError ())
  , parsePackageInfo :: PackageName -> RegistryResponse -> Either ParseError PackageInfo
  , parseVersionDetails :: RegistryResponse -> Version -> Either ParseError PackageDetails
  , parseVersionList :: RegistryResponse -> Either ParseError [Version]
  }
```

The effectful fields return plain `IO`, not `App`: an adapter closes over its own
state (HTTP manager, credentials) and never imports the proxy's `Env`/`App`, so
backends stay decoupled from the core. The `parse*` fields are pure. See
[Technology Stack → the effect model](technology-stack.md#key-decisions).

`parsePackageInfo` takes the **route-requested `PackageName`** as a validation input:
the proxy always knows it, so the adapter validates the upstream's self-reported name
against it rather than trusting the self-report (see
[The route name is the served name's validation authority](#the-route-name-is-the-served-names-validation-authority)).
The served name is therefore always one an upstream genuinely reported, never
substituted.

Nothing above the registry layer imports registry-specific types. The proxy core
operates only on `PackageInfo` (the packument-level view) and `PackageDetails`
(the per-version snapshot the rules engine evaluates; see
[`core/src/Ecluse/Core/Package.hs`](../../core/src/Ecluse/Core/Package.hs)). A registry
adapter is responsible for projecting its wire format into these types.

**Supported implementations at launch:** npm registry protocol only. The
`RegistryClient` abstraction exists from day one to make future backends
(PyPI, RubyGems, …) additive rather than structural changes.

`RegistryClient` is the **ecosystem (protocol) handle**, fetch, publish, and parse, nothing more. It deliberately does **not** carry authentication, because
protocol and auth are **orthogonal axes**: AWS **CodeArtifact**, GCP **Artifact
Registry**, and a self-hosted Verdaccio/Nexus all speak the *same* npm protocol
and differ only in how a bearer token is obtained. Folding "CodeArtifact-ness"
into the npm adapter would force a near-duplicate adapter per cloud; instead the
npm `RegistryClient` is used **unchanged** and paired with a
[`CredentialProvider`](cloud-backends.md#credential-provider) that mints the
token. The backend matrix is therefore *ecosystem × credential provider*, and the
cells compose freely (npm-on-CodeArtifact, npm-on-Artifact-Registry,
pypi-on-static, …). See [Cloud Backends](cloud-backends.md#cloud-backends).
