# Registry Model

> Part of the [Écluse architecture overview](../architecture.md).

## Registry roles

The proxy is configured with **four registry roles** — two reads and two writes. They
are distinct roles, configured separately, but several may map to the *same* physical
registry (the publication target and the mirror target are each, most commonly, the
private upstream):

| Role | Purpose |
|------|---------|
| **Private upstream** | Authoritative, already-vetted source. A **tarball** found here is served immediately, unfiltered. A **packument**'s versions are trusted and **merged** with the gated public set (see [Packument merge](#packument-merge-across-upstreams)) rather than short-circuiting the public fetch. |
| **Public upstream** | Source of versions not (yet) in the private upstream; rules are applied to everything from here. For a **tarball** it is the fallback on a private miss; for a **packument** it is fetched **alongside** the private upstream and merged in. |
| **Mirror target** | Where approved public packages are written after passing rules. May be the same registry as the private upstream (most common) or a different one (e.g. separate internal/public stores). |
| **Publication target** | Where **client-published first-party packages** are written (`npm publish` through the proxy). The write counterpart to the private read role; may be the same registry as the private upstream (so published packages are then readable via the private leg) or a different one. Distinct from the mirror target: *client*-driven first-party content vs *proxy*-driven approved-public content. See [Publishing first-party packages](#publishing-first-party-packages-the-publication-target). |

### Credential flow and authority

How reads are credentialled is a **per-mount choice** — the
[credential strategy](access-model.md), which separates *who is calling* (edge
authentication), *what they may retrieve* (authorisation), and *what token an
upstream wants on the wire* (credential supply). The strategies are detailed in the
[Access & Credential Model](access-model.md); the per-role behaviour is:

- **Private upstream (read)** — depends on the mount's strategy. Under the default
  **`passthrough`**, Écluse **forwards the client's own credential**
  (`Authorization` / `_authToken`) verbatim and the upstream authorises each request
  (Écluse substitutes no identity). Under **`service`**, Écluse reads with its **own**
  [`CredentialProvider`](cloud-backends.md#credential-provider) token and authority
  moves to the edge. Under **`delegated-cache`** the upstream stays the authority via
  a cheap per-request probe, and the shared entry is filled either by the caller's
  forwarded token or by Écluse's own token — an orthogonal population choice (see
  [credential strategies](access-model.md#credential-strategies-per-mount)).
- **Public upstream (read/fallback)** — queried **anonymously** under every strategy.
  The client's credential is **never** forwarded here; sending an internal token to
  the public registry would be a credential disclosure. (If a public mirror itself
  needs auth, that is Écluse's *own* configured credential — never the client's.)
- **Mirror target (write)** — always Écluse's own credential: the
  [`CredentialProvider`](cloud-backends.md#credential-provider) mints the token to
  publish approved packages. Often the same registry as the private upstream — so its
  URL **defaults to `PRIVATE_UPSTREAM_URL`** when unset — but a different identity on
  it: the *client* reads it, *Écluse* writes it, and the write credential is selected
  explicitly (it does not fold with the URL).
- **Publication target (write)** — the **client's own forwarded credential**
  (`passthrough`): a `npm publish` is relayed to the publication target, which
  authorises the publisher. Symmetric with the private-upstream read under
  `passthrough` — Écluse substitutes no identity and mints no token of its own here
  (unlike the mirror target). The client's publish token is forwarded **only** to the
  publication target.

The non-negotiable invariant, under **every** strategy: **the client's credential is
never sent to the public upstream.** (Whether it reaches the private upstream is
strategy-specific — it does under `passthrough`, not under `service`.)

#### The private upstream's metadata is not cached across clients (under `passthrough`)

Under the default **`passthrough`** strategy the private upstream is the
**per-client authority** for who may read what, so its packument metadata is **not
cached across clients**: it is re-consulted on **every request**, with that client's
**own** forwarded credential, so the upstream re-authorises each client itself. Only
the **anonymous public (gated) origin** is held in the
[metadata cache](web-layer.md#metadata-cache). (The **`service`** and
**`delegated-cache`** strategies *do* share the private origin — safely, because the
bytes are identity-independent and each serve is freshly authorised: the edge under
`service`, a per-request probe under `delegated-cache`. How the shared entry is
populated is an orthogonal choice; see
[Access & Credential Model → Caching](access-model.md#caching).)

The reason is a cross-client disclosure hazard. The cache key carries **no
credential dimension** (it is the upstream base URL plus the package — a credential
is never part of a cache key). So if the private origin were cached, one client could
warm an entry for `@org/secret` and, within the TTL, a differently-scoped or
unauthorised client would get a cache **hit** — served the first client's private
document, their own token never validated upstream. Caching the private origin would
therefore **bypass the upstream's per-client authorisation**. The public origin has no
such hazard: it is fetched anonymously, so one shared entry serves every client
without crossing any trust boundary — there is nothing per-client to preserve.

Outbound requests are further constrained by the
[security invariants](security.md): an **outbound host allowlist**,
**internal-range blocking**, **identifier canonicalisation**, and **bounded
responses**, so a crafted identifier or a hostile upstream can neither steer a
fetch to an unintended target nor exhaust the proxy (issue #11).

## Publishing first-party packages (the publication target)

The roles above are read-plus-mirror; the **publication target** adds the one
client-driven *write* path. A client's `npm publish` (`PUT /{pkg}`) is accepted at the
mount and relayed to the publication target, so the proxy mediates the publish the same
way it mediates reads — rather than forcing first-party publishers into a separate,
out-of-band flow.

- **What it writes.** First-party / internal packages the client publishes — the write
  counterpart to the private read role. Contrast the mirror target, which the *proxy*
  writes with *approved public* packages after the rules gate. Same
  `RegistryClient.publishArtifact` primitive; different trigger, content, and credential.
- **Anti-shadowing guard (the load-bearing control).** A publish is **refused** unless
  its package name falls within the operator's configured **publish scope allow-list**
  (the MVP mechanism — e.g. `@acme/*`). This is what stops a client publishing a name
  that shadows an existing public package — a dependency-confusion vector the proxy must
  not enable. (Future work may add richer name grammars or live collision resolution;
  the allow-list is the MVP.)
- **Credential — passthrough.** The publisher's own token is forwarded to the
  publication target (see [Credential flow and authority](#credential-flow-and-authority));
  Écluse authorises nothing itself and mints no token here.
- **No read-back role.** The publication target is **write-only** from the proxy's view.
  Published packages are read back through the **private upstream** — so to serve what
  was published, the operator configures the publication target to be the *same
  registry* as the private upstream (or has the private upstream aggregate it). This
  keeps the read model at two sources.
- **Opt-in.** The path exists only when `PUBLICATION_TARGET_URL` is configured; with no
  publication target a `PUT /{pkg}` is rejected with **`405 Method Not Allowed`**.

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
the degenerate identity. It is realised as a lawful **`Monoid`**: the identity is
the empty merge (zero inputs), `<>` is the trusted-wins union with
order-independent divergence detection, and the fold is `foldMap` over each input's
contribution. The fold is **associative and identity-respecting but intentionally
not commutative** — each survivor is labelled with the *position* of the input that
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
  versions are **gated** — the rules engine filters them (see
  [Applying verdicts to a packument](rules-engine.md#applying-verdicts-to-a-packument))
  — before they enter. The merged packument is deliberately **mixed-provenance**:
  `trusted(private) ∪ filtered(public)`.
- **Collision → private wins; divergence is a signal.** When the same version key
  appears in both upstreams, the private copy wins (it is the authority). But if
  the public copy **contradicts the private one on a shared integrity algorithm**
  (an algorithm both carry whose digests disagree) for that version, that divergence
  is exactly the supply-chain tampering Écluse exists to catch: it is **detected,
  logged, and metered** (and may fail-closed on that version), never silently
  reconciled. An **asymmetric** digest set — one upstream also carrying a legacy
  digest the other omits, with no disagreement on any *shared* algorithm — describes
  the same bytes and is **not** a divergence: a weak digest agreeing never suppresses
  a contradicting strong one, and a strong digest agreeing makes the asymmetric weak
  one irrelevant.
- **A below-floor public version is inadmissible (admission, not merge).** Divergence
  detection compares a version's integrity fingerprint across upstreams, so a public
  version whose strongest digest is too weak (or absent) is a blind spot: two
  differing-byte copies that carry no digest, or only a collision-broken one, can
  fingerprint-collide so a divergence goes undetected. Écluse resolves this **at
  admission, not in the merge**: a public version whose strongest digest does not meet
  the configurable **integrity floor** (`PROXY_MIN_PUBLIC_INTEGRITY`, default SHA-256)
  is **refused before it reaches the merge** — the artifact gate `403`s it
  (`MissingIntegrity` for no digest, `BelowIntegrityFloor` for a too-weak one) and the
  gated set drops it from the served listing — so it never contributes a weak
  fingerprint and a client never sees a version it could not safely verify. With the
  floor enforced at admission, a below-floor public version never reaches the merge, so
  the cross-upstream divergence reasons over fingerprints that are each anchored on a
  strong digest; the **private-weak / public-strong** cross-check on a shared weak
  digest stays valid because the public copy independently cleared the floor. The
  **trusted private upstream is exempt** (its versions enter unfiltered, so a SHA-1-only
  private version is still served). This is
  [security invariant 5](security.md#invariants).
- **Reconcile over the union.** `dist-tags.latest` follows the **keep-unless-denied,
  stable-preferring** rule (see
  [Applying verdicts to a packument](rules-engine.md#applying-verdicts-to-a-packument)):
  kept as the precedence-winning source published it when it survives, else
  repointed to the highest stable survivor. Tags pointing at an absent version are
  dropped; `time` is the union restricted to surviving versions but **retains
  non-version bookkeeping keys** (`created`/`modified`). Precedence — for versions,
  tags, and `time` alike — is resolved **by provenance (trusted wins), not input
  order**. The cross-field coherence this requires (every `dist-tags` target is a
  present `versions` key) is an invariant held by tests, not the type.
- **Partial-upstream availability.** If one upstream fails while another succeeds,
  the merge serves the **best-effort union** of what resolved, with a degraded
  signal — readiness stays
  [lenient about public-upstream reachability](web-layer.md#meta-routes-ping-health-and-search).
  Only when *nothing* resolves does the request error (per the
  [serve error model](web-layer.md#error-model)).

**Where the merge lives.** Above the [`RegistryClient`](#registry-abstraction)
handle, as a **pure, ecosystem-agnostic** operation over `PackageInfo` — *not* inside
an adapter. The handle stays single-registry (`fetchMetadata` fetches one registry;
`parsePackageInfo` parses one document); the core fans out across the configured
upstreams, parses each, and folds the results. So a new ecosystem's adapter does
not re-implement merging, and the merge is unit-tested over hand-built
`PackageInfo` with no network. The merged document is something **no single
upstream produces** — Écluse authors it — which is why its served schema is owned
(see [API Surface](api-surface.md#the-synthesized-packument-schema--the-trust-boundary)).

### The route name is the served name's validation authority

The proxy always knows the requested package name from the **route**, so an upstream
packument's self-reported top-level `name` is at most a **cross-check**, never the
served authority. The route name is the authority for **validation, not rewriting**:
the served packument's `name` is always a value an upstream *genuinely reported*
(which, having passed validation, equals the route name) — never a substituted,
manufactured, or empty value the proxy invented.

The check is applied **per origin, at the serve boundary**, as the upstream packument is
projected:

- If an origin's self-reported `name` **agrees** with the route name, its
  contribution is admitted into the merge as normal (trusted-private unfiltered,
  gated-public rule-filtered).
- If an origin's self-reported `name` **disagrees**, that origin is treated as
  **untrusted for this request**: its contribution is **dropped from the merge** —
  degraded exactly like the existing undecodable-packument path — and the mismatch is
  **logged** (a katip warning carrying the requested name, the upstream's reported
  name, and the origin). An *absent* or otherwise undecodable name remains an
  undecodable-packument degrade, as before; only a *present-but-different* name is a
  mismatch.

This preserves **graceful degradation**: a single misreporting upstream just drops
out, and any *other* origin that returned a valid packument for the route name still
serves `200`. A bad upstream never denies a package another upstream serves.

When **no** origin yields a valid packument *because the responding origins
mismatched*, the request is a **`502 Bad Gateway`** — a responding upstream returned
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

- The **typed `PackageInfo`** is the *decision* surface — which versions survive,
  which source wins a collision, what `latest` resolves to, which integrity
  divergences exist.
- The **raw `Value`** is the *served* surface. Those decisions are computed on the
  typed model but **applied structurally to the raw bytes** — denied versions
  removed, tarball URLs rewritten, `latest` repointed — so every unmodeled wire key
  is relayed unchanged. The served body is **never re-serialised from the lossy
  typed model** (see
  [API Surface → synthesized-packument schema](api-surface.md#the-synthesized-packument-schema--the-trust-boundary)).

So the merge is a **producer→consumer pipeline** across slices: the adapter filters
and rewrites a single public packument (deciding over `PackageInfo`, editing the
`Value`); the ecosystem-agnostic core merges; the serve layer applies the outcome
to the raw `Value`(s). The merge stage therefore emits a **merge plan** — the
surviving set, the per-version precedence winner, the resolved `latest`, and the
detected divergences — that the serve layer **replays onto the raw bytes**, rather
than a finished typed document the serve layer would have to re-encode (which would
drop unmodeled fields). Each stage carries its raw `Value` alongside the typed view
so losslessness survives the whole pipeline.

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
The served name is therefore always one an upstream genuinely reported — never
substituted.

Nothing above the registry layer imports registry-specific types. The proxy core
operates only on `PackageInfo` (the packument-level view) and `PackageDetails`
(the per-version snapshot the rules engine evaluates — see
[`src/Ecluse/Package.hs`](../../src/Ecluse/Package.hs)). A registry
adapter is responsible for projecting its wire format into these types.

**Supported implementations at launch:** npm registry protocol only. The
`RegistryClient` abstraction exists from day one to make future backends
(PyPI, RubyGems, …) additive rather than structural changes.

`RegistryClient` is the **ecosystem (protocol) handle** — fetch, publish, and parse
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
