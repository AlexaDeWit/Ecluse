# Access & Credential Model

> Part of the [Écluse architecture overview](../architecture.md).

Écluse sits in the read path of someone else's build, so three concerns that look
like one — "auth" — are kept deliberately apart:

- **Edge authentication** — *who is calling the proxy?*
- **Authorisation (retrievability)** — *which packages may this caller retrieve?*
- **Credential supply** — *what bearer token does each upstream require on the wire?*

The simplest design fuses all three into a single act: forward the caller's
credential to the private upstream and let the upstream answer all three, on every
request. That is correct and builds no auth machinery, but it forbids sharing the
private origin of the [metadata cache](web-layer.md#metadata-cache) across callers
([issue #115](https://github.com/AlexaDeWit/Ecluse/issues/115)) and pins every read
to a per-request upstream round-trip. Separating the concerns turns "how Écluse
handles credentials" into a **per-mount credential strategy** — a universally-safe
default plus two opt-in strategies that recover caching.

**Écluse never builds an authentication system.** Authorisation is always
*delegated* — to the upstream (its native authority) or to the deployment edge
(network / service mesh / gateway). The strategies differ only in *where* authority
sits and *what may be cached as a result*.

## The four-corner trade-off

Four properties are all desirable; **a strategy can hold at most three:**

- **Delegate authority** — the upstream stays the authority for retrievability;
  nothing to build.
- **Shareable cache** — one fetch + parse + merge serves many callers.
- **No client-credential state** — Écluse retains nothing derived from a caller's
  credential beyond the request that bears it.
- **No per-request round-trip** — a cache hit is served without consulting an
  upstream (lowest latency; tolerant of an upstream blip).

| Strategy | Delegate authority | Shareable cache | No client-cred state | No per-request round-trip |
|---|:--:|:--:|:--:|:--:|
| **`passthrough`** (default) | ✓ | ✗ | ✓ | ✗ |
| **`delegated-cache`** | ✓ | ✓ | ✓ | ✗ |
| **`service`** | edge | ✓ | ✓ | ✓ |
| _`memoised` (deferred)_ | ✓ | ✓ | ✗ | ✓ |

`passthrough` gives up two corners — it is the simplest and the only one safe under
*any* upstream authorisation model. Each other strategy gives up exactly one. Which
to choose is a property of the **operator's environment** — chiefly their upstream's
authorisation model — not something Écluse can settle for everyone, so it is
configured per mount with `passthrough` as the floor.

## Credential strategies (per mount)

### `passthrough` — the default, universally safe

Écluse **forwards the caller's own credential** to the private upstream, which
authorises each request; the public upstream is queried anonymously with the
caller's credential stripped. The private origin is **fetched per request and never
entered into the shared cache** (see [Caching](#caching), and
[the private upstream's metadata is not cached across clients](registry-model.md#the-private-upstreams-metadata-is-not-cached-across-clients-under-passthrough)).
This is what the [walking skeleton](../../planning/delivery-plan.md) ships, and it
is correct regardless of whether the upstream's read authorisation is coarse or
fine-grained — the upstream re-decides every request.

### `delegated-cache` — the upstream decides retrievability; Écluse caches the compute

The defining move is on the **read** side: the expensive compute — the merged,
filtered packument, or the verified artifact bytes — is held in the **shared** cache,
and **before any cache hit is served the request is authorised against the upstream
with a cheap probe** (e.g. an authenticated `whoami`/`HEAD` that succeeds iff the
caller may read the mount). The upstream therefore remains the authority for *who may
retrieve what*, while the costly fetch + parse + merge is reused across callers.

This holds **no client-credential state** (the probe forwards the caller's credential
transiently, exactly as `passthrough` does) and costs a per-request probe — which
must be **cheaper than the fetch it replaces**, and is only available where the
upstream offers such a probe. The probe's **granularity must match the upstream's**
(see [Authorisation granularity](#authorisation-granularity)); a probe coarser than
the upstream's authorisation would over-grant, and the proxy must not.

**How the shared entry is *populated* is orthogonal to this** — an operational
choice, not a safety one, because the per-request probe (not the fetch's provenance)
is what authorises each serve (see [Caching](#caching)). The compute may be
**caller-populated** — filled lazily by the first authorised caller's own forwarded
token, holding **no Écluse read credential at all** — or **service-populated** —
filled with Écluse's own [`CredentialProvider`](cloud-backends.md#credential-provider)
token, which costs a read credential but lets Écluse warm or refresh an entry
proactively rather than waiting for an authorised caller. Either way the bytes are the
same canonical document, so either way the probe gates retrievability identically.

### `service` — the edge authenticates; Écluse brokers

The caller is authenticated at the **edge** (network, mesh, or a gateway — see
[Edge authentication](#edge-authentication)); Écluse then reads **all** upstreams
with its **own workload identity** via the [`CredentialProvider`](cloud-backends.md#credential-provider).
Everyone who passes the edge sees the same view, so the cache is fully shared, and
Écluse holds the **smallest credential surface of any strategy** — one short-lived,
least-privilege service token, and **no caller credentials at all**. The trade is
that the **edge, not the upstream, is the authority** for who may use the proxy;
selecting `service` is an explicit operator assertion to that effect.

### `memoised` — deferred, documented for completeness

A fourth point caches the upstream's authorisation **verdict**, keyed by a hash of
the caller's credential, to drop the per-request round-trip *without* a service
credential. It is **not in the shipping set**: it holds credential-derived state (a
honeypot to threat-model) and serves within a self-chosen revalidation window
(revocation latency), and no upstream exposes a token TTL we could lean on. Recorded
so the design space is explicit; it would require an explicit opt-in that names the
trade.

## Publishing: the publication target (passthrough write)

The strategies above govern **reads**. The one client-driven **write** path —
`npm publish` to the
[publication target](registry-model.md#publishing-first-party-packages-the-publication-target) —
uses **passthrough**, symmetric with the `passthrough` read of the private upstream:
Écluse forwards the **publisher's own** `Authorization` / `_authToken` to the
publication target, which authorises the publisher. Écluse substitutes no identity and
mints no token of its own for this path — unlike the **mirror target** write, which is
always Écluse's own `CredentialProvider` token.

The universal invariant holds: the client's token reaches only the private upstream (on
read, under `passthrough`) and the publication target (on publish) — **never** the
public upstream. Before any forward, the publish path enforces the **publish scope
allow-list** (the anti-shadowing guard): a name outside the operator's configured scopes
is refused with no upstream write attempted. The forwarded credential is also **never
carried across a redirect** — like every credential-bearing request, the publish relay
disables redirect-following, so a `3xx` from the publication target returns to the client
rather than chasing the credential to the `Location` (see
[Security → a credential-bearing request never follows a redirect](security.md#egress-scope-what-the-outbound-controls-guard-and-what-they-do-not)).

> ⚠️ **The publish surface authorises *names*, not *callers* — protect it.** The scope
> allow-list limits **which package names** a publish may target; it is **not**
> authentication. If a static `PUBLICATION_TARGET_TOKEN` is configured **and** the edge is
> open (no `PROXY_AUTH_TOKEN`), **any unauthenticated client can publish** under the
> operator's credential within the allowed scopes. Écluse does **not** block this — it
> cannot see environment-level protections (gateway, mesh/mTLS, network policy) — so
> protecting the publish surface (with `PROXY_AUTH_TOKEN` **or** an external layer) is an
> **operator-architecture responsibility** (see
> [Security → the first-party publish surface must be protected](security.md#the-first-party-publish-surface-must-be-protected-a-shared-responsibility)).

## Edge authentication

The npm client authenticates to a registry with an **opaque bearer** in `.npmrc`
(`//host/:_authToken=`) or via `npm login` — it does **not** speak SigV4, per-request
mTLS, or interactive OIDC. So edge authentication must terminate into a storable
bearer, or be handled by infrastructure in front of Écluse. The modes:

1. **Open** — no app-level check; access is gated entirely at the network layer
   (VPC, mesh). Appropriate on a closed network.
2. **Static token** — `PROXY_AUTH_TOKEN`; the caller presents it as `Bearer` /
   `_authToken`. Standard npm tooling supports it directly.
3. **Trusted edge identity** — a fronting authenticating proxy / cloud IAP / service
   mesh performs SSO or mTLS and asserts a verified identity (a signed header / mTLS
   SAN) that Écluse trusts. Sound **only** if Écluse is reachable *exclusively*
   through that edge — otherwise the assertion is spoofable (a network invariant the
   deployment must hold).

Validating **cloud IAM at the npm edge** is out: the npm client cannot speak it (it
remains a gateway concern). Richer per-user token issuance (`npm login` web SSO, CI
OIDC exchange) is a possible future, not a launch item, and would be ecosystem-specific
where the strategies above are not.

## Authorisation granularity

The available strategies depend on the upstream's authorisation granularity:

- **Coarse (repo-level)** — a valid token ⇒ the caller may read the whole mount. GCP
  Artifact Registry is always repo-level; AWS CodeArtifact is, in the common case
  (repo-scoped IAM and a repo-scoped authorisation token). A **per-mount** probe
  suffices for `delegated-cache`.
- **Fine (per-package)** — different callers may read different packages (e.g.
  CodeArtifact resource policies). `delegated-cache` then needs a **per-resource**
  probe before serving a hit — more probes, but it still caches the expensive
  compute. A probe coarser than the upstream's authorisation would over-grant.

`passthrough` is safe under either, because the upstream re-decides every request.

## Safe defaults and unrepresentable unsafe combinations

- **The default is `passthrough`.** A correct deployment needs nothing else.
- **A shared private-origin cache under `passthrough` is forbidden by construction**,
  not merely documented against: the metadata cache admits a private entry **only**
  under `service` / `delegated-cache`. This makes the
  [#115](https://github.com/AlexaDeWit/Ecluse/issues/115) cross-client disclosure
  hazard *unrepresentable* rather than a discipline.
- **`service` requires an explicit "the edge authorises callers" assertion** in
  config — it is the one strategy that moves authority off the upstream.
- Unknown or contradictory strategy configuration **fails fast at startup**,
  consistent with [config validation](configuration.md#validation-fail-fast-reject-the-unknown).

## Caching

What makes a shared private cache safe is **two invariants**; the strategy only
decides how the second is met:

1. **The cache stores no credential-derived state.** A cache key carries **no
   credential dimension** (it is the upstream base URL plus the package), and a cache
   value is the canonical document — never a credential or a credential-derived
   verdict. (This invariant is exactly what rules `memoised` out of the shipping set.)
2. **A shared *private* entry is served only after freshly authorising *that*
   caller.** No caller ever receives a private document without that request being
   authorised first.

Given those, the **serve-time authorisation method** — not how the bytes were
populated — is what governs sharing of the private origin of the
[metadata cache](web-layer.md#metadata-cache):

- **`passthrough`** — serve-time authorisation *is* the per-request fetch with the
  caller's token, so there is no shared private entry at all; only the anonymous
  public (gated) origin is cached. (This *is* the resolution of #115 in the default:
  there is no shared private entry to leak.)
- **`delegated-cache`** — the private origin is **shared**, and every hit is gated by a
  fresh per-request authorisation **probe** with the caller's token; the upstream
  re-decides retrievability on each serve.
- **`service`** — the edge has already authorised the caller, so a shared private
  entry is served with no per-request upstream check.

**Population is orthogonal to all of this.** Because invariant 2 authorises *every*
serve, it does not matter whether a shared entry was filled by a caller's own fetch
or by Écluse's service identity — no caller is served bytes they have not just been
authorised for. Population is therefore an operational choice (lazy/caller-populated,
holding no read credential, versus proactively warmed/service-populated), not a
security boundary.

This rests on one precondition: the cached document is **identity-independent in
content** — a packument is canonical *per package*, and artifact bytes are
content-addressed by `dist.integrity`, so authorisation governs *whether* a caller
may read an entry, never *what* its bytes are. The probe gates **retrievability**,
not **content**. (Were an upstream ever to return identity-specific *content* rather
than a plain allow/deny, population would re-enter the safety picture; npm upstreams
do not.)

## Credential supply: the `CredentialProvider`, generalised

The [`CredentialProvider`](cloud-backends.md#credential-provider) handle mints and
refreshes a bearer for **any upstream endpoint that requires one** — the
mirror-target **write** always, and the private-upstream **read** under `service`
(always) and a **service-populated** `delegated-cache`. `passthrough` reads — and a
**caller-populated** `delegated-cache` — use the forwarded caller token and no read
provider. A mount that needs a private-upstream read credential configures a **read**
provider in addition to its mirror-target one; both are the same handle, the same
refresh/single-flight/breaker policy, differing only in the per-cloud `mintToken`
leaf.

## Multi-instance is an isolation tool, not an authorisation mechanism

Running separate Écluse instances per tenant (each a flat `service`/edge deployment)
is a legitimate **blast-radius / policy** isolation choice, orthogonal to the
credential strategy — but it is not a substitute for one, and it scales to **team**
granularity, never per-developer. Reach for it when you want hard isolation or a
distinct policy per tenant, not to avoid choosing a strategy.

## Universal invariants (every strategy)

- The caller's credential is **never** sent to the public upstream.
- Outbound fetches stay within the [security invariants](security.md): the host
  allowlist, internal-range block, identifier canonicalisation, and bounded responses.
- Public versions are **always** gated by the [rules engine](rules-engine.md);
  trusted private versions enter the
  [packument merge](registry-model.md#packument-merge-across-upstreams) unfiltered.
  A strategy changes *how the private origin is fetched and cached* — never *whether
  public versions are gated*.
