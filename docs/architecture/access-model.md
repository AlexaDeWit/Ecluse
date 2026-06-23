# Access & Credential Model

> Part of the [Écluse architecture overview](../architecture.md).

Écluse sits in the read path of someone else's build, so three concerns that look
like one — "auth" — are kept deliberately apart:

- **Edge authentication** — *who is calling the proxy?*
- **Authorization (retrievability)** — *which packages may this caller retrieve?*
- **Credential supply** — *what bearer token does each upstream require on the wire?*

The simplest design fuses all three into a single act: forward the caller's
credential to the private upstream and let the upstream answer all three, on every
request. That is correct and builds no auth machinery, but it forbids sharing the
private leg of the [metadata cache](web-layer.md#metadata-cache) across callers
([issue #115](https://github.com/AlexaDeWit/Ecluse/issues/115)) and pins every read
to a per-request upstream round-trip. Separating the concerns turns "how Écluse
handles credentials" into a **per-mount credential strategy** — a universally-safe
default plus two opt-in strategies that recover caching.

**Écluse never builds an authentication system.** Authorization is always
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
| _`memoized` (deferred)_ | ✓ | ✓ | ✗ | ✓ |

`passthrough` gives up two corners — it is the simplest and the only one safe under
*any* upstream authorization model. Each other strategy gives up exactly one. Which
to choose is a property of the **operator's environment** — chiefly their upstream's
authorization model — not something Écluse can settle for everyone, so it is
configured per mount with `passthrough` as the floor.

## Credential strategies (per mount)

### `passthrough` — the default, universally safe

Écluse **forwards the caller's own credential** to the private upstream, which
authorizes each request; the public upstream is queried anonymously with the
caller's credential stripped. The private leg is **fetched per request and never
entered into the shared cache** (see [Caching](#caching), and
[the private upstream's metadata is not cached across clients](registry-model.md#the-private-upstreams-metadata-is-not-cached-across-clients-under-passthrough)).
This is what the [walking skeleton](../../planning/delivery-plan.md) ships, and it
is correct regardless of whether the upstream's read authorization is coarse or
fine-grained — the upstream re-decides every request.

### `delegated-cache` — the upstream decides retrievability; Écluse caches the compute

The expensive compute — the merged, filtered packument, or the verified artifact
bytes — is produced once with a **service credential** and held in the **shared**
cache. Before any cache hit is served, the request is **authorized against the
upstream with a cheap probe** (e.g. an authenticated `whoami`/`HEAD` that succeeds
iff the caller may read the mount). The upstream therefore remains the authority for
*who may retrieve what*, while the costly fetch + parse + merge is reused across
callers.

This holds **no client-credential state** (the probe forwards the caller's
credential transiently, exactly as `passthrough` does) and costs a per-request
probe — which must be **cheaper than the fetch it replaces**, and is only available
where the upstream offers such a probe. The probe's **granularity must match the
upstream's** (see [Authorization granularity](#authorization-granularity)); a probe
coarser than the upstream's authorization would over-grant, and the proxy must not.

### `service` — the edge authenticates; Écluse brokers

The caller is authenticated at the **edge** (network, mesh, or a gateway — see
[Edge authentication](#edge-authentication)); Écluse then reads **all** upstreams
with its **own workload identity** via the [`CredentialProvider`](cloud-backends.md#credential-provider).
Everyone who passes the edge sees the same view, so the cache is fully shared, and
Écluse holds the **smallest credential surface of any strategy** — one short-lived,
least-privilege service token, and **no caller credentials at all**. The trade is
that the **edge, not the upstream, is the authority** for who may use the proxy;
selecting `service` is an explicit operator assertion to that effect.

### `memoized` — deferred, documented for completeness

A fourth point caches the upstream's authorization **verdict**, keyed by a hash of
the caller's credential, to drop the per-request round-trip *without* a service
credential. It is **not in the shipping set**: it holds credential-derived state (a
honeypot to threat-model) and serves within a self-chosen revalidation window
(revocation latency), and no upstream exposes a token TTL we could lean on. Recorded
so the design space is explicit; it would require an explicit opt-in that names the
trade.

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

## Authorization granularity

The available strategies depend on the upstream's authorization granularity:

- **Coarse (repo-level)** — a valid token ⇒ the caller may read the whole mount. GCP
  Artifact Registry is always repo-level; AWS CodeArtifact is, in the common case
  (repo-scoped IAM and a repo-scoped authorization token). A **per-mount** probe
  suffices for `delegated-cache`.
- **Fine (per-package)** — different callers may read different packages (e.g.
  CodeArtifact resource policies). `delegated-cache` then needs a **per-resource**
  probe before serving a hit — more probes, but it still caches the expensive
  compute. A probe coarser than the upstream's authorization would over-grant.

`passthrough` is safe under either, because the upstream re-decides every request.

## Safe defaults and unrepresentable unsafe combinations

- **The default is `passthrough`.** A correct deployment needs nothing else.
- **A shared private-leg cache under `passthrough` is forbidden by construction**,
  not merely documented against: the metadata cache admits a private entry **only**
  under `service` / `delegated-cache`. This makes the
  [#115](https://github.com/AlexaDeWit/Ecluse/issues/115) cross-client disclosure
  hazard *unrepresentable* rather than a discipline.
- **`service` requires an explicit "the edge authorizes callers" assertion** in
  config — it is the one strategy that moves authority off the upstream.
- Unknown or contradictory strategy configuration **fails fast at startup**,
  consistent with [config validation](configuration.md#validation-fail-fast-reject-the-unknown).

## Caching

The strategy is exactly what determines whether the private leg of the
[metadata cache](web-layer.md#metadata-cache) is shareable:

- **`passthrough`** — the private leg is per-caller and **not shared**; only the
  anonymous public (gated) leg is cached. (This *is* the resolution of #115 in the
  default: there is no shared private entry to leak.)
- **`service`** — the private leg is fetched with one identity, so it is
  identity-independent and **shared freely**.
- **`delegated-cache`** — the private leg is service-fetched and **shared**, but
  every hit is gated by a fresh per-request authorization probe.

A cache key never carries a credential dimension under any strategy; sharing is made
safe by *how the entry was fetched and authorized*, not by keying on the caller.

## Credential supply: the `CredentialProvider`, generalized

The [`CredentialProvider`](cloud-backends.md#credential-provider) handle mints and
refreshes a bearer for **any upstream endpoint that requires one** — the
mirror-target **write** always, and the private-upstream **read** under `service` /
`delegated-cache`. `passthrough` reads use the forwarded caller token and no
provider. A mount may therefore configure a **read** credential provider for its
private upstream in addition to its mirror-target one; both are the same handle, the
same refresh/single-flight/breaker policy, differing only in the per-cloud
`mintToken` leaf.

## Multi-instance is an isolation tool, not an authorization mechanism

Running separate Écluse instances per tenant (each a flat `service`/edge deployment)
is a legitimate **blast-radius / policy** isolation choice, orthogonal to the
credential strategy — but it is not a substitute for one, and it scales to **team**
granularity, never per-developer. Reach for it when you want hard isolation or a
distinct policy per tenant, not to avoid choosing a strategy.

## Universal invariants (every strategy)

- The caller's credential is **never** sent to the public upstream.
- Outbound fetches stay within the [security invariants](security.md): the host
  allowlist, internal-range block, identifier canonicalization, and bounded responses.
- Public versions are **always** gated by the [rules engine](rules-engine.md);
  trusted private versions enter the
  [packument merge](registry-model.md#packument-merge-across-upstreams) unfiltered.
  A strategy changes *how the private leg is fetched and cached* — never *whether
  public versions are gated*.
