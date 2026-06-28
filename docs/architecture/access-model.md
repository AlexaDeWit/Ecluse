# Access & Credential Model

> Part of the [Écluse architecture overview](../architecture.md).

Écluse sits in the read path of someone else's build, so three concerns that look
like one — "auth" — are kept deliberately apart:

- **Edge authentication** — *who is calling the proxy?*
- **Authorisation (retrievability)** — *which packages may this caller retrieve?*
- **Credential supply** — *what bearer token does each upstream require on the wire?*

The simplest design fuses all three into a single act: forward the caller's
credential to the private upstream and let the upstream answer all three, on every
request. That is correct and builds no auth machinery; its only costs are a
per-request upstream round-trip and no sharing of the private origin across callers.
**Écluse accepts both costs by design** — it is a thin network broker, efficient
caching is already the upstreams' job, and the gain from a shared *private* cache does
not justify the re-authorisation machinery it would demand (see
[Why Écluse never caches the private origin](#why-écluse-never-caches-the-private-origin)).
Separating the
concerns turns "how Écluse handles credentials" into a **per-mount credential
strategy**, but the strategies differ only in *where authority sits and whose
credential reaches the upstream*, never in what may be cached.

**Écluse never builds an authentication system.** Authorisation is always
*delegated* — to the upstream (its native authority) or to the deployment edge
(network / service mesh / gateway). The strategies differ only in *where* authority
sits and *what may be cached as a result*.

## Why Écluse never caches the private origin

A shared cache of the *private* origin is tempting — one fetch + parse + merge could
serve many callers, but it is never safe for free. A cache key carries no credential
dimension, so a shared private entry can be served safely only if **either** every hit
is re-authorised against the upstream on each request (a per-request authorisation
**probe**) **or** authority is moved off the upstream to the deployment edge so that
everyone past the edge is entitled to the same view. Both buy cache-sharing with
standing **threat-mitigation overhead**: a probe whose granularity must exactly match
the upstream's (or it over-grants), or an edge that becomes the sole authority.

**Écluse declines that trade.** It is a thin network broker, and efficient caching of
package metadata and artifacts is already handled well by the upstreams it fronts, so
the private origin is read **per request** and **never entered into the shared cache**.
This keeps the cross-client
disclosure hazard (catalogued as
[threat #9](https://alexadewit.github.io/Ecluse/threat-model.html)) *unrepresentable by
construction* rather than fenced off by a probe, and keeps hot-path work minimal. Only the anonymous **public-gated** origin is cached
(one shared document, no per-caller authority to preserve).

The per-mount **credential strategy** therefore varies only in *where authority sits
and whose credential reaches the private upstream*, never in what may be cached. Two
strategies ship:

| Strategy | Authority | Caller credential forwarded | Private origin cached |
|---|:--:|:--:|:--:|
| **`passthrough`** (default) | upstream | yes (transiently) | no |
| **`service`** | edge | no | no |

`passthrough` is the floor — the simplest, and the only one safe under *any* upstream
authorisation model. `service` is an opt-in for deployments that authorise at the edge
and prefer Écluse forward no caller credentials at all. The cache-recovering designs
`delegated-cache` and `memoised` are **rejected**: both exist only to share the private
cache Écluse forbids (see [Rejected: the cache-recovering strategies](#rejected-the-cache-recovering-strategies)).

## Credential strategies (per mount)

### `passthrough` — the default, universally safe

Écluse **forwards the caller's own credential** to the private upstream, which
authorises each request; the public upstream is queried anonymously with the
caller's credential stripped. The private origin is **fetched per request and never
entered into the shared cache** (see [Caching](#caching), and
[the private upstream's metadata is never cached across clients](registry-model.md#the-private-upstreams-metadata-is-never-cached-across-clients)).
This is the proxy's default behaviour, and it
is correct regardless of whether the upstream's read authorisation is coarse or
fine-grained — the upstream re-decides every request.

### `service` — the edge authenticates; Écluse reads with its own identity

The caller is authenticated at the **edge** (network, mesh, or a gateway; see
[Edge authentication](#edge-authentication)); Écluse then reads the upstreams with its
**own workload identity** via the [`CredentialProvider`](cloud-backends.md#credential-provider),
forwarding **no caller credentials at all** — the smallest credential surface of any
strategy (one short-lived, least-privilege token). Like `passthrough`, the private
origin is read **per request and never shared-cached**; `service` differs only in
*whose* identity makes that read and *where* authority sits: the **edge, not the
upstream, is the authority** for who may use the proxy, so selecting `service` is an
explicit operator assertion to that effect. It is the choice for deployments that
authorise at the edge and do not want caller credentials flowing through the proxy at
all (sidestepping the credential-aggregation surface `passthrough` carries; see
[Security → trust assumptions & credential posture](security.md#trust-assumptions--credential-posture)).

### Rejected: the cache-recovering strategies

Two further designs were considered and **rejected**, because both exist only to share
the private origin across callers — the one thing Écluse
[forbids](#why-écluse-never-caches-the-private-origin):

- **`delegated-cache`** would hold the merged/filtered compute in the shared cache and
  gate each hit with a per-request authorisation **probe** against the upstream. Safe
  in principle, but it buys cache-sharing with standing overhead — a probe whose
  granularity must exactly match the upstream's, or it over-grants — that the broker
  model does not justify.
- **`memoised`** would cache the upstream's authorisation *verdict*, keyed by a hash of
  the caller's credential, to drop the round-trip without a service credential.
  Rejected outright: it holds **credential-derived state** (a honeypot to
  threat-model) and serves within a self-chosen revalidation window (revocation
  latency), with no upstream token TTL to lean on.

Reintroducing a shared private cache later would be a deliberate design change that
must first re-establish per-hit authorisation (the `delegated-cache` probe), never a
config toggle.

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

> ⚠️ **The publish surface authorises *names*, not *callers*.** The scope allow-list limits
> **which package names** a publish may target; it is **not** authentication. A static
> `PUBLICATION_TARGET_TOKEN` makes Écluse publish under its **own** credential, so it is
> **fail-closed**: configured without `PROXY_AUTH_TOKEN`, Écluse **refuses to boot**
> (`PublishStaticCredentialNeedsEdge`), making "static publish credential + open edge" —
> which would otherwise let **any unauthenticated client publish** under the operator's
> credential within the allowed scopes — unrepresentable. `PROXY_AUTH_TOKEN` is the
> verifiable edge Écluse checks itself; an external layer (gateway, mesh/mTLS, network
> policy) is defence-in-depth but does **not** satisfy this requirement. Pure passthrough
> (no static token) needs none of it — the publisher's own forwarded token is the authority
> (see [Security → a static publish credential is fail-closed](security.md#a-static-publish-credential-is-fail-closed)).

## Edge authentication

The npm client authenticates to a registry with an **opaque bearer** in `.npmrc`
(`//host/:_authToken=`) or via `npm login` — it does **not** speak SigV4, per-request
mTLS, or interactive OIDC. So edge authentication must terminate into a storable
bearer, or be handled by infrastructure in front of Écluse. The modes:

1. **Open**, no app-level check; access is gated entirely at the network layer
   (VPC, mesh). Appropriate on a closed network; the assumption this rests on is
   [threat #3](https://alexadewit.github.io/Ecluse/threat-model.html).
2. **Static token** — `PROXY_AUTH_TOKEN`; the caller presents it as `Bearer` /
   `_authToken`. Standard npm tooling supports it directly.
3. **Trusted edge identity** — a fronting authenticating proxy / cloud IAP / service
   mesh performs SSO or mTLS and asserts a verified identity (a signed header / mTLS
   SAN) that Écluse trusts. Écluse honours the assertion **only over a verifiable
   binding to that edge** — mutual TLS from the edge, or a shared secret / HMAC the
   edge signs the assertion with, and **fails fast** on a `trusted-edge` mount that
   configures neither (an [unrepresentable unsafe
   combination](#safe-defaults-and-unrepresentable-unsafe-combinations)). A *bare*
   trusted header is forgeable into **granted** access anywhere Écluse is reachable
   other than through the edge — strictly worse than `open`'s "no token, no access" —
   so the binding, not trust alone, is what makes the assertion unspoofable. Reaching
   Écluse *exclusively* through that edge remains the deployment's part, but is no
   longer the sole protection.

Validating **cloud IAM at the npm edge** is out: the npm client cannot speak it (it
remains a gateway concern). Richer per-user token issuance (`npm login` web SSO, CI
OIDC exchange) is a possible future, not a launch item, and would be ecosystem-specific
where the strategies above are not.

## Authorisation granularity

With no shared private cache, upstream authorisation granularity is **not Écluse's
concern**. Under `passthrough` the upstream re-decides every request against the
caller's own token — coarse (repo-level, e.g. GCP Artifact Registry, or repo-scoped
CodeArtifact) or fine (per-package, e.g. CodeArtifact resource policies), it just
works. Under `service` the **edge** is the per-caller authority and the upstream sees a
single workload identity. The granularity-matching burden only arises for a per-hit
probe over a shared private cache — the `delegated-cache` design Écluse
[rejects](#rejected-the-cache-recovering-strategies).

## Safe defaults and unrepresentable unsafe combinations

- **The default is `passthrough`.** A correct deployment needs nothing else.
- **A shared private-origin cache is forbidden by construction**, not merely documented
  against: under **no** strategy does the metadata cache admit a private entry — only
  the anonymous public-gated origin is cached, and the private origin is read per
  request. This makes the cross-client disclosure
  hazard *unrepresentable* rather than a discipline.
- **`service` requires an explicit "the edge authorises callers" assertion** in
  config — it is the strategy that moves authority off the upstream.
- **`trusted-edge` requires a verifiable edge binding** (mutual TLS from the edge, or
  a shared secret / HMAC on the asserted identity); a `trusted-edge` mount configured
  with **neither** is **rejected at startup**, not merely discouraged — a bare trusted
  header is forgeable into granted access wherever Écluse is reachable other than
  through the edge, so the unsafe combination is made unrepresentable rather than left
  to a network hope.
- Unknown or contradictory strategy configuration **fails fast at startup**,
  consistent with [config validation](configuration.md#validation-fail-fast-reject-the-unknown).

## Caching

Écluse caches **only the anonymous public-gated origin** of the
[metadata cache](web-layer.md#metadata-cache) — one shared document per package, with
no per-caller authority to preserve. The **private origin is never entered into the
shared cache, under any strategy**; it is read per request:

- under **`passthrough`**, with the caller's own forwarded token (the upstream
  re-authorises each read);
- under **`service`**, with Écluse's own workload identity (the edge has already
  authorised the caller).

This removes the cross-client disclosure hazard:
there is no shared private entry to leak across callers — by construction, not by
discipline. On the **tarball leg** the per-request read is the credentialed
[conventional read](registry-model.md#serving-a-tarball-a-conventional-private-read-an-honoured-public-location)
of the artifact itself, no packument round-trip, so the private upstream (under
`passthrough`) or the service identity (under `service`) authorises each artifact read.

The one cache that exists — the anonymous public origin — stores **no
credential-derived state**: its key carries no credential dimension (the upstream base
URL plus the package), and its value is the canonical public document, never a
credential or a credential-derived verdict.

## Credential supply: the `CredentialProvider`, generalised

The [`CredentialProvider`](cloud-backends.md#credential-provider) handle mints and
refreshes a bearer for **any upstream endpoint that requires one** — the
mirror-target **write** always, and the private-upstream **read** under `service`.
`passthrough` reads use the forwarded caller token and need no read provider. A
`service` mount therefore configures a **read** provider in addition to its
mirror-target one; both are the same handle, the same refresh/single-flight/breaker
policy, differing only in the per-cloud `mintToken` leaf.

## Multi-instance is an isolation tool, not an authorisation mechanism

Running separate Écluse instances per tenant (each a flat `service`/edge deployment)
is a legitimate **blast-radius / policy** isolation choice, orthogonal to the
credential strategy, but it is not a substitute for one, and it scales to **team**
granularity, never per-developer. Reach for it when you want hard isolation or a
distinct policy per tenant, not to avoid choosing a strategy.

## Universal invariants (every strategy)

- The caller's credential is **never** sent to the public upstream.
- Outbound fetches stay within the [security invariants](security.md): the host
  allowlist, internal-range block, identifier canonicalisation, and bounded responses.
- Public versions are **always** gated by the [rules engine](rules-engine.md);
  trusted private versions enter the
  [packument merge](registry-model.md#packument-merge-across-upstreams) unfiltered.
  A strategy changes *whose credential fetches the private origin*, never *whether it
  is cached* (it never is) or *whether public versions are gated*.
