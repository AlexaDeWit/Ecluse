# Access and credential model

> Part of the [Écluse architecture overview](../architecture.md).

Écluse sits in the read path of someone else's build, so it keeps three concerns apart:

- **Edge authentication**: who is calling the proxy?
- **Authorisation (retrievability)**: which packages may this caller retrieve?
- **Credential supply**: what bearer token does each upstream require on the wire?

The simplest design fuses all three: forward the caller's credential to the private
upstream and let the upstream answer them on every request. That is correct and builds no
auth machinery; its costs are a per-request upstream round-trip and no sharing of the
private origin across callers. Écluse accepts both by design. It is a thin network broker,
caching is the upstreams' job, and a shared *private* cache is never worth the
re-authorisation machinery it demands (below). Authorisation is always *delegated*, to the
upstream or to the deployment edge; Écluse never builds an authentication system.
Separating the concerns turns credential handling into a **per-mount credential strategy**
that varies only in *where authority sits and whose credential reaches the private
upstream*, never in what may be cached.

## Why Écluse never caches the private origin

A shared cache of the *private* origin is tempting (one fetch could serve many callers) but
never safe for free. A cache key carries no credential dimension, so a shared private entry
is safe only if **either** every hit is re-authorised against the upstream (a per-request
authorisation **probe**) **or** authority moves to the edge so everyone past it shares one
view. Both buy cache-sharing with standing overhead: a probe whose granularity must exactly
match the upstream's, or it over-grants; or an edge that becomes the sole authority.

Écluse declines the trade. The private origin is read **per request** and **never entered
into the shared cache**, so the cross-client disclosure hazard
([threat #9](https://ecluse-proxy.com/threat-model.html)) is unrepresentable by
construction rather than fenced off by a probe. Only the anonymous **public-gated** origin
is cached, one shared document with no per-caller authority to preserve. Reintroducing a
shared private cache later would be a deliberate design change that must first re-establish
per-hit authorisation, never a config toggle.

## Credential strategies (per mount)

Two strategies ship. Both read the private origin per request and never share-cache it;
they differ only in *whose* credential makes that read and *where* authority sits.

| Strategy | Authority | Caller credential forwarded | Private origin cached |
|---|:--:|:--:|:--:|
| **`passthrough`** (default) | upstream | yes (transiently) | no |
| **`service`** | edge | no | no |

- **`passthrough`** (default, universally safe). Écluse forwards the caller's own
  credential to the private upstream, which authorises each request; the public upstream is
  queried anonymously with that credential stripped. Correct whether the upstream's read
  authorisation is coarse or fine-grained, since the upstream re-decides every request. The
  only strategy safe under *any* upstream authorisation model.
- **`service`** (edge authenticates; Écluse reads with its own identity). The caller is
  authenticated at the **edge** (see [Edge authentication](#edge-authentication)); Écluse
  then reads the upstreams with its own workload identity via the
  [`CredentialProvider`](cloud-backends.md#credential-provider), forwarding no caller
  credentials at all (the smallest credential surface, one short-lived least-privilege
  token). Selecting `service` asserts that the **edge, not the upstream, is the authority**
  for who may use the proxy. It sidesteps the credential-aggregation surface `passthrough`
  carries (see
  [Security → trust assumptions & credential posture](security.md#trust-assumptions--credential-posture)).

The cache-recovering designs `delegated-cache` and `memoised` are **rejected**: both exist
only to share the private cache Écluse forbids.

### Rejected: the cache-recovering strategies

- **`delegated-cache`** would hold the merged/filtered compute in the shared cache and gate
  each hit with a per-request authorisation **probe**. Safe in principle, but the probe's
  granularity must exactly match the upstream's or it over-grants, overhead the broker model
  does not justify.
- **`memoised`** would cache the upstream's authorisation *verdict*, keyed by a hash of the
  caller's credential. Rejected outright: it holds credential-derived state (a honeypot) and
  serves within a self-chosen revalidation window (revocation latency), with no upstream
  token TTL to lean on.

## Publishing: the publication target (passthrough write)

The strategies above govern reads. The one client-driven **write**, `npm publish` to the
[publication target](registry-model.md#publishing-first-party-packages-the-publication-target),
uses **passthrough**: Écluse forwards the publisher's own `Authorization` / `_authToken`,
which the target authorises. Écluse substitutes no identity and mints no token here, unlike
the mirror-target write, which always uses Écluse's own `CredentialProvider` token.

The universal invariant holds: the client's token reaches only the private upstream (on
read, under `passthrough`) and the publication target (on publish), **never** the public
upstream. Before any forward, the publish path enforces the **publish scope allow-list**
(anti-shadowing): a name outside the configured scopes is refused with no upstream write.
Like every credential-bearing request, the publish relay disables redirect-following, so a
`3xx` from the target returns to the client rather than chasing the credential to the
`Location` (see
[Security → a credential-bearing request never follows a redirect](security.md#egress-scope-what-the-outbound-controls-guard-and-what-they-do-not)).

> ⚠️ **The publish surface authorises *names*, not *callers*.** The scope allow-list limits
> which package names a publish may target; it is not authentication. A static
> `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN` makes Écluse publish under its **own**
> credential, so it is fail-closed: set without `ECLUSE_AUTH_TOKEN`, Écluse refuses to boot
> (`PublishStaticCredentialNeedsEdge`), making "static publish credential + open edge",
> which would let any unauthenticated client publish under the operator's credential,
> unrepresentable. `ECLUSE_AUTH_TOKEN` is the verifiable edge Écluse checks itself; an
> external layer (gateway, mesh/mTLS, network policy) is defence-in-depth but does not
> satisfy this. Pure passthrough (no static token) needs none of it, the publisher's own
> token is the authority (see
> [Security → a static publish credential is fail-closed](security.md#a-static-publish-credential-is-fail-closed)).

## Edge authentication

The npm client authenticates with an **opaque bearer** in `.npmrc` (`//host/:_authToken=`)
or via `npm login`; it does not speak SigV4, per-request mTLS, or interactive OIDC. So edge
authentication must terminate into a storable bearer or be handled in front of Écluse. The
modes:

1. **Open**, no app-level check; access is gated at the network layer (VPC, mesh).
   Appropriate on a closed network; the assumption it rests on is
   [threat #3](https://ecluse-proxy.com/threat-model.html).
2. **Static token**, `ECLUSE_AUTH_TOKEN`; the caller presents it as `Bearer` / `_authToken`.
   Standard npm tooling supports it directly.
3. **Trusted edge identity**, a fronting proxy / cloud IAP / service mesh asserts a verified
   identity (a signed header / mTLS SAN). Écluse honours it **only over a verifiable binding
   to that edge** (mutual TLS, or a shared secret / HMAC on the assertion) and **fails fast**
   on a `trusted-edge` mount with neither (an
   [unrepresentable unsafe combination](#safe-defaults-and-unrepresentable-unsafe-combinations)).
   A bare trusted header is forgeable into granted access anywhere Écluse is reachable other
   than through the edge, so the binding, not trust alone, makes the assertion unspoofable.

Validating **cloud IAM at the npm edge** is out (the npm client cannot speak it; a gateway
concern). Richer per-user token issuance (`npm login` web SSO, CI OIDC exchange) is a
possible future, and would be ecosystem-specific where the strategies above are not.

## Authorisation granularity

With no shared private cache, upstream authorisation granularity is **not Écluse's
concern**. Under `passthrough` the upstream re-decides every request against the caller's
token, coarse (repo-level, e.g. GCP Artifact Registry) or fine (per-package, e.g.
CodeArtifact resource policies); under `service` the edge is the per-caller authority and
the upstream sees one workload identity. Granularity-matching would only matter for a
per-hit probe over a shared private cache, which Écluse rejects.

## Safe defaults and unrepresentable unsafe combinations

- **The default is `passthrough`.** A correct deployment needs nothing else.
- **A shared private-origin cache is forbidden by construction**: under no strategy does the
  metadata cache admit a private entry.
- **`service` requires an explicit "the edge authorises callers" assertion** in config.
- **`trusted-edge` requires a verifiable edge binding** (mutual TLS, or a shared secret /
  HMAC on the asserted identity); a mount with neither is **rejected at startup**.
- Unknown or contradictory strategy configuration **fails fast at startup**, consistent with
  [config validation](configuration.md#validation-fail-fast-reject-the-unknown).

## Caching

Écluse caches **only the anonymous public-gated origin** of the
[metadata cache](web-layer.md#metadata-cache), one shared document per package. Its key
carries no credential dimension (the upstream base URL plus the package) and its value is
the canonical public document, never a credential or a credential-derived verdict. The
private origin is read per request (see
[Why Écluse never caches the private origin](#why-écluse-never-caches-the-private-origin)):
under `passthrough` with the caller's forwarded token, under `service` with Écluse's own
identity. On the **tarball leg** that per-request read is the credentialed
[conventional read](registry-model.md#serving-a-tarball-a-conventional-private-read-an-honoured-public-location)
of the artifact itself, no packument round-trip.

One further store sits beside it: the **assembled-representation store**, which memoises the
encoded merged document keyed by its
[derived validator](web-layer.md#middleware-and-helper-libraries), a fingerprint of every
input the document is a function of, **including the digest of the private document this
request's own authorised fetch returned**. That content key keeps the store inside the
model: a credential-blind key would let one caller's entry answer another, but a content key
can only be produced by a caller whose own per-credential private read returned identical
content. The private fetch and authorisation are never shared or skipped; only the
byte-identical transform of already-authorised inputs is. A different private view is a
different key and misses by construction.

## Credential supply: the `CredentialProvider`

The [`CredentialProvider`](cloud-backends.md#credential-provider) mints and refreshes a
bearer for **any upstream endpoint that requires one**: the mirror-target **write** always,
and the private-upstream **read** under `service`. `passthrough` reads use the forwarded
caller token and need no read provider. A `service` mount therefore configures a read
provider alongside its mirror-target one; both are the same handle and refresh /
single-flight / breaker policy, differing only in the per-cloud `mintToken` leaf.

## Multi-instance is an isolation tool, not an authorisation mechanism

Running separate Écluse instances per tenant is a legitimate blast-radius / policy isolation
choice, orthogonal to the credential strategy, but not a substitute for one, and it scales
to **team** granularity, never per-developer. Reach for it when you want hard isolation or a
distinct policy per tenant, not to avoid choosing a strategy.

## Universal invariants (every strategy)

- The caller's credential is **never** sent to the public upstream.
- Outbound fetches stay within the [security invariants](security.md): https-only egress
  with certificate validation, the host allowlist, identifier canonicalisation, and bounded
  responses.
- Public versions are **always** gated by the [rules engine](rules-engine.md); trusted
  private versions enter the
  [packument merge](registry-model.md#packument-merge-across-upstreams) unfiltered.
