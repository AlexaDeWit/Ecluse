# Multi-Ecosystem Hosting

> Part of the [Écluse architecture overview](../architecture.md).

A single Écluse process serves one or more ecosystems from one listener, by
**mounting** each registry under a path prefix on a shared base URL:

```
https://registry.internal.example.com/npm    → npm mount
https://registry.internal.example.com/pypi   → PyPI mount
```

No second instance, host, or port is needed per ecosystem. This is the "virtual
repository" model proven by Artifactory and Nexus: one host, many repositories
under paths, several ecosystems side by side.

## Mounts

**One mount per ecosystem.** The served ecosystem is a mount's identity: there is
exactly one mount per ecosystem, and its path prefix is **derived from the
ecosystem, not configured** (npm → `/npm`, PyPI → `/pypi`). An operator chooses
*which* ecosystems to serve, never how they are addressed — so a prefix can neither
collide nor be mistyped.

A **mount** binds its ecosystem to:

- a **registry adapter** — the `RegistryClient` for that ecosystem (see
  [Registry Abstraction](registry-model.md#registry-abstraction));
- a **three-registry tuple** — its own private upstream, public upstream, and
  mirror target (see [Three-Registry Model](registry-model.md#three-registry-model));
- an **error renderer** — the ecosystem's client-facing denial/error surface (npm's
  `{"error": …}` JSON object; a different shape for PyPI), so the agnostic web layer
  decides an error's *status* but holds no ecosystem *body* shape of its own (see
  [Web Layer → Error model](web-layer.md#error-model));
- optionally, a **per-ecosystem rule refinement** — a named map that merges over the
  shared [rule policy](configuration.md#rule-policy) (itself layered on the built-in
  default), so different ecosystems may run under different policies. Omitted, that
  ecosystem uses the shared policy unchanged.

A binding carries all of these **as one unit**, so a mount cannot be half-wired and
there is no ecosystem default to fall back to — its grammar, renderer, and serve
dependencies are chosen together at the composition root.

**Every registry is path-mounted; there is no root mount.** A registry never sits
at `/`, so adding a second ecosystem later never changes an existing consumer's
URLs — the cost a root mount would impose. (The web-layer prefix is a non-empty
segment list, making a root mount unrepresentable rather than merely discouraged.)

The mount map is therefore keyed by ecosystem (`ecosystem → mount`). The
single-ecosystem setup described under
[Configuration](configuration.md#configuration) — one npm mount — is the degenerate
case, still under its own derived prefix.

## Why path prefixes work

Both npm and pip treat their configured endpoint as a **base URL and derive
every request path relative to it**; neither assumes the registry sits at the
root of a host. A client is pointed once at a mount's base and the rest follows:

- **npm** — the `registry` (and `@scope:registry`) setting is a base URL that may
  include a path; auth tokens are keyed by that base *including its path*, so
  credentials scope cleanly to a mount.
- **pip** — the index URL points at wherever the Simple API root lives, and file
  URLs are resolved relative to the index page, so a prefix is transparent.

The per-ecosystem request shapes and base-relative behaviour are documented in
[`research/reverse-engineering/npm.md`](../research/reverse-engineering/npm.md) and
[`research/reverse-engineering/pypi.md`](../research/reverse-engineering/pypi.md).

## The load-bearing requirement: URL rewriting

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

## Dispatch

Routing is a thin layer above the registry adapters: match the request path's
**leading segment** to a mount, strip the prefix, and hand the remainder — now an
ordinary ecosystem-native path — to that mount's adapter and the standard
[Request Lifecycle](../architecture.md#request-lifecycle). The proxy core and the
adapters are unchanged by the presence of multiple mounts; only the front door
learns to fan out. A mount prefix should be accepted with or without a trailing
slash, since the base-URL join behaviour differs subtly between clients — an area
to validate against the real `npm` and `pip` clients during implementation.

## Capability manifest

Because dispatch is a fan-out over a fixed set of mounts and each mount classifies
into the same closed [`Route`](web-layer.md#raw-wai-not-a-web-framework) set, the
mounts double as the source for Écluse's **capability manifest**: enumerate the
`Route` constructors across the configured mounts and you have the full supported
surface, per ecosystem, with no hand-authored duplication. Each mount's adapter
contributes its per-ecosystem path template and support status (e.g. `Search` →
`501`), and the rendered docs **tag operations by ecosystem** so one document reads
as "one server, these protocols." See
[API Surface & Capability Manifest](api-surface.md).

## Alternative: host-based routing

The same single process could instead distinguish ecosystems by **hostname**
(`npm.registry.example.com`, `pypi.registry.example.com`), dispatching on the
request's host rather than a path prefix. This yields root-path URLs (rewriting
only swaps the host, never injects a prefix) at the cost of a DNS name and TLS
coverage per ecosystem. It is still one instance — the choice is routing *style*,
not instance count. **Path-prefix mounting is the default** (one name, one
certificate, no DNS choreography); host-based routing is available where
per-ecosystem hostnames are specifically wanted.
