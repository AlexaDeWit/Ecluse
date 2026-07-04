# Multi-ecosystem hosting

> Part of the [Écluse architecture overview](../architecture.md).

A single Écluse process serves one or more ecosystems from one listener, mounting each
registry under a path prefix on a shared base URL:

```
https://registry.internal.example.com/npm    → npm mount
https://registry.internal.example.com/pypi   → PyPI mount
```

No second instance, host, or port per ecosystem. This is the "virtual repository" model of
Artifactory and Nexus: one host, many repositories under paths.

## Mounts

**One mount per ecosystem.** The served ecosystem is a mount's identity, and its path
prefix is derived from the ecosystem, not configured (npm → `/npm`, PyPI → `/pypi`). An
operator chooses which ecosystems to serve, never how they are addressed, so a prefix can
neither collide nor be mistyped.

A mount binds its ecosystem to:

- a **registry adapter**, the `RegistryClient` for that ecosystem (see
  [Registry Abstraction](registry-model.md#registry-abstraction));
- a **registry set**, its four named roles (private + public upstream, mirror +
  publication target), several of which may map to the same registry, most commonly the
  private upstream (see [Registry roles](registry-model.md#registry-roles));
- an **error renderer**, the ecosystem's client-facing error surface (npm's
  `{"error": …}` object; a different shape for PyPI), so the web layer decides an error's
  status but holds no ecosystem body shape (see [Error model](web-layer.md#error-model));
- optionally, a **per-ecosystem rule refinement**, a named map merging over the shared
  [rule policy](configuration.md#rule-policy), so ecosystems may run under different
  policies. Omitted, that ecosystem uses the shared policy unchanged.

A binding carries all of these as one unit, so a mount cannot be half-wired and there is no
ecosystem default: its grammar, renderer, and serve dependencies are chosen together at the
composition root.

**Every registry is path-mounted; there is no root mount.** A registry never sits at `/`,
so adding a second ecosystem never changes an existing consumer's URLs. (The web-layer
prefix is a non-empty segment list, making a root mount unrepresentable.)

The mount map is keyed by ecosystem (`ecosystem → mount`). The single-ecosystem setup
([Configuration](configuration.md#configuration)), one npm mount, is the degenerate case,
still under its own derived prefix.

## Why path prefixes work

Both npm and pip treat their configured endpoint as a base URL and derive every request
path relative to it; neither assumes the registry sits at a host root. Point a client once
at a mount's base and the rest follows:

- **npm**, the `registry` (and `@scope:registry`) setting is a base URL that may include a
  path; auth tokens are keyed by that base including its path, so credentials scope cleanly
  to a mount.
- **pip**, the index URL points at wherever the Simple API root lives, and file URLs
  resolve relative to the index page, so a prefix is transparent.

The per-ecosystem request shapes and base-relative behaviour are documented in
[`research/reverse-engineering/npm.md`](../research/reverse-engineering/npm.md) and
[`research/reverse-engineering/pypi.md`](../research/reverse-engineering/pypi.md).

## The load-bearing requirement: URL rewriting

Registry responses embed absolute artifact locations, npm's `dist.tarball`, and on public
PyPI file URLs on a separate host entirely. Forwarded unchanged, a client resolves metadata
through the proxy but downloads bytes directly from upstream, bypassing the gate.

So a mount rewrites embedded artifact URLs to stay under its own prefix before serving
metadata:

- **npm**, rewrite `dist.tarball` to `{mount-base}/{pkg}/-/{file}`.
- **PyPI**, emit artifact URLs relative to the Simple index (cleanest, the client resolves
  them under the mount automatically) or absolute under the mount.

Keeping artifacts same-host under the prefix has a second benefit: npm attaches credentials
only to requests on the registry host, so same-host artifact URLs keep auth flowing on
tarball fetches a separate host would drop.

Because rewriting emits absolute URLs, a mount must know its own externally-visible base
URL. Inferring it from request headers is unreliable behind load balancers and TLS
terminators, so the public base is explicit configuration, per mount.

## Dispatch

Routing is a thin layer above the adapters: match the path's leading segment to a mount,
strip the prefix, and hand the remainder, now an ecosystem-native path, to that mount's
adapter and the standard [Request Lifecycle](../architecture.md#request-lifecycle). The core
and adapters are unchanged by multiple mounts; only the front door fans out. A mount prefix
is accepted with or without a trailing slash, since base-URL join behaviour differs subtly
between clients.

## Graceful rollover

A rolling deploy or pod eviction takes an instance down while clients and the load balancer
still point at it. Stopping the moment it receives `SIGTERM` would cut off in-flight
requests, including a half-streamed artifact, surfacing as a `503` and, behind a service
mesh, a poisoned connection-pool entry. Écluse closes that window with a full graceful drain
on `SIGTERM` and `SIGINT`:

1. **Readiness flips, liveness holds.** In the draining state `GET /readyz` returns `503`
   while `GET /livez` stays `200`. The readiness `503` is the signal a load balancer or
   service mesh watches to stop routing new traffic here. Liveness stays green deliberately:
   a draining instance is alive and finishing its work, so an orchestrator must not kill it
   early, that is readiness's job.

2. **Going-away header.** While draining, every response carries `Connection: close`. An
   HTTP/1.1 keep-alive pool would otherwise reuse an already-open socket even after
   readiness flips, landing a request on a closing process and producing the very rollover
   `503` the flip meant to avoid. The header closes the socket after the response, so the
   next request opens a fresh connection the mesh routes to a ready instance.

3. **Drain in-flight work, then exit.** The instance stops accepting new connections and
   waits for in-flight requests and artifact streams to finish before exiting, so a
   half-delivered tarball completes rather than being severed. The wait is bounded by
   `ECLUSE_SHUTDOWN_DRAIN_TIMEOUT` (default 30 seconds; see
   [Configuration](configuration.md#configuration)), after which it exits regardless so a
   stuck request cannot pin the old instance open through a deploy.

**Operator note.** Set the platform's termination grace period longer than
`ECLUSE_SHUTDOWN_DRAIN_TIMEOUT` so the orchestrator does not `SIGKILL` mid-drain (e.g. a
Kubernetes `terminationGracePeriodSeconds` above the configured drain). The sequence assumes
the LB acts on `/readyz`; a deployment that load-balances without a readiness check should
add one.

**Local development.** On an interactive terminal, two keys force an immediate halt that
bypasses the drain: a second `Ctrl+C` (the first begins the drain) and `Ctrl+D` (end of
standard input). This is gated on standard input being a TTY, so in production (non-TTY or
closed stdin) no such watcher exists and the only path is the signal-driven drain above.

## Capability manifest

Because dispatch fans out over a fixed set of mounts and each classifies into the same
closed [`Route`](web-layer.md#raw-wai-not-a-web-framework) set, the mounts are the source
for the capability manifest: enumerate the `Route` constructors across the mounts for the
full supported surface per ecosystem, with no hand-authored duplication. Each adapter
contributes its per-ecosystem path template and support status (e.g. `Search` → `501`), and
the rendered docs tag operations by ecosystem so one document reads as "one server, these
protocols." See [API Surface & Capability Manifest](api-surface.md).

## Alternative: host-based routing

The same process could instead distinguish ecosystems by hostname
(`npm.registry.example.com`, `pypi.registry.example.com`), dispatching on the request host
rather than a path prefix. This yields root-path URLs (rewriting only swaps the host) at the
cost of a DNS name and TLS coverage per ecosystem, still one instance. Path-prefix mounting
is the default (one name, one certificate, no DNS choreography); host-based routing is
available where per-ecosystem hostnames are wanted.
