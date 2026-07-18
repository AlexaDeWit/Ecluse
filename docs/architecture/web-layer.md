# Web layer

> Part of the [Écluse architecture overview](../architecture.md).

The front door is a raw `wai` `Application` served by `warp`. It routes a request, streams
artifacts with bounded memory, and applies cross-cutting concerns as middleware. Routing is two
layers: mount dispatch matches the leading path segment to a mount (see
[Multi-ecosystem mounts](#multi-ecosystem-mounts)) and strips its prefix, then the remaining
ecosystem-native path goes to that mount's router. Deny-by-default is structural: a path no route
claims is a `404`, and a tarball name that parses for a different package is a path-confusion
attempt the route declines rather than fabricate into a coordinate.

## Multi-ecosystem mounts

A single Écluse process serves one or more ecosystems from one listener by mounting each registry
under a path prefix on a shared base URL:

```
https://registry.internal.example.com/npm    → npm mount
https://registry.internal.example.com/pypi   → PyPI mount
```

There is one mount per ecosystem, and its prefix is derived from the ecosystem, not configured
(npm → `/npm`, PyPI → `/pypi`), so a prefix can neither collide nor be mistyped. No mount sits at
`/`, so adding an ecosystem never changes an existing consumer's URLs. Each mount also carries an
optional per-ecosystem [rule refinement](configuration.md#rule-policy) merged over the shared
policy; the single-npm setup is the degenerate case under its own derived prefix.

URL rewriting is load-bearing. Registry responses embed absolute artifact locations (npm's
`dist.tarball`; on public PyPI, file URLs on a separate host). Forwarded unchanged, a client would
resolve metadata through the proxy but download bytes directly from upstream, bypassing the gate.
So a mount rewrites embedded artifact URLs under its own prefix (`{mount-base}/{pkg}/-/{file}`)
before serving metadata. Same-host artifacts have a second benefit: npm attaches credentials only
to requests on the registry host, so same-host artifact URLs keep auth flowing on tarball fetches a
separate host would drop. Because rewriting emits absolute URLs and header inference is unreliable
behind load balancers and TLS terminators, a mount must know its own externally-visible base URL as
explicit configuration (`server.publicUrl` plus its derived prefix).

## Meta-routes: ping, health, and search

`/livez` and `/readyz` are kept distinct for orchestration. Liveness means the process is
responsive; readiness means config is loaded and the listener is serving. Readiness is deliberately
lenient about public-upstream reachability: the proxy still serves private hits when public is
down, so an upstream blip must not pull a healthy pod from rotation. `/-/ping` answers locally, and
`/-/v1/search` returns `501`, a discovery convenience rather than an install path.

## Capability manifest

Écluse speaks package-registry protocols (npm at launch; PyPI planned), not a bespoke HTTP API:
clients (`npm`, `pnpm`, `yarn`) hardcode the protocol and never read an API description, so the
published OpenAPI document is not a client-integration contract. It is a **capability manifest**: a
human-facing statement of which protocols this server speaks and what is, and isn't, supported per
ecosystem, which stops being self-evident as mounts multiply.

### What the manifest covers, and what it doesn't

Écluse documents its coverage of each protocol, not the protocol itself:

- **Owned and synthesised responses are modelled in full**: the error/denial envelope, the health
  and meta routes, and the synthesised packument that Écluse authors (see
  [Packument merge](registry-model.md#packument-merge-across-upstreams)).
- **Opaque pass-through is described, not re-specified**: tarball and artifact responses stream
  verbatim (see [Streaming](#streaming-and-resource-lifetime)); their status, media type, and body
  are upstream-controlled, so the operation carries a wildcard binary `default` response rather than
  a false finite status set.
- **Unsupported routes are a documented boundary**: `GET /-/v1/search` returns `501`, stated
  explicitly so a reader learns the limit from the manifest, not from an error response.

### The synthesised-packument schema = the trust boundary

The served packument is Écluse's merged-and-filtered view (private versions trusted, public gated;
see [Packument merge](registry-model.md#packument-merge-across-upstreams)), a document no single
upstream produces, and the highest-scrutiny piece of the manifest. Its schema is therefore **owned**
here, and is modelled as *partial* and *open*: only the fields Écluse reads and transforms
(`versions`, `dist-tags`, `time`, and each version's `dist`) are described, and
`additionalProperties: true` everywhere states that every unlisted field relays unchanged from the
contributing upstream (private wins a collision). The schema is thus a precise statement of what
the gate touches and what it does not.

## Streaming and resource lifetime

The proxy pulls from upstream only as fast as the client drains: constant memory regardless of
artifact size, with backpressure for free.

The proxy streams artifacts through **without hashing them**, relying on the client's own integrity
check against the packument's `dist.integrity`, which it preserves unaltered when
[filtering](rules-engine.md#applying-verdicts-to-a-packument) (npm always verifies it). Proxy-side
serve verification is deferred until a weakly-verifying ecosystem (e.g. PyPI) or a non-verifying
client lands; the mirror worker does verify before publishing to the sanitised home (see
[Mirror queue](cloud-backends.md#mirror-queue)).

A `HEAD` must never run the full-`GET` streaming pump: a bodiless `HEAD` that opened the upstream
connection and pumped a whole body warp then discards is a DoS-amplification lever (cheap `HEAD`s
forcing arbitrary full-artifact upstream fetches), so `HEAD` is handled explicitly in dispatch
rather than by the `Autohead` middleware.

## Metadata cache

The parsed packument metadata is held in a short-TTL, size-bounded, in-memory cache keyed by
package, so concurrent resolutions of a popular package collapse to one upstream call.

What is cached is the metadata, **not the verdict**: rules are re-evaluated each request, so
time-sensitive rules (`AllowIfOlderThan`) stay correct. This is in-memory metadata only; on-disk
artifact caching is out of scope, and the mirror remains the durable store.

The cache holds the anonymous public (gated) origin only; the private origin is never cached but
read per request. Écluse [forbids a shared private cache](access-model.md#caching), so no caller's
private view can leak to another within the TTL, while the anonymous public origin crosses no trust
boundary and is cached freely.

## Serve admission and upstream pools

The packument path and a tarball miss's public-metadata gate share one process-wide admission
bound; a request that waits out its budget for a slot is shed with `503` and `Retry-After`. Shedding
instantly would be self-amplifying, since the refusal work competes for the cores the admitted work
needs. Health probes, cheap local routes, and trusted private tarball hits bypass admission: the hit
already streams in constant memory, and holding a metadata slot for a slow download would let
clients starve packument traffic.

The public and private connection pools are configured independently. The private pool takes the
larger share, because a trusted tarball hit streams outside admission, so its demand is the
steady-state inbound hit fan-out.

## Error model

Every served response is the rendering of one serve outcome. A small type (`ServeDecision` in
`Ecluse.Core.Server.Response`) maps each outcome to the right status rather than collapsing
everything into a generic 403 or 500. For a concrete artifact request the decision renders directly:

| Outcome | Status |
|---|---|
| Admit | `200` (streamed) |
| Policy denial (incl. deny-by-default) | `403` + denial body |
| Undecidable, transient | `503` + `Retry-After` |
| Undecidable, permanent | `500` |
| upstream miss | `404` (forwarded) |

The rule: `503` only when the condition is believed to resolve (a transient upstream or advisory
condition); otherwise `500`, since retrying a permanent inability to decide cannot help.

A packument request has no single status: the document is
[merged across upstreams](registry-model.md#packument-merge-across-upstreams) and filtered by
provenance (see [Applying verdicts](rules-engine.md#applying-verdicts-to-a-packument)). A status is
chosen only when nothing survives the merge, by the most recoverable cause: `503` if any rejection
was transient or a needed upstream was unavailable; else `502` if a responding upstream returned an
invalid response (a packument whose self-reported name is for a different package, see
[name validation](registry-model.md#the-route-name-is-the-served-names-validation-authority));
`500` if none is retryable but an exclusion is a permanent inability; else `403`. Never `404`: the
versions existed and were withheld, and a genuinely absent package is a separate upstream miss.
(`packumentStatus` in `Ecluse.Core.Server.Response` is the counterpart of `artifactStatus`.)

The serve-outcome model decides status, not body shape: an ecosystem's route contract supplies the
matching response constructor and codec. A request matching no mount is a neutral `404 Not Found` in
`text/plain`. The denial-body shape and `ECLUSE_SERVER__HELP_MESSAGE` handling are in
[Rules engine → denial responses](rules-engine.md#denial-responses).
