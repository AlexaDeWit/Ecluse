# Web layer

> Part of the [Écluse architecture overview](../architecture.md).

The front door is a raw `wai` `Application` served by `warp`. It routes a request, streams
artifacts with bounded memory, and applies cross-cutting concerns as middleware.

## Raw WAI, not a web framework

A proxy is a passthrough over a small, irregular URL surface: npm paths carry URL-encoded
slashes (`/@scope%2Fpkg`, `/pkg/-/pkg-1.0.0.tgz`) and reserved meta-routes
(`/-/npm/v1/security/advisories/bulk`). Matching on `pathInfo` in a raw `Application` is simpler
than encoding that shape at the type level (servant), and it keeps the streaming control a
framework's response handling would hide. Routing sits in two layers: mount dispatch (see
[Multi-ecosystem mounts](#multi-ecosystem-mounts)) matches the leading path segment to a mount
and strips the prefix, and the ecosystem-native path that remains goes to that mount's router.

### The route table belongs to the ecosystem

A route is an ecosystem's own concern, so a route is one record (`Route` in
`Ecluse.Core.Server.Route`) and an ecosystem's table is a list of them. `routerOf` folds the
list into the mount's router (npm's is `npmRouter = routerOf npmNotFound npmRoutes`). Each
record binds status, body shape, and renderer together in its response contract, and the
[capability manifest](#capability-manifest) renders the same records, so no separate status list
can drift.

Deny by default is structural: `routerOf` has no catch-all branch, so a request no route
claims is a `404`. A route's builder returns `Nothing` to refuse a request it pattern-matched
but won't serve; an artifact name that parses for a different package is a path-confusion
attempt, so the route declines it and the request falls through to the `404` rather than being
fabricated into a coordinate. The pipeline handlers (`Ecluse.Core.Server.Pipeline`) stay
ecosystem-neutral, reaching a registry's metadata client and packument assembly as injected
capabilities on `PackumentDeps`, so adding an ecosystem adds a table and changes nothing in
`Ecluse.Runtime.Server`.

### npm's table

The npm table encodes three npm-specific facts. `pathInfo` splits on `/` before
percent-decoding, so an encoded scoped name (`/@scope%2Fpkg`) arrives as one segment and a bare
scope as two, and both normalise to the same `PackageName`. Reserved meta-routes (`/-/…`) match
first, since a package name is never a lone `-`. And a tarball's file name must parse for its
own package, so a name addressing another package's artifact is refused.

Only `GET`, `HEAD`, and `PUT` are answered (`PUT /{pkg}` is the publish). A `HEAD` is a bodiless
variation of its `GET`, not a distinct action, so the router selects the head-mode handler,
which is load-bearing on the artifact path where running the `GET` handler and discarding the
body would stream a whole artifact to nowhere (see [HEAD on artifacts](#head-on-artifacts)). Any
other method, and anything unrecognised, is a `404`, so deny-by-default holds for methods as
well as paths. The pure table is unit-testable with no server: feed `matchRoute` a method and
segments and assert which route claimed it, and the scoped-name decoder and artifact-coordinate
capture (`takePackage` is the package-name parser) are asserted directly against an independent
hand-written reference.

## Multi-ecosystem mounts

A single Écluse process serves one or more ecosystems from one listener by mounting each
registry under a path prefix on a shared base URL:

```
https://registry.internal.example.com/npm    → npm mount
https://registry.internal.example.com/pypi   → PyPI mount
```

There's one mount per ecosystem, and its prefix is derived from the ecosystem, not configured
(npm → `/npm`, PyPI → `/pypi`), so a prefix can neither collide nor be mistyped. No mount sits
at `/`, so adding an ecosystem never changes an existing consumer's URLs. A mount binds, as one
unit resolved at boot, the ecosystem's capability record (the `RegistryAdapter`), its serve
surface (the [router](#the-route-table-belongs-to-the-ecosystem) and route contracts), its four
[registry roles](registry-model.md#registry-roles) over the
[protocol boundary](registry-model.md#registry-abstraction), and an optional per-ecosystem
[rule refinement](configuration.md#rule-policy) merged over the shared policy. The single-npm
setup is the degenerate case, still under its own derived prefix.

URL rewriting is load-bearing. Registry responses embed absolute artifact locations (npm's
`dist.tarball`; on public PyPI, file URLs on a separate host). Forwarded unchanged, a client
would resolve metadata through the proxy but download bytes directly from upstream, bypassing
the gate. So a mount rewrites embedded artifact URLs under its own prefix (npm's `dist.tarball`
→ `{mount-base}/{pkg}/-/{file}`) before serving metadata. Keeping artifacts on the same host
has a second benefit: npm attaches credentials only to requests on the registry host, so
same-host artifact URLs keep auth flowing on tarball fetches a separate host would drop.
Because rewriting emits absolute URLs and header inference is unreliable behind load balancers
and TLS terminators, a mount must know its own externally-visible base URL as explicit
configuration (the server's public URL, `server.publicUrl`, plus its derived prefix).

## Meta-routes: ping, health, and search

- `/-/ping` is answered locally with `200` and `{}`. `npm ping` checks the endpoint it talks
  to (the proxy) is up, so there's no reason to round-trip upstream.
- `/livez` and `/readyz` are kept distinct for orchestration. Liveness means the process is
  responsive, and in single-process mode it also reflects the mirror worker's consume-loop
  heartbeat, so a stalled worker fails liveness (see
  [Process model](cloud-backends.md#process-model-the-unified-multicall-binary)). Readiness
  means config is loaded and the listener is serving; it's deliberately lenient about
  public-upstream reachability, since the proxy still serves private hits when public is down,
  so an upstream blip must not pull a healthy pod from rotation.
- Search (`/-/v1/search`) returns `501 Not Implemented` with a short message pointing to the
  public registry's website: it's a discovery convenience, not an install path.

Everything else unrecognised is a `404`.

## Capability manifest

Écluse speaks package-registry protocols (npm at launch; PyPI planned), not a
bespoke HTTP API. Clients (`npm`, `pnpm`, `yarn`) hardcode the registry protocol and never
read an API description, so the OpenAPI document here is not a client-integration contract.
It is a **capability manifest**: one human-facing statement of which protocols this server
speaks and exactly what is, and isn't, supported per ecosystem. As mounts multiply (`/npm`,
`/pypi`, ...) that stops being self-evident, and the manifest is where it becomes legible.

### What the manifest covers, and what it doesn't

Écluse documents its coverage of each protocol, not the protocol itself, which maps onto how
each route is handled:

- **Owned and synthesised responses are modelled in full**: the error/denial envelope, the
  health and meta routes, and the synthesised packument that Écluse authors (see
  [Packument merge](registry-model.md#packument-merge-across-upstreams)).
- **Opaque pass-through is described, not re-specified**: tarball and artifact responses
  stream verbatim (see [Streaming](#streaming-and-resource-lifetime)). Their
  status, media type, and body are upstream-controlled, so the operation carries an explicit
  OpenAPI `default` response with wildcard binary content rather than a false finite status
  set.
- **Unsupported routes are a documented boundary**: `GET /-/v1/search` returns `501`, stated
  explicitly so a reader learns the limit from the manifest, not from an error response.

Re-specifying npm's full packument or registry protocol is out of scope: that is npm's
contract, and clients hardcode it.

### Source of truth: the route table

The manifest is generated, not hand-written, and holds no per-route knowledge of its own. It
walks the same route records the router runs: each adapter declares its routes, and every record
carries a path template, what serving it does, and an abstract `ResponseContract`. The route
table and the `ResponseContract` machinery are specified in
[the route table above](#the-route-table-belongs-to-the-ecosystem); the manifest is one more
reader of them, so a route cannot be declared without response documentation. The core's contract
vocabulary is deliberately OpenAPI-free (`ResponseDoc` and the closed `BodySchema`), because the
`openapi3` dependency tree must never reach the running proxy, so adding PyPI is adding a mount,
not describing a protocol. In the rendered docs, Redoc tags are ecosystems, so the document reads
as "one server, these protocols", and a route's `operationId` is its ecosystem-local name
qualified by its mount (`npm.packument`), where global uniqueness is guaranteed.

### The synthesised-packument schema = the trust boundary

The served packument is Écluse's merged-and-filtered view (private versions trusted, public
gated; see [Packument merge](registry-model.md#packument-merge-across-upstreams)), a document
no single upstream produces. Its schema is therefore **owned** here, and it is the
highest-scrutiny piece of the manifest. It is modelled as *partial* and *open*: only the
fields Écluse reads and transforms (`versions`, `dist-tags`, `time`, and each version's
`dist`) are described, and `additionalProperties: true` everywhere states that every unlisted
field relays unchanged from the contributing upstream (private wins a collision). The schema
is thus a precise statement of what the gate touches and what it does not.

Unlike the other owned types, this schema is **hand-authored, not codec-derived**: an open
schema has no clean `autodocodec` representation, and the served body is the raw upstream
`Value` edited in place, never re-serialised through a codec. So a valid instance is not a
proof that the filtered document is coherent (that every `dist-tags` target is a surviving
`versions` key); that cross-field coherence is not schema-expressible and stays a
[property test](rules-engine.md#applying-verdicts-to-a-packument).

### How it's built and published

The manifest is derived build data, a pure function of `(config, mounts)` that moves only when
the code moves: paths and methods are projections of the route records, and status, media type,
and body are projections of the same `ResponseContract` that is the handler's response
capability, with `HEAD` derived from the `GET` contract. The owned emitted types (the
error/denial envelope and the config) define their JSON through one `autodocodec` codec that
derives both the `aeson` instance and the OpenAPI / JSON-Schema, so wire format and documented
schema cannot diverge, and the config's JSON Schema falls out for free. The synthesised packument
is the lone exception, hand-authored as above; npm's inbound wire decoding stays lenient
hand-rolled `aeson` (see [Technology stack](technology-stack.md#technology-stack)).

The `openapi-gen` executable (kept out of the shipped library closure, like the benchmarks)
assembles the document with `openapi3` and writes `openapi.json`; there is no `GET /openapi.json`
route. `task docs-site` and `task site` run it and render a static Redoc page into `./_site` for
GitHub Pages. The Redoc bundle is vendored and hash-pinned (the `mermaidJs` `fetchurl` pattern),
so the site needs no Node. Output is deterministic (pinned key ordering, fixed base URLs), so a
regeneration is a reviewable diff, and `openapi.json` is regenerated at publish time, not
committed.
## Control plane vs. data plane

The most important split in the HTTP code:

- The data plane streams artifacts and fetches metadata through `http-client`.
- The control plane (SQS for the mirror queue, STS, and CodeArtifact's `GetAuthorizationToken`,
  the AWS [`CredentialProvider`](cloud-backends.md#credential-provider)'s `mintToken`) goes
  through `amazonka`.

This matters most for CodeArtifact: its npm repository is a standard HTTPS npm endpoint, so
Écluse mints a bearer token from `GetAuthorizationToken` (control plane) then fetches
packuments and tarballs with ordinary `http-client` (data plane). The streaming path never
touches `amazonka`'s conduit/`ResourceT` machinery, exactly where naive streaming through a
proxy goes wrong. The same split is the design for other cloud backends as they land (see
[Cloud backends](cloud-backends.md#cloud-backends)).

On the data plane the private-upstream fetch forwards the caller's own credential (passthrough,
the shipped posture; see [access model](access-model.md)). The caller's `Authorization` is
always stripped before any public-upstream fetch.

## Streaming and resource lifetime

A WAI streaming response body runs after the handler returns, so a lexically-scoped resource
(`bracket`, `withResponse`, `runResourceT`) released at handler return is already gone by the
time the body streams, a use-after-free that frameworks hiding the response continuation invite.
Raw WAI avoids it by construction: `Application` is continuation-passing, so resource
acquisition brackets the typed responder call itself, and the upstream connection lives for
exactly the streamed body's duration, closing only when warp returns `ResponseReceived`. `write`
fills warp's bounded output buffer and blocks on the socket send when it spills, so the proxy
pulls from upstream only as fast as the client drains: constant memory regardless of artifact
size, with backpressure for free. There's no `ResourceT` and no conduit on the hot path, and no
unrestricted WAI `Response` reaches a pipeline module.

The proxy streams artifacts through without hashing them, relying on the client's own integrity
check against the packument's `dist.integrity`, which it preserves unaltered when
[filtering](rules-engine.md#applying-verdicts-to-a-packument) (npm always verifies it).
Proxy-side serve verification is deferred until a weakly-verifying ecosystem (e.g. PyPI) or a
non-verifying client lands; the mirror worker does verify before publishing to the sanitised
home (see [Mirror queue](cloud-backends.md#mirror-queue)).

### HEAD on artifacts

A `HEAD` must never run the full-`GET` streaming pump. A bodiless `HEAD` that opened the
upstream connection and pumped a whole body warp then discards is wasted egress and a
DoS-amplification lever: cheap `HEAD`s forcing arbitrary full-artifact upstream fetches. That's
why the [`Autohead` middleware](#middleware-and-helper-libraries) isn't used; `HEAD` is handled
explicitly in dispatch.

On the tarball route a `HEAD` runs the identical gating and upstream-request construction as
`GET` (edge auth, host allowlist and internal-range block, the same-host `dist.tarball` gate,
the [origin trust split](access-model.md)) but issues the upstream request as a `HEAD`, relays
its status and safe headers with no body, and enqueues no mirror job. The packument route works
the same way, emitting the same headers including the `Content-Length` of the would-be merged
body; here the defence is only HTTP correctness, since a packument is assembled locally with no
artifact egress. A `304` by either method is answered off the derived validator without
assembling at all.

## Metadata cache

Resolving a package re-fetches its upstream packument(s), parses them, and evaluates rules. To
avoid repeating that, the parsed packument metadata (all versions' `PackageDetails`) is held in
a short-TTL, size-bounded, STM-backed in-memory cache keyed by package. A packument request and
the [tarball-gating](../architecture.md#request-lifecycle) fetches that follow share it, and
concurrent resolutions of a popular package collapse to one upstream call.

What's cached is the metadata, not the verdict: rules are re-evaluated each request, so
time-sensitive rules (`AllowIfOlderThan`) stay correct. Only each upstream's fetch and parse is
memoised, per source, since a packument is
[merged across upstreams](registry-model.md#packument-merge-across-upstreams); the merge,
filter, and `latest` repoint are recomputed. Past that point sits the assembled-representation
store, which holds the encoded merged document keyed by its derived validator, a content address
over every serve input, so a recurring serve returns stored bytes with no re-assembly and can
never be stale (changed inputs miss by key). This is in-memory metadata only; on-disk artifact
caching is out of scope, and the mirror remains the durable store.

The cache holds the anonymous public (gated) origin only; the private origin is never cached
but read per request. Écluse [forbids a shared private cache](access-model.md#caching), so no
caller's private view can leak to another within the TTL, while the anonymous public origin
crosses no trust boundary and is cached freely.

## Serve admission and upstream pools

The packument path and a tarball miss's public-metadata gate share one process-wide admission
bound: at most `ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT` metadata materialisations run at once. Work
that finds the cap busy waits briefly for a slot (a budget equal to the shed path's
`Retry-After: 1` hint) in a waiting room bounded at the capacity; only a request that finds the
room full or waits out its budget gets `503 Service Unavailable` with `Retry-After: 1`. Shedding
instantly would be self-amplifying, since the refusal work competes for the cores the admitted
work needs. Health probes, cheap local routes, and trusted private tarball hits bypass
admission: the hit already streams in constant memory, and holding a metadata slot for a slow
download would let clients starve packument traffic.

The public and private `http-client` managers have independently configurable per-host pools
(`publicConnectionsPerHost`, `privateConnectionsPerHost`), both defaulting to a share of the
process file-descriptor limit rather than to `ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT`, since each
pooled connection is one descriptor. The private pool takes the larger share, because a trusted
tarball hit streams outside admission, so its demand is the steady-state inbound hit fan-out.

## Error model

Every served response is the rendering of one serve outcome. A small type (`ServeDecision` in
`Ecluse.Core.Server.Response`) maps each outcome to the right status rather than collapsing
everything into a generic 403 or 500. For a concrete artifact request (one specific version)
the decision renders directly:

| Outcome | Status |
|---|---|
| Admit | `200` (streamed) |
| Policy denial (incl. deny-by-default) | `403` + denial body |
| Undecidable, transient | `503` + `Retry-After` |
| Undecidable, permanent | `500` |
| upstream miss | `404` (forwarded) |

The rule: `503` only when the condition is believed to resolve (a transient upstream or
advisory condition); otherwise `500`, since retrying a permanent inability to decide can't help.

A packument request has no single status: the document is
[merged across upstreams](registry-model.md#packument-merge-across-upstreams) and filtered by
provenance (see [Applying verdicts](rules-engine.md#applying-verdicts-to-a-packument)). A status
is chosen only when nothing survives the merge, by the most recoverable cause: `503` if any
rejection was transient or a needed upstream was unavailable; else `502` if a responding
upstream returned an invalid response (a packument whose self-reported name is for a different
package, see
[name validation](registry-model.md#the-route-name-is-the-served-names-validation-authority));
`500` if none is retryable but an exclusion is a permanent inability; else `403`. Never `404`:
the versions existed and were withheld, and a genuinely absent package is a separate upstream
miss. (`packumentStatus` in `Ecluse.Core.Server.Response` is the counterpart of
`artifactStatus`.)

The serve-outcome model decides an error's status but holds no body shape of its own. An
ecosystem's route contract supplies the matching response constructor and codec (npm's
`{"error": …}` object in `Ecluse.Core.Registry.Npm.Serve`). A request matching no mount is a
neutral `404 Not Found` in `text/plain`. The denial-body shape and `ECLUSE_SERVER__HELP_MESSAGE`
handling are in [Rules engine → denial responses](rules-engine.md#denial-responses).

### The typed request perimeter

The pipeline reports every routine failure as a value, so an exception reaching the web layer
is an escape from some dependency's typed contract. Each effectful route runs under a perimeter
(`perimeterGuard` in `Ecluse.Runtime.Server`). Pre-commit (nothing written to the client), the
escape is classified into the closed `RequestFault` vocabulary (`Ecluse.Core.Server.Fault`),
counted on `ecluse.serve.perimeter.faults`, logged with an audit payload, and answered with the
route's declared neutral `500`; no fault detail reaches a client. Post-commit (the response has
begun), there's no second response to give, so the escape rethrows: warp tears the connection
down and the `scOnException` hook records it, filtered through `defaultShouldDisplayException` so
routine client disconnects stay quiet. Asynchronous exceptions are never caught: cancellation
tears a request down like any thread. The perimeter is one of the two outer edges of the
system-wide [fault model](fault-model.md), which owns the disposition vocabulary this section
renders over HTTP.

## Middleware and helper libraries

The dividing principle: adopt libraries for cross-cutting infrastructure identical for every
service; hand-roll anything that encodes the domain or wire contract.

Écluse composes `wai-extra` middleware around the `Application`: `RequestSizeLimit` (a defensive
body cap), `RealIp`/`ForwardedFor` (correct client IP behind a load balancer), and `Timeout`.
It deliberately avoids two: `Autohead`, which answers HEAD by running the GET handler and
discarding the body (see [HEAD on artifacts](#head-on-artifacts)), and `Gzip`, since artifacts
are already compressed and re-compressing would fight the backpressure above. `unliftio` lifts
`bracket`/`finally`/`async` into the reader so resource-safety stays ergonomic. Handlers run in
`Handler`, a reader over a per-request `RequestCtx` that pairs the shared `ServeRuntime`
(`ctxRuntime`: data-plane managers, caches, queue, recording ports) with the matched mount's
binding (`ctxMount`); shared runtime state lives in `ServeRuntime`, not a `StateT` layer.

The router (`routerOf`/`matchRoute`), the response and error helpers, a thin `katip` logging
middleware, and conditional-GET / ETag handling are hand-rolled. For pass-through bodies
(artifacts) the client's validators are relayed upstream and `304`s passed back unchanged. For
transformed bodies (every packument, merged and filtered) the served body differs from any
single upstream's, so Écluse serves its own `ETag`, derived from the serve's inputs (the origin
bodies' digests, the surviving version sets, the mount base URL). It can never call a changed
document unchanged, though it may change spuriously (a harmless extra `200`, never a wrong
`304`), and deriving it from inputs lets a `304` skip assembly entirely. Routing libraries
(`wai-routes`, `wai-routing`) are declined: segment-based, they'd fight the encoded-slash
handling the small pure router gets right.

## Graceful shutdown

A rolling deploy or pod eviction takes an instance down while clients and the load balancer
still point at it. On `SIGTERM`/`SIGINT` Écluse runs a full graceful drain so in-flight work
isn't cut off:

1. Readiness flips, liveness holds. `GET /readyz` returns `503` while `GET /livez` stays `200`.
   The readiness `503` is the signal a load balancer or mesh watches to stop routing new traffic
   here; liveness stays green because a draining instance is alive and finishing work, so an
   orchestrator must not kill it early.
2. Going-away header. While draining, every response carries `Connection: close`, so a
   keep-alive pool closes the socket after the response and the next request opens a fresh
   connection the mesh routes to a ready instance.
3. Drain, then exit. The instance stops accepting new connections and waits for in-flight
   requests and in-progress artifact streams to finish (a half-delivered tarball runs to
   completion), bounded by `ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT` (default 30 seconds), then
   exits regardless so a stuck request can't pin the old instance open.

Set the platform's termination grace period longer than `ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT`
so the orchestrator doesn't `SIGKILL` mid-drain (for example a Kubernetes
`terminationGracePeriodSeconds` comfortably above it). When Écluse is attached to an interactive
terminal, a second `Ctrl+C` (or `Ctrl+D`) forces an immediate halt that bypasses the drain;
this is gated on standard input being a TTY, so production has no such bypass.
