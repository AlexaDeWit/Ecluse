# Web layer

> Part of the [Écluse architecture overview](../architecture.md).

The front door is a raw `wai` `Application` served by `warp`. It routes a request, streams artifacts
with bounded memory, and applies cross-cutting concerns as middleware.

## Raw WAI, not a web framework

A proxy is a passthrough over a small, irregular URL surface: npm paths carry URL-encoded slashes
(`/@scope%2Fpkg`, `/pkg/-/pkg-1.0.0.tgz`) and reserved meta-routes
(`/-/npm/v1/security/advisories/bulk`). Matching on `pathInfo` in a raw `Application` is simpler and
more flexible than encoding that shape at the type level (servant) or adopting a framework whose
response handling hides the streaming control we depend on (see
[Streaming](#streaming-and-resource-lifetime)).

Routing sits in two layers. Mount dispatch (see [Multi-ecosystem mounts](#multi-ecosystem-mounts))
matches the leading path segment to a mount and strips the prefix; what remains is an ecosystem-native
path, handed to that mount's **router**.

### The route table belongs to the ecosystem

A route is an ecosystem's own concern. npm's `/{pkg}/-/{file}.tgz` and RubyGems' whole-registry
`/versions` have nothing in common but the fact that *something must be done about them*. So each
ecosystem's adapter declares its routes as an ordered table of **route patterns** (literal segments,
and named captures that carry their own parsers), and that one declaration is interpreted twice:

- the **runtime** interpretation is the mount's `MountRouter`, which the server dispatches through;
- the **manifest** interpretation is a `RouteSpec` per route, which the
  [capability manifest](#capability-manifest) renders.

Two readings of one table cannot drift apart, so the documented surface cannot lie about the routed
one.

What the web layer shares across ecosystems is not the routes but the **kind of action** a route can
name:

```haskell
type MountRouter = Method -> [Text] -> RouteAction

data RouteAction
  = AnswerLocally (MountRenderer -> Response)                       -- pure, no upstream
  | RunPipeline   (Request -> Respond -> Handler ResponseReceived)  -- the data plane
```

The data-plane handlers (`Ecluse.Core.Server.Pipeline`) are themselves ecosystem-neutral: a registry's
metadata client, packument assembly, and artifact-request formation reach them as injected
capabilities on `PackumentDeps`, never as imports. So an ecosystem routes its own URLs onto whichever
shared handlers apply, and names its own actions for the routes that have no counterpart. **Each
adapter's table is total over its own routes**, which means a branch for a route the ecosystem cannot
receive is unrepresentable rather than merely discouraged.

The web layer therefore holds no route knowledge at all. It asks the matched mount's router for an
action and either responds with it or runs it under the [perimeter](#the-typed-request-perimeter).
Adding an ecosystem adds a router and changes nothing in `Ecluse.Runtime.Server`.

### npm's table

Three npm-specific facts it encodes: `pathInfo` splits on `/` *before* percent-decoding, so an encoded
scoped name (`/@scope%2Fpkg`) arrives as one segment while a bare scope arrives as two, and both
normalise to the same `PackageName`; reserved meta-routes (`/-/…`) are matched first, since a package
name is never a lone `-`; and a tarball's file name must parse for *its own* package, so a name
addressing another package's artifact is refused rather than fabricated into a coordinate.

Only `GET`, `HEAD`, and `PUT` are answered (`PUT /{pkg}` is the publish). A `HEAD` is a **bodiless
variation** of its `GET` rather than a distinct action, so npm's table classifies it identically and
the router selects the head-mode handler; that is load-bearing on the artifact path, where running the
`GET` handler and discarding the body would stream a whole artifact to nowhere (see
[HEAD on artifacts](#head-on-artifacts)). Any other method, and anything unrecognised, is a `404`, so
deny-by-default holds at the routing layer for methods as well as paths.

Keeping the table pure makes it unit-testable with no server: feed it a method and segments, assert
the route.

## Multi-ecosystem mounts

A single Écluse process serves one or more ecosystems from one listener by **mounting** each registry
under a path prefix on a shared base URL:

```
https://registry.internal.example.com/npm    → npm mount
https://registry.internal.example.com/pypi   → PyPI mount
```

There is exactly one mount per ecosystem, and its path prefix is derived from the ecosystem, not
configured (npm → `/npm`, PyPI → `/pypi`), so a prefix can neither collide nor be mistyped. Every
registry is path-mounted; none sits at `/`, so adding an ecosystem later never changes an existing
consumer's URLs. A mount binds, as one unit, the ecosystem's registered capability record
(the `RegistryAdapter` in `Ecluse.Core.Registry.Adapter`, resolved by ecosystem once at
boot): its serve surface (its [router](#the-route-table-belongs-to-the-ecosystem) and the
error renderer, the client-facing denial/error surface, so the agnostic layer holds no
ecosystem body shape; see
[Error model](#error-model)), its four [registry roles](registry-model.md#registry-roles)
over the [protocol boundary](registry-model.md#registry-abstraction), and an optional
per-ecosystem [rule refinement](configuration.md#rule-policy) that merges over the shared
policy. The single-npm
setup is the degenerate case, still under its own derived prefix.

**URL rewriting is load-bearing.** Registry responses embed absolute artifact locations (npm's
`dist.tarball`; on public PyPI, file URLs on a separate host). Forwarded unchanged, a client would
resolve metadata through the proxy but download bytes directly from upstream, bypassing the gate. So
a mount rewrites embedded artifact URLs to stay under its own prefix (npm's `dist.tarball` →
`{mount-base}/{pkg}/-/{file}`) before serving metadata. Keeping artifacts on the same host has a
second benefit: npm attaches credentials only to requests on the registry host, so same-host artifact
URLs keep auth flowing on tarball fetches a separate host would drop. Because rewriting emits absolute
URLs and header inference is unreliable behind load balancers and TLS terminators, a mount must know
its own externally-visible base URL as explicit per-mount configuration.

## Meta-routes: ping, health, and search

- **`/-/ping`**, answered locally with `200` (`{}`). `npm ping` checks that the endpoint it talks to
  (the proxy) is up, so there is no reason to round-trip upstream.
- **Liveness / readiness** (for orchestration, e.g. `/livez`, `/readyz`), kept distinct. Liveness is
  "the process is responsive," and in single-process mode also reflects the mirror worker's
  consume-loop heartbeat (a stalled worker fails liveness; see
  [Process model](cloud-backends.md#process-model-the-unified-multicall-binary)). Readiness is "config loaded and the listener is
  serving"; it is deliberately lenient about public-upstream reachability, since the proxy still
  serves private hits when public is down, so readiness must not flap on an upstream blip and pull a
  healthy pod from rotation.
- **Search (`/-/v1/search`)**, not supported at launch. A discovery convenience, not an install path,
  so the route returns `501 Not Implemented` with a short message pointing to the public registry's
  website rather than scope-creeping a filtered or passthrough search now.

Everything else unrecognised stays `Unsupported` → `404`.

## Capability manifest

Écluse publishes a capability manifest: an OpenAPI 3 document, generated at build time and published to
the docs site (not served, no `GET /openapi.json` route). It is rendered from each mounted adapter's
declarative route table (`serveRoutes`, a `RouteSpec` per served route), the *same* table that
ecosystem's [router](#the-route-table-belongs-to-the-ecosystem) dispatches on, across the configured
[mounts](#multi-ecosystem-mounts). A correspondence test holds the documented paths and methods
against the live routing, so the manifest cannot drift from what the server serves. The full rationale, schema strategy, and publish pipeline are
the canonical [API Surface & Capability Manifest](api-surface.md).

## Control plane vs. data plane

The most important split in the HTTP code:

- **Data plane**, streaming artifacts and fetching metadata, goes through `http-client`.
- **Control plane**, SQS (mirror queue), STS, and CodeArtifact's `GetAuthorizationToken` (the AWS
  [`CredentialProvider`](cloud-backends.md#credential-provider)'s `mintToken`), goes through
  `amazonka`.

This matters most for CodeArtifact: its npm repository is a standard HTTPS npm endpoint, so Écluse
obtains a bearer token from `GetAuthorizationToken` (control plane, `amazonka`) then fetches
packuments and tarballs with ordinary `http-client` (data plane). The streaming path therefore never
touches `amazonka`'s conduit/`ResourceT` machinery, exactly where naive streaming-through-a-proxy
goes wrong. The same split holds on GCP (Pub/Sub and the Artifact Registry token are control-plane,
the npm data plane is unchanged `http-client`; see [Cloud Backends](cloud-backends.md#cloud-backends)).

On the data plane, credential handling follows the mount's
[credential strategy](access-model.md): under `passthrough` the client's `Authorization` is forwarded
to the private upstream; under `service` the private fetch uses Écluse's own `CredentialProvider`
token. The client's `Authorization` is always stripped before any public-upstream fetch.

## Streaming and resource lifetime

A WAI streaming response body runs *after* the handler returns, Warp serialises it while writing to
the socket. So a resource with lexical scope (`bracket`, `withResponse`, `runResourceT`) released
when the handler returns is already gone by the time the body streams: a use-after-free / GC race.
This is why frameworks that hide the response continuation make memory-bounded artifact streaming
awkward.

Raw WAI avoids it by construction. `Application` is continuation-passing, *you* call `respond`, so the
resource acquisition can bracket the `respond` call itself:

```haskell
serveArtifact mgr upstreamReq respond =
  withResponse upstreamReq mgr $ \up ->            -- upstream connection acquired
    respond $ responseStream status200 (relayHeaders up) $ \write flush -> do
      let pump = do
            chunk <- brRead (responseBody up)
            unless (BS.null chunk) (write (byteString chunk) >> pump)
      first <- brRead (responseBody up)
      unless (BS.null first) $ do
        write (byteString first)
        flush                                       -- first byte out promptly
        pump                                        -- closed only after Warp returns
```

The upstream connection lives for exactly the duration of the streamed body and is closed only when
Warp returns `ResponseReceived`. `write` fills Warp's bounded output buffer and blocks on the socket
send when it spills, so we pull from upstream only as fast as the client drains: constant memory
regardless of artifact size, with backpressure for free. Only the first chunk is explicitly flushed
(prompt first byte); later chunks coalesce in the output buffer. No `ResourceT`, no conduit on the hot
path.

**Integrity on the serve path.** The proxy streams artifacts through without hashing them, relying on
the client's own integrity check against the packument's `dist.integrity`, which the proxy preserves
unaltered when [filtering](rules-engine.md#applying-verdicts-to-a-packument) (npm always verifies it).
Proxy-side serve verification is deferred until a weakly-verifying ecosystem (e.g. PyPI) or a
non-verifying client lands. The mirror worker does verify before publishing to the sanitised home (see
[Mirror Queue](cloud-backends.md#mirror-queue)).

### HEAD on artifacts

A `HEAD` must never run the full-`GET` streaming pump: a bodiless `HEAD` would open the upstream
connection and pump a whole body Warp then discards, wasted egress and a DoS-amplification lever (cheap
`HEAD`s forcing arbitrary full-artifact upstream fetches). This is why the
[`Autohead` middleware](#middleware-and-helper-libraries) is not used; `HEAD` is handled explicitly in
dispatch.

On the tarball route a `HEAD` runs the identical gating and upstream-request construction as `GET`
(edge auth, host allowlist and internal-range block, the same-host `dist.tarball` policy, the
[origin trust split](access-model.md), honoured-tarball-host resolution) but issues the upstream
request as a `HEAD` and relays its status and safe headers (`Content-Type`, `Content-Length`, `ETag`,
`Last-Modified`, `Accept-Ranges`) with no body. A private hit probes the private upstream, a private
miss falls through to the public origin, every refusal renders the same serve-outcome status with an
empty body, and a `HEAD` admit enqueues no mirror job.

The packument route works the same way, running the identical pipeline and gating as the packument
`GET` (so a `HEAD` returns the status a `GET` would) and emitting the same headers, including the
`Content-Length` of the would-be merged body and the own `ETag` (a conditional `HEAD` answers a
bodiless `304`). Here the defence is only HTTP correctness (a `HEAD` reply must carry no body), not DoS
amplification, since a packument is assembled locally with no artifact egress. A `HEAD` still
materialises the merged body to size `Content-Length`; a `304` (either method) is answered off the
derived validator without assembling at all.

## Metadata cache

Resolving a package re-fetches its upstream packument(s), parses them, and evaluates rules. To avoid
repeating that, the parsed packument metadata (all versions' `PackageDetails`) is held in a short-TTL,
size-bounded, STM-backed in-memory cache keyed by package (the `cache` library). A packument request
and the [tarball-gating](../architecture.md#request-lifecycle) fetches that follow share it, and
concurrent resolutions of a popular package collapse to one upstream call.

What is cached is the metadata, not the verdict: rules are re-evaluated each request, so
time-sensitive rules (`AllowIfOlderThan`) stay correct; only each upstream's fetch+parse is memoised
(per source, since a packument is
[merged across upstreams](registry-model.md#packument-merge-across-upstreams)), while the merge,
filter, and `latest` repoint are recomputed. The one memoisation past that point is the
**assembled-representation store**: the encoded merged document keyed by its derived validator, a
content address over every serve input, so a recurring (public entry, private content, plan) triple
serves stored bytes with no re-assembly and can never be stale (changed inputs miss by key). The TTL
is short and conditional-GET revalidates on expiry (see [Middleware](#middleware-and-helper-libraries));
brief staleness is benign. This is in-memory metadata only; on-disk artifact caching stays out of
scope, and the mirror remains the durable store.

The cache holds the anonymous public (gated) origin only, under every strategy; the private origin is
never cached but read per request. Écluse [forbids a shared private cache](access-model.md#caching)
outright, so no client's private view can leak to another within the TTL; the anonymous public origin
crosses no trust boundary and is cached freely.

## Serve admission and upstream pools

The packument path and a tarball miss's public-metadata gate share one process-wide, brief-wait
admission bound. At most `ECLUSE_SERVE_MAX_IN_FLIGHT` metadata materialisations run at once; work
finding the cap busy waits briefly for a slot in a waiting room bounded at the capacity (wait budget
equal to the shed path's `Retry-After: 1` hint), and a newcomer never jumps a non-empty room. Only a
request that finds the room full or waits out its budget gets the mount's error shape as `503 Service
Unavailable` with `Retry-After: 1`. So the bound holds by construction: resident parse structures
never exceed the cap, waiting memory never exceeds one blocked green thread per room place, and a burst
that merely brushes the cap degrades into short queueing delay instead of a refusal the client
immediately retries (instant shedding is self-amplifying: the refusal work competes for the cores the
admitted work needs). Health probes, cheap local routes, and trusted private tarball hits bypass
admission (the hit is already constant-memory, and holding a metadata slot for a slow download would
let clients starve packument traffic without protecting any parse structure).

The public and private `http-client` managers have independently configurable per-host pools, both
defaulting to a share of the process file-descriptor limit (each pooled connection is one descriptor)
rather than following `ECLUSE_SERVE_MAX_IN_FLIGHT`. The private pool takes the larger share because a
trusted tarball hit streams outside admission, so its demand is the steady-state inbound hit fan-out,
not the admission capacity. The public pool takes half that share: its metadata misses are
single-flight-coalesced and admission-bounded, but the onboarding fail-over's artifact streams and the
worker's back-fill fetches ride the same manager without coalescing. A larger pool never opens more
sockets than concurrency demands; it only governs how many are kept for reuse.

## Error model

Every served response is the rendering of one serve outcome. A small type maps each to the right
status rather than collapsing everything into a generic 403/500:

```haskell
data ServeDecision = Admit | Reject Rejection

data Rejection = Rejection
  { reason  :: RejectReason
  , message :: Text            -- intuitive, client-facing
  }

data RejectReason
  = ByPolicy RuleName          -- a rule denied it (incl. deny-by-default)
  | Unavailable Transience     -- could not be decided (see Rules Engine)
  | MissingIntegrity           -- public version carries no integrity digest (admission)
  | UpstreamInvalid            -- a responding upstream returned a packument for a different package

data Transience
  = WillResolve (Maybe RetryAfter)  -- transient: upstream 5xx/429, advisory source down
  | WontResolve                     -- not expected to self-heal (internal/parse error)
```

`RetryAfter` is a `newtype` over a whole-seconds `Int` (it becomes the `Retry-After` header), and a
`ByPolicy` carries a `RuleName` newtype rather than a bare string.

For a concrete artifact request (one specific version) the decision renders directly:

| Outcome | Status |
|---|---|
| `Admit` | `200` (streamed) |
| `Reject (ByPolicy …)` | `403` + denial body |
| `Reject (Unavailable (WillResolve ra))` | `503` + `Retry-After` |
| `Reject (Unavailable WontResolve)` | `500` |
| upstream miss | `404` (forwarded) |

The rule: `503` only when we believe it will resolve (a transient upstream or advisory condition);
otherwise `500`, since retrying a permanent/internal inability to decide cannot help.

For a packument request there is no single status: the document is
[merged across upstreams](registry-model.md#packument-merge-across-upstreams) and filtered by
provenance (see [Applying verdicts](rules-engine.md#applying-verdicts-to-a-packument)). A status is
chosen only when nothing survives the merge, by the most recoverable cause: `503` if any rejection was
`WillResolve` or a needed upstream was unavailable; else `502` if a responding upstream returned an
invalid response (a packument whose self-reported name is for a different package, see
[name validation](registry-model.md#the-route-name-is-the-served-names-validation-authority)); `500`
if none is retryable but an exclusion is a permanent inability (`WontResolve`); else `403`. Never
`404`: the versions existed and were withheld; a genuinely absent package is a separate upstream miss.
(`packumentStatus` in `Ecluse.Core.Server.Response` is the counterpart of `artifactStatus`.)

The serve-outcome model and status mapping live in `Ecluse.Core.Server.Response`, which decides an
error's status but holds no body shape of its own. Each mount supplies a `MountRenderer` that shapes
the error bytes in its ecosystem's surface (npm's `{"error": …}` object in
`Ecluse.Core.Registry.Npm.Serve`), so rendering splits into two tiers: a request matching no mount is
a neutral `404 Not Found` in `text/plain`, while every in-mount error renders through that mount's
renderer. The denial-body shape and `ECLUSE_HELP_MESSAGE` handling are in
[Rules Engine → Denial responses](rules-engine.md#denial-responses).

### The typed request perimeter

The pipeline reports every routine failure as a value, so an exception reaching the web layer is an
escape from some dependency's typed contract. Each effectful route runs under a perimeter
(`perimeterGuard` in `Ecluse.Runtime.Server`) that makes the disposition explicit rather than
leaving it to warp's defaults:

- **Pre-commit** (nothing has been written to the client): the escape is classified into the closed
  `RequestFault` vocabulary (`Ecluse.Core.Server.Fault`) -- a recognised wiring/contract fault is a
  `GateFault`, an escape from the response-assembly leg (wrapped in the confined `RenderEscape`
  marker where the assembled render runs) is a `RenderFault`, anything else `UnclassifiedFault` --
  counted on `ecluse.serve.perimeter.faults`, logged with an audit payload (path, cause, bounded
  detail), and answered with the mount-shaped neutral `500`. No fault detail ever reaches a client.
- **Post-commit** (the response has begun, tracked by the perimeter's respond wrapper): there is no
  second response to give, so the escape rethrows; warp tears the connection down and the
  `scOnException` hook records it through the structured logger (filtered through
  `defaultShouldDisplayException`, so routine client disconnects stay quiet).

Asynchronous exceptions are never caught: cancellation tears a request down like any thread. Warp's
own `setOnExceptionResponse` remains only as the neutral-body backstop for faults with no mount
context (middleware, warp itself). The perimeter is one of the two outer edges of the system-wide
[fault model](fault-model.md), which owns the disposition vocabulary this section uses.

## Middleware and helper libraries

The dividing principle: adopt libraries for cross-cutting infrastructure identical for every service;
hand-roll anything that encodes our domain or wire contract.

- **Adopt `wai-extra` middleware** (already a dependency): `RequestSizeLimit` (defensive body cap),
  `RealIp`/`ForwardedFor` (correct client IP behind a load balancer), and `Timeout`, composed around
  the `Application`. Two it deliberately avoids: `Autohead` (it answers HEAD by running the GET handler
  and discarding the body, which on a tarball route would stream a whole artifact to nowhere; HEAD is
  handled explicitly in dispatch, see [HEAD on artifacts](#head-on-artifacts)), and `Gzip` (artifacts
  are already compressed, and re-compressing would fight the backpressure above).
- **Adopt `unliftio`** for the whole shell, where `ReaderT Env IO` runs: it lifts
  `bracket`/`finally`/`async` into the reader so resource-safety stays ergonomic. Handlers run in the
  reader over a per-request `RequestCtx` pairing `Env` with the matched mount's
  [`MountBinding`](#multi-ecosystem-mounts); shared mutable state lives as `TVar`s in `Env`, not a
  `StateT` layer.
- **Hand-roll** the router (`classify`), the response/error helpers (`Ecluse.Core.Server.Response`;
  each mount's error surface in its adapter, e.g. `Ecluse.Core.Registry.Npm.Serve`), a thin `katip`
  logging middleware (so request logs join the same structured stream), and conditional-GET / ETag
  handling. For pass-through bodies (artifacts, including a private-upstream tarball) the client's
  validators are relayed upstream and `304`s passed back unchanged; for transformed bodies (every
  packument, [merged](registry-model.md#packument-merge-across-upstreams) and filtered) the served body
  differs from any single upstream's, so Écluse serves its own `ETag` and answers conditional requests
  against it. The own-ETag is derived from the serve's inputs (the origin bodies' digests hashed once
  at fetch, the per-source surviving version sets, the mount base URL), of which the served document is
  a deterministic function: it can never call a changed document unchanged, though it may change
  spuriously (a harmless extra `200`, never a wrong `304`). Deriving it from inputs lets a `304` skip
  assembly, encode, and output hashing entirely, and lets a `200` stream without materialising the body
  for a hash pass.
- **Decline** routing libraries (`wai-routes`, `wai-routing`): largely dormant and segment-based, so
  they fight the encoded-slash handling a small pure `classify` gets right.
- **Defer** `http-reverse-proxy` (revisit only if the hand-rolled core starts reinventing it; the need
  to synthesise denial responses argues against a transparent proxy), the tracing/metrics middleware
  (now in [Observability](observability.md), deferred until it lands), and `warp-tls` (only if TLS is
  not terminated upstream).

## Graceful shutdown

A rolling deploy or pod eviction takes an instance down while clients and the load balancer still
point at it. On `SIGTERM`/`SIGINT` Écluse runs a full graceful drain so in-flight work is not cut off:

1. **Readiness flips, liveness holds.** The instance enters a draining state where `GET /readyz`
   returns `503` while `GET /livez` stays `200`. The readiness `503` is the signal a load balancer or
   mesh watches to stop routing new traffic here; liveness stays green because a draining instance is
   alive and finishing work, so an orchestrator must not kill it early.
2. **Going-away header.** While draining, every response carries `Connection: close`, so a keep-alive
   pool closes the socket after the response and the next request opens a fresh connection the mesh
   routes to a ready instance, rather than landing on a closing process.
3. **Drain, then exit.** The instance stops accepting new connections and waits for in-flight requests
   and in-progress artifact streams to finish (a half-delivered tarball runs to completion), bounded
   by `ECLUSE_SHUTDOWN_DRAIN_TIMEOUT` (default 30 seconds), after which it exits regardless so a stuck
   request cannot pin the old instance open.

Set the platform's termination grace period longer than `ECLUSE_SHUTDOWN_DRAIN_TIMEOUT` so the
orchestrator does not `SIGKILL` mid-drain (e.g. a Kubernetes `terminationGracePeriodSeconds`
comfortably above it). When Écluse is attached to an interactive terminal, a second `Ctrl+C` (or
`Ctrl+D`) forces an immediate halt that bypasses the drain; this is gated on standard input being a
TTY, so production has no such bypass.
