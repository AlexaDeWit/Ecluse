# Web Layer

> Part of the [Écluse architecture overview](../architecture.md).

The front door is a raw `wai` `Application` served by `warp`. It does three
jobs: route an incoming request, stream artifacts through with bounded memory,
and apply cross-cutting concerns as middleware. Three decisions shape it.

## Raw WAI, not a web framework

A proxy is fundamentally a passthrough over a small, irregular URL surface, npm
paths carry URL-encoded slashes (`/@scope%2Fpkg`, `/pkg/-/pkg-1.0.0.tgz`) and
reserved meta-routes (`/-/npm/v1/security/advisories/bulk`). Matching on
`pathInfo` in a raw `Application` is simpler and more flexible than encoding that
shape at the type level (servant) or adopting a framework whose response
handling hides the streaming control we depend on (see
[Streaming](#streaming-and-resource-lifetime)).

Routing sits in two layers. **Mount dispatch** (see [Dispatch](hosting.md#dispatch))
matches the leading path segment to a mount and strips the prefix. What remains
is an ecosystem-native path, classified by a **pure** function into a small route
type:

```haskell
data Route = Packument PackageName | Tarball PackageName Text | Ping | Search | Unsupported

classify :: [Text] -> Route
```

Keeping `classify` pure makes the whole routing table unit-testable with no
server, feed it `pathInfo`, assert the `Route`. Two npm-specific facts it must
encode: `pathInfo` splits on `/` *before* percent-decoding, so an encoded scoped
name (`/@scope%2Fpkg`) arrives as a single segment; and reserved meta-routes
(`/-/…`) are matched first, since a real package name can never begin with `-`.
Anything unrecognised is `Unsupported` → 404, so deny-by-default holds at the
routing layer too.

## Meta-routes: ping, health, and search

Beyond packuments and artifacts, a few non-package routes:

- **`/-/ping`**, answered **locally** with `200` (`{}`). `npm ping` checks that the
  registry endpoint it talks to, the proxy, is up, so there is no reason to
  round-trip upstream.
- **Liveness / readiness** (for orchestration, e.g. `/livez`, `/readyz`), kept
  distinct. *Liveness* is "the process is responsive," and in single-process mode
  also reflects the mirror worker's consume-loop heartbeat (a stalled worker fails
  liveness; see [Process model](cloud-backends.md#process-model)). *Readiness* is
  "config loaded and the listener is serving"; it is deliberately **lenient about
  public-upstream reachability**, the proxy still serves private-upstream hits
  when public is down, so readiness must not flap on an upstream blip and pull a
  healthy pod from rotation.
- **Search (`/-/v1/search`)**, **not supported at launch.** It is a discovery
  convenience, not an install path, so rather than scope-creep a filtered or
  passthrough search now, the route returns `501 Not Implemented` with a short
  message pointing users to the public registry's website. Revisit only if demand
  warrants.

Everything else unrecognised stays `Unsupported` → `404` (deny-by-default at the
routing layer).

## Capability manifest

Beyond the per-request routes, Écluse **publishes** a **capability manifest**, an
OpenAPI 3 document stating which registry protocols this server speaks and what is
/ isn't supported. It is **statically generated at build time** (from the closed
`Route` enumeration above × the configured
[mounts](hosting.md#capability-manifest)) and published to the docs site, **it is
not served; there is no `GET /openapi.json` route or any WAI wiring**. Generating it
from the same `Route` enumeration the server routes on means it cannot drift from
what the server actually routes (`Search` shows as `501`, tarballs as opaque
streamed media), and the rendered docs group operations by ecosystem. The
synthesised (merged + filtered) packument is an **owned** schema. The full
rationale, the schema strategy (`autodocodec` + `openapi3`), and the build-time
generation + CI publish (static Redoc on GitHub Pages, node-free) are in
[API Surface & Capability Manifest](api-surface.md).

## Control plane vs. data plane

The single most important split in the HTTP code:

- **Data plane**, streaming artifacts and fetching metadata, goes through
  `http-client`.
- **Control plane**, SQS (mirror queue), STS, and CodeArtifact's
  `GetAuthorizationToken` (the AWS
  [`CredentialProvider`](cloud-backends.md#credential-provider)'s `mintToken`),  goes through `amazonka`.

This matters most for CodeArtifact. Its npm repository is a **standard HTTPS npm
endpoint**: obtain a bearer token from `GetAuthorizationToken` (control plane,
`amazonka`), then fetch packuments and tarballs with ordinary `http-client` (data
plane). The streaming path therefore never touches `amazonka`'s
conduit/`ResourceT` machinery, which is exactly where naive
streaming-through-a-proxy goes wrong.

The same split holds on GCP, Pub/Sub and the Artifact Registry token are
control-plane work, while the npm data plane is unchanged `http-client` (see
[Cloud Backends](cloud-backends.md#cloud-backends)).

On the data plane, credential handling follows the mount's
[credential strategy](access-model.md): under the default `passthrough` the client's
`Authorization` is **forwarded to the private upstream**; under `service` the private fetch uses Écluse's own
[`CredentialProvider`](cloud-backends.md#credential-provider) token instead. The
client's `Authorization` is **always stripped before any public-upstream fetch**, an internal token must never leave for the public registry, regardless of strategy.

## Streaming and resource lifetime

A WAI streaming response body **runs after the handler returns**, Warp
serialises it while writing to the socket. So a resource with lexical scope
(`bracket`, `withResponse`, `runResourceT`) released when the handler returns is
already gone by the time the body streams: a use-after-free / GC race. This is
the classic trap, and it is why frameworks that hide the response continuation
make memory-bounded artifact streaming awkward.

Raw WAI avoids it by construction. `Application` is continuation-passing, *you*
call `respond`, so the resource acquisition can bracket the `respond` call
itself:

```haskell
serveArtifact mgr upstreamReq respond =
  withResponse upstreamReq mgr $ \up ->            -- upstream connection acquired
    respond $ responseStream status200 (relayHeaders up) $ \write flush -> do
      let pump = do
            chunk <- brRead (responseBody up)
            unless (BS.null chunk) (write (byteString chunk) >> flush >> pump)
      pump                                          -- closed only after Warp returns
```

The upstream connection lives for exactly the duration of the streamed body and
is closed only when Warp returns `ResponseReceived`. `write` blocks on the socket
send buffer, so we pull from upstream only as fast as the client drains,**constant memory regardless of artifact size**, with backpressure for free. No
`ResourceT`, no conduit on the hot path.

**Integrity on the serve path.** The proxy streams artifacts through without
hashing them, relying on the client's own integrity check, the packument's
`dist.integrity`, which the proxy preserves unaltered when
[filtering](rules-engine.md#applying-verdicts-to-a-packument) (npm always verifies
it). Proxy-side serve verification is deferred; revisit it when a weakly-verifying
ecosystem (e.g. PyPI) or a non-verifying client lands. The mirror **worker does
verify** before publishing to the sanitized home (see
[Cloud Backends → Mirror Queue](cloud-backends.md#mirror-queue)).

### HEAD on artifacts

A `HEAD` on the tarball route must **never** run the full-`GET` streaming pump above:
a bodiless `HEAD` would otherwise open the upstream artifact connection and pump a
whole artifact body that Warp then discards for the reply, wasted upstream egress
and a DoS-amplification lever (a client forcing arbitrary full-artifact upstream
fetches with cheap, bodiless `HEAD`s). This is exactly why the [`Autohead`
middleware](#middleware-and-helper-libraries) is *not* used.

`HEAD` is therefore handled **explicitly in dispatch**, not by re-running the `GET`
handler. The contract: a `HEAD` on the tarball route goes through the **identical
gating and upstream-request construction** as the `GET` path, edge authentication,
the host allowlist and internal-range block, the same-host `dist.tarball`
[tarball-host policy](#streaming-and-resource-lifetime), the trusted/untrusted
[origin trust split](access-model.md), and the honoured-tarball-host resolution, but
issues the upstream request as a **`HEAD`** and relays its status and safe response
headers (`Content-Type`, `Content-Length`, `ETag`, `Last-Modified`, `Accept-Ranges`
where present) with **no body**. It is the correct, non-amplifying reverse-proxy
behaviour: a private hit probes the private upstream as a `HEAD`, a private miss falls
through to a `HEAD` of the public origin exactly as the `GET` path falls through, and
every refusal (a policy `403`, a forwarded `404`, a transient `503`, an internal
`500`, the edge `401`) renders the same serve-outcome status with an **empty body**
(HTTP semantics: a `HEAD` reply carries no message body). A `HEAD` admit enqueues **no
mirror job**, mirroring stays demand-driven on the `GET` path, since a `HEAD` serves
no bytes to back-fill.

`HEAD` on the **packument** route is handled the same way, explicitly in dispatch, not
by re-running the `GET` handler. It runs the **identical pipeline and gating** as the
packument `GET` (the same fetch, cross-upstream merge, rule filter, and no-survivors
decision, so a `HEAD` that a `GET` would answer `403`/`503`/`502`/`500` returns that
same status) and emits the **identical status and headers**, including the
`Content-Length` of the would-be merged body and the **own `ETag`** the
conditional-request machinery computes (a conditional `HEAD` whose `If-None-Match`
matches answers a bodiless `304`, exactly as the `GET` would), with the body suppressed
by the same bodiless wrapper the tarball `HEAD` uses. What it defends differs from the
tarball case: a packument body is assembled **locally** (a metadata fetch plus the
merge), so answering it triggers **no artifact egress**, this is an HTTP-correctness
fix (a `HEAD` reply must carry no body), not the DoS-amplification control the tarball
`HEAD` closes. A `HEAD` alone still materialises the merged body, to size its
`Content-Length`; a `GET` streams the encoding straight to the socket, and a `304`
(either method) is answered off the derived validator without assembling at all.

## Metadata cache

Resolving a package re-fetches its upstream packument(s), parses them, and
evaluates rules. To avoid repeating that, the parsed **packument metadata** (all
versions' `PackageDetails`) is held in a **short-TTL, size-bounded in-memory
cache** keyed by package, an STM-backed TTL cache (the `cache` library). Both paths
share it: a packument request and the
[tarball-gating](../architecture.md#request-lifecycle) fetches that follow reuse a
single fetch+parse instead of repeating it, and concurrent resolutions of a
popular package collapse to one upstream call.

What is cached is the **metadata, not the verdict**: rules are re-evaluated on the
cached metadata each request, so time-sensitive rules (`AllowIfOlderThan`)
and the separately-cached advisory tier stay correct, only each upstream's
fetch+parse is memoised (per source, since a packument is
[merged across upstreams](registry-model.md#packument-merge-across-upstreams)); the
pure merge, filter, and `latest` repoint are recomputed each request. The one
memoisation past that point is the **assembled-representation store**: the encoded
merged document keyed by its derived validator, a content address over every serve
input, so a recurring (public entry, private content, plan) triple serves stored
bytes with no re-assembly or re-encode, can never be stale (changed inputs miss by
key), and never crosses a client boundary (a different private view is a different
key; the [access model](access-model.md#caching) carries the full argument). The
verdict itself is still never cached: the rules run first, and their outcome is part
of the key. The TTL is
short and **conditional-GET revalidates** on
expiry (see [Middleware](#middleware-and-helper-libraries)); brief staleness is
benign and even aligned with the resilience posture, a brand-new publish need not
appear instantly. This is **in-memory metadata only**; on-disk artifact caching
stays out of scope, and the mirror remains the durable store.

The cache holds the **anonymous public (gated) origin only**, under **every** strategy:
the **private origin is never cached**. The private upstream is the per-client authority.
Under `passthrough`, each request is re-authorised with the client's own forwarded credential.
Under `service`, reads use Écluse's own identity behind the edge.
Because a cache key carries no credential dimension, a shared private entry would allow one client's document to be
served to another within the TTL, bypassing per-client authorisation. Écluse therefore
[forbids a shared private cache](access-model.md#why-écluse-never-caches-the-private-origin)
outright and reads the private origin **per request** (see
[the private upstream's metadata is never cached across clients](registry-model.md#the-private-upstreams-metadata-is-never-cached-across-clients)).
The public origin is anonymous under every strategy, so a single shared entry crosses no
trust boundary and is cached freely.

## Serve admission and upstream pools

The packument path and a tarball miss's public-metadata gate share one
process-wide, non-queuing admission bound. At most
`ECLUSE_SERVE_MAX_IN_FLIGHT` metadata materialisations may run at once; excess
work receives the mount's error shape as `503 Service Unavailable` with
`Retry-After: 1`. Refusal is immediate, so overload cannot turn into an
application backlog whose resident parse structures and latency grow with client
concurrency. Health probes and cheap local routes bypass admission. A trusted
private tarball hit also bypasses it: the artifact stream is already
constant-memory, and holding a metadata slot for a slow download would let clients
starve packument traffic without protecting any parse structure.

The public and private `http-client` managers have independently configurable
per-host pools. Public same-key misses are single-flight-coalesced, so the public
pool keeps the library's conservative per-host default. The private pool is sized
on a different basis: a trusted tarball hit **streams outside admission**, so the
pool's demand is the inbound hit fan-out rather than the admission capacity, and it
defaults to a share of the process file-descriptor limit (the pool's real physical
ceiling, since each pooled connection is one descriptor) rather than following
`ECLUSE_SERVE_MAX_IN_FLIGHT`. A larger pool never opens more sockets than the
concurrency already demands; it only governs how many are kept for reuse rather
than re-handshaked.

## Error model

Every served response is the rendering of one **serve outcome**. A small, nuanced
type keeps client-facing errors intuitive and maps each to the right status,
rather than collapsing everything into a generic 403/500:

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

`RetryAfter` is a `newtype` over a whole-seconds `Int` (it becomes the `Retry-After`
header), and a `ByPolicy` carries a `RuleName` newtype rather than a bare string.

For a **concrete artifact** request (one specific version) the decision renders
directly:

| Outcome | Status |
|---|---|
| `Admit` | `200` (streamed) |
| `Reject (ByPolicy …)` | `403` + denial body |
| `Reject (Unavailable (WillResolve ra))` | `503` + `Retry-After` |
| `Reject (Unavailable WontResolve)` | `500` |
| upstream miss | `404` (forwarded) |

The rule: **`503` only when we believe it will resolve** (a transient upstream or
advisory condition); otherwise `500`, retrying a permanent/internal inability to
decide cannot help, so we do not invite it.

For a **packument** request there is no single status, the document is
[merged across upstreams](registry-model.md#packument-merge-across-upstreams) and
filtered based on provenance (see
[Rules Engine → Applying verdicts](rules-engine.md#applying-verdicts-to-a-packument)). A status is chosen only
when *nothing* survives the merge, following the most recoverable cause: `503` if
any rejection was `WillResolve` **or a needed upstream was unavailable** (a retry
may yield survivors); else `502` if a responding upstream returned an **invalid
response**, a packument whose self-reported name is for a *different* package (see
[Registry Model → name validation](registry-model.md#the-route-name-is-the-served-names-validation-authority));
`500` if none is retryable but an exclusion is a permanent inability (`WontResolve`);
else `403`. Never `404`, the versions existed and were withheld; a genuinely absent
package is a separate upstream miss, distinct from the `502` a misreporting upstream
earns. (`packumentStatus` in `Ecluse.Core.Server.Response` is the code-level counterpart
of `artifactStatus`.)

The serve-outcome model and the per-outcome status mapping live in
`Ecluse.Core.Server.Response`, which decides an error's *status* but holds **no body
shape of its own**. Each mount supplies a `MountRenderer` that shapes the error
bytes in its ecosystem's surface, npm's `{"error": …}` object lives in
`Ecluse.Core.Registry.Npm.Serve`, so the agnostic layer carries no ecosystem body.
Rendering therefore splits into **two tiers**: a request matching **no mount** is a
neutral `404 Not Found` in `text/plain` (there is no ecosystem to render it), while
every in-mount error, a policy `403`, an unrecognised-path `404`, a `501`, renders
through that mount's renderer. The denial-body shape and `ECLUSE_HELP_MESSAGE`
handling are in [Rules Engine → Denial Responses](rules-engine.md#denial-responses).

## Middleware and helper libraries

The dividing principle: **adopt libraries for cross-cutting infrastructure that
is identical for every service; hand-roll anything that encodes our domain or
wire contract.**

- **Adopt, `wai-extra` middleware** (already a dependency): `RequestSizeLimit`
  (defensive body cap), `RealIp`/`ForwardedFor` (correct client IP behind a load
  balancer), and `Timeout`, composed around the `Application`. Two it
  deliberately does *not* use: `Autohead`, it answers HEAD by running the GET
  handler and discarding the body, which on a tarball route would open the
  upstream and stream a whole artifact to nowhere; a HEAD on the tarball or packument
  route is instead handled explicitly in dispatch (see [HEAD on artifacts](#head-on-artifacts));
  and `Gzip`, artifacts are already compressed, and re-compressing the stream
  would fight the backpressure above.
- **Adopt, `unliftio`** for the whole shell, where `ReaderT Env IO` runs: it lifts
  `bracket`/`finally`/`async` into the reader so resource-safety stays ergonomic.
  Request handlers run in the reader too, over a per-request `RequestCtx` pairing
  `Env` with the matched mount's [`MountBinding`](hosting.md#mounts), so per-mount
  deps are read from context rather than re-threaded; shared mutable state lives as
  `TVar`s in `Env`, not a `StateT` layer.
- **Hand-roll**, the router (`classify`), response/error helpers (the agnostic
  serve-outcome model and status mapping in `Ecluse.Core.Server.Response`; each mount's
  ecosystem error surface, npm's `{"error": …}` shape, in its adapter, e.g.
  `Ecluse.Core.Registry.Npm.Serve`), a thin `katip` logging middleware (so request logs join
  the same structured stream as everything else, rather than `wai-extra`'s stock
  logger), and conditional-GET / ETag handling: for **pass-through** bodies
  (artifacts, including a private-upstream tarball, served unfiltered) the
  client's validators are relayed upstream and `304`s passed back unchanged; for
  **transformed** bodies (every packument, now always
  [merged across upstreams](registry-model.md#packument-merge-across-upstreams) and
  filtered) the served body differs from any single upstream's, so we serve our
  **own** `ETag` and answer conditional requests against it,  relaying an upstream
  validator there would cache or validate the wrong bytes. The own-ETag is **derived
  from the serve's inputs** (the origin bodies' digests, hashed once at fetch; the
  per-source surviving version sets; the mount base URL), of which the served document
  is a deterministic function: it can never call a changed document unchanged, though
  it may change spuriously (a harmless extra `200`, never a wrong `304`). Deriving it
  from inputs is what lets a `304` skip the assembly, encode, and any output hashing
  entirely, and lets a `200` stream its body without materialising it for a hash
  pass, the revalidation traffic a CI fleet with restored npm caches generates is
  answered at the cost of the per-request private fetch alone.
- **Decline**, routing libraries (`wai-routes`, `wai-routing`, …): largely
  dormant and segment-based, so they fight the encoded-slash handling that a
  small pure `classify` gets right.
- **Defer**, `http-reverse-proxy` (revisit only if the hand-rolled core starts
  reinventing it; our need to intercept and *synthesise* denial responses argues
  against a transparent proxy), the tracing/metrics middleware (now specified in
  [Observability](observability.md), OpenTelemetry WAI instrumentation,  deferred until it lands), and `warp-tls` (only if TLS is not terminated
  upstream).
