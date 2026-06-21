# Web Layer

> Part of the [Écluse architecture overview](../architecture.md).

The front door is a raw `wai` `Application` served by `warp`. It does three
jobs: route an incoming request, stream artifacts through with bounded memory,
and apply cross-cutting concerns as middleware. Three decisions shape it.

## Raw WAI, not a web framework

A proxy is fundamentally a passthrough over a small, irregular URL surface — npm
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
server — feed it `pathInfo`, assert the `Route`. Two npm-specific facts it must
encode: `pathInfo` splits on `/` *before* percent-decoding, so an encoded scoped
name (`/@scope%2Fpkg`) arrives as a single segment; and reserved meta-routes
(`/-/…`) are matched first, since a real package name can never begin with `-`.
Anything unrecognized is `Unsupported` → 404, so deny-by-default holds at the
routing layer too.

## Meta-routes: ping, health, and search

Beyond packuments and artifacts, a few non-package routes:

- **`/-/ping`** — answered **locally** with `200` (`{}`). `npm ping` checks that the
  registry endpoint it talks to — the proxy — is up, so there is no reason to
  round-trip upstream.
- **Liveness / readiness** (for orchestration, e.g. `/livez`, `/readyz`) — kept
  distinct. *Liveness* is "the process is responsive," and in single-process mode
  also reflects the mirror worker's consume-loop heartbeat (a stalled worker fails
  liveness; see [Process model](cloud-backends.md#process-model)). *Readiness* is
  "config loaded and the listener is serving"; it is deliberately **lenient about
  public-upstream reachability** — the proxy still serves private-upstream hits
  when public is down, so readiness must not flap on an upstream blip and pull a
  healthy pod from rotation.
- **Search (`/-/v1/search`)** — **not supported at launch.** It is a discovery
  convenience, not an install path, so rather than scope-creep a filtered or
  passthrough search now, the route returns `501 Not Implemented` with a short
  message pointing users to the public registry's website. Revisit only if demand
  warrants.

Everything else unrecognized stays `Unsupported` → `404` (deny-by-default at the
routing layer).

## Control plane vs. data plane

The single most important split in the HTTP code:

- **Data plane** — streaming artifacts and fetching metadata — goes through
  `http-client`.
- **Control plane** — SQS (mirror queue), STS, and CodeArtifact's
  `GetAuthorizationToken` (the AWS
  [`CredentialProvider`](cloud-backends.md#credential-provider)'s `mintToken`) —
  goes through `amazonka`.

This matters most for CodeArtifact. Its npm repository is a **standard HTTPS npm
endpoint**: obtain a bearer token from `GetAuthorizationToken` (control plane,
`amazonka`), then fetch packuments and tarballs with ordinary `http-client` (data
plane). The streaming path therefore never touches `amazonka`'s
conduit/`ResourceT` machinery — which is exactly where naive
streaming-through-a-proxy goes wrong.

The same split holds on GCP — Pub/Sub and the Artifact Registry token are
control-plane work, while the npm data plane is unchanged `http-client` (see
[Cloud Backends](cloud-backends.md#cloud-backends)).

## Streaming and resource lifetime

A WAI streaming response body **runs after the handler returns** — Warp
serializes it while writing to the socket. So a resource with lexical scope
(`bracket`, `withResponse`, `runResourceT`) released when the handler returns is
already gone by the time the body streams: a use-after-free / GC race. This is
the classic trap, and it is why frameworks that hide the response continuation
make memory-bounded artifact streaming awkward.

Raw WAI avoids it by construction. `Application` is continuation-passing — *you*
call `respond` — so the resource acquisition can bracket the `respond` call
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
send buffer, so we pull from upstream only as fast as the client drains —
**constant memory regardless of artifact size**, with backpressure for free. No
`ResourceT`, no conduit on the hot path.

**Integrity on the serve path.** The proxy streams artifacts through without
hashing them, relying on the client's own integrity check — the packument's
`dist.integrity`, which the proxy preserves unaltered when
[filtering](rules-engine.md#applying-verdicts-to-a-packument) (npm always verifies
it). Proxy-side serve verification is deferred; revisit it when a weakly-verifying
ecosystem (e.g. PyPI) or a non-verifying client lands. The mirror **worker does
verify** before publishing to the sanitized home (see
[Cloud Backends → Mirror Queue](cloud-backends.md#mirror-queue)).

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

data Transience
  = WillResolve (Maybe Seconds)  -- transient: upstream 5xx/429, advisory source down
  | WontResolve                  -- not expected to self-heal (internal/parse error)
```

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
advisory condition); otherwise `500` — retrying a permanent/internal inability to
decide cannot help, so we do not invite it.

For a **packument** request there is no single status — instead each `Reject`
version is filtered out (see
[Rules Engine → Applying verdicts](rules-engine.md#applying-verdicts-to-a-packument)),
and a status is chosen only when *nothing* survives: `503` if any rejection was
`WillResolve`, else `403`.

The denial-body shape and `PROXY_HELP_MESSAGE` handling are in
[Rules Engine → Denial Responses](rules-engine.md#denial-responses). This type
lives in `Ecluse.Server.Response`.

## Middleware and helper libraries

The dividing principle: **adopt libraries for cross-cutting infrastructure that
is identical for every service; hand-roll anything that encodes our domain or
wire contract.**

- **Adopt — `wai-extra` middleware** (already a dependency): `RequestSizeLimit`
  (defensive body cap), `RealIp`/`ForwardedFor` (correct client IP behind a load
  balancer), and `Timeout`, composed around the `Application`. Two it
  deliberately does *not* use: `Autohead` — it answers HEAD by running the GET
  handler and discarding the body, which on a tarball route would open the
  upstream and stream a whole artifact to nowhere (HEAD on artifacts is handled
  explicitly instead); and `Gzip` — artifacts are already compressed, and
  re-compressing the stream would fight the backpressure above.
- **Adopt — `unliftio`** for the worker/service layer, where `ReaderT Env IO`
  runs: it lifts `bracket`/`finally`/`async` into the reader so resource-safety
  stays ergonomic. Request handlers stay in plain `IO` taking `Env`, so the hot
  path carries no transformer lifting.
- **Hand-roll** — the router (`classify`), response/error helpers (the npm
  `{"error": …}` shape lives in an `Ecluse.Server.Response` module, grown as
  repetition surfaces), a thin `katip` logging middleware (so request logs join
  the same structured stream as everything else, rather than `wai-extra`'s stock
  logger), and conditional-GET / ETag handling: for **pass-through** bodies (artifacts, and
  unfiltered private-upstream metadata) the client's validators are relayed
  upstream and `304`s passed back unchanged; for **transformed** bodies (filtered
  packuments) the served body differs from upstream's, so we compute our **own**
  `ETag` over what we serve and answer conditional requests against that —
  relaying the upstream validator there would cache or validate the wrong bytes.
- **Decline** — routing libraries (`wai-routes`, `wai-routing`, …): largely
  dormant and segment-based, so they fight the encoded-slash handling that a
  small pure `classify` gets right.
- **Defer** — `http-reverse-proxy` (revisit only if the hand-rolled core starts
  reinventing it; our need to intercept and *synthesize* denial responses argues
  against a transparent proxy), the tracing/metrics middleware (now specified in
  [Observability](observability.md) — OpenTelemetry WAI instrumentation —
  deferred until it lands), and `warp-tls` (only if TLS is not terminated
  upstream).
