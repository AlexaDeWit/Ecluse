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
  logger), and conditional-GET / ETag relay (forwarding the client's validators
  upstream and relaying `304`s is domain behaviour).
- **Decline** — routing libraries (`wai-routes`, `wai-routing`, …): largely
  dormant and segment-based, so they fight the encoded-slash handling that a
  small pure `classify` gets right.
- **Defer** — `http-reverse-proxy` (revisit only if the hand-rolled core starts
  reinventing it; our need to intercept and *synthesize* denial responses argues
  against a transparent proxy), metrics middleware (`wai-middleware-prometheus`
  or `katip` + `ekg`, when observability lands), and `warp-tls` (only if TLS is
  not terminated upstream).
