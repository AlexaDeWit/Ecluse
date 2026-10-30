# Web layer

> Part of the [Écluse architecture overview](../architecture.md).

How Écluse serves HTTP: routing, streaming, caching, admission, and response status. The front door
is a raw `wai` `Application` served by `warp`. It routes a request, streams artifacts in bounded
memory, and applies cross-cutting concerns as middleware.

Routing has two layers. Mount dispatch matches the leading path segment to a mount (see
[Multi-ecosystem mounts](#multi-ecosystem-mounts)) and strips its prefix. The remaining
ecosystem-native path then goes to that mount's router. Deny-by-default is structural. A path no
route claims is a `404`. A tarball name that parses for a different package is a path-confusion
attempt, and the route declines it rather than fabricate a coordinate from it.

## Multi-ecosystem mounts

One Écluse process serves one or more ecosystems from one listener. It mounts each registry under a
path prefix on a shared base URL:

```
https://registry.internal.example.com/npm      → npm mount
https://registry.internal.example.com/pypi     → PyPI mount
https://registry.internal.example.com/cargo    → Cargo mount
https://registry.internal.example.com/rubygems → RubyGems mount
```

Each ecosystem gets one mount, and Écluse derives that mount's prefix from the ecosystem
(npm → `/npm`, PyPI → `/pypi`, Cargo → `/cargo`, RubyGems → `/rubygems`). Nobody configures a
prefix, so no two prefixes can collide and nobody can mistype one. No mount sits at `/`, so adding
an ecosystem never changes an existing consumer's URLs. Each mount also carries an optional per-ecosystem
[rule refinement](configuration.md#rule-policy) merged over the shared policy. A single-npm setup is
the single-mount case, under its own derived prefix.

A mount carries its ecosystem's credential presentation.
npm uses `Authorization: Bearer`. PyPI uses HTTP Basic under any username. Cargo uses a raw `Authorization` token.
RubyGems reads keep the complete HTTP Basic pair, and RubyGems publication uses the raw API-key value of `Authorization`.
Écluse forwards the recovered credential upstream in that same form.
The web layer compares its secret against the configured edge token in constant time and refuses a mismatch.

URL rewriting keeps artifact downloads on the proxy. Registry responses embed absolute artifact locations: npm's
`dist.tarball`, and on public PyPI, file URLs on a separate host. Forwarded unchanged, those URLs let
a client resolve metadata through the proxy and then download the bytes straight from upstream, past
the gate. A mount therefore rewrites embedded artifact URLs under its own protocol-specific
route and prefix before serving metadata.

Cargo's sparse index contains no artifact URL. Local discovery under `/cargo/` supplies the proxy's download template and, when publication is configured, its one publication API.
The [registry model](registry-model.md#cargo-integration) explains source selection and credential authority.

Same-host artifacts have a second benefit. The npm client attaches credentials only to requests on
the registry host. A same-host artifact URL keeps that auth on a tarball fetch. A separate host
would drop the credentials. Rewriting emits absolute URLs, and header inference is unreliable behind
load balancers and TLS terminators. So a mount must know its own externally-visible base URL as
explicit configuration (`server.publicUrl` plus its derived prefix).

## Meta-routes: ping, health, and search

`/livez` and `/readyz` stay distinct for orchestration. Liveness means the process responds and,
where a mirror worker runs, its consume loop keeps making progress.
Readiness includes startup, draining, and the first advisory sync of at least one mount whose own
rules deny on the advisory database, with the state of every mount in the body.
Public-upstream reachability is not a readiness requirement, because private hits can still serve
during a public outage. The [operator probe contract](https://ecluse-proxy.com/docs/operations/#health-probes)
also describes role-specific conditions. `/-/ping` answers locally, and
`/-/v1/search` returns `501`, a discovery convenience rather than an install path.
The three dist-tag routes, `GET /-/package/{package}/dist-tags` and the `PUT` and `DELETE` of
`/-/package/{package}/dist-tags/{tag}`, return `501` too: a dist-tag is a mutable named pointer,
and Écluse implements none. A `HEAD` reads like its `GET`, so it returns the same `501` with no
body, while a `POST` over those paths takes the deny-by-default `404`.

## OpenAPI spec

Écluse speaks npm, PyPI, Cargo, and RubyGems read and publication protocols rather than a bespoke HTTP API.
Package clients use those protocols rather than an API description, so the
published OpenAPI spec is not a client-integration contract. It states which protocols this
server speaks, and what each ecosystem does and does not support. That stops being self-evident as mounts multiply.

### What the spec covers, and what it doesn't

Écluse documents its coverage of each protocol, not the protocol itself:

- **The spec models owned and synthesised responses in full**: the error/denial envelope, the
  meta routes (`/-/ping`, `/-/v1/search`), and the packument Écluse synthesises (see
  [Packument merge](registry-model.md#packument-merge-across-upstreams)). `/livez` and `/readyz`
  are middleware above the mounts, so they sit outside the spec.
- **It describes opaque pass-through instead of re-specifying it**: tarball and artifact responses
  stream verbatim (see [Streaming](#streaming-and-resource-lifetime)). Upstream controls their
  status, media type, and body, so the operation carries a wildcard binary `default` response rather
  than a false finite status set.
- **Unsupported routes are a documented boundary**: `GET /-/v1/search`,
  `GET /-/package/{package}/dist-tags`, and the `PUT` and `DELETE` of
  `/-/package/{package}/dist-tags/{tag}` return `501`, and each read also contributes its
  bodiless `HEAD` operation. The manifest states that, so a reader learns the limit there and
  not from an error response.

### The synthesised-packument schema = the trust boundary

The served packument is Écluse's merged and filtered view: private versions trusted, public gated
(see [Packument merge](registry-model.md#packument-merge-across-upstreams)). No single upstream
produces that document, which makes it the highest-scrutiny piece of the manifest. The manifest
therefore owns its schema and models it as *partial* and *open*. It describes only the fields Écluse
reads and transforms (`versions`, `dist-tags`, `time`, and each version's `dist`).
`additionalProperties: true` permits adapter-retained fields beyond those schema properties. It
does not promise to relay unknown upstream fields. npm extraction retains an explicit supported
field set and replaces author lists with source-specific pointers. Private metadata still wins a
version collision before assembly.

## Streaming and resource lifetime

For npm, PyPI, Cargo, and RubyGems, the proxy pulls from upstream only as fast as the client drains.
These streamed bodies use constant memory regardless of artifact size. Only the time spent waiting on upstream counts toward the
progress floor (`limits.progressWindow`, `limits.minProgressBytes`), so a slow client never trips it.
An upstream that falls below it aborts the stream, and the client sees an incomplete download.

Four deadlines bound a request and its upstream exchanges: the 60-second request timeout, the
serve-path cap 10 seconds under it, the progress floor, and a fixed 30-second wait for a connection
and then the response headers. As in http-client, that wait gives the headers what the connection
left of it, counted from the end of any request body. When http-client retries on a new connection
because a pooled one was stale, the retry keeps what is left of the same wait. One sweeper thread
checks every deadline in the process every 100 ms, so each one ends about 100 ms late: one tick,
plus the sweep and scheduling. Arming a deadline adds it to a list for the arming thread's core, so
request threads never wait on each other to arm one, as they would on GHC's process-wide timer
queue. Amazonka's S3 calls and the OSV and EPSS fetches keep their own clients' timers.
The mirror worker has no serve-path cap, so it bounds Cargo and RubyGems archive preparation and
RubyGems companion validation at the 60-second request timeout, on the same sweeper.

The npm, PyPI, Cargo, and RubyGems adapters stream artifact responses without hashing their bodies. Integrity on this path depends
on the client checking the preserved metadata hashes. The proxy therefore asks the upstream for
every artifact and Core Metadata companion in identity coding. A registry CDN gzips a companion
when the request permits it, and gzip bytes miss the advertised digest. npm documents integrity
checking on cache insertion and extraction
([npm cache](https://docs.npmjs.com/cli/v11/commands/npm-cache/)). pip checks
index-supplied hashes against download corruption, while locally pinned hashes provide a separate
check against a changed remote source ([pip secure installs](https://pip.pypa.io/en/stable/topics/secure-installs/)).
Do not assume every client or configuration provides identical checks. PyPI reads already ship
with this client-verification dependency. The npm mirror worker verifies bytes before publication
(see [Mirror queue](cloud-backends.md#mirror-queue)).

A `HEAD` must never run the full-`GET` streaming pump. A bodiless `HEAD` that opened the upstream
connection and pumped a whole body for warp to discard is a DoS-amplification lever. Cheap `HEAD`s
would force arbitrary full-artifact upstream fetches. So dispatch handles `HEAD` explicitly, not the
`Autohead` middleware.

## RubyGems client integrity

Compact Index `/info` replies carry archive SHA-256 values from the admitted winning source.
Supported Bundler clients check those values during the first installation, without requiring a pre-existing lockfile.
Archive delivery uses the shared streaming relay. Mirror ingestion verifies bytes before publication.
The [operator protocol contract](https://ecluse-proxy.com/docs/protocol-support/#rubygems) defines the supported clients and cache behaviour.

## Metadata cache

The local backend retains selected versions, assembled responses and the public RubyGems discovery directory.
The directory shares the retention pool and contains names and native version tokens, without detailed per-gem metadata.
It never retains full package metadata, including a compact full representation. Concurrent full reads share active work,
then completion removes the flight registration. This path does not weigh, encode, or insert
full entries. A subsequent full read fetches again, so an earlier listing does not make a later
selected-version read an upstream-free operation.

One selected provider owns all four retention capabilities: full metadata, selected versions,
assembled responses, and the public RubyGems directory. Its constructor assigns one storage class to every capability.
The composition root chooses the shipped local provider without an external dependency.
Local full retention is absent even if an adapter supplies full operations. Unsupported capabilities
stay uncached, and a failed provider never falls back to a retained local copy.
Single-flight and request admission remain local coordination, separate from persistent storage.

Selected reads call the provider's selected capability directly. They never fetch a retained full
entry through the generic cache. An external adapter can read a selected remote projection without
transferring or decoding a full document locally. The adapter owns its TTL, codec, bounded decoding,
identity checks, and representation. Source, ecosystem, package, version, digest, and artifact
identities must survive that boundary. No request can bypass the private authorisation or rules.

A selected public read first prepares its provider operation while holding admission. A local
retained value, including a cached absence, stays captured for this request, so eviction between
the lookup and its use cannot turn a hit into a fetch. Preparation performs no origin fetch or
external lookup.

Recency is a storage-policy hint, not a remote LRU requirement. Occupancy reporting is optional
and describes the adapter's charged bytes and entry counts, not its server's exact heap use.
The local provider reports its bounded stores' charges. Each external operation has a deadline
capped at one second. A failed read fetches metadata from its origin or renders an assembled response
from this request's authorised inputs. A failed write skips retention.
Cancellation propagates. Writes run inline with no pending write queue. There is no external client,
codec, or service configuration in the shipped provider.

Credential refresh state, advisory snapshots, HTTP connection pools, and mirror queues retain
their separate control-state contracts. Operator-owned private registries are registry roles,
not an implicit second metadata retention provider.

Repeated full fetches can increase upstream work. Dependency-graph captures establish large full
working sets, but no successful paired runtime comparison establishes the size of this trade-off.
Performance reports must compare equal successful work and distinguish retained bytes from transient
working data, allocation, and upstream transfer.

The local selected-version store charges each retained release field, including the full backing
allocation of each text slice. Repeated artifacts, hashes, licences, and trust evidence each carry
a node allowance. A fixed allowance covers the entry and scalar fields, and a per-byte version
allowance covers parsed ordering keys. Shared allocations count repeatedly. This is conservative
accounting, not exact heap residency. A release above the aggregate budget is served without retention
or eviction, and occupancy reports the charged weights. Cached absences keep their smaller charge.

Managed mirror records use a separate retained-graph charge. It includes expected descriptors,
verified digests, companion references, and the full backing allocations of source strings and keys.
The view owns revision bytes and charges their length and cache key before retention.
PyPI fragment structure has its own allowance. Other fragment shapes retain the selected-entry allowance.
Work reservations for decoding, updates, workers and encoding remain independent of this cache charge.

The selected-version, assembled, and public RubyGems directory stores share one TTL, one byte bound
and one entry bound. No store reserves a static share. Under pressure, the inserting store evicts its
least-recently used entries until the aggregate fits or its eviction floor stops it. The shipped
floors are zero. One store cannot evict another store's live entries. Reads and retaining inserts
reclaim expired entries across every store, so TTL bounds this unfairness to one TTL window during
active use. The directory store reports its bytes on the full-store gauge under `store=directory`.
Per-store recency and expiry indexes change with the entries and aggregate accounting in one
transaction. Recency updates add work to cache hits but avoid sorting a whole store during eviction.

The cache holds the metadata, not the verdict. The rules engine re-evaluates the rules on every
request, so time-sensitive rules (`AllowIfOlderThan`) stay correct.
On-disk artifact caching is out of scope, and the mirror stays the durable store.

Raw metadata retention and full-read request coalescing apply only to the anonymous public
origin. The serve path fetches private metadata separately for each request, preserving the
private registry's authorisation decision. Assembled responses can contain private data under
the input-fingerprint rules below.

The assembled-representation store beside it memoises the encoded merged document under a content
fingerprint of every input. That fingerprint includes the digest of the private document this
request's own authorised fetch returned, plus each source's surviving versions and exact admitted artifact coordinates.
A changed integrity floor after restart therefore changes the validator when it removes a file but keeps its release.
No request shares or skips the private fetch and its authorisation.

## Metadata compression

Écluse gzips admitted listings itself, because the proxy in front of it may not. AWS ALB does not
compress responses, and Istio compresses only through a resource an operator must opt in to and
may not be allowed to install. `server.metadataGzip` is on by default, and
`ECLUSE_SERVER__METADATA_GZIP=false` restores the identity responses exactly.

[Metadata admission, authorised response reuse and per-response compression](https://ecluse-proxy.com/docs/threat-model/#metadata-listing-and-admission)

After admission, `Accept-Encoding` selects gzip, identity, or a `406` when neither is permitted.
Input-derived validators return `304` before rendering or compression. Otherwise the proxy
reads or renders identity bytes, then compresses only when gzip was selected.
RubyGems Compact Index `/versions` and `/info` replies and compact specifications instead hash the
rendered identity bytes, after current authorisation and admission. A matching `304` therefore skips
compression but still pays the fetch, merge, or serialisation.

- **Only admitted listings.** Artifacts, sidecars, probes, refusals and publish replies are never
  compressed by Écluse. RubyGems compact gemspecs are admitted native metadata.
  The coding is chosen after every authority and policy check.
- **One cached form.** The assembled cache holds identity bytes under the identity tag. Each gzip
  response compresses them again, on a hit as on a miss, at level 1 for speed.
- **Validators per coding.** The gzip tag is weak and derived from the identity tag, because a
  codec change may change the bytes. Listing responses and their `304`s carry
  `Vary: Accept-Encoding`. A gzip `HEAD` compresses to state the exact length.
- **Cost.** Compression runs inside the CPU gate, so a gzip response holds its slot longer. Each
  pays the memory gate 512 KiB for the deflater and 36 KiB per 32 KiB output buffer, and returns
  both when the request ends.

Compression makes a listing's length depend on its content. A listing that merges private and
public metadata is therefore a length-observation surface, which the
[threat model](https://ecluse-proxy.com/docs/threat-model/) assesses. Each response gets its own
deflater, so no compression history crosses requests. A front proxy's compression is outside this
setting.

## Serve admission and upstream pools

Listings and public artifact metadata decisions pass two gates: a memory gate, then a process-wide
CPU gate. The memory gate charges a small entry step against the
[memory budget](configuration.md#runtime-sizing-cores-and-heap-ceiling) before the request takes a
CPU slot, so a new request waiting for memory holds no CPU slot. The CPU capacity follows the core
count or an explicit operator pin.

Once admitted, a listing pays for its full reads chunk by chunk and for its response before it
builds it. A completed read keeps less than it held while parsing, so its charge steps down to what
it keeps. Once the body exists, nothing reaches the reads' documents, so their charges go back
before the body is sent. A request that joins another's public fetch or render pays nothing for it,
and an assembled hit or a conditional `304` pays for its reads alone. A gzip response also pays for
its compression, hit or miss. A public artifact decision reads one selected release, so its entry
step is its whole charge. A started request that runs out of budget
pauses at its next charge, keeping its CPU slot. A pause during an upstream read ends at the
serve-path cap, and a pause before a render at the request timeout. Both gates stay held through
metadata evaluation and while the listing response is built. A listing then gives up its CPU slot
and keeps its memory charge while Écluse writes the body, so a slow client holds memory but no CPU
slot. Public artifact requests release both after the metadata decision, before streaming the
admitted artifact.

Each gate has a bounded waiting room and a 1 s wait budget. The deadline sweeper ends an expired
wait on its next tick, normally within another 100 ms. A request that finds a room full joins
bounded overflow outside both gates. While queued, it holds no slot, room place or memory charge.
Each gate wakes the oldest request on its own overflow line when a room place frees. The CPU gate
also signals when a slot returns to an empty room. Sweeper ticks cover capacity changes without a
signal. Original arrival order survives retries and movement between lines. A new request can
still take a place before a woken request retries.

Overflow allows retries for 0.5 s from joining overflow. Expiry normally ends the queue wait within another
100 ms. A retry started before expiry uses each gate's normal room wait and lets admitted work
finish. The overflow budget therefore does not bound total request latency. Eight reservations
per serve slot cover queued requests and their retries, including work admitted by a retry.
Cancellation and completion release each reservation once. Holding it through admitted work can
cause more immediate refusals than counting only parked requests, but preserves the resource cap.

An expired wait sheds with `503` and `Retry-After: 1`. A full overflow refuses at once with the
same headers and permits normal connection reuse. An ingress or sidecar can pool its connections
to the pod. Closing those connections adds accepts and pool reconnections without requiring the
end client to slow down. Overflow bounds held admission state, but does not impose a per-client
request rate. A private artifact hit that needs an admitted indexed location (Cargo, RubyGems) or
a mirror-state check passes both gates for its decision, then releases them before streaming.
Health probes, cheap local routes and other trusted private artifact hits bypass both gates. The
mirror worker runs outside the gates. A slow artifact client therefore holds no serve metadata slot while its
download drains.

First-party publication reads its upload under a separate byte-weighted gate alone, then passes
both gates once the body is in memory. It holds them through upload inspection, target preparation
and the relay, as a listing holds its slot through its upstream reads. A slow uploader therefore
holds byte budget but no CPU slot, so it cannot make listings and artifact decisions shed. The
memory gate still bounds Cargo and RubyGems archive inspection, which runs in memory.

The public and private connection pools take independent settings. The private pool takes the larger
share, because a trusted tarball hit streams outside admission, which makes its demand the
steady-state inbound hit fan-out.

## Error model

Every served response renders one serve outcome. A small type (`ServeDecision` in
`Ecluse.Core.Server.Response`) maps each outcome to its status, rather than collapsing everything
into a generic 403 or 500 response. For a concrete artifact request the decision renders directly:

| Outcome | Status |
|---|---|
| Admit | `200` (streamed) |
| Policy denial (incl. deny-by-default) | `403` + denial body |
| Undecidable, transient | `503` + `Retry-After` |
| Undecidable, permanent | `500` |
| upstream miss | `404` (forwarded) |

The rule: return `503` only when the condition should resolve, such as a transient upstream or
advisory condition. Otherwise return `500`, because retrying a permanent inability to decide cannot
help.

A packument request has no single status. Écluse
[merges the document across upstreams](registry-model.md#packument-merge-across-upstreams) and
filters it by provenance (see [Applying verdicts](rules-engine.md#applying-verdicts-to-a-packument)).
The proxy chooses a status only when nothing survives the merge, and the most recoverable cause
wins:

- `503` if any rejection was transient, or a needed upstream was unavailable.
- Otherwise `502` if a responding upstream returned an invalid response: a packument whose
  self-reported name is for a different package (see
  [name validation](registry-model.md#the-route-name-is-the-served-names-validation-authority)).
- Otherwise `500` if none is retryable but an exclusion is a permanent inability.
- Otherwise `403`.

Never `404`: the versions existed and Écluse withheld them, and a genuinely absent package is a
separate upstream miss. (`packumentStatus` in `Ecluse.Core.Server.Response` is the counterpart of
`artifactStatus`.)

The serve-outcome model decides the status, not the body shape: an ecosystem's route contract
supplies the matching response constructor and codec. A request matching no mount is a neutral
`404 Not Found` in `text/plain`. [Rules engine → denial responses](rules-engine.md#denial-responses)
covers the denial-body shape and `ECLUSE_SERVER__HELP_MESSAGE` handling.
