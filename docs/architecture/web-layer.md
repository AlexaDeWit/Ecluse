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
https://registry.internal.example.com/npm    → npm mount
https://registry.internal.example.com/pypi   → PyPI mount
```

Each ecosystem gets one mount, and Écluse derives that mount's prefix from the ecosystem
(npm → `/npm`, PyPI → `/pypi`). Nobody configures a prefix, so no two prefixes can collide and
nobody can mistype one. No mount sits at `/`, so adding an ecosystem never changes an existing
consumer's URLs. Each mount also carries an optional per-ecosystem
[rule refinement](configuration.md#rule-policy) merged over the shared policy. A single-npm setup is
the degenerate case, under its own derived prefix.

A mount also carries its ecosystem's credential presentation: the form its clients put a
credential in (npm: `Authorization: Bearer`; PyPI: HTTP Basic under any username). Écluse uses the
same form when it forwards the credential upstream, rendering the recovered pair verbatim. The web
layer compares the secret half of what the mount recovered against the configured edge token in
constant time and refuses anything else.

URL rewriting is load-bearing. Registry responses embed absolute artifact locations: npm's
`dist.tarball`, and on public PyPI, file URLs on a separate host. Forwarded unchanged, those URLs let
a client resolve metadata through the proxy and then download the bytes straight from upstream, past
the gate. A mount therefore rewrites embedded artifact URLs under its own protocol-specific
route and prefix before serving metadata.

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

Écluse speaks npm and PyPI read protocols, with npm-only publication, not a bespoke HTTP API.
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

The proxy pulls from upstream only as fast as the client drains: constant memory regardless of
artifact size, with backpressure for free.

The proxy streams artifact responses without hashing their bodies. Integrity on this path depends
on the client checking the preserved metadata hashes. npm documents integrity checking on cache
insertion and extraction ([npm cache](https://docs.npmjs.com/cli/v11/commands/npm-cache/)). pip checks
index-supplied hashes against download corruption, while locally pinned hashes provide a separate
check against a changed remote source ([pip secure installs](https://pip.pypa.io/en/stable/topics/secure-installs/)).
Do not assume every client or configuration provides identical checks. PyPI reads already ship
with this client-verification dependency. The npm mirror worker verifies bytes before publication
(see [Mirror queue](cloud-backends.md#mirror-queue)).

A `HEAD` must never run the full-`GET` streaming pump. A bodiless `HEAD` that opened the upstream
connection and pumped a whole body for warp to discard is a DoS-amplification lever. Cheap `HEAD`s
would force arbitrary full-artifact upstream fetches. So dispatch handles `HEAD` explicitly, not the
`Autohead` middleware.

## Metadata cache

The local backend retains selected versions and assembled responses. It never retains full
metadata, including a compact full representation. Concurrent full reads share active work,
then completion removes the flight registration. This path does not weigh, encode, or insert
full entries. A subsequent full read fetches again, so an earlier listing does not make a later
selected-version read an upstream-free operation.

One selected provider owns all three retention capabilities: full metadata, selected versions,
and assembled responses. Its constructor assigns one storage class to every capability.
The composition root chooses the shipped local provider without an external dependency.
Local full retention is absent even if an adapter supplies full operations. Unsupported capabilities
stay uncached, and a failed provider never falls back to a retained local copy.
Single-flight and request admission remain local coordination, separate from persistent storage.

Selected reads call the provider's selected capability directly. They never fetch a retained full
entry through the generic cache. An external adapter can read a selected remote projection without
transferring or decoding a full document locally. The adapter owns its TTL, codec, bounded decoding,
identity checks, and representation. Source, ecosystem, package, version, digest, and artifact
identities must survive that boundary. No request can bypass the private authorisation or rules.

A selected public read first prepares its provider operation while holding CPU admission.
A local retained value, including a cached absence, stays captured for this request, so its lower
materialisation allowance cannot lead to a fetch after eviction. Preparation performs no origin
fetch or external lookup. The request acquires materialisation admission before executing deferred
work and evaluating current rules. An external lookup uses the cold allowance even when its remote
provider later reports a hit.

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
materialisation, allocation, and upstream transfer.

The local selected-version store charges each retained release field, including the full backing
allocation of each text slice. Repeated artifacts, hashes, licences, and trust evidence each carry
a node allowance. A fixed allowance covers the entry and scalar fields, and a per-byte version
allowance covers parsed ordering keys. Shared allocations count repeatedly. This is conservative
accounting, not exact heap residency. A release above the aggregate budget is served without retention
or eviction, and occupancy reports the charged weights. Cached absences keep their smaller charge.

Selected-version and assembled stores share one byte bound and one entry bound. Neither store
reserves a static share. Under pressure, the inserting store evicts its least-recently used entries
until the aggregate fits or its eviction floor stops it. The shipped floors are zero.
One store cannot evict another store's live entries. Reads and retaining inserts reclaim expired
entries across both stores, so TTL bounds this unfairness to one TTL window during active use.
Per-store recency and expiry indexes change with the entries and aggregate accounting in one
transaction. Recency updates add work to cache hits but avoid sorting a whole store during eviction.

The cache holds the metadata, not the verdict. The rules engine re-evaluates the rules on every
request, so time-sensitive rules (`AllowIfOlderThan`) stay correct.
On-disk artifact caching is out of scope, and the mirror stays the durable store.

Metadata retention and full-read single-flight use the anonymous public (gated) origin only. It never holds the private origin: the
serve path fetches that origin per request and never hands it to the cache. No caller's private view
can leak to another inside the TTL, because Écluse forbids a shared private cache. The anonymous public origin crosses no trust boundary, so the cache
holds it freely.

The assembled-representation store beside it memoises the encoded merged document under a content
fingerprint of every input. That fingerprint includes the digest of the private document this
request's own authorised fetch returned, plus each source's surviving versions and exact admitted artifact coordinates.
A changed integrity floor after restart therefore changes the validator when it removes a file but keeps its release.
No request shares or skips the private fetch and its authorisation.

## Serve admission and upstream pools

Listings and public artifact metadata decisions acquire a process-wide CPU gate and then a separate
materialisation gate. The CPU capacity follows the core count or an explicit operator pin.
Materialisation charges static workload estimates against an independent capacity. The
[memory plan](configuration.md#runtime-sizing-cores-and-heap-ceiling) explains why neither these
estimates nor their scheduling minimum guarantee that a request fits the heap.

A listing reserves output work plus one full-read allowance per permitted configured origin before
fetching them concurrently. First-party names omit the public origin, and mounts without a private
upstream omit that origin. An assembled hit or a conditional `304` does not reduce the initial charge.
Both gates remain held through metadata evaluation and the listing response. Public artifact
requests release both after the metadata decision, before streaming the admitted artifact.

Each gate has a bounded waiting room and wait budget. A full waiting room or expired wait sheds
with `503` and `Retry-After`. Health probes, cheap local routes and trusted private artifact hits
bypass these metadata gates. The mirror worker runs outside these serve gates. A slow artifact
client therefore holds no serve metadata slot while its download drains.

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
