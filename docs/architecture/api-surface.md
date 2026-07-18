# API surface and capability manifest

> Part of the [Écluse architecture overview](../architecture.md).

Écluse speaks package-registry protocols (npm at launch; PyPI and RubyGems planned), not a
bespoke HTTP API. Clients (`npm`, `pnpm`, `yarn`) hardcode the registry protocol and never
read an API description, so the OpenAPI document here is not a client-integration contract.
It is a **capability manifest**: one human-facing statement of which protocols this server
speaks and exactly what is, and isn't, supported per ecosystem. As mounts multiply (`/npm`,
`/pypi`, ...) that stops being self-evident, and the manifest is where it becomes legible.

## What the manifest covers, and what it doesn't

Écluse documents its coverage of each protocol, not the protocol itself, which maps onto how
each route is handled:

- **Owned and synthesised responses are modelled in full**: the error/denial envelope, the
  health and meta routes, and the synthesised packument that Écluse authors (see
  [Packument merge](registry-model.md#packument-merge-across-upstreams)).
- **Opaque pass-through is described, not re-specified**: tarball and artifact responses
  stream verbatim (see [Streaming](web-layer.md#streaming-and-resource-lifetime)). Their
  status, media type, and body are upstream-controlled, so the operation carries an explicit
  OpenAPI `default` response with wildcard binary content rather than a false finite status
  set.
- **Unsupported routes are a documented boundary**: `GET /-/v1/search` returns `501`, stated
  explicitly so a reader learns the limit from the manifest, not from an error response.

Re-specifying npm's full packument or registry protocol is out of scope: that is npm's
contract, and clients hardcode it.

## Source of truth: the route table

The manifest is generated, not hand-written, and holds no per-route knowledge of its own. It
walks the same route records the router runs: each adapter declares its routes, and every record
carries a path template, what serving it does, and an abstract `ResponseContract`. The route
table and the `ResponseContract` machinery belong to the
[web layer](web-layer.md#the-route-table-belongs-to-the-ecosystem); the manifest is one more
reader of them, so a route cannot be declared without response documentation. The core's contract
vocabulary is deliberately OpenAPI-free (`ResponseDoc` and the closed `BodySchema`), because the
`openapi3` dependency tree must never reach the running proxy, so adding PyPI is adding a mount,
not describing a protocol. In the rendered docs, Redoc tags are ecosystems, so the document reads
as "one server, these protocols", and a route's `operationId` is its ecosystem-local name
qualified by its mount (`npm.packument`), where global uniqueness is guaranteed.

## The synthesised-packument schema = the trust boundary

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

## How it's built and published

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
