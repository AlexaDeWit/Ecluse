# API surface and capability manifest

> Part of the [Écluse architecture overview](../architecture.md).

Écluse speaks package-registry protocols (npm at launch; PyPI and RubyGems planned), not a
bespoke HTTP API. Clients (`npm`, `pnpm`, `yarn`) hardcode the registry protocol and never
read an API description, so an OpenAPI document here is not a client-integration contract.
It is a **capability manifest**: one human-facing statement of which protocols this server
speaks and exactly what is, and isn't, supported per ecosystem. As mounts multiply (`/npm`,
`/pypi`, …) that stops being self-evident, and the manifest is where it becomes legible.

## What the manifest covers, and what it doesn't

Écluse documents its *coverage of* each protocol, not the protocol itself. That maps onto
how each route is handled:

- Owned / synthesised responses are fully modelled: the error/denial envelope, the health
  and meta routes, and the synthesised packument (Écluse parses, merges across upstreams,
  filters, and re-serialises it; see
  [Packument merge](registry-model.md#packument-merge-across-upstreams)) are documents
  Écluse authors, so their schemas are described in full.
- Opaque pass-through is described, not re-specified: tarball/artifact bytes stream verbatim
  (see [Streaming](web-layer.md#streaming-and-resource-lifetime)); the manifest gives the
  operation and media type and links to the upstream protocol.
- Unsupported routes are a documented boundary: `GET /-/v1/search` → `501` is stated
  explicitly, so a reader learns the limit from the manifest, not from an error response.

Re-specifying npm's full packument or registry protocol is out of scope: that is npm's
contract, and clients hardcode it.

## Source of truth: the route table × mounts

The manifest is derived, not hand-written, and it holds **no per-route knowledge of its own**.

Each ecosystem's adapter declares its routes as a list of records
([above](web-layer.md#the-route-table-belongs-to-the-ecosystem)). One record carries a route's
path template, what serving it *does*, **and its documentation**: its summary, its status set,
and the shape of each body. The manifest walks the same records the router runs
(`serveRoutes`, their erased `RouteSpec` projection) and renders whatever it is handed.

There is nothing here to keep in step with the routes, so there is nothing to drift. A route
cannot be declared without its documentation, because the record will not construct without it.
That is a stronger guarantee than a total map over a route sum, which stays exhaustive only for
as long as someone keeps it so.

The documentation types in the core are deliberately **OpenAPI-free** (`RouteDoc`, and a closed
`BodyDoc` vocabulary of the body shapes Écluse emits). Naming the body a route carries is a core
concern; knowing what a JSON Schema is is not, and the `openapi3` dependency tree must never
reach the running proxy. So the core says *which* body, and this generator's interpreter maps
that to a schema, total over `BodyDoc` in the other direction: a new body shape cannot go
unrendered. Adding PyPI is adding a mount, not describing a protocol.

In the rendered docs, tags are ecosystems: Redoc groups operations by mount, so the document
reads as "one server, these protocols". A route's `operationId` is its ecosystem-local name
qualified by its mount (`npm.packument`), which is where global uniqueness is guaranteed.

## The synthesised-packument schema = the trust boundary

The served packument is Écluse's merged-and-filtered view (private versions trusted, public
gated; see [Packument merge](registry-model.md#packument-merge-across-upstreams)), so its
schema is **owned** and modelled as *partial*: the fields Écluse reads and transforms
(`versions`, `dist-tags`, `time`, `dist`) are described, and everything else carries
`additionalProperties: true` with a note that unlisted fields relay unchanged from the
contributing upstream (private wins on collision). The schema states the trust boundary
exactly: here is what the gate touches; everything else is upstream's, untouched. Its
cross-field coherence after merge and filter (every `dist-tags` target is a surviving
`versions` key) is not schema-expressible and stays a
[property test](rules-engine.md#applying-verdicts-to-a-packument); a green schema is not a
proof that the filtered document is coherent.

## How it's built and published

- Code-first schemas: owned types (error envelope, synthesised packument, config) define
  their JSON via `autodocodec`, which derives the `aeson` instances *and* the OpenAPI /
  JSON-Schema from one codec, so wire format and documented schema cannot diverge. The
  document is assembled with `openapi3`, its paths rendered from each mount's adapter route
  grammar above. (npm's inbound wire decoding stays lenient hand-rolled `aeson`;
  autodocodec is for what Écluse owns and emits, see
  [Technology Stack](technology-stack.md#technology-stack).)
- Generated and published, not served: a build-time generator (kept out of the library closure,
  like the benchmarks) produces the document from a fixed canonical config, a pure function of
  (config, mounts), with no `GET /openapi.json` route. `task docs-site` / `task site` run it and
  render a static Redoc page, staging it with `openapi.json` into `./_site` for GitHub Pages,
  published by `pages.yml` on push to `main`. The spec is derived build data, regenerated at publish
  time, not committed. The Redoc bundle is vendored and hash-pinned (the `mermaidJs` `fetchurl`
  pattern), so the CI shell needs no Node and the site has no external runtime dependency.
  Output is deterministic (pinned key ordering, fixed base URLs), so a regeneration is a reviewable
  diff.
- Confidence without a fuzzer: external contract fuzzers (Schemathesis, Dredd / Prism) are
  Python / Node and clash with the node-free posture; autodocodec gives conformance largely
  by construction, backed by `hedgehog` round-trips on the codecs.

## Contract drift controls

The manifest is generated directly from the code, so it moves only when the code moves, a
change a reviewer sees in the diff. Its paths *and* its per-route documentation are both
projections of the route records the router runs
([above](#source-of-truth-the-route-table--mounts)), so a route cannot be mounted without a
documented operation: the record will not construct without one. Its owned schemas are the
`autodocodec` codecs that also back the `aeson` instances, so documented schema and wire format
cannot diverge. The manifest's unit tests (`ManifestSpec`) hold this together: the render stays
well-formed, and the documented paths and methods are checked against the live routing.

> The synthesised packument is the schema exception. It is an *open* schema
> (`additionalProperties: true`), so "drift" there means "did we drop a field we promised to
> relay", which only the lossless round-trip property test can answer (see
> [Packument merge](registry-model.md#packument-merge-across-upstreams)), not a schema
> validator.

The route table has a live guard: the manifest's rendered paths are held against the same route
records the router runs, so a change to a path template or a route's method fails unless the
manifest moves with it. Two gaps remain.
The documented status codes and response-body shapes are not held against the running server
(the serve handlers choose status imperatively, with no shared enumeration), so those stay
documentation the manifest owns rather than a checked correspondence. And nothing yet answers
the question worth gating for an external consumer: is a change breaking or safe-additive? That
needs a semantic OpenAPI differ (oasdiff-class). Écluse has no external consumers reading this
manifest, so the differ waits until a consumer needs it, and `openapi.json` stays derived build
data with no stored golden to diff against.

## Config as JSON Schema (a free corollary)

The [configuration](configuration.md#configuration) model decodes from JSON, so giving its
owned types autodocodec codecs yields a JSON Schema for the config at the same cost, usable
for editor validation and operator docs, consistent with the fail-fast strict decoding
already required.
