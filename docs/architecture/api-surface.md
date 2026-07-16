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
- Opaque pass-through is described, not re-specified: tarball/artifact responses stream verbatim
  (see [Streaming](web-layer.md#streaming-and-resource-lifetime)). Their status, media type, and
  body are upstream-controlled, so the operation carries an explicit OpenAPI `default` response
  with wildcard binary content rather than a false finite status set.
- Unsupported routes are a documented boundary: `GET /-/v1/search` → `501` is stated
  explicitly, so a reader learns the limit from the manifest, not from an error response.

Re-specifying npm's full packument or registry protocol is out of scope: that is npm's
contract, and clients hardcode it.

## Source of truth: the route table × mounts

The manifest is derived, not hand-written, and it holds **no per-route knowledge of its own**.

Each ecosystem's adapter declares its routes as a list of records
([above](web-layer.md#the-route-table-belongs-to-the-ecosystem)). One record carries a route's
path template, what serving it *does*, and an abstract `ResponseContract`. The contract is indexed
by the response value the handler must produce and owns both interpretations of that value: how it
becomes a WAI response and the `ResponseDoc` entries the manifest renders. The manifest walks the
same records the router runs (`serveRoutes`, their erased `RouteSpec` projection).

The `ResponseContract` constructor is private. Exact response leaves bind one status, body shape,
and renderer together; `chooseContract` combines leaves into the closed response sum a handler can
produce. Dispatch existentially packages that contract with its handler and gives the handler only
the corresponding typed responder. A route therefore cannot be declared without response
documentation, and its handler cannot send an unrestricted WAI response around that documentation.

The contract types in the core are deliberately **OpenAPI-free** (`ResponseDoc` and the closed
`BodySchema` vocabulary). Naming the body a response carries is a core concern; knowing what a
JSON Schema is is not, and the `openapi3` dependency tree must never reach the running proxy. The
manifest interpreter is total over `BodySchema`, so a new body shape cannot go unrendered. Adding
PyPI is adding a mount, not describing a protocol.

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

The manifest is generated directly from the code, so it moves only when the code moves. Paths and
methods are projections of the route records the router runs. Status, media type, and body are
projections of the same `ResponseContract` that is the handler's response capability. Exact routes
use a closed response sum; transparent relays use an explicit OpenAPI `default`, accurately keeping
the contract open. `HEAD` is derived from the `GET` contract in both interpreters, preserving status
and headers while removing every documented and emitted body. A route's pre-commit `500` fallback is
also a value admitted by its contract.

Owned JSON leaves encode through the same `autodocodec` codec the manifest turns into a schema. The
manifest tests keep the projection honest: every route method is rendered, each operation has one
document per response key, exact packument statuses are asserted, `HEAD` bodies are absent, and the
tarball and publish relays retain their explicit defaults.

> The synthesised packument is the schema exception. It is an *open* schema
> (`additionalProperties: true`), so "drift" there means "did we drop a field we promised to
> relay", which only the lossless round-trip property test can answer (see
> [Packument merge](registry-model.md#packument-merge-across-upstreams)), not a schema
> validator.

Two deliberately separate checks remain. The synthesised packument's hand-authored schema still
needs its lossless projection properties because no codec constructs that document. And nothing yet
answers whether a manifest change is breaking or safe-additive; that needs a semantic OpenAPI differ
(oasdiff-class) once an external consumer depends on the manifest. `openapi.json` remains derived
build data with no stored golden in the meantime.

## Config as JSON Schema (a free corollary)

The [configuration](configuration.md#configuration) model decodes from JSON, so giving its
owned types autodocodec codecs yields a JSON Schema for the config at the same cost, usable
for editor validation and operator docs, consistent with the fail-fast strict decoding
already required.
