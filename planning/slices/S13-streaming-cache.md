---
id: S13
title: Streaming + conditional-GET/ETag + metadata cache
milestone: M2, Web front door
status: merged
depends-on: [S12]
test-tier: [unit]
arch-refs:
  - docs/architecture/web-layer.md#streaming-and-resource-lifetime
  - docs/architecture/web-layer.md#metadata-cache
  - docs/architecture/web-layer.md#middleware-and-helper-libraries
pr: 93
---

# S13, Streaming + conditional-GET/ETag + metadata cache

> Milestone **M2** · depends on: [S12](S12-wai-app-middleware.md) · tier: unit

**Goal.** Three serving primitives the pipeline relies on: bounded-memory artifact
streaming (the bracket-around-`respond` pattern), conditional-GET/ETag handling, and
the short-TTL in-memory metadata cache shared by the packument and tarball-gating
paths.

**Acceptance criteria.**
- [x] **Streaming**: artifacts stream through with constant memory via
  `withResponse`/`responseStream`, the upstream connection bracketed around the
  `respond` call so it lives exactly as long as the streamed body (no
  use-after-free); backpressure from `write` blocking on the socket.   _web-layer.md#streaming-and-resource-lifetime_
- [x] **Conditional-GET/ETag**: for **pass-through** bodies (artifacts, unfiltered
  private-upstream metadata) relay the client's validators upstream and pass `304`s
  back; for **transformed** bodies (filtered packuments, S09) compute our **own**
  `ETag` over the served bytes and answer conditional requests against that.   _web-layer.md#middleware-and-helper-libraries_
- [x] **Metadata cache**: an STM-backed, short-TTL, size-bounded cache (the `cache`
  library) keyed by package holding the parsed `PackageInfo`; both the packument and
  the tarball-gating fetches reuse one fetch+parse; concurrent resolutions of a
  popular package collapse to one upstream call. **Metadata, not verdict**, rules
  re-evaluate on cached metadata each request.  _web-layer.md#metadata-cache_
- [x] Cache lives in `Env` (filling the S01 slot); TTL/size are configurable (S03).

**File scope.**
- `src/Ecluse/Server/Stream.hs`, the streaming helper.
- `src/Ecluse/Server/Conditional.hs`, ETag/If-None-Match handling (own vs relayed).
- `src/Ecluse/Server/Cache.hs`, the TTL metadata cache + accessors.
- `src/Ecluse/Env.hs`, add the cache (additive).
- `ecluse.cabal`, add `cache`.
- `test/unit/Ecluse/Server/{StreamSpec, ConditionalSpec, CacheSpec}.hs`, streaming over an in-process upstream (constant memory, backpressure), own-vs-relayed ETag, cache hit/miss/TTL/collapse.

**Test tier.** Unit, `hspec-wai` + in-process upstream stub for streaming and
conditional behaviour; STM cache semantics directly.

**Notes / risks.** The streaming lifetime is the classic WAI trap, get the bracket
placement right (resource released only after Warp returns `ResponseReceived`). The
own-ETag for filtered bodies must be computed over **what we serve**, not upstream's
body. Cache only metadata; never cache a decision (time-sensitive rules + the
separately-synced advisory tier must stay correct).
