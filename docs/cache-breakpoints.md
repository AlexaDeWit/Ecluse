# Dependency-graph cache experiment

This experiment investigates cache capacity using real pnpm dependency resolution.
It changes no production retention or admission policy.
Runtime measurements are informational.

The initial harness captures and freezes registry responses, runs isolated installers,
and models full-store capacity over their observed HTTP intervals.
The Valkey candidate, repeated comparison report, and final recommendation remain pending.
No result from this initial harness establishes a production policy or managed-service benefit.

## Inputs and execution

Run through the pinned development shell after building `bench-load`:

```bash
env -u IN_NIX_SHELL nix develop --command task build
env -u IN_NIX_SHELL nix develop --command task cache-breakpoints -- prepare
env -u IN_NIX_SHELL nix develop --command task cache-breakpoints -- capture
env -u IN_NIX_SHELL nix develop --command task cache-breakpoints -- cell local-1 268435456 saerskriven
env -u IN_NIX_SHELL nix develop --command task cache-breakpoints -- cell off-1 0 saerskriven
```

`GRAPH_EXPERIMENT` selects a fresh directory under `scratchpad/`.
Preparation pins Saerskriven at `89054e5e4857ca2e94e663d4344257b49cb68dd2` and preserves
its workspace manifests, catalog, and configuration. It removes the input pnpm lockfile.
The Next input pins Next, React, and React DOM in a standalone manifest.
The installer is pnpm 11.25.0. Preparation records the Node version and platform.
Preparation downloads the installer as a separate tool, before the measured install.

Each client starts with a new project copy, store, and metadata cache.
The client resolves, downloads, checks integrity, and extracts artifacts.
Lifecycle scripts and pnpmfile hooks stay disabled. The experiment does not execute the application or its tests.
Workspace configuration still applies. The proxy clock is fixed in `policy-clock`.
The installer uses the host clock, so graph equality needs checking across capture dates.

Capture mode alone fetches anonymous public npm responses.
It stores complete decompressed bodies with URL, time, SHA-256, size, status, and response headers.
The existing npm and PyPI corpus files remain untouched.
Frozen mode fails on uncaptured requests and verifies captured bytes before serving them.
Artifact bytes remain original. Metadata changes the registry authority to the local origin.
The production proxy performs name checks, bounded reads, rule evaluation, URL rewriting,
validator generation, and artifact streaming.
Its configured private origin returns 404 per request.

The origin process runs separately from the proxy and installer.
HTTP observations include request headers, path, timing, response headers, status, and capture weight.
Each client saves its outcome, generated lockfile, and installed inventory.
Compare versions, source identities, and integrities before comparing performance.
A failed install is evidence, never an equivalent successful workload.

## Cells and models

The `cell` command accepts a comma-separated project list.
`saerskriven,saerskriven` starts same-project clients and `saerskriven,next` starts mixed clients.
`GRAPH_CLIENT_STAGGER_SECONDS` spaces their starts. Each client retains its own caches.
`GRAPH_CLIENT_CONCURRENCY` fixes the client's network concurrency, default 16.
`GRAPH_UPSTREAM_DELAY_US` adds the same origin delay to every cell, default 5000.
`GRAPH_FULL_ENTRIES` and `GRAPH_TTL_SECONDS` set independent limits, default 100000 and 3600.
`GRAPH_BODY_LIMIT` defaults to the production 12582912 bytes.
A changed body limit must appear in the results beside any default-limit refusal.

A zero full budget uses the existing store's minimum one-byte capacity.
It retains neither the full raw document nor its all-version typed view.
It keeps full single-flight, selected-version retention, and assembled-response retention.
The other stores keep their existing defaults.

`model.json` sweeps working bytes divided by capacity at
0.25, 0.5, 0.9, 1, 1.1, 2, 4, and 8.
Weights come from the actual captured bodies after registry-authority rewriting.
They use the production `weighCacheEntry` accounting, which is not exact heap size.
The model keeps the entry limit above the observed cardinality.
It reports useful reuse age, intervening admitted bytes, expiry, eviction, refusal, and collapse.
Artifact probes neither populate nor refresh the model's full store.

The model uses request completion as an insertion bound.
It does not claim to observe the actual cache insertion event.
Policy changes alter client timing, so model curves do not replace real policy comparisons.
It does not model selected-version or assembled retention, socket buffers, or allocation.

## Required evidence before a recommendation

- Run matched repeated blocks with rotated policy order and equal resolved graphs.
- Include cold and warm proxy states, same-project and mixed-project concurrency, and TTL crossings.
- Separate byte eviction, count eviction, oversized refusal, and expiry.
- Compare local hits, miss leaders, followers, and materialising external hits.
- Measure proxy allocation, GC, RSS peak, post-GC memory, latency, and failures separately from installer and origin costs.
- Use an enforced process memory limit before making a memory-fit claim.
- Measure external misses, insertion copies, concurrent large values, and bounded unavailable-cache fallback.
- Report compact representation as an executable candidate or a quantified unresolved dependency.
- Separate observed local Valkey costs from RTT, bandwidth, and price projections.

The initial proxy report includes RTS allocation, GC, peak live heap, post-GC memory,
`/proc/self/status`, and existing cache counters.
These values alone do not establish memory fit.
The recorder adds file and observation overhead to the measured proxy.
That overhead must remain consistent or receive a separate uninstrumented control.
