# Dependency-graph cache experiment

This experiment investigates cache capacity using real pnpm dependency resolution.
It changes no production retention or admission policy.
Runtime measurements are informational.

The harness captures registry responses, runs isolated installers, and compares retention variants.
It provides a raw-byte Valkey candidate, optional cache-event observation, and interval models.
The research findings distinguish runtime measurements from models and unresolved production choices.

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
The preserved Saerskriven configuration sets `minimumReleaseAge: 10080`.
Every experimental client overrides that rule to zero, recorded in its outcome.
This controlled input differs from the workspace's normal security posture.
It prevents the installer's host clock from changing the frozen graph as releases age.
After capture, the proxy clock becomes the latest capture time plus two days.
That clock remains fixed in `policy-clock` for all policy variants.

Capture mode alone fetches anonymous public npm responses.
It stores complete decompressed bodies with URL, time, SHA-256, size, status, and response headers.
The existing npm and PyPI corpus files remain untouched.
Frozen mode fails on uncaptured requests and verifies captured bytes before serving them.
Artifact bytes remain original. Metadata changes the registry authority to the local origin.
The production proxy performs name checks, bounded reads, rule evaluation, URL rewriting,
validator generation, and artifact streaming.
Its configured private origin returns 404 per request.

The origin process runs separately from the proxy and installer.
The experiment pins its origin port and rejects a changed port before another cell starts.
HTTP observations include redacted headers, path, timing, status, and capture weight.
Clients receive an empty environment apart from the pinned tool path and empty npm configuration.
The capture registry refuses authentication and cookie headers.
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
The refusal path still weighs and re-encodes full candidates.
Its cost is not the cost of a future backend that avoids that work.

## External retention and memory limits

Set `GRAPH_VALKEY_PORT=18103` and use full budget zero for the external candidate.
Each cell starts a fresh Valkey process from the pinned Nix package set.
`GRAPH_EXTERNAL_BYTES` sets its `maxmemory`, default 268435456, with `allkeys-lru` eviction.
The cell verifies the started process owns the port and uses a fresh key namespace.
The client keeps at most 16 pooled connections and no local full values.
Its 100000-microsecond command deadline includes pool waiting. `GRAPH_VALKEY_TIMEOUT_US` overrides it.
Writes finish synchronously. GET failure or timeout falls back to the frozen origin.
SET failure preserves the origin result. `GRAPH_VALKEY_MODE=paused` exercises the timeout path.
`unavailable` leaves the cache port closed and exercises connection failure.

Raw cache hits repeat production body bounds, decoding, projection, name checks,
artifact-location checks, source digest calculation, and rule evaluation.
Only the configured anonymous public source uses Valkey. Private or credential-bearing reads bypass it.
Aggregate counters distinguish actual origin requests from external hits, misses, writes, and fallback.
Valkey INFO snapshots report server memory, operation counts, eviction, and network totals separately.

Every cell runs the proxy in its own user systemd service.
`GRAPH_MEMORY_MAX` sets its memory limit, default 2G, with swap disabled.
The runner saves applied properties, cgroup memory figures, and failures.
The installer, origin, and Valkey remain outside that cgroup.
The proxy uses two GHC capabilities and 20 admission slots by default.
`GRAPH_PROXY_SLOTS` selects another explicit slot count.
This is the harness operating point, not the shipping 512 MiB memory plan.

Main runtime cells use `GRAPH_TRACE_HTTP=0 GRAPH_CACHE_EVENTS=0`.
This disables per-request file writes in the proxy, origin, and Valkey client.
Cheap aggregate counters remain enabled, and each installer saves its lockfile and outcome.
Diagnostic cells enable both traces to record actual full-store insertion generations,
reuses, removals, refused weights, and collapse attempts.
File writes delay responses and hold the insertion lock during mutation events.
Diagnostic timings therefore describe a perturbed execution and do not supply the main runtime comparison.

The frozen origin serves identity-encoded bodies. Captured size means decompressed JSON size.
Shipping upstream requests can negotiate gzip, while this Valkey candidate transfers raw values.
Local results do not establish a managed-service network benefit, compression ratio, TLS cost,
availability, shared contention, or price. Those remain explicit sensitivities or unresolved limits.

`model.json` sweeps working bytes divided by capacity at
0.25, 0.5, 0.9, 1, 1.1, 2, 4, and 8.
Weights come from the actual captured bodies after registry-authority rewriting.
They use the production `weighCacheEntry` accounting, which is not exact heap size.
The model keeps the entry limit above the observed cardinality.
It reports full-store reuse opportunities, age, intervening admitted bytes, expiry, eviction, refusal, and collapse.
Artifact probes neither populate nor refresh the model's full store.
Artifact opportunities are an upper bound: retained version entries can mask full-store reads after churn.
Reconcile them with the measured `ecluse.metadata_cache.version.full_hits` counter.

The model uses request completion as an insertion bound.
It does not claim to observe the actual cache insertion event.
Policy changes alter client timing, so model curves do not replace real policy comparisons.
It does not model selected-version or assembled retention, socket buffers, or allocation.
Failed installer cells emit no capacity model. Successful traces require all successful request weights.
The completeness report counts excluded unsuccessful requests.

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

The proxy report includes RTS allocation, GC, peak live heap, post-GC memory,
`/proc/self/status`, and existing cache counters.
These values alone do not establish memory fit.
Report diagnostic traces separately from the uninstrumented runtime cells.
