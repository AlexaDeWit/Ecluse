# Testing strategy

Where a test belongs, what each tier gates, and how CI measures coverage. One thing decides a
test's tier: **the external collaborator the code under test needs**, and so how deterministic the
test can be. Three kinds:

- **unit** needs no collaborator (pure logic or in-process doubles),
- **integration** needs an *emulable* service, reached through a container,
- **smoke** needs an *un-emulable* live service.

The first two are hermetic and **gate** merges. Smoke makes live calls and is **allowed to fail by
design**. Two further gating tiers, residency and end-to-end, sit alongside them, for seven `cabal`
test-suites in all. One rule spans every tier (see *What gates, and what doesn't*), so read that
before you choose a new test's home.

## Unit tests: `ecluse-core-unit`, `ecluse-runtime-unit`, `ecluse-unit` (gating)

Pure, fast, deterministic `hspec` and `hedgehog` tests over all pure logic: the rules engine,
parsers, and configuration. No IO, no Docker. They run on every push in milliseconds. Properties
exercise the rules engine: deny-by-default, deny-precedence over allows, and per-rule predicates.
This tier tests the credential provider's refresh, cache, and expiry policy with an injected clock
and a fake `mintToken`. The real mint runs only in smoke (see that caveat below).
Proxy request-lifecycle tests run against an in-process WAI stub, so they assert the full
fetch → parse → rules → mirror path without a network.

The tier is three suites, split by which library a spec may link. Each suite's `build-depends`
enforces the split:

- **`ecluse-core-unit`** covers `Ecluse.Core.*` (depends on `ecluse-core` only).
- **`ecluse-runtime-unit`** covers the `Ecluse.Runtime.*` capabilities that need no
  application library: the cloud adapters, the telemetry SDK wiring, and logging.
- **`ecluse-unit`** covers the composition shell and the app-tier specs. It depends on
  the `ecluse` app library, so it can drive runtime handles through
  `runServer`/`runWorker`, which is why the `Ecluse.Runtime.Server` and
  `Ecluse.Runtime.Env` specs live here.

Each tests its tree in isolation, mirrored under `core/test/unit`, `runtime/test/unit`, and
`test/unit`. A spec module is the tested module's full name with `Spec` appended, and its file sits
at the tested module's path under the suite's source directory, library prefix included, so
`Ecluse.Core.Cve.Slot` is tested by `core/test/unit/Ecluse/Core/Cve/SlotSpec.hs`, module
`Ecluse.Core.Cve.SlotSpec`. The prefix stays in the name, so a spec states which library it tests
without the reader knowing which suite holds it. A suite outside the unit tier puts its tier token
before `Spec`: `IntegrationSpec`, `E2ESpec`, `SmokeSpec`, `ResidencySpec`. The integration spec for
`Ecluse.Core.Server.Pipeline.Tarball` is therefore
`Ecluse.Core.Server.Pipeline.TarballIntegrationSpec`, and the unit spec beside it keeps the bare
`Spec`. Run all three: `cabal test ecluse-core-unit ecluse-runtime-unit ecluse-unit`.

## Integration tests: `ecluse-integration` (gating)

Exercise cloud-backed code (the `MirrorQueue` and `CredentialProvider` handles) against a real
emulator, driven by `testcontainers`. The AWS backend runs against a **ministack** container, a
lightweight LocalStack alternative, with `amazonka` pointed at `http://<container>:4566` and
throwaway credentials. The telemetry specs run a real OTLP **Collector** container the same way. Both
are hermetic: no real cloud account, no real credentials.

The tier needs a running Docker daemon. CI's `ubuntu-26.04-arm` runner provides one. Locally,
install Docker: Nix ships the toolchain, not the daemon. Run: `cabal test ecluse-integration` (or
`task test-integration`).

> **Token-mint caveat.** No emulator covers the managed-registry token API (CodeArtifact's
> `GetAuthorizationToken`). The only un-emulable part is the `mintToken` leaf of the
> `CredentialProvider`, so this tier mocks it at that handle. The unit tier covers the generic
> refresh, cache, and expiry policy around it with an injected clock. The real mint runs end-to-end
> only in the non-gating smoke tier.

## Residency gate: `ecluse-residency` (gating)

The bounded-memory streaming gate streams a 1 MiB and a 100 MiB artifact through the tarball relay.
It covers both the trusted private-hit leg and the gated public leg. It asserts that peak live bytes
stay invariant in artifact size within a fixed margin. It is its own suite, not an
`ecluse-integration` spec, because the measurement needs process isolation and the RTS statistics
flag (`-with-rtsopts=-T`) in its `ghc-options`. It runs outside coverage too. No Docker, loopback
WAI stubs only. Run: `cabal test ecluse-residency` (or `task test-residency`). `task check` includes
it via `cabal-checks`.

Metadata probes use a fresh child process for each corpus package and retained shape.
They authenticate every capture against the byte count and SHA-256 in `bench/corpus/pins.json`.
The complete corpus must include a capture above the previous 3,687,514-byte maximum.
The four shapes are the original strict bytes, a decoded `Value`, the production typed
projection alone, and the shared `CacheEntry` containing the typed projection, serving document,
and source digest. npm serving documents retain only supported fields. The raw shape remains
the complete decoded source for comparison. The combined measurement preserves sharing, so adding
the two separate measurements does not give its size.

Each child first prepares and releases one copy to initialise runtime state before its baseline.
It records a major-GC baseline, roots a newly prepared shape with a stable pointer,
collects again, dereferences the root, frees it, and records another collection.
The checks require positive retained growth. After release, at most 16 KiB can remain
above baseline, and that residual must not exceed one tenth of the held growth.
Preparation fully consumes the derived rendering of retained metadata without retaining the
rendered string. That forces opaque fields without adding production `NFData` instances.
The retained samples include any backing arrays reachable through the selected shape.

| Output | Meaning |
|---|---|
| `wireBytes` | Capture bytes before parsing, with identity content encoding |
| `compactBytes` | Re-encoded raw JSON bytes for raw and shared shapes |
| `cacheWeight` | The selected entry's charge, or the historical full-entry charge for shared shapes |
| `versions` | Projected version count for typed and shared shapes |
| `baselineLive`, `heldLive`, `releasedLive` | Absolute live bytes after each major collection |
| `retained_per_wire_byte` | Held minus baseline live bytes, divided by capture bytes |
| `preparationAllocated` | Cumulative preparation allocation, including forcing and accounting |
| `preparationMaxLive` | Process high-water sample through preparation, including warmup and forcing |
| `existing_model_bytes` | The unchanged production expansion estimate for comparison |

These counters distinguish the retained heap from allocation and cache accounting.
The high-water sample includes rendering and cannot establish the production read/decode/project
peak or a bound on transient buffers. The [listing probe](#listing-peaks) measures that peak. The
probes use production projection functions with the default structural limits. They do not execute
the HTTP bounded read or prove that shipping response limits admit each capture.

The retained-byte gate uses the following corpus envelopes. The figures come from all nine npm and
three PyPI captures in the arm64 Build job of
[CI run 36545262450](https://github.com/AlexaDeWit/Ecluse/actions/runs/36545262450/job/109330092515),
with GHC 9.10.3, Cabal `-O1`, one capability, and a warmed process. npm's shared cache entry is its
packed full read.
These are regression limits for authenticated fixtures, not a universal metadata expansion model.
A calibrated gate is the smallest quarter step at least 8% above its measured maximum. The PyPI raw
gate keeps its default.

| Ecosystem | Retained shape | Maximum heap bytes per source byte | Package | Gate | Margin |
|---|---|---:|---|---:|---:|
| npm | Wire bytes | 0.999938894 | typescript | 1.25 | 25.0% |
| npm | Raw `Value` | 6.477760939 | express | 7 | 8.1% |
| npm | Typed projection | 0.443638073 | react | 0.5 | 12.7% |
| npm | Shared cache entry | 0.693002413 | react | 0.75 | 8.2% |
| PyPI | Wire bytes | 0.999638868 | numpy | 1.25 | 25.0% |
| PyPI | Raw `Value` | 4.120186179 | boto3 | 7 | 69.9% |
| PyPI | Typed projection | 1.593904341 | requests | 1.75 | 9.8% |
| PyPI | Shared cache entry | 3.041720821 | requests | 3.5 | 15.1% |

A shared cache entry is what a listing's full read holds. Its gate is a regression limit, and the
same test also checks that it stays within the memory gate's full-read charge, read from each
ecosystem's adapter. The same test checks the listing output charge: twice a shared entry's encoded
size, for the lazy encoding and its strict copy, must stay within the output charge. The
[listing probe](#listing-peaks) checks both charges against a read's peak and a render's working
set. Raise a charge in the adapter, not here, when a representation outgrows it.
Each denominator is the original authenticated source size, including omitted fields.
For example, the TypeScript shared shape retains 7,284,096 heap bytes from 15,693,959 source bytes.
Its re-encoded serving document is 10,181,045 bytes. That encoded size and the source probe's
`compact_byte_estimate` are different from measured retained heap, and neither is this gate's denominator.

The 48 rows left -17,224 to -1,000 bytes above their warmed baselines after release.
The 16 KiB release tolerance leaves 17,384 bytes above the observed maximum.
The second release condition requires at least 90% of each held growth to disappear.
Signed integer differences preserve samples that fall below baseline without unsigned wraparound.

The compact denominator gives a different accounting ratio. Requests' shared retained bytes
divided by its compact encoding equal 3.876594335, the maximum across both ecosystems. The 7.5
factor leaves 93.5% margin above it. It is not an active-work bound.
The local provider retains selected releases and assembled bytes, so full shared shapes do not
size its entry-count control. An assembled-output mean cannot size a shared count of both forms.
These residency measurements do not determine the shared entry-count allowance.

Separate Vite and Next source probes give held-byte/compact-estimate ratios of 6.4900 and 6.4389.
Their exact encoded sizes are unmeasured. Those probes force accounting without warmed preparation
or derived rendering, so they do not establish the same fully forced retained envelope.

### Listing peaks

`MemoryModelResidencySpec` also measures the most live data a listing holds, which the memory gate's
[charges](architecture/configuration.md#runtime-sizing-cores-and-heap-ceiling) must cover. For each
capture, a fresh child process:

1. streams the capture in 32 KiB chunks through the production parser, digest and projection,
   enforces artifact locations against the capture's registry, and holds the cache entry
2. renders the served body of a single-source listing in which every version survives, as the
   strict bytes a response sends
3. reads the capture again, and holds that entry with and then without its served document

It reads and releases the capture once first, so the baseline holds the read's one-off state.
It samples live bytes before the measured read, holding the entry, and holding the served body. The
runtime's high-water after each phase gives that phase's peak. After the read phase it is an upper
bound, since the warm-up read can set the high-water first. A sample also counts what the code still
to run references, so step 3 takes both of its samples in one function. The child runs with
`+RTS -F1 -A128k`: the old generation may not grow past its live data, so nearly every collection
is major, and the high-water samples live data at least once per 128 KiB allocated. Under the
default flags, the typescript read's high-water equalled what it keeps, because no major collection
fell inside its transient. A 1 MiB nursery misses the peaks of captures under 1 MiB, which finish
within a few collections.

A merged listing gets the same measurement in six shapes per capture. The child reads a trusted
private document and a gated public one the same way, holds both, and renders their merge through
the serving path's assembly. It reports the output basis the serving path computes. The shapes are:

- identical: both documents are the capture, as a private mirror of the whole package holds
- overlapping: each holds two thirds of the capture's versions, one third of them in both
- disjoint: each holds half, none in both
- publish order: the private copy holds the newest quarter of the versions by publish time, the
  model the `heavy-private-25pct` load scenarios serve, and the public one is the capture
- heavy base: the private copy holds every tenth version by publish time, with text half the
  capture's size that the response renders, and the public one is the capture. npm's served
  document takes only its name, an author pointer, and the `created` and `modified` time stamps
  from the base, so npm's text is in each release's deprecation notice. PyPI's is the project
  status reason.
- oldest heavy base: as the heavy base, with the oldest tenth of the versions, whose releases are
  the smallest

The single-source listing and the first four shapes are realistic: they model private copies
that deployments serve, and they set the output charge and its regression limit. The heavy bases
stress the basis instead, and the output charge must hold them.

`Ecluse.Test.Corpus.Merge` writes each shaped document from the capture, and
`Ecluse.Test.Corpus.Subset` cuts it to its versions. A cut document stays consistent: an npm cut
keeps the times and dist-tags of its versions. Each merge child is its own process on one
capability, so the spec runs one child per processor at a time.

The checks compare bytes with each ecosystem's charges:

- The reads' peak fits what the meter holds after the full-read charges: whole 1 MiB steps, at
  least the entry step. A capture under one step can peak above its per-byte charge, and this check
  shows that what the meter holds still covers one such read. It does not check two sub-step reads
  at once. A listing that reads a private and a public document of under one step on one ticket
  can exceed what the meter holds by a fraction of a step, which the sampler's measurement of live
  data outside the charges absorbs.
- The peak through the reads and the render fits what the meter holds after the full-read and
  output charges, counted the same way. Every listing's output working set, a heavy base's or a
  capture's under one step included, fits the whole steps its output charge alone buys.
- From one step of basis up, every listing's output working set, a heavy base's included, fits the
  output charge on the basis. The working set is the larger of the listing's peak above the
  documents it holds and twice the served body. No collection observes the instant the lazy
  encoding and its strict copy are both live, so the check counts both.
- The basis of an identical merge is one document and that of a disjoint merge is both. That of an
  overlapping merge is less than both, and that of a publish-order or heavy-base merge is at least
  the capture and less than both.
- From one step up, the tier fails once a single read's peak or a realistic listing's output
  working set passes a regression limit per ecosystem. The rule for a limit is the smallest quarter
  step at least 8% above the maximum. The read limits, set by that rule from the first table below,
  are 1.0 per source byte for npm (react, 0.888), 0.4 under its charge, and 3.5 for PyPI (boto3,
  3.070), 0.4 under its charge. The output limits, set by that rule from the realistic shapes in
  the second table, are 1.75 per basis byte for npm (@aws-sdk/client-s3, 1.524, 14.8% margin), 0.25
  under its charge, and 1.5 for PyPI (boto3, 1.242, 20.7% margin), 0.1 under its charge. One
  example checks that each limit sits below its charge.
- Every single-source npm listing's held entry stays smaller than the source it was read from.
  `entryBelowSource` names the ecosystems this check covers. PyPI's entry holds each file as
  aeson's tree beside its typed view, which outgrows the file.
- A single-source listing's entry frees live bytes when it drops its served document, and the
  document's weight, expanded as a cache expands it, covers them.

The following figures come from the arm64 Build job of
[CI run 36537317917](https://github.com/AlexaDeWit/Ecluse/actions/runs/36537317917/job/109304478187),
with GHC 9.10.3, Cabal `-O1` and one capability. Each figure is heap bytes per source byte: the
read's peak and the held entry above the baseline, the listing's peak through the read and the
render above the held entry, and the served body's length. npm's figures are for its packed full
read.

| Ecosystem | Package | Source MiB | Read peak | Entry | Peak above entry | Served body |
|---|---|--:|--:|--:|--:|--:|
| npm | typescript | 14.97 | 0.587 | 0.465 | 0.677 | 0.659 |
| npm | @types/node | 10.63 | 0.400 | 0.304 | 0.191 | 0.175 |
| npm | react | 6.67 | 0.888 | 0.695 | 0.526 | 0.495 |
| npm | webpack | 4.96 | 0.486 | 0.326 | 0.725 | 0.712 |
| npm | @aws-sdk/client-s3 | 3.97 | 0.526 | 0.352 | 0.777 | 0.762 |
| npm | express | 0.77 | 1.117 | 0.571 | 0.581 | 0.576 |
| npm | @babel/core | 0.76 | 0.919 | 0.479 | 0.580 | 0.575 |
| npm | request | 0.29 | 1.561 | 0.624 | 0.937 | 0.529 |
| npm | lodash | 0.24 | 1.689 | 0.654 | 1.035 | 0.362 |
| PyPI | numpy | 2.65 | 2.427 | 2.065 | 0.885 | 0.596 |
| PyPI | boto3 | 2.10 | 3.070 | 2.855 | 1.012 | 0.621 |
| PyPI | requests | 0.12 | 4.005 | 3.096 | 0.909 | 0.657 |

The full-read charges take each listing's read peak per source byte, merges included, among
listings with at least one step of sources. In the arm64 Build job of
[CI run 37923084079](https://github.com/AlexaDeWit/Ecluse/actions/runs/37923084079/job/113795530414),
npm's realistic listings peak at 0.955 (express, publish order) and its heavy bases at 1.387
(express, oldest heavy base). PyPI's listings peak at 3.070 (boto3, single document), heavy bases
included.

The arm64 Build job of
[CI run 36562979100](https://github.com/AlexaDeWit/Ecluse/actions/runs/36562979100/job/109388236444)
gives each listing's output working set per byte of its basis, for a single document and for each
merge shape.

| Ecosystem | Package | Single | Identical | Overlapping | Disjoint | Publish order | Heavy base | Oldest heavy base |
|---|---|--:|--:|--:|--:|--:|--:|--:|
| npm | typescript | 1.319 | 1.319 | 1.318 | 1.318 | 1.319 | 1.546 | 1.598 |
| npm | @types/node | 0.350 | 0.350 | 0.350 | 0.350 | 0.350 | 0.900 | 0.942 |
| npm | react | 0.990 | 0.990 | 0.987 | 0.988 | 0.990 | 1.325 | 1.329 |
| npm | webpack | 1.423 | 1.423 | 1.411 | 1.402 | 1.178 | 1.602 | 1.679 |
| npm | @aws-sdk/client-s3 | 1.524 | 1.524 | 1.513 | 1.502 | 1.524 | 1.667 | 1.665 |
| npm | express | 1.151 | 1.151 | 1.122 | 1.091 | 1.017 | 1.388 | 1.427 |
| npm | @babel/core | 1.149 | 1.149 | 1.148 | 1.148 | 1.149 | 1.431 | 1.461 |
| npm | request | 1.058 | 1.058 | 0.943 | 0.854 | 0.839 | 1.201 | 1.223 |
| npm | lodash | 1.034 | 0.944 | 0.807 | 0.774 | 0.911 | 1.077 | 1.092 |
| PyPI | numpy | 1.193 | 1.193 | 1.195 | 1.199 | 0.956 | 1.467 | 1.541 |
| PyPI | boto3 | 1.242 | 1.242 | 1.242 | 1.242 | 1.242 | 1.495 | 1.496 |
| PyPI | requests | 1.315 | 1.315 | 1.323 | 1.318 | 1.185 | 1.544 | 1.588 |

Under the output charges, npm 2.0 and PyPI 1.6 per basis byte, the heavy bases hold at most 0.963
of their charge from one step of basis up (numpy, oldest heavy base) and 0.992 below it (requests,
oldest heavy base).

The rules for the charges and the limits take the listings of at least one step, as
[configuration.md](architecture/configuration.md#runtime-sizing-cores-and-heap-ceiling) sets out.
From one step up, each listing's peak above the documents it holds stays below twice its served
body, so twice the served body sets the output working set.

### Read evaluation

`MetadataResidencySpec` checks that a full read hands back a fully evaluated result. For each
capture, a fresh child process reads through the production projection and enforces artifact
locations against the capture's registry, as a production fetch does. It then takes two samples of
the same rooted cache entry:

- the live bytes with the entry at weak head normal form, as production holds a read result
- the live bytes after the child forces the entry through its derived rendering

The samples may differ by at most 1 KiB in either direction, because a deferred field can hold
more or less than its value. The entry's digest sits in a 4 KiB pinned memory block, and the names
that forcing renders also use pinned memory. Once they fill that block, the runtime starts another
and both stay live. So the child fills one pinned block before the first sample, and both samples
count the same blocks. The file handle that read the capture has a finalizer, so it and its buffer
stay live until the finalizer thread runs. Each sample therefore collects, lets that thread run, and
collects again, so neither sample depends on when the thread was scheduled.

A second check holds only the typed view. It places weak pointers on the served document, on each
served release or file object, and on each non-empty member map. Every pointer must clear after a
major collection. While the document is also rooted, every pointer must survive one, so the check
can observe liveness.

### Streaming source probes

The same residency executable accepts `--metadata-source-probe ECOSYSTEM MODE NAME VERSION LIMIT PATH`.
`ECOSYSTEM` is `npm` or `pypi`. The older invocation without it remains an npm probe.
PyPI pins use the same PEP 440 canonicalisation as production artifact routes.
`LIMIT` is the decompressed body ceiling in bytes. `PATH` remains the complete source capture.
Modes are `BufferedLegacy`, `BufferedCompact`, `StreamedFull`, `StreamedSelected` and
`StreamedVersions`. The first uses the prior complete Aeson representation. The second feeds held
bytes to the new parser, separating input buffering from projection changes. The streamed modes
read the file in 32 KiB chunks through the production driver. `StreamedVersions` is npm-only.
Every streamed mode hashes the source to report it. Production selected reads skip that hash.

Each invocation makes one read with no warm-up. Accounting walks force the retained result without
`Show` or output encoding. `read_project_ns` covers that read, projection and forcing.
The existing `Measurement` record supplies allocation and major-GC live samples.
`compactBytes` and `cacheWeight` are zero because this mode neither encodes nor admits a cache entry.
`compact_byte_estimate` reports the representation estimate separately.

Capture the child process's peak RSS externally, for example with GNU `time`.
That peak includes the runtime, input, useful output, parser buffers and native lexer allocations.
RTS allocation and live bytes do not include every native allocation. Report both scopes.
The native lexer uses batches proportional to the chunk size, and can construct scalar number
tokens while skipping unknown fields. It never retains a complete unknown value tree.

Results include source SHA-256, consumed bytes, version count, byte ceiling and a status.
Refusals and empty results return a failing exit status. A larger ceiling used for isolated parser
measurements does not demonstrate admission under the shipping default.
Compare equal successful workloads. Full-reference results do not establish a speedup over the
prior selected-version or inventory algorithms. The warmed retained-shape gate remains a separate
measurement from this first-read source probe.

### Selected-retention probes

Process-sampled deltas do not calibrate tiny selected objects. Pair
`--metadata-selected-retention-probe ECOSYSTEM NAME VERSION LIMIT PATH` with
`--metadata-selected-control-probe ECOSYSTEM NAME VERSION LIMIT PATH` for warmed GC comparisons.
Both read, weigh and encode the same selected result. The control discards it before collection
and roots a constant marker already present in its warmed baseline. Both retain the result's
would-be accounting charge as a scalar. GC snapshots return three strict counters so a preceding
`RTSStats` record cannot become part of the next sample.
Capture authentication follows these GC snapshots, so external process peaks include a separate
whole-capture read and must not be attributed to the selected object.

Raw live-byte deltas can remain negative. Interpret a selected value only when its repeated delta
interval exceeds the matched control interval. Otherwise report the result as unresolved.
Three value and three control processes per capture separated all twelve selected values.
Their differences ranged from 2,176 bytes for boto3 to 42,712 bytes for webpack.
All corresponding production charges exceeded those differences. These objects exclude store keys
and index overhead, so they do not establish a shared-entry count divisor or a universal bound.

### Ingest ceiling checks

Vite and Next were separate acceptance stress cases. All twelve full/selected reads completed at a
48 MiB ingest ceiling. Their original bodies were 38,945,461 and 31,270,567 bytes respectively.
The largest body has 29.24% byte headroom. Next's median full-read process RSS was 376,426,496 bytes.
This is input capability evidence, not successful-install or deployment-size evidence.
Six synthetic boundary checks extended the existing ignored-field fixture with legal whitespace.
Full and selected reads consumed exactly 134,217,728 bytes at that limit, refused one extra byte,
and consumed the larger body when the explicit limit rose by one byte. These checks exercise
decompressed byte accounting and overrides, not the heap cost of arbitrary 128 MiB metadata.

## Smoke tests: `ecluse-smoke` (allowed to fail, non-gating)

Make live calls to public registries (npm today) to confirm our JSON decoding and protocol handling
match reality. They depend on uncontrolled external services, so an occasional failure is normal
and never blocks a merge. The CI `gate` does not depend on them. Treat a failure as a
prompt to investigate (protocol drift or flakiness?), not a blocker. Run: `cabal test ecluse-smoke`.

This tier is also where the one un-emulable cloud surface runs end-to-end: the real token *mint*
(`CredentialProvider`'s `mintToken`) against the live cloud. It needs real external access, so it is
allowed to fail and stays isolated to one small function, an accepted residual risk.

The telemetry Datadog check lives here too. With Datadog API credentials in the environment, it emits
a uniquely stamped span and metric through the real export path. It then polls the Datadog API until
they appear. It is secret-gated (skipped without credentials) and non-gating, so a Datadog outage or
ingestion lag never blocks a merge. It is the only telemetry check that reaches the Datadog SaaS.

The hermetic span and metric assertions run in `ecluse-integration`. A request drives an in-process
Écluse, and a real Collector container asserts the spans and metrics arrived. The unit tier covers
config parsing, the denial span-attribute mapping, the JSONL scribe, and the metric-label guard.

## End-to-end tests: `ecluse-e2e` (gating)

The only tier that assembles the whole system through the real composition root and drives it with
the real `npm` and `pip` clients. It runs the published OCI image (`nix build .#dockerImage`), an
nginx public-upstream stub, a Verdaccio private upstream and mirror target, and a ministack emulator
for the mirror queue and the advisory store, as containers on a Docker network. It then asserts
client- and mirror-observable outcomes:

- an allow-listed package installs,
- Écluse blocks a rules-denied package and never mirrors it,
- an installed package round-trips server → worker to the private mirror,
- mirroring an older version after a newer one leaves the mirror's `dist-tags.latest` alone,
- a tampered artifact fails the integrity gate and never publishes,
- `pip` installs a wheel from a `pypi` mount in hash-checking mode, pinned to the sha256 the
  served Simple index advertised, so the installed bytes are the advertised ones.

The Dredger cases seed Verdaccio through the proxy and mirror worker, then run the same
image with an identity deny and no advisory database. They cover `--once`, `--dry-run`,
consent refusal, first-party protection, and listing preservation after the final version
is deleted. `Ecluse.DredgerE2ESpec` also drives `listPackagesIn` against the store and compares
complete package-version snapshots with the audit records and cycle counts. Its first-party
fixture holds two versions. These cases do not verify behaviour against a real CodeArtifact repository.

A further Dredger group covers what the next private read sees after a cleanup. It runs a second
Verdaccio as the proxy's `privateUpstream`, seeds the mirror through a proxy that still permits the
versions, and seeds the cache through the fixture publisher. After one cycle it reads both
inventories, the cache's metadata and artifact directly, and the same version through the proxy and
a fresh `npm` project. The group also refuses the cache's write methods at the nginx forwarder and
completes the residual on a later run, withholds one public artifact so no bytes remain to
re-admit, and republishes a late copy into each store for the next cycle to find. Verdaccio stands
in for a retaining cache here, and the unit tier models a read that restores a copy, so neither
covers CodeArtifact's own retention, permissions, or deletion.

A fourth Dredger group rolls policy out across roles in the wrong order. One case runs a proxy with
`--no-worker` on a durable ministack queue, stops it, starts a stricter proxy that denies the target,
and only then starts a dedicated `ecluse mirror` container on the same queue under the old policy.
The stricter proxy serves that late mirror write on a private hit, and repeated cycles then remove
both store copies while an unaffected and a first-party version stay. Two further cases let an
old-policy Dredger delete a version the newer roles permit: a fresh client restores it through real
admission when the public source still holds bytes, and the version stays gone when it does not.
That second outcome is the accepted residual the threat model records, not a defect to repair. The
`ecluse-integration` worker cases cover the same split beneath the roles: one enqueued `MirrorJob`
is refused by a worker booted with a stricter policy and published by one booted with the older
policy, each acknowledged against the emulator.

One further Dredger scenario connects advisory compilation to revocation. Pilot compiles the `v1`
corpus through the product image and uploads it to the emulated advisory store. The proxy syncs it,
and `npm` installs both versions of the corpus fixture the `v2` delta condemns, which the worker
mirrors. Pilot then publishes the `v2` generation, the proxy swaps it in, and a candidate-mode
Dredger cycle deletes the affected version under a named `DenyIfCve`. The scenario then confirms the
next install of that version fails, its artifact request is refused with `403` and re-mirrors
nothing, and the stated fix still installs from the store.

It catches composition-root and cross-component regressions nothing else does. The mount rewrites a
served `dist.tarball` to an absolute installable URL under `ECLUSE_SERVER__PUBLIC_URL`, because
`npm` cannot install the path-relative form.

It gates as its own parallel job the CI `gate` depends on. It is far heavier than the rest of the
gate: an image build, multiple containers, and the npm CLI. But it is hermetic. The nginx and
Verdaccio upstreams are local, so unlike smoke it has no external dependency to flake on, which
makes gating safe. Its weight keeps it out of the local `task gate` and `task check`. Run
`task test-e2e` on demand to build the image, load it, and run the suite. It needs a Docker daemon
and the `npm` and `pip` clients, both from the dev shell, and skips every case as `pending` when
`ECLTEST_E2E_IMAGE` is unset.

The egress guard refuses internal addresses on the public path. So the containers run on
the RFC 5737 documentation subnet `203.0.113.0/24`, which the guard treats as external. The real
default-build image runs unmodified, with no production escape hatch.

This tier runs the real `npm` CLI against real packages, so an upstream lifecycle script
(`preinstall`/`install`/`postinstall`/`prepare`) could execute arbitrary code inside our own CI. The
harness therefore sets `npm_config_ignore_scripts` for every npm child it spawns. The committed
root `.npmrc` carries the same `ignore-scripts=true` for in-repo npm and Renovate. That file cannot
reach the throwaway projects outside the repo tree, hence the env var. A gating case installs a
probe whose `postinstall` would write a sentinel, and asserts the sentinel never appears, so the
guard cannot rot silently. `ignore-scripts` skips lifecycle scripts only, so it leaves the
resilience scenarios alone.

The pip case carries the same prohibition. A source distribution runs its own build backend on
install, so the harness passes `--only-binary=:all:` and installs a wheel alone, and it points
`PIP_CONFIG_FILE` at `/dev/null` because `--isolated` still reads the global and site config files.

## Benchmarks (non-gating)

Use the benchmark tier to assess cost alongside the seven Cabal test suites. None of its
three workflows gates a merge or belongs in branch protection as a required check.
The workflow YAML owns schedules and run options.
Each workflow puts its report in the GitHub run summary and uploads the listed files on that run's page.
Reports support manual comparisons only. No workflow stores a cross-run baseline or consumes another run's results.

| Workflow | Measurement | Downloadable report files |
|---|---|---|
| [Work per request](../.github/workflows/bench.yml) | Time and allocations for the benchmark groups over committed and synthetic corpora | `bench-results.csv`, `bench-output.txt` |
| [Performance acceptance](../.github/workflows/perf-acceptance.yml) | Full-document and selective-decode overhead on live registry documents against reviewed budgets | `perf-acceptance-report.md` |
| [Load](../.github/workflows/bench-load.yml) | npm and PyPI successes, latency, memory, and collector cost through a proxy process under each pod shape, with separate ecosystem sections and baseline sources | `bench-load-results.md`, per pod shape or for the GC-thrash probe |

Read a red result according to its measurement:

- Work-per-request benchmarks fail on build errors, harness crashes, failed complexity assertions, or an advisory row that leaves a version undecidable. They do not compare performance against regression thresholds.
- Performance acceptance fails on an overhead budget breach. An unavailable live registry produces an unavailable result, not a breach.
  Each ecosystem's budgets name the CPU architecture they were calibrated on. On another architecture every leg reports as uncalibrated, and the run passes.
  Its report separates upstream time from Écluse overhead. A breach needs a human decision about a code regression or a budget revision.
- Load benchmarks use `oha` against a proxy process. A run fails when a scenario or a ramp step gets no successful response,
  when the kernel OOM-kills a proxy, when a proxy exits on heap overflow, and when a proxy ends any other way than the clean
  shutdown the harness asks for, early exits included. It also fails when the harness or a proxy cannot boot, when `oha`
  cannot run, and when a fixture preflight sees an unexpected status, index shape, or wheel body. Throughput, latency, and
  memory have no regression threshold. Shared-runner noise and the load run's cost make it unsuitable as a per-PR signal, so
  it never runs on a pull request and never gates a merge.

Budget values and calibration belong in [acceptance/criteria.json](../acceptance/criteria.json).
Corpus pins and capture policy belong in [bench/corpus/pins.json](../bench/corpus/pins.json).

### Load tests under a pod shape

Each load scenario runs in its own child process. The child serves the stub upstreams in process
and starts the proxy as a separate process: `bench-load --serve-proxy`, which boots through the same
path as `ecluse proxy` and reads its configuration from `ECLUSE_*` variables. The proxy dials the
stubs over plain HTTP on loopback, which the `dev-http-egress` build allows, and serves RTS
statistics on a loopback control port. Telemetry is on, with the Prometheus scrape as its only
exporter, so the harness can sample the admission gauges. The proxy logs into pipes that the
harness drains, keeping only the head and tail of each stream, so its cgroup is charged for unread
pipe buffers (at most 64 KiB per pipe) but not for the log's page cache. On CI the build step has
just built or restored the executable, so its text pages are already cached when the proxy starts
and are not charged to it. The harness does not enforce that.

`BENCH_LOAD_POD` names the pod shape: `unlimited`, or cores and a memory limit such as
`2cpu-1gib` or `4cpu-2gib`. Under a limited shape the proxy starts inside its own cgroup, a child
of the directory `BENCH_LOAD_CGROUP` names, with `memory.max` at the limit, `memory.swap.max` at
zero, and `cpu.max` at the cores. The load generator and the stubs stay outside that cgroup. The
proxy links the shipped RTS options and gets only `-T`, for its statistics, through `GHCRTS` at
launch. Its boot reads the cgroup and derives its capabilities, its heap ceiling, and its memory
plan as it would in a pod. The unlimited shape sets `runtime.cores` to the harness's capability
count instead. The workflow delegates the cgroup subtree with `sudo` before the run, enables the
cpu, memory, and pids controllers for it without a task limit, and turns swap off. The cgroup
outlives the proxy, so an OOM kill stays countable after the process is gone, and the harness
retires any proxy cgroup a killed run left. A scheduled run measures `unlimited`, `2cpu-1gib`,
`4cpu-1gib`, and `4cpu-2gib` in a matrix. A dispatch picks one shape, `all`, or `thrash` for the
GC-thrash probe. To measure a branch, dispatch the workflow on that branch after merging this
harness into it.
Hosted runners have four processors, so a four-core shape shares them with `oha` and the stubs.

Each scenario reports:

- successes in the window (the primary figure), refusals (`429` and `503`), other statuses,
  transport failures, and the p50 and p99 of successful responses only
- the proxy's allocation per successful request beside the attempt count, its GC share of CPU,
  its major collections and the mean live data they left, its RTS `max_live_bytes` and
  `max_mem_in_use_bytes`, and whether its small-object live data crossed the compaction threshold.
  A paired scenario or a ramp divides by every success in the window
- the allocation per refusal: the same window's allocation divided by its `429` and `503`
  responses. It and the allocation per success each charge the whole window to one kind of
  response, so each is an upper bound
- the idle floor after boot, before any load: live data after a major collection and the cgroup's
  `memory.current`
- cgroup `memory.peak` against `memory.max` and against the RTS's own peak, `memory.stat` (`anon`,
  `file`, `kernel`, `sock`) as the window closes, the `memory.events` counters, and the CPU time
  the quota withheld during the window
- how the proxy ended: a clean shutdown, a heap overflow (from its own report or the RTS exit
  status), a kernel OOM kill, or another exit
- the CPU admission and the memory admission budget at boot, read from the proxy's boot log, with
  the runtime lines quoted
- `ecluse.serve.admission.in_flight` and the proxy's thread count (`pids.current`) sampled each
  second, and every admission series at the end of the window
- the metadata cache's hit, miss, and collapsed request counts in the window for the full,
  version, and assembled stores, from scrapes of the proxy's Prometheus exposition at each end of
  the window. A collapsed request waited for another request's fetch instead of making its own

Each ecosystem section also shows:

- the rule policy the first scenario's proxy logged at boot: each rule its configuration names,
  with its type, its other keys, and the layers they came from, and the order each mount
  evaluates the rules in. A proxy of either pass that logged a different policy is named
- a cost table that sets each scenario's allocation per success and success p50 beside the same
  figures from the concurrency-one pass, which runs the scenario again on a fresh proxy with the
  base concurrency set to one. A scenario that scales its own connections keeps that scale. The
  table adds the allocation per refusal, each allocation with the count it divides by, and the
  missed and collapsed cache lookups summed over the stores. One request can look up more than
  one store, so these count lookups, not requests. A loaded figure far above its concurrency-one
  figure points at contention, or at work the concurrent requests did not share. A scenario
  outside the concurrency-one pass shows `n/a` for those figures

A boot that fails because the runtime could not start an OS thread is booted once more, two
seconds later, into a fresh cgroup, so every reading comes from the boot that succeeded. That
failure is a task limit reached outside the harness, which sets none. The harness prints the
failure with the process limits, the task counts, and the failed attempt's cgroup as it read them,
and the report counts the boot attempts. Any other boot failure fails the scenario.

Three scenarios stress admission under memory pressure. `npm/herd` sends 100 simultaneous cold
`typescript` listings to an idle proxy. `npm/warm-under-cold` measures assembled hits and retained
selected reads while a second generator drives heavy-tier listings that never reuse an assembled
response. `npm/ramp` steps from 10 to 400 connections, one configured duration per step, and
reports each step. None of the three joins the concurrency-one pass.

The private-copy scenarios model a mirror target that is also the private upstream. Its document
for a package holds the versions the deployment has mirrored. Every listing decodes its own private
copy, because a private read passes the caller's credentials through and cannot share work with
other callers. The private stub returns each capture cut to the newest share of its versions by
publish time (upload time on PyPI), rounded up to a whole version, and at least one. Installs
resolve to recent releases, so a mirror fills from the newest end first.
`Ecluse.Test.Corpus.Subset` makes the cut, as it does for the publish-order merge shape in
[Listing peaks](#listing-peaks). The public stub returns the complete capture, and the public cache
TTL is 0. The private copy stays fixed for the run, so each scenario measures one point:

| Scenario | Private copy of each capture |
|---|---|
| `npm/merge-cold`, `pypi/index-cold` | The comparison point, not a cut: a synthetic overlay of versions the public document does not hold, three for npm and one wheel for PyPI |
| `npm/heavy-private-5pct`, `pypi/heavy-private-5pct` | The newest 5% of the versions |
| `npm/heavy-private-25pct`, `pypi/heavy-private-25pct` | The newest 25% of the versions |
| `npm/heavy-private`, `pypi/heavy-private` | The complete capture, as a private registry that proxies the public one returns |

Each ecosystem's report lists the last three rows in that order, after the cold listing and its
advisory variants. All four rows join the concurrency-one pass.

Three npm and three PyPI scenarios load an advisory database, so the per-version advisory cost
shows in the load report. Before the proxy boots, the scenario compiles the captured advisories
under `bench/corpus/advisories/` through Pilot's compiler, and fails when they compile to no range.
It then serves the artifact from a loopback stub of the object store, outside the proxy's cgroup.
`advisories.url` names the stub's bucket, and `AWS_ENDPOINT_URL` points the proxy's S3 client at
the stub, so the proxy syncs the artifact as it would from S3. The harness takes the idle floor and
starts the scenario only once the proxy's scrape shows the database's
`ecluse.advisory.database.age.seconds`. It fails the scenario when that takes more than a minute
or the proxy exits first. Each scenario is otherwise its no-database counterpart, and the report
lists it right after that counterpart, so their allocations per success sit side by side.

| Scenario | No-database counterpart | Rule policy | What the database lookups find |
|---|---|---|---|
| `npm/merge-cold-advisories` | `npm/merge-cold` | The shipped policy | Advisories for `@babel/core`, `express`, `lodash`, `react`, `request`, and `webpack`, and none for the other captures |
| `npm/merge-cold-all-advisory-rules` | `npm/merge-cold` | The shipped policy with `DenyIfCve` at CVSS 8 and `DenyIfEpss` at 0.5, both failing closed | As `npm/merge-cold-advisories`. The highest EPSS score in the corpus is 0.213, so `DenyIfEpss` never denies and adds only its evaluation cost |
| `npm/revalidate-not-modified-advisories` | `npm/revalidate-not-modified` | The shipped policy | Nothing: `@types/node` has no advisory, so this measures a present database and an absent package |
| `pypi/index-cold-advisories` | `pypi/index-cold` | The shipped policy | Advisories for `numpy` and `requests`, and none for `boto3` |
| `pypi/index-cold-all-advisory-rules` | `pypi/index-cold` | The shipped policy with `DenyIfCve` at CVSS 8 and `DenyIfEpss` at 0.5, both failing closed | As `pypi/index-cold-advisories`, with `DenyIfEpss` never denying for the same reason |
| `pypi/revalidate-not-modified-advisories` | `pypi/revalidate-not-modified` | The shipped policy | Nothing: `boto3` has no advisory, so this measures a present database and an absent package |

A fail-closed advisory rule answers a version it cannot decide with 503, and the report counts
that 503 with the admission refusals. When the proxy records such a failure in
`ecluse.rule.effectful.failures` during the window, the scenario's section flags the count.

`BENCH_LOAD_SCENARIOS` runs a comma-separated subset, such as `npm/merge-cold,npm/herd`.
`BENCH_LOAD_THRASH_LIMITS_MIB` runs the GC-thrash probe instead of the passes: one scenario
(`BENCH_LOAD_THRASH_SCENARIO`, `npm/heavy-private` unless set) at `BENCH_LOAD_THRASH_CPUS` cores
(two unless set) under each listed memory limit, highest first. The probe records OOM kills and
heap overflows as its reading, so they do not fail it. It fails only when no limit produced a report.

### Benchmark captures

The catalogue records complete npm packuments and PyPI PEP 691 Simple JSON snapshots.
Each capture keeps the upstream response body unchanged, including prereleases, operational fields,
and PyPI serial metadata. The catalogue records its source, capture time, actual media type, byte
size, and SHA-256 digest. Requests use identity content encoding, so these bytes match decompressed
JSON input rather than compressed transport traffic.

Run `task gen-bench-corpus` only for a deliberate recapture. Version pins identify workloads and do
not limit the captured releases. Run `BENCH_CORPUS_VERIFY=1 task gen-bench-corpus` to check committed
sizes, hashes, provenance fields, and basic document shape without network access.
The harness separately validates each capture through the production adapter before measurement.

| Group | Capture use |
|---|---|
| `wire+project (per package)` | Complete bodies feed decoding and production full-document projection on every iteration. |
| `single-version metadata (per package)` | Complete bodies feed production full-document and selective projections. |
| `cold production reads (per package)` | Unchanged complete bodies pass through the production npm and PyPI full-document and selected-version HTTP readers on every iteration. |
| Realistic serve, merge, rules, and version groups | Inputs derive from complete captures, with preparation outside the measured operation. |
| Load metadata and cache scenarios | Fixture upstreams serve the captured metadata and rewrite artifact authorities for the local harness. The private upstream of the 5% and 25% private-copy points serves cut captures, and that of the 100% points serves the capture bytes uncut. These are derived bodies, not byte-identity measurements. |
| Scaled groups | Synthetic bodies measure growth separately and do not establish wire-to-resident ratios. |

The projection groups measure decoding from held bytes, including the structural guards.
They do not measure source hashing or the production HTTP wrappers.

The cold-read group calls `fetchFullManifest` and `fetchVersionMetadata` through uncached production
clients. Each iteration includes request formation, loopback HTTP, bounded response consumption,
source hashing on full reads, extraction, projection, and artifact-location checks. The manager
redirects every request to the local replay server and disables proxies. No timed request reaches a
live registry. Source bytes and artifact URLs stay unchanged. Metadata is always cold, although the
HTTP manager can reuse connections. The group measures neither cache hits nor cache admission.

Each row reports time and RTS allocation per capture in `bench-results.csv`.
Rows name the body ceiling and distinguish successful reads from body-limit refusals.
Every capture runs with `defaultLimits`. A capture above that body ceiling also runs with an explicit
cap equal to its byte size. Other structural limits stay unchanged. Preflight fails on unexpected
errors, missing selected versions, incorrect byte counts, or a full-document digest mismatch.
Selected reads target the same greatest retained text key as the selective projection group.

Every measured result is compared with its preflight reference through typed fields and compact
payloads. This forces the returned data without encoding it or forcing the lazy cache charge.
The comparison cost and local HTTP server work contribute to the result. Capture loading, preflight,
manager creation, and server startup sit outside the measured iteration. Held inputs and reference
results remain resident. RTS allocation does not measure total process memory or native parser storage.
The group excludes TLS, external network latency, compression, response assembly, and telemetry export.

Work-per-request reports do not provide an automatic comparison against main or a fixed control group.
[#1305](https://github.com/AlexaDeWit/Ecluse/issues/1305) owns that comparison. Match successful work,
capture hashes, limits, and forcing when comparing these rows against another revision.
Replacing trimmed captures breaks historical comparability, so comparisons must use the same capture hashes.

The wire-to-resident factor still requires measurements of raw and typed retention on these bodies
under [#1421](https://github.com/AlexaDeWit/Ecluse/issues/1421).
Capture byte sizes alone do not establish an expansion ratio, and this corpus change does not
recalibrate `expandWireBytes` or acceptance budgets.

### Advisory rule rows

The `rules with an advisory database (per package)` group shows what an advisory database adds to
one request's rule phase. It measures the typescript, react, @types/node, and numpy captures, and
reports time and RTS allocation per request like the other rows. Each iteration prepares the policy,
decides every release, and builds the filter plan. A row with the fail-closed deny rules fails when
any version ends undecidable, because an unanswered read would measure an outage instead of a
decision. Under the shipped policy, an artifact that serves nothing makes the remediation rule
abstain, which would pass for a speed-up. So setup also checks that each artifact serves ranges:
the captured one for react and numpy, and the generated one for its targets and every filler
package.

| Row set | Advisory database | Policy |
|---|---|---|
| `shipped policy without a database` | None | `AllowIfOlderThan` (7 days) and `AllowIfRemediatesCve` |
| `shipped policy over corpus advisories` | Captured advisories | `AllowIfOlderThan` (7 days) and `AllowIfRemediatesCve` |
| `all advisory rules over corpus advisories` | Captured advisories | The shipped policy plus fail-closed `DenyIfCve` (CVSS 8) and `DenyIfEpss` (EPSS 0.5) |
| `all advisory rules over synthetic advisories` | Generated worst case | The same four rules, over typescript and numpy only |

The captured advisories are the osv.dev records for the corpus packages, unchanged, under
`bench/corpus/advisories/`, with the EPSS feed rows for their CVE aliases. Their licences and
attribution are in `bench/corpus/advisories/README.md`. The `advisories` entry in
`bench/corpus/pins.json` pins each file's size and SHA-256 and records the sources and capture
times, and setup refuses a file that differs from its pin.

The generated worst case gives typescript and numpy 200 advisories each. Each advisory spans a
sixteenth of the package's releases and is fixed at a real release. The advisories alternate
between a critical (9.8) and a medium (4.2) vector, one on each side of the `DenyIfCve` threshold
of 8, and their EPSS scores step from 0 to 0.95. The generated database also holds one advisory for
each of 20,000 other package names, so each lookup searches a table far larger than its package's
own rows.

Setup compiles both through `Ecluse.Core.Osv.Compile` over loopback HTTP, as Pilot does, and a slot
serves each artifact, as a synced mount reads it. The repository holds no compiled artifact, so a
schema change recompiles the fixtures on the next run. The run makes no external request.

The worst-case rows run one iteration each, to bound their run time. Their reports carry no
spread estimate. Allocation varies little between runs, so one iteration still gives their
allocation per request.

### Request patterns

The finite replay families run once per captured identity sequence, with a new proxy and empty
stores for each cell. They do not run the HTTP warm-up or the concurrency-one attribution pass.
The existing warmed repeats remain controls. Finite hot-set repeats include their first cold pass.
No family represents all deployments. The small captured identity space limits extrapolation.

| Family | Axis |
| --- | --- |
| Hot-set control | Set size and repeat count |
| Cold install | Distinct names, each requested once by one client |
| CI fleet | Clients sharing one sequence and their start skew |
| Heterogeneous fleet | Shared fraction and disjoint private names per client |
| Zipf | Exponent, captured space, seed, and finite draw count |
| Restart | Empty process cache and interval between client arrivals |
| Scan | Repeated full scans with one shared selected-version and assembled byte bound |

`GHCRTS=-T bench-load npm/pattern-cold-install` runs one cell, with the RTS statistics the driver
turns on for each scenario it runs. Substitute `pypi` for its Simple-index trace.
Each cell stops when its finite sequence completes or its whole-replay deadline expires.
Duration knobs apply only to the legacy duration drivers.
Clients send sequential requests after their scheduled start. Slow responses extend the run.
Restart models post-restart arrivals into an empty cache, with the production 60-second TTL.
It does not model a pre-restart heap or claim that a short default run measures expiry.

| Environment variable | Default |
| --- | --- |
| `BENCH_PATTERN_NAMES` | All captured names, or two per heterogeneous client |
| `BENCH_PATTERN_CLIENTS` | Four, or two heterogeneous clients |
| `BENCH_PATTERN_SKEW_US` | 100000 |
| `BENCH_PATTERN_ROUNDS` | Four for repeat, scan, and Zipf draws |
| `BENCH_PATTERN_OVERLAP` | 0.5, rounded down to a common-name count |
| `BENCH_PATTERN_ZIPF_EXPONENT` | 1.1 |
| `BENCH_PATTERN_ARRIVAL_US` | 100000 between restart clients |
| `BENCH_PATTERN_SEED` | 42 |
| `BENCH_PATTERN_DEADLINE_US` | 120000000, covering client start delays and response reads |
| `BENCH_PATTERN_FULL_BYTES` | Must be zero. The local backend never retains full metadata, regardless of capacity |
| `BENCH_PATTERN_CACHE_BYTES` | Unset, the proxy sizes the shared local byte budget from its heap. A set value must be positive |
| `BENCH_PATTERN_NOW` | Latest authenticated capture time plus two days. Override with an ISO8601 UTC time |
| `BENCH_PATTERN_SELECTED_VERSION` | Unset for listing-only. `pinned` follows each npm listing with its captured public tarball coordinate |

Selected npm replay measures the HTTP metadata gate: listing, private tarball miss, public version
admission, and artifact relay. It projects the pinned artifact from the complete capture and renders
the production tarball route. The public stub supplies labelled synthetic artifact bytes, so this
sequence does not model an npm install or validate the captured integrity digest against a download.
Public metadata and artifact request counts stay separate. Captured metadata and policy stay unchanged.

Every pattern cell reports its evaluation clock, which the proxy's rules evaluate against. Paired
listing-only and artifact-follow-up cells use the same clock. Set `BENCH_PATTERN_NOW` explicitly when
comparing runs from different captures. Duration-driven scenarios evaluate against the wall clock.

RTS figures describe the proxy process alone. Its peak statistics cover its whole life, including
boot. GC-observed live heap does not establish the maximum transient working set.
Timed allocation and GC deltas cover the replay window.

Unsupported distinct-name and overlap requests fail instead of creating synthetic package aliases.
Each report states the parameters, distinct wire bytes, and shared accounted capacity.
Occupancy, retention refusals, retention fraction, and collapsed fraction remain per-store observations. The wire-to-resident comparison
uses matching accounted bytes for the full store, computed through production projection and
the historical `weighCacheEntry` helper before measurement. Version and assembled working-set bytes remain unavailable.
The separate full-store wire-equivalent estimate excludes retained artifact keys.
Full candidate accounting runs only during diagnostic preparation. Local requests never weigh or
retain full entries, and their effective full capacity is zero. The `assembled-response-hit`
and cache capacity scenarios measure assembled-response reuse, while full reads still fetch.
The selectors `npm/cached-public-hit` and `pypi/cached-public-hit` were renamed to
`npm/assembled-response-hit` and `pypi/assembled-response-hit`. Update benchmark invocations accordingly.
The finite report retains scheduled, completed, successful, refused, other HTTP failure, transport
failure, and unfinished totals and rates. Its success fraction divides by all scheduled requests.
Successful throughput and latency exclude error responses. HTTP refusals count 429 and 503, while
other non-success statuses have a separate count. Allocation divides by successful responses, with
the attempt count beside it, and is unavailable when no response succeeds. Public upstream requests remain separate from store outcomes. Selected reads use their provider
capability directly, so there is no full-entry shortcut count.

The `pattern-cold-install-default-body-cap` cell keeps the default body limit. Other finite cells use
a stated benchmark-only cap derived from the largest actual stub body after URL rewriting.
Structural limits stay at their defaults. Never read faster refusals as better successful throughput.
Complete captures and the collapse telemetry are prerequisites for interpreting these results.
Capture byte counts must match the provenance manifest before replay starts.
A build without the required telemetry catalogue reports cache evidence as unavailable.

Compare repeated seeds and skew settings at equal total memory and equal successful work.
Full metadata is always ineligible for local retention. Its effective capacity is zero, and
this path performs no retention weighing, encoding, insertion, or capacity-refusal accounting.
`BENCH_PATTERN_FULL_BYTES` accepts only zero and does not select a different retention mode.

Use `BENCH_PATTERN_CACHE_BYTES` to vary the shared eligible-store budget. The previous
`BENCH_PATTERN_VERSION_BYTES` and `BENCH_PATTERN_ASSEMBLED_BYTES` knobs now fail with a migration message.
Report the pod memory target and the aggregate byte and entry bounds for every comparison.
Version and assembled rows report the same shared ceiling, not independently funded capacities. Full candidate charges
are historical diagnostics prepared before measurement, not retained local bytes or admission work.
TTL zero changes both eligible stores. Keep 200-body replay separate from the legacy 304 scenario,
because 304 avoids assembled-store resolution.

## The vendored JSON lexer

The npm and PyPI metadata reads walk the tokens of the lexer in `vendor/json-stream/`. Two kinds of
check hold that walk and that lexer to upstream json-stream's behaviour.

- **Differential properties** in `ecluse-core-unit` compare the walk with json-stream's own parser
  combinators on generated bodies: every emitted field, the byte count, the refusal, and whether a
  failure is the nesting limit. The bodies carry escapes, lone surrogates, invalid and overlong
  UTF-8, raw control bytes, deep nesting, long numbers and wide exponents, duplicate keys and
  truncation, split at random. They run with the suite, or alone with
  `cabal test ecluse-core-unit --test-options='--match Json --match Reader'`.
- **A lexer fuzz harness**, `test/fuzz/json-lexer/lexer_fuzz.c`, runs the vendored C lexer beside
  upstream's at the vendored tree's base commit under libFuzzer, AddressSanitizer and
  UndefinedBehaviorSanitizer. It fails on any difference in return code, lexer state or result
  records, with the input cut into pieces at random. Run it with `task fuzz-json-lexer`, ten minutes
  by default, or pass libFuzzer options after `--`. clang and libFuzzer come from the flake's pinned
  nixpkgs, and the corpus grows under `dist-fuzz/json-lexer/`. It does not run in CI.

Both lexers overflow a signed `long` in `handle_number` on an integer of 19 digits or more, before
they discard the value and parse the digits again. Nixpkgs' hardening makes the overflow wrap, so the
harness builds with `-fno-wrapv` to let UndefinedBehaviorSanitizer see it, and
`test/fuzz/json-lexer/ubsan.supp` names those two functions so fuzzing continues past it. The seed
`nineteen-digit-integer` reaches it. `FUZZ_KNOWN=report task fuzz-json-lexer` reports it instead.

## One pattern for every ecosystem

Every ecosystem follows the testing pattern that npm and PyPI follow. npm is the model, because it
has the most work put into it.
[Adding an ecosystem](adding-an-ecosystem.md) sets out the matching performance pattern. Two
obligations come with the testing pattern, beside the checklist in
[Onboarding an ecosystem](#onboarding-an-ecosystem):

- **Hold each metadata walk to a reference reader.** A differential property in `ecluse-core-unit`
  compares the walk with an independent reader of the same fields, written with json-stream's
  parser combinators. For a new ecosystem, that reader is a test oracle written for the walk. npm's
  reference, `npmFields` in `Ecluse.Core.Registry.Npm.Streaming`, is the reader npm ran before its
  walk. It stays production code, because `Ecluse.Core.Registry.Npm.Project.versionListParser`
  still reads with it. PyPI's reference, `pypiFields` in `Ecluse.Test.Registry.PyPI.Streaming`, is
  the reader PyPI ran before its walk.
- **Give every test a counterpart in every ecosystem.** When a change adds a test, a benchmark row
  or a load scenario for one ecosystem, it adds a counterpart for each other ecosystem. This applies
  wherever the other ecosystem has the same path, even while its support is incomplete, because its
  existing paths still need measuring. A cost that shows in one ecosystem often has a twin in
  another, and a missing counterpart hides it. Where no counterpart can exist yet, the pull request
  names the gap.

## Onboarding an ecosystem

An ecosystem counts as onboarded when it supplies each item below for its supported operations.
Register new modules in [ecluse.cabal](../ecluse.cabal) and the applicable harness entry point.
`<Ecosystem>` denotes the module component, such as `Npm` or `PyPI`, and `<ecosystem>` denotes the corpus directory name.
The pending links identify work needed to bring existing ecosystems up to this bar.

| Obligation | Expected file or module pattern | Worked examples and current gaps |
|---|---|---|
| Unit contracts, gating in `ecluse-core-unit` | Mirror `Ecluse.Core.Registry.<Ecosystem>.*` with `core/test/unit/Ecluse/Core/Registry/<Ecosystem>/*Spec.hs`, including the adapter contracts | [npm](../core/test/unit/Ecluse/Core/Registry/Npm/) and [PyPI](../core/test/unit/Ecluse/Core/Registry/PyPI/). The shared [adapter spec](../core/test/unit/Ecluse/Core/Registry/AdapterSpec.hs) pins ecosystem dispatch. |
| Recorded corpus outputs, gating in `ecluse-core-unit` | List the captures in [Ecluse.Test.Corpus](../test/support/Ecluse/Test/Corpus.hs) with a `CaptureUpstream`, give `core/test/unit/Ecluse/Core/Registry/<Ecosystem>/StreamingSpec.hs` a `CorpusRead`, and record each capture's lines in [corpus-outputs.tsv](../core/test/unit/fixtures/corpus-outputs.tsv) | The [npm](../core/test/unit/Ecluse/Core/Registry/Npm/StreamingSpec.hs) and [PyPI](../core/test/unit/Ecluse/Core/Registry/PyPI/StreamingSpec.hs) specs compare each capture's outputs, computed through `Ecluse.Test.Corpus.Outputs`, with its recorded lines. No tool writes the file. A new capture's spec fails, and its failure output lists the computed lines as Haskell string literals, with each tab shown as `\t`, to review and record. |
| Walk parity, gating in `ecluse-core-unit` | `core/test/unit/Ecluse/Core/Registry/<Ecosystem>/ReaderSpec.hs`, a differential property over bodies from `Ecluse.Test.Registry.JsonBytes` | [npm](../core/test/unit/Ecluse/Core/Registry/Npm/ReaderSpec.hs) and [PyPI](../core/test/unit/Ecluse/Core/Registry/PyPI/ReaderSpec.hs), against the references that [One pattern for every ecosystem](#one-pattern-for-every-ecosystem) names. |
| Adapter integration, gating in `ecluse-integration` | `test/integration/Ecluse/Core/Registry/<Ecosystem>/AdapterIntegrationSpec.hs` for metadata and artifact routes against local upstreams | [PyPI adapter](../test/integration/Ecluse/Core/Registry/PyPI/AdapterIntegrationSpec.hs). Existing npm coverage lives in [PipelineIntegrationSpec](../test/integration/Ecluse/Core/Server/PipelineIntegrationSpec.hs) and its [pipeline specs](../test/integration/Ecluse/Core/Server/Pipeline/), without a separate adapter module. |
| At least one real-client install, gating in `ecluse-e2e` | `test/e2e/Ecluse/E2E/<Ecosystem>/InstallE2ESpec.hs`, with fixtures under `test/e2e/Ecluse/E2E/Fixtures/<Ecosystem>.hs` | npm and pip installs currently share [E2ESpec.hs](../test/e2e/Ecluse/E2ESpec.hs), using [npm](../test/e2e/Ecluse/E2E/Fixtures/Npm.hs) and [PyPI](../test/e2e/Ecluse/E2E/Fixtures/PyPI.hs) fixtures. [#1304](https://github.com/AlexaDeWit/Ecluse/issues/1304) supplies the per-ecosystem spec layout. |
| Walk residency, gating in `ecluse-residency` | `test/residency/Ecluse/Core/Registry/<Ecosystem>/ReaderResidencySpec.hs`, registered in `test/residency/Main.hs` | [npm](../test/residency/Ecluse/Core/Registry/Npm/ReaderResidencySpec.hs) and [PyPI](../test/residency/Ecluse/Core/Registry/PyPI/ReaderResidencySpec.hs) check that eight times more dropped input leaves the bytes a walk holds level, sampled through `Ecluse.Core.Registry.Json.WalkProbe`. |
| Metadata residency captures and limits, gating in `ecluse-residency` | Append the ecosystem's capture list to `packages` in [Probe.hs](../test/residency/Ecluse/Core/Server/MemoryModel/Probe.hs), and add its arms to that module's `project`, `captureUpstream`, `readSource`, `streamFull` and `readLegacySource`. In [MemoryModelResidencySpec.hs](../test/residency/Ecluse/Core/Server/MemoryModelResidencySpec.hs), add its arms to `capture`, `envelopePermille`, `peakLimits`, `entryBelowSource` and `probeIdentity`, and add it to the ecosystem list of the check that keeps each regression limit below its charge. In `Ecluse.Test.Corpus.Subset` and `Ecluse.Test.Corpus.Merge`, add its document cut and its heavy-base text | Seven of these pass silently when left out. `packages` feeds both metadata residency specs, so a capture list it does not append is skipped, and the only corpus-wide check is that some capture exceeds 3,687,514 bytes. The limit check covers only the ecosystems in its `for_ [Npm, PyPI]`. An ecosystem without peak limits skips the [listing checks](#listing-peaks), one that `entryBelowSource` does not name skips the entry-below-source check, and one without retained-heap limits takes the generic ones. `readLegacySource` reads, and `Ecluse.Test.Corpus.Merge` cuts, an ecosystem they do not name as npm. [Listing peaks](#listing-peaks) holds the calibration. |
| Read evaluation, gating in `ecluse-residency` | The same captures, and an arm for the ecosystem's served-document form in `documentKeys` in [MetadataResidencySpec.hs](../test/residency/Ecluse/Core/Registry/MetadataResidencySpec.hs) | Without that arm, the weak-pointer check of [Read evaluation](#read-evaluation) finds only the document itself and fails. |
| Work-per-request instance and corpus | Register an `EcosystemBench` in [Ecluse.Test.EcosystemBench](../test/support/Ecluse/Test/EcosystemBench.hs), with frozen bytes under `bench/corpus/<ecosystem>/`, pins in `bench/corpus/pins.json`, and a synthetic byte generator | npm and PyPI run every metadata group through the shared record. [PyPI captures](../bench/corpus/pypi/) use the shipped PEP 691 Simple JSON format. Generator checks cover decoding, projection, selective reads, and artifact URL rewriting. New instances require no changes to the benchmark groups or report renderer. |
| Performance acceptance budgets | Ecosystem budgets in `acceptance/criteria.json`, consumed by `acceptance/app/Main.hs` using the benchmark corpus | The [driver](../acceptance/app/Main.hs) measures live npm packuments and PyPI PEP 691 Simple JSON documents for the shared corpus identities. Each ecosystem has its own report section. [Criteria](../acceptance/criteria.json) record the budgets and calibration evidence. Frozen capture bytes are not acceptance measurements. |
| Load fixture | `bench/load/Ecluse/BenchLoad/<Ecosystem>.hs` exporting an `UpstreamFixture`, registered in `bench/load/Main.hs` | [npm](../bench/load/Ecluse/BenchLoad/Npm.hs) and [PyPI](../bench/load/Ecluse/BenchLoad/PyPI.hs) run metadata, artifact, and cache scenarios through shared proxy wiring. Each fixture gives `Ecluse.BenchLoad.PrivateCopy` its corpus, stub, listing URL and `Ecluse.Test.Corpus.Subset` cut for the private-copy scenarios. PyPI checks PEP 691 indices and wheel bodies before load, and uses a labelled configured baseline. Its eviction cache stays below the actual corpus working set. PyPI worker mirroring waits for [#765](https://github.com/AlexaDeWit/Ecluse/issues/765). |

The shared residency gate remains in
[`test/residency/Ecluse/Core/Server/Pipeline/TarballResidencySpec.hs`](../test/residency/Ecluse/Core/Server/Pipeline/TarballResidencySpec.hs).
Extend its cases if an ecosystem adds a distinct artifact relay path.
Live protocol checks use `test/smoke/Ecluse/Core/Registry/<Ecosystem>SmokeSpec.hs`, following
the [npm example](../test/smoke/Ecluse/Core/Registry/NpmSmokeSpec.hs). PyPI has no corresponding protocol smoke module yet.
Smoke coverage never replaces a gating case.

## OSV advisory fixtures

Advisory-shaped test data comes from committed OSV JSON, apart from the benchmarks' generated
worst case. The suites read `test/fixtures/osv/`
(`v1/`, plus the `v2/` delta), and the benchmarks read `bench/corpus/advisories/`
([Advisory rule rows](#advisory-rule-rows)). A suite derives everything it consumes from those files at test time.
No `osv.db` is ever committed as a binary, so a fixture cannot drift from the artifact contract
(`Ecluse.Core.Osv.Schema`). Helpers in `ecluse-test-support` assemble the osv.dev-shaped zip, plus
*hostile* artifacts for rejection tests. They compile the corpus through the real OSV pipeline
(`Ecluse.Core.Osv.Compile`, in `ecluse-core`, so `ecluse-core-unit` can link it). The corpus carries
versions, so shadow-swap tests observe an ETag change and a rule-outcome flip.
`Ecluse.Test.OsvSpec` pins each version's rows exactly, so editing the corpus updates the pin in the
same PR.

## Tests and Docker

The integration and end-to-end tiers are the only ones that start Docker containers. Integration goes
through `testcontainers` (ministack, the OTLP collector). The e2e tier goes through the raw `docker`
harness: the proxy image plus its nginx/Verdaccio data plane. Both stamp every container with two
labels: `com.ecluse.test` = `integration` | `e2e`, and `com.ecluse.test.scope` = a **per-worktree**
id. That id comes from `ECLTEST_SCOPE`, which every container-running target sets:
`task test-integration`, `task test-e2e`, and the `coverage` tier `task check` runs.

Every harness and CI variable uses the `ECLTEST_` prefix, never `ECLUSE_`. The config loader claims
the whole `ECLUSE_` prefix and aborts the boot on any variable under it that is not a config key, so
a harness variable on that prefix would stop the very proxy the tests are booting.

Both harnesses tear their own containers down on a normal exit, and the `docker run`s carry `--rm`.
The gap is a **hard kill** (SIGKILL, OOM, a timed-out command), which runs no cleanup and leaves the
topology behind. Two reaping commands close it, both driven by `scripts/test-containers.sh`:

- **`task test-clean`** removes only *this worktree's* test containers and networks (keyed on
  `com.ecluse.test.scope`), so it is safe to run while other worktrees have suites running. The
  container-running targets run it automatically before and after the suite.
- **`task test-clean-all`** removes *every* Écluse test container/network/image on the
  daemon regardless of scope. Reach for it only when no other suite is running.

Inspect what is lingering with `docker ps --filter label=com.ecluse.test`. The label writer
is `Ecluse.Test.Containers`, kept in lock-step with the reaper.

**Every image the test tiers pull is fully digest-pinned (`name@sha256:...`), and a mutable tag is
never pulled.** A tag can be re-pointed to a poisoned image between pulls, while a digest is
immutable. A *type* enforces it: a pull site accepts only a validated `PinnedImageRef`
(`Ecluse.Test.Container.Image`), so an unpinned pull is unrepresentable and aborts the suite before
pulling. Every pin lives in that same module beside the validator, and a harness names the pin
rather than the digest. To absorb Docker Hub throttling on the shared runners, the CI jobs warm those
exact references first through `scripts/docker-prepull.sh`. The `ci.yml` comments own that rationale.

## What gates, and what doesn't

Two things are easy to get backwards:

- **The integration tier is not "the tier for thorough tests."** A test goes to integration because
  its collaborator can only be a *real* (emulated) service, not because the test is broad. A
  cross-component test that needs no live external service is a **unit** test, even when it wires the
  whole pipeline. The proxy request-lifecycle runs against an in-process WAI stub in `ecluse-unit`.
  Put a test wherever its subject runs *deterministically*.
- **The smoke tier is a drift *detector*, never a correctness *guarantee*.** It depends on
  uncontrolled external services, so it cannot gate, and nothing we rely on for correctness may live
  *only* there. Every load-bearing behaviour owes a deterministic, gating mirror in the unit or
  integration tier. A smoke test only confirms the model still matches the live world. Version
  ordering is the template. The gate checks it offline against a committed fixture, and the smoke
  suite also regenerates that fixture from the live oracles as a differential check.

Beyond the test tiers, two static-analysis jobs gate. **`weeder`** reports library code not reachable
from the entry point (`Ecluse.run`). **`stan`** runs HIE-based partial-function and bug analysis at
the floor in `.stan.toml`. Each is its own parallel job the CI `gate` depends on, and a finding above
its floor blocks the merge. Among the always-on jobs, only `smoke` is non-gating.

The Haskell work runs as parallel jobs, so no job waits on another's steps. `build` compiles every
target and then runs the residency suite, the doctests, and `cabal check`. `coverage` is a matrix
with one runner per instrumented suite. `docs`, `e2e`, `weeder`, `stan`, and `static-checks` each
hold their own runner. `codecov-notify` follows the coverage legs and releases the Codecov statuses.

CI's primary architecture is arm64: every job that builds or tests the code runs on the
`ubuntu-26.04-arm` runner. amd64 is also supported: the release dry-run builds and starts the amd64
image on the `ubuntu-26.04` runner, and no test tier runs on amd64. Every job names its Ubuntu
release, never `ubuntu-latest`, so a move to a new Ubuntu release is a reviewed change.
`scripts/ci-runner-policy.sh` (in `task lint-workflows`) fails a job on any other runner unless its
allow-list names the job with a reason.

The release dry-run also gates. It runs `release-build.yml`, the reusable workflow that
`release.yml` builds its images with, so both architectures build natively and without a cache, as
in a release. `release-dry-run-assemble` assembles the multi-arch index without pushing it,
and `release-dry-run-boot` starts each image with `--version` on its own architecture. No dry-run
job logs in to a registry, signs, attests, or pushes. The nightly run and a manual dispatch also
scan both images' SBOMs with grype. That scan is report-only and never gates.

Every job restores caches and only a main run ever saves one, so a pull request reads the default
branch's entries and adds none of its own. Each cache key has exactly one writer, because GitHub
caches are immutable per key and two savers would race for the one entry.

There is **one Nix-store cache**, for arm64, keyed on `flake.nix` and `flake.lock`. Every cache key
carries the runner's architecture, because a store or build tree from one architecture is useless
on the other, and only arm64 has a writer. A job on an amd64 runner that restores caches starts
cold. The `docs` job writes the Nix-store entry, because it realises the widest closure. It roots
the `.#ci` dev shell and the flake checks, so the saved store carries both, and every other arm64
job restores that one entry. Two entries, one per closure, cost
more than they saved: the two build graphs shared about nine tenths of their derivations, so each
entry held mostly the same store paths, and the job that restored the Haskell one then refetched
the whole dev shell before it could run.

The cabal side keeps two families, because a documentation build wants a doc-variant of every
dependency and cannot reuse the regular one. `build` writes the regular `cabal-store` and
`dist-newstyle`. The Pages job writes the doc-variant `cabal-store-docs-v2` and `dist-docs`. A job
that builds into its own directory under its own flags skips the `dist-newstyle` restore: `coverage`
builds instrumented into `dist-coverage`, and `weeder` and `stan` build with `-fwrite-ide-info` into
`dist-analysis`. Each `coverage` leg writes its own `dist-coverage` key, so no two legs race and
each leg starts warm for the suite it owns.

[`scripts/prune-caches.sh`](../scripts/prune-caches.sh) lists the key families the workflows write.
A family missing from that list is reaped on the next sweep, which is how a retired key stops
occupying the quota.

A PR that edits documentation only skips the Haskell jobs. The `changes` job classifies it
against an allow-list of documentation paths in
[`scripts/ci-classify-change.sh`](../scripts/ci-classify-change.sh), which fails closed: an
unlisted path runs everything. The static checks run on every PR either way, because the site
build reads the very files such a PR edits, and it fails on a broken internal link or anchor.
Such a PR uploads no coverage and skips the `codecov-notify` job, so the required
`codecov/project` status stays pending by design, and the repo owner merges it by administrator
bypass.

The same script skips the release dry-run for a PR whose paths are all documentation, Haskell
source, runbooks, or analysis-tool configuration. The `build` and `docs` jobs compile that source
on arm64, but such a PR builds no image and compiles nothing on amd64. The flake, `ecluse.cabal`,
`cabal.project`, the freeze, the Taskfile, the workflows, the CI actions, the scripts,
`test/oracles/` (it feeds the `.#ci` shell), and any unlisted path run it. A push to main, the nightly
run, and a manual dispatch always run it. The `gate` job accepts a skipped job from these two
filters and from nothing else, so a job that silently never ran still fails the gate.

The Haddock job's flake checks and the release image build run under
`scripts/ci-build-diagnostics.sh`. The image build also prints each derivation's build log, and
Nix fails it after 20 minutes without output. On Linux, the wrapper observes output bytes through
two `tee` processes and their `/proc` IO counters.
Output silence produces process, memory and disk snapshots on stderr. Snapshots exclude
command arguments and environment variables. Missing diagnostics do not change the build result.
Cancellation stops the command's process group after a two-second grace period.

| Variable | Default | Meaning |
|---|---|---|
| `CI_BUILD_QUIET_SECONDS` | `120` | Seconds without output before a snapshot, also the minimum interval between snapshots |
| `CI_BUILD_POLL_SECONDS` | `1` | Seconds between output checks |
| `CI_BUILD_MAX_SNAPSHOTS` | `20` | Maximum snapshots per invocation |

Each diagnostic command has a five-second deadline and a 201-line output limit.
`task test-scripts` checks output streaming, exit status, snapshot limits and cancellation without a Haskell build.

## Coverage: Codecov (gating)

CI measures coverage per gating suite and reports it to [Codecov](https://about.codecov.io/).
Generation is local and tool-agnostic. A suite is built instrumented: HPC, in an isolated
`dist-coverage/` that leaves the normal build cache alone. Then `hpc-codecov` converts the
`.tix`/`.mix` output to Codecov's native JSON. `scripts/coverage.sh` produces one tier. The Taskfile
`coverage` task assembles the merged view inline.

**Codecov is the merged authority, and `task coverage` reproduces it.** Codecov merges the per-flag
uploads into one project total. A single tier's number therefore *under-counts* the modules the
others exercise: only integration covers the SQS `MirrorQueue` and the worker's fetch/publish path.
`task coverage` runs the three instrumented unit suites plus `ecluse-integration` and
`hpc combine --union`s them into `coverage/combined.json`, so it agrees with the dashboard. It runs
the integration tier, so it needs a Docker daemon. Without one it fails and points at the fast path.
For a quick, Docker-free loop, `task coverage-unit` (default `SUITE=ecluse-unit`, or another suite)
measures one tier and prints loudly that it is a partial view.

**What CI uploads.** The `coverage` job is a four-leg matrix, one runner per instrumented suite.
Each leg builds and runs its own suite through `scripts/coverage.sh` and uploads the JSON that
produces: `ecluse-core-unit`, `ecluse-runtime-unit`, and `ecluse-unit` (all under the Codecov flag
`unit`), and `ecluse-integration` (flag `integration`). Only the integration leg needs a Docker
daemon.

**When the Codecov statuses post.** `notify.manual_trigger: true` in
[`codecov.yml`](../codecov.yml) holds every Codecov status and comment until the CLI asks for them.
The `codecov-notify` job makes that call, and it runs only once all four legs are green, so
`codecov/project` and `codecov/patch` post once, against the complete four-upload report. A failed
leg skips the job, and the statuses then never post: `gate` is already red through `coverage`, and
a required status that stays pending is the correct outcome rather than a number read off a partial
report. The smoke and e2e tiers upload nothing: they are not built
with HPC, so a line only they exercise reads as uncovered. Never reason "the e2e test covers it". A
path that needs coverage needs a unit or integration test.

The combined command removes a *reporting* confusion, a local single-tier read that disagrees with
the merged dashboard. It does not paper over gaps. If the *merged* report still shows a module's
error arms red (e.g. `Worker.hs`'s fail-closed integrity-mismatch branch), that is a genuine
uncovered path a test owes.

The gate is Codecov's two commit statuses, both in [`codecov.yml`](../codecov.yml).
`codecov/project` allows no regression versus the PR base, within a 1% threshold. `codecov/patch`
requires new and changed lines at ≥ 85%, a floor that verifies behaviour rather than a number to
chase. Uploads use GitHub OIDC (`use_oidc: true`), so there is no `CODECOV_TOKEN` to leak. Coverage
measures library code only. It excludes `app/**`, `bench/**`, and `test/**`, and drops every
`Ecluse.Test.*` module of `ecluse-test-support` from the HPC report too.
[`docs/style.md`](style.md) → "Data types and deriving" decides which derived instances the 85%
patch bar treats as accepted partials.

**References:** [testcontainers](https://hackage.haskell.org/package/testcontainers) ·
[ministack](https://github.com/ministackorg/ministack) (local AWS emulator, image
`ministackorg/ministack`, port 4566).

## Style

Tests are documentation too, so keep them as readable as the code.

- **Structure with `hspec`**: `describe` per function/area, `it` with a full-sentence
  expectation.

  ```haskell
  describe "evalRule" $ do
      it "AllowScope allows a matching scope" $
          evalRule inertRuleDeps ctx (AllowScope (mkScope "myorg")) (pkg (Just "myorg") 0)
              >>= (`shouldSatisfy` isAllow)
  ```

- **Name fixtures and helpers, and give them signatures** (`now :: UTCTime`,
  `pkg :: Maybe Text -> Integer -> RuleEvidence`). A small builder that fills defaults and
  exposes only the axis under test keeps each case to one line.
- **Add small predicate/extractor helpers** (`isAllow`, `approvedBy`) instead of inlining
  pattern matches in assertions.
- **Express invariants as `hedgehog` properties** under `describe "properties"` with `forAll`
  and `(===)`: an invariant that must hold for *every* input (order-independence, a round-trip
  law) belongs here.
- **Share cross-suite helpers through `ecluse-test-support`** (`test/support/`). A helper more
  than one suite needs lives there, never copied per suite. Its modules mirror the main-library
  namespace, so a helper for `Ecluse.X` lives in `Ecluse.Test.X`: the digest fixtures and
  `unsafeHash` for `Ecluse.Core.Package` live in `Ecluse.Test.Package`. Cross-cutting helpers
  live in `Ecluse.Test.Support`. A helper only one suite uses stays local.
