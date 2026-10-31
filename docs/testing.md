# Testing strategy

Where a test belongs, what each tier gates, and how CI measures coverage. One thing decides a
test's tier: **the external collaborator the code under test needs**, and so how deterministic the
test can be. Three kinds:

- **unit** needs no collaborator (pure logic or in-process doubles),
- **integration** needs an *emulable* service, reached through a container,
- **smoke** needs an *un-emulable* live service.

The first two are hermetic and **gate** merges. Smoke makes live calls and is **allowed to fail by
design**. Residency and end-to-end checks add further gating tiers.
The suite declarations live in `ecluse.cabal`. Read *What gates, and what doesn't* before
choosing a tier for a new test.

## Unit tests: `ecluse-core-unit`, `ecluse-runtime-unit`, `ecluse-unit` (gating)

Deterministic `hspec` and `hedgehog` tests cover pure logic and local stubs, including the rules
engine, parsers, and configuration. They run on every push without Docker or live external services.
Properties exercise the rules engine: deny-by-default, deny-precedence over allows, and per-rule
predicates.
This tier tests the credential provider's refresh, cache, and expiry policy with an injected clock
and a fake `mintToken`. A local HTTP stub tests CodeArtifact token and expiry extraction.
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

Exercise the mirror queue and advisory transport against a real emulator, driven by `testcontainers`.
The AWS backend runs against a **ministack** container, a lightweight LocalStack alternative, with `amazonka` pointed at `http://<container>:4566` and
throwaway credentials. One ministack container serves the whole test process, so each spec names its
own queues and buckets. The telemetry specs run a real OTLP **Collector** container the same way. Both
are hermetic: no real cloud account, no real credentials.

The complete tier needs a running Docker daemon. CI's `ubuntu-26.04-arm` runner provides one.
Run `task test-integration` for the complete fixed declaration list in [integration/Spec.hs](../test/integration/Spec.hs).
The separate `cabal run ecluse-integration-manifest` command prints its JSON manifest without acquiring services.
Independent obligations and actual Hspec declarations must agree before fixture acquisition.
Missing prerequisites, empty suites, pending cases, and missing execution results fail.
Reports and the full manifest go under `scratchpad/integration/<run-id>/`.
In-process integration records an inapplicable product image identity.
The instrumented coverage run executes every group and audits the full report.
It retains one integration report, the other three suite reports, and the existing product-module denominator.

> **Token-mint caveat.** No emulator covers CodeArtifact's `GetAuthorizationToken` API.
> The [unit spec](../runtime/test/unit/Ecluse/Runtime/Credential/CodeArtifactSpec.hs)
> asserts the token and expiry extracted from a stubbed response. Other unit tests cover refresh,
> cache, and expiry with an injected clock and fake mint. No automatic test verifies a real AWS mint.

## Residency gate: `ecluse-residency` (gating)

The bounded-memory streaming gate streams a 1 MiB and a 100 MiB artifact through the tarball relay.
It covers both the trusted private-hit leg and the gated public leg. It asserts that peak live bytes
stay invariant in artifact size within a fixed margin. It is its own suite, not an
`ecluse-integration` spec, because the measurement needs process isolation and the RTS statistics
flag (`-with-rtsopts=-T`) in its `ghc-options`. It runs outside coverage too. No Docker, loopback
WAI stubs only. Run: `cabal test ecluse-residency` (or `task test-residency`). `task check` includes
it via `cabal-checks`.

Managed state has an isolated retained-graph check in `Mirror.State.WeightResidencySpec`.
Each child decodes the unchanged 1000-version record, compares it with itself through
equality, and retains only the record through a stable pointer. This forces expected descriptors,
opaque parsed versions, verified hashes, companion references, and source fields independently
of the weigher. Post-GC live growth must fit the record charge alone. Release uses the same
16 KiB and one-tenth limits as the other retained probes. Smaller adversarial controls cover
repeated descriptors and hashes, verified identities, companions, long versions, and text backing aliases.
These controls extend the decoded graph directly where a wire round trip would remove aliases.
The cold, warm, and replacement probes retain their separate combined lifetime checks.
The state-only charge is not an admission allowance. Update, worker, encoding and inventory
reservations retain their conservative selected-release accounting.

Archive decoder probes run on the pinned Linux/glibc runtime. Hosted residency proof runs on
arm64. The test-only allocator wrapper and its runtime results remain unverified on amd64.
After four warmups, each gzip, bzip2 and XZ case performs 64 early exits through return, refusal or
cancellation without a GC. VM growth and allocator-retained growth must each stay within 1 MiB.
The Haskell control allocates at least 16 MiB while meeting both gates. Eight simultaneously open
XZ decoders must exceed both gates before their scopes close.

The metadata deflater has its own probes, which share the archive probes' child process, control
and allocator counter. They cover the same 64 early exits by return, refused charge and
cancellation, and a fourth set that cancels at the third output buffer of an incompressible body.
A workspace probe samples the allocator while a deflater is open and requires the native peak plus
one output buffer charge to fit the 512 KiB workspace the deflater charges. The peak was 268,192
bytes on both arm64 CI and the amd64 build host, with 0 bytes left after release. A heap probe
holds a 1 MiB incompressible body's gzip output within the buffer charges.

The allocator counter adds glibc `mallinfo2` fields `uordblks` and `hblkhd` across all arenas and
allocator mmap regions. It includes freed thread-cache chunks and allocator overhead, so it does
not count exact live decoder ownership. ByteString payloads use the GHC heap. The Haskell control
checks that separation on the hosted target. Unsupported allocator platforms fail the probe
explicitly. The report retains RSS as diagnostics because freed pages can remain resident and
RSS growth does not track Haskell allocation bytes. Each row names its mode and includes the
allocator fields before and after the interval. No allocator tunables or caches change.

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
probes read each capture's held bytes through the production read driver, under the default limits
with a body limit of at least the capture's size. They do not execute the HTTP exchange or prove
that shipping response limits admit each capture. The shared shape's entry holds the read's typed
projection and serving document with a size and digest that the probe takes from the capture's
bytes. An entry with the read's own digest retains 4 KiB more in this probe, the pinned block that
digest sits in, and the shape's figures leave that block out.

Each capture has its own regression limit for each retained shape: its measured figure plus 3%,
rounded up to a permille. Retained bytes repeat to the byte between runs, so the whole 3% is room
for a regression, and growth of more than 3% fails. The figures are current, not historical maxima,
so a recalibration after a gain lowers the limit. These limits hold authenticated fixtures. They are
not a universal metadata expansion model, and they stay independent of the admission factors.

[Build job 117558292470](https://github.com/AlexaDeWit/Ecluse/actions/runs/39085443993/job/117558292470)
measured the figures at `8981c1b82`, with GHC 9.10.3, Cabal `-O1`, one capability and a warmed
process per shape. The nine main runs before it gave the same retained bytes. The table gives the
largest figure for each ecosystem and shape, and that capture's limit.

| Ecosystem | Retained shape | Largest heap bytes per source byte | Package | Its limit |
|---|---|---:|---|---:|
| npm | Wire bytes | 0.999793 | @types/node | 1.030 |
| npm | Raw `Value` | 6.473954 | express | 6.669 |
| npm | Typed projection | 0.446552 | react | 0.460 |
| npm | Shared cache entry | 0.699300 | react | 0.721 |
| PyPI | Wire bytes | 0.996879 | numpy | 1.027 |
| PyPI | Raw `Value` | 4.116995 | boto3 | 4.241 |
| PyPI | Typed projection | 1.732844 | requests | 1.785 |
| PyPI | Shared cache entry | 2.601638 | requests | 2.680 |

A shared cache entry is what a listing's full read holds. Its gate is a regression limit, and the
same test also checks that it stays within the memory gate's full-read charge, read from each
ecosystem's adapter. The same test checks the listing output charge: twice a shared entry's encoded
size, for the lazy encoding and its strict copy, must stay within the output charge. The
[listing probe](#listing-peaks) checks both charges against a read's peak and a render's working
set. Raise a charge in the adapter, not here, when a representation outgrows it.

The largest shared entry also sets the kept charge: what a completed read holds until the render.
The kept charge is 1.25 times the largest shared figure, rounded up to a tenth. That is 0.9 for npm
(react, 0.699300) and 3.3 for PyPI (requests, 2.601638). Each shared entry's regression limit must
stay below the kept charge, and `CalibrationResidencySpec` derives the charge from these figures.
Each denominator is the original authenticated source size, including omitted fields.
For example, the TypeScript shared shape retains 7,343,736 heap bytes from 15,693,959 source bytes.
Its re-encoded serving document is 10,181,045 bytes. That encoded size and the source probe's
`compact_byte_estimate` are different from measured retained heap, and neither is this gate's denominator.

The 48 rows left -19,912 to -2,480 bytes above their warmed baselines after release.
The 16 KiB release tolerance leaves 18,864 bytes above the observed maximum.
The second release condition requires at least 90% of each held growth to disappear.
Signed integer differences preserve samples that fall below baseline without unsigned wraparound.

The compact denominator gives a different accounting ratio. Boto3's shared retained bytes
divided by its compact encoding equal 2.974360272, the maximum across both ecosystems. The fixed 7.5
cache-weight factor leaves 152.2% margin above it. It is not an active-work bound.
The local provider retains selected releases and assembled bytes, so full shared shapes do not
size its entry-count control. An assembled-output mean cannot size a shared count of both forms.
These residency measurements do not determine the shared entry-count allowance.

Separate Vite and Next source probes give held-byte/compact-estimate ratios of 6.4900 and 6.4389.
Their exact encoded sizes are unmeasured. Those probes force accounting without warmed preparation
or derived rendering, so they do not establish the same fully forced retained envelope.

### Listing peaks

PyPI's full-read charge is 3.5 after retaining canonical sidecars.
npm keeps its full-read charge of 1.1. Output charges are 2.0 for npm and 1.9 for PyPI.
Kept charges are 0.9 for npm and 3.3 for PyPI.

A listing's charges end in phases. Each full read pays its full-read charge per byte as it reads.
When the read completes, its charge steps down to the kept charge on the bytes it read. A read that
failed keeps nothing. The render adds the output charge. When the body exists, the reads' kept
charges go back, so only the output charge and a gzip response's compression charges stay while
the body is sent. Everything left returns when the request ends. A request that joins another's
public read paid nothing for it, so it gives nothing back.

[Build job 114592183296](https://github.com/AlexaDeWit/Ecluse/actions/runs/38180562637/job/114592183296)
measured all 84 listing shapes and 48 retained probes at actual checkout `372e02a14222e65128bd6cae284e335ca4e41b46`.
That checkout merges source `9d78a8a1f79aac63fe3e401ea9d0ab22cd19ee0e` into base `9b20b528d03a7958cc6471a542db2a516db76732`.

Boto3's single listing reaches 6,157,344 read bytes above baseline from 2,206,044 source bytes, a ratio of 2.791124746.
It sets both qualifying read maxima: realistic listings and all shapes, each restricted to at least one 1 MiB source step.
The 25% margin rounded up to a tenth gives 3.5. The all-shape figure rounds up to 2.8, so the read charge is 3.5.

The 1,024-byte forcing allowance passes in that run and remains unchanged.
The [earlier packed qualification](#qualified-packed-integration) retains its original 3.2 charge and 2.75 limit as historical evidence.

Retaining canonical PyPI sidecars raises the measured output working set. At checkout `372e02a14222e65128bd6cae284e335ca4e41b46`,
NumPy's disjoint merge reaches 1.476585 per basis byte, the largest qualifying realistic shape among all 84 listing rows.
The documented 25% margin rounded up to a tenth gives 1.9.
The same run's 48 retained probes include a maximum twice-encoded/source ratio of 1.753574 for requests, within the charge.
Heavy NumPy's largest output ratio is 1.734852, also within the charge. The source is [Build job 114592183296](https://github.com/AlexaDeWit/Ecluse/actions/runs/38180562637/job/114592183296).
A representation change requires all 84 listing shapes, 48 retained probes, cold reads, renders and default loads.
The historical snapshots below retain their original measurements and limits.

`MemoryModelResidencySpec` also measures the most live data a listing holds, which the memory gate's
[charges](architecture/configuration.md#runtime-sizing-cores-and-heap-ceiling) must cover. For each
capture, a fresh child process:

1. reads the capture in 32 KiB chunks through the production read driver, as a fetch from the
   capture's registry runs it, and holds the cache entry
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

[ProbeMode](../test/residency/Ecluse/Core/Server/MemoryModel/ProbeMode.hs) represents the child modes
and renders their existing arguments and RTS flags.
[HeapSample](../test/residency/Ecluse/Core/Server/MemoryModel/HeapSample.hs) shares the GC sampling
used by the probes. Sampling positions and cadence remain unchanged.

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
that deployments serve. They set the output charge, and each has its own regression limit. The heavy bases
stress the basis instead, and the output charge must hold them. For the read charge, the realistic
shapes set its margin figure, and the heavy bases count only in its all-shape figure. The margin
figure is the larger for both ecosystems, so it sets both charges.

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
- From one step of source up, a single listing's read peak stays under its capture's limit: the
  measured figure plus 4%, rounded up to a permille. A read peak moves by up to 0.34% between
  runs, and 4% is ten times that, rounded up. The figure is the largest over the runs measured.
- From one step of basis up, the output working set of the single listing and of each realistic
  merge stays under its own limit: the measured figure plus 3%, rounded up to a permille. An output
  working set repeats to the byte between runs.
- Every regression limit stays below its charge. Measurements must use the same main base as the
  candidate.
- Each phase fits what the meter holds once the reads step down. The held entries fit the kept
  charges and the headroom of the reads' whole steps. The entries and the output working set fit
  the kept charges and what the output charge adds. The output charge spends that headroom first,
  then takes whole steps. The served body fits what the meter holds once the kept charges go back.
- Every single-source npm listing's held entry stays smaller than the source it was read from.
  Each residency record's `residencyEntryBelowSource` selects this check. PyPI's packed files and typed view
  still outgrow the source, so that boundary remains npm-only.
- A single-source listing's entry frees live bytes when it drops its served document, and the
  document's weight, expanded as a cache expands it, covers them.

The residency-only [Calibration module](../test/residency/Ecluse/Core/Server/MemoryModel/Calibration.hs)
stores exact `Rational` figures. One `Calibration` record per ecosystem holds the listing maxima
that set the charges, and `marginCharge` and `fullReadCharge` apply the charge rule. One `Figures`
record per capture holds its retained shapes, its single listing's read peak, and each realistic
listing's output working set. `heldLimit` and `readPeakLimit` apply the regression rules, and
`keptCharge` applies the charge rule to the largest shared entry.
Production keeps its admission literals. `CalibrationResidencySpec` checks their equality with the
derived charges and pins the rounding. The coverage check fails when a capture has no `Figures`
record, or a record names no capture.

To recalibrate a regression limit, read the figures from the `metadata-residency`,
`metadata-listing` and `metadata-merge` lines of arm64 CI build logs. Take each read peak as the
largest over the runs. Replace the old figure even when the new one is lower. A charge
recalibration updates the listing maxima and the production literal together.

Each `metadata-merge` report adds `derived_full_read_permille` and
`full_read_headroom_per_source_byte`. These fields are informational and add no gate.
Merged reads retain the whole-step checks above, without a per-byte read gate.
Single-document read limits and realistic output limits retain their existing checks.

`PackumentResidencySpec` takes a heap census of the live listing path in a fresh child process
for each capture of at least one meter step. A private and a public origin both serve the capture,
and one listing runs through the production route after a warm-up. The requests share connection
pools but use separate caches, so the measured request reads and renders over warmed transport.
Both runtimes stay rooted through every sample, so their pools and caches keep the process-wide
lifetime used in production.
Each census point runs a deadline sweep before collecting. Disarmed deadlines otherwise retain
completed upstream read workers until the next sweep. A sweep between the response samples can
release those workers' stacks and inflate the apparent request state.
Isolation prevents an earlier example's fixture cleanup from changing a later example's baseline.
It samples live bytes after major collections before the listing and after policy decides every
version. It also samples after merge planning and while the response is sent. A last sample follows
the listing while the test still holds the response. The checks are:

- At both points before the render, what the listing holds fits what the meter holds. This covers
  the policy's and the merge plan's working sets, which the listing probe does not run.
- When policy has decided, the listing holds at least one shared entry, so the census sees the
  documents while the request holds them.
- While the response is sent, the request holds at most 64 KiB more than the test holds after
  it. So the reads' documents are unreachable once the body exists, and their kept charges can go
  back. One shared entry of a capture this size is at least 1.4 MB.

Each example reports a `listing-census` line with the held and charged bytes at each point.

#### Qualified packed integration

The native arm64 [Build job 114400618836](https://github.com/AlexaDeWit/Ecluse/actions/runs/38115887129/job/114400618836)
qualified source `62b458ab54dbdff7a296458ed094be74621ec20b` through synthetic checkout
`bb77622f04db001a47f55e7be29c48d9c1171b04`. Its literal parents are actual main
`efece1f1fca410dc1d214dcc76a99f106e2a0ff7` and that packed source.
The matching [main Build job 114397383334](https://github.com/AlexaDeWit/Ecluse/actions/runs/38113685171/job/114397383334)
measured literal `efece1f1fca410dc1d214dcc76a99f106e2a0ff7`.
Both jobs passed 186 examples and reported all 84 listing shapes, 48 retained probes and 12 authenticated captures.
They used `ubuntu-26.04-arm`, GHC 9.10.3, Cabal `-O1` and one capability per measurement child.

Among sources of at least one 1 MiB meter step, PyPI's realistic and all-shape maxima are both
`1402444/551511`, or `2.542912108734005`, for boto3's single listing.
Actual main's corresponding maximum is `1716370/551511`, or `3.112122876968909`.
The read peak falls by 18.2901% at those measured source identities.
The exact 25% margin is `1753055/551511`. Rounding it up to a tenth gives 3.2.
The all-shape maximum rounds up to 2.6, so the existing charge rule requires `3200` permille.
The independent read regression rule rounds `1.08 * 1402444/551511` up to a quarter, giving `2750` permille.

Applying candidate factors `1100/2000` for npm and `3200/1600` for PyPI to the recorded byte counts
passes every listing's paid read, combined peak and output working-set checks.
The checks keep integer ceiling and whole-step rounding, including the minimum 1 MiB entry step.
Realistic listings also retain the independent output regression limits of 1.75 for npm and 1.5 for PyPI.
The 48 retained records pass their independent limits and the candidate Shared admission checks.
PyPI's independent Shared limit remains 3.5. The fixed cache expansion remains 7.5.

The measured retained Shared bytes change as follows:

- requests: 374,624 to 296,072, a 20.9682% reduction.
- numpy: 5,735,504 to 4,535,168, a 20.9282% reduction.
- boto3: 6,292,008 to 4,958,112, a 21.1998% reduction.

The corpus `charge` goldens record compact cache units from `weighCachedDoc`, not active full-read admission.
All 12 existing charge byte lengths and SHA-256 hashes match the qualified document accounting.
They remain unchanged with the independent 7.5 expansion, as do served bytes, ETags, typed facts and selected-read goldens.

These measurements qualify the stated source identities, not a future factor-update head.
The final candidate requires fresh independent review, required CI, native measurements and default npm and PyPI loads.
The timing requirement remains active. This memory qualification makes no HTTP throughput claim.

#### Packed-file qualification before parser integration

This snapshot retains the earlier packed-file measurements and their original source identities.
Its figures and charge conclusions do not qualify the integrated native-parser head.

- From one step up, the tier fails once a single read's peak or a realistic listing's output
  working set passes a regression limit per ecosystem. The rule rounds 1.08 times the maximum up
  to a quarter step. The read limits are 1.0 for npm (react, 0.862843) and 3.0 for PyPI (boto3,
  2.628749), respectively 0.2 and 0.3 below their charges. The output limits remain 1.75 for npm
  (@aws-sdk/client-s3, 1.523900) and 1.5 for PyPI (boto3, 1.242301), respectively 0.25 and 0.1 below
  their charges. One example checks that each limit sits below its charge.

The listing figures below come from the native `ubuntu-26.04-arm` Build job of
[CI run 38103656822](https://github.com/AlexaDeWit/Ecluse/actions/runs/38103656822/job/114364621043).
The synthetic checkout is `245113a792af38b0206d8bee11581b527ec447f0`. Its literal parents are actual
`main` at `7f214208ea93a54584f0687bef6870d68f7ac8a8` and PR #1619's runtime head at
`bb3a29d7b47bf5af0cef82a47007c66244f19f86`. The branch's merge base is
`21dfe1ed026ced46e76f0c978229144a687809ee`, distinct from that measured main parent.
The baseline measured literal `7f214208ea93a54584f0687bef6870d68f7ac8a8` in
[CI run 38102664684](https://github.com/AlexaDeWit/Ecluse/actions/runs/38102664684/job/114362617303).
Neither qualification makes `bb3a29d7` a merged main commit.

Both jobs passed 182 examples with GHC 9.10.3, Cabal `-O1` and one capability per child.
Each reports all 84 unique listings and 48 unique retained probes. Their input sizes and SHA-256
values match all 12 committed captures at the respective source refs.
Each figure is heap bytes per source byte: the read peak and held entry above the baseline,
the peak through reading and rendering above the held entry, and the served body's length.
Both ecosystems use packed full reads.

| Ecosystem | Package | Source MiB | Read peak | Entry | Peak above entry | Served body |
|---|---|--:|--:|--:|--:|--:|
| npm | typescript | 14.97 | 0.567 | 0.467 | 0.677 | 0.659 |
| npm | @types/node | 10.63 | 0.390 | 0.305 | 0.191 | 0.175 |
| npm | react | 6.67 | 0.863 | 0.698 | 0.526 | 0.495 |
| npm | webpack | 4.96 | 0.493 | 0.327 | 0.725 | 0.712 |
| npm | @aws-sdk/client-s3 | 3.97 | 0.530 | 0.353 | 0.777 | 0.762 |
| npm | express | 0.77 | 1.121 | 0.570 | 0.582 | 0.576 |
| npm | @babel/core | 0.76 | 0.917 | 0.481 | 0.581 | 0.575 |
| npm | request | 0.29 | 1.546 | 0.620 | 0.926 | 0.529 |
| npm | lodash | 0.24 | 1.667 | 0.658 | 1.010 | 0.362 |
| PyPI | numpy | 2.65 | 2.016 | 1.634 | 0.670 | 0.596 |
| PyPI | boto3 | 2.10 | 2.629 | 2.250 | 0.724 | 0.621 |
| PyPI | requests | 0.12 | 3.910 | 2.460 | 1.450 | 0.657 |

The full-read charges use listings with at least one step of sources, merges included.
The [charge rule](architecture/configuration.md#runtime-sizing-cores-and-heap-ceiling) takes the larger
of the realistic maximum with a 25% margin and the all-shape maximum, each rounded up to a tenth.
npm's exact maxima are `178072/186707` (express, publish order) and `173941/162402` (express,
oldest heavy base). Their rounded figures are 1.2 and 1.1, so npm keeps 1.2.
PyPI's maximum for both sets is `1449784/551511` (boto3, single document), or 2.62874901860525.
The margin gives 3.3 and the all-shape figure gives 2.7, so PyPI's charge falls from 4.0 to 3.3.
Its read limit rounds `1.08 * 1449784/551511` up to a quarter step, giving 3.0 instead of 3.5.
The other limits follow the same rule and remain unchanged.

The actual-main PyPI maximum is `1762724/551511`, or 3.196171971184618.
The new maximum is 17.7532% lower. Across all 21 PyPI listing shapes, 20 read peaks fall.
Requests' oldest heavy base rises from 619,832 to 634,176 bytes over baseline, with 191,775 source bytes.
That sub-step listing still fits the whole-step charge. All 84 listings fit the adjusted read,
combined-peak and output working-set bounds. Every served length and output basis matches the baseline.
These heap samples do not establish cold-read timing or HTTP throughput.
The adjusted head still requires fresh independent review, required CI, native measurements and default npm and PyPI loads.

The same run gives these read peaks per source byte. An asterisk marks a listing below one step,
which the per-byte calibration excludes. The paid-step checks still cover it.

| Ecosystem | Package | Single | Identical | Overlapping | Disjoint | Publish order | Heavy base | Oldest heavy base |
|---|---|--:|--:|--:|--:|--:|--:|--:|
| npm | typescript | 0.567 | 0.517 | 0.520 | 0.528 | 0.554 | 0.696 | 0.715 |
| npm | @types/node | 0.390 | 0.348 | 0.351 | 0.358 | 0.377 | 0.576 | 0.600 |
| npm | react | 0.863 | 0.780 | 0.783 | 0.788 | 0.853 | 0.894 | 0.893 |
| npm | webpack | 0.493 | 0.410 | 0.441 | 0.471 | 0.406 | 0.641 | 0.661 |
| npm | @aws-sdk/client-s3 | 0.530 | 0.442 | 0.474 | 0.501 | 0.501 | 0.663 | 0.660 |
| npm | express | 1.121* | 0.864 | 0.942 | 1.007* | 0.954 | 1.046 | 1.071 |
| npm | @babel/core | 0.917* | 0.713 | 0.799 | 0.894* | 0.840* | 0.937 | 0.952 |
| npm | request | 1.546* | 1.142* | 1.273* | 1.255* | 1.198* | 1.247* | 1.278* |
| npm | lodash | 1.667* | 1.169* | 1.427* | 1.402* | 1.343* | 1.282* | 1.393* |
| PyPI | numpy | 2.016 | 1.825 | 1.836 | 1.865 | 1.879 | 1.676 | 1.680 |
| PyPI | boto3 | 2.629 | 2.438 | 2.451 | 2.469 | 2.555 | 2.095 | 2.096 |
| PyPI | requests | 3.910* | 3.352* | 3.310* | 3.686* | 3.391* | 2.987* | 3.307* |

The same run gives each listing's output working set per byte of its basis below.
The realistic maxima from one step of basis up remain 1.523900 for npm (@aws-sdk/client-s3) and
1.242301 for PyPI (boto3). The charge rule therefore leaves the output factors at 2.0 and 1.6.

| Ecosystem | Package | Single | Identical | Overlapping | Disjoint | Publish order | Heavy base | Oldest heavy base |
|---|---|--:|--:|--:|--:|--:|--:|--:|
| npm | typescript | 1.319 | 1.319 | 1.318 | 1.318 | 1.319 | 1.546 | 1.598 |
| npm | @types/node | 0.350 | 0.350 | 0.350 | 0.350 | 0.350 | 0.900 | 0.942 |
| npm | react | 0.990 | 0.990 | 0.987 | 0.988 | 0.990 | 1.325 | 1.329 |
| npm | webpack | 1.423 | 1.423 | 1.411 | 1.402 | 1.178 | 1.602 | 1.679 |
| npm | @aws-sdk/client-s3 | 1.524 | 1.524 | 1.513 | 1.502 | 1.524 | 1.667 | 1.665 |
| npm | express | 1.151 | 1.151 | 1.122 | 1.091 | 1.017 | 1.388 | 1.427 |
| npm | @babel/core | 1.149 | 1.149 | 1.148 | 1.148 | 1.149 | 1.431 | 1.461 |
| npm | request | 1.058 | 1.058 | 0.945 | 0.854 | 0.821 | 1.201 | 1.223 |
| npm | lodash | 1.010 | 1.014 | 1.060 | 0.799 | 0.896 | 1.077 | 1.092 |
| PyPI | numpy | 1.193 | 1.193 | 1.195 | 1.199 | 0.956 | 1.467 | 1.541 |
| PyPI | boto3 | 1.242 | 1.242 | 1.242 | 1.242 | 1.242 | 1.495 | 1.496 |
| PyPI | requests | 1.450 | 1.767 | 1.323 | 1.318 | 1.215 | 1.544 | 1.588 |

Under the output charges, npm 2.0 and PyPI 1.6 per basis byte, heavy bases hold at most 0.963
of their charge from one step of basis up (numpy, oldest heavy base).
Below one step, requests' identical merge reaches 1.105 times its unrounded output charge.
The whole meter steps cover that working set, as the paid-step check requires.

The rules for the charges and the limits take the listings of at least one step, as
[configuration.md](architecture/configuration.md#runtime-sizing-cores-and-heap-ceiling) sets out.
From one step up, each listing's peak above the documents it holds stays below twice its served
body, so twice the served body sets the output working set.

The same run's retained Shared entries peak at 0.696386694 for npm (react) and 2.404536002 for PyPI
(requests). They fit the independent retained gates of 1.0 and 3.5 and the full-read charges of 1.2
and 3.3. The retained gates do not scale with admission and keep their existing calibration.
PyPI's separate charge check now imposes the tighter 3.3 ceiling on every Shared measurement.
All 12 Shared measurements also keep twice the encoded size within the unchanged output factors of 2.0 and 1.6.

| PyPI capture | Actual-main Shared bytes | Packed Shared bytes | Change |
|---|---:|---:|---:|
| requests | 374,560 | 296,008 | -20.9718% |
| numpy | 5,735,496 | 4,535,160 | -20.9282% |
| boto3 | 6,291,928 | 4,958,024 | -21.2002% |

#### Native-parser qualification before packed-file integration

This is the merged parser's qualification snapshot before this packed PyPI integration.
Its measurements and charge conclusions apply to the source identities it names.
They are not new evidence for the factor-update candidate.

The listing figures below come from one native `ubuntu-26.04-arm` Build job of
[CI run 38107103485](https://github.com/AlexaDeWit/Ecluse/actions/runs/38107103485/job/114374944192).
It checked out PR merge `3f63e1e7912aa0b118ea39044e32c368380f9a58`, whose tree matches
`689b7a19f4c3c1840a115cc185582075293c050e` and includes `main` at `7f214208`.
The run used GHC 9.10.3, Cabal `-O1` and one capability per child. It reports 84 unique listings:
all 12 captures in the single-source shape and all six merge shapes.
Each figure is heap bytes per source byte: the read's peak and the held entry above the baseline,
the listing's peak through the read and the
render above the held entry, and the served body's length. npm's figures are for its packed full
read.

| Ecosystem | Package | Source MiB | Read peak | Entry | Peak above entry | Served body |
|---|---|--:|--:|--:|--:|--:|
| npm | typescript | 14.97 | 0.556 | 0.467 | 0.677 | 0.659 |
| npm | @types/node | 10.63 | 0.375 | 0.305 | 0.191 | 0.175 |
| npm | react | 6.67 | 0.837 | 0.698 | 0.526 | 0.495 |
| npm | webpack | 4.96 | 0.464 | 0.327 | 0.725 | 0.712 |
| npm | @aws-sdk/client-s3 | 3.97 | 0.487 | 0.353 | 0.777 | 0.762 |
| npm | express | 0.77 | 0.975 | 0.570 | 0.582 | 0.576 |
| npm | @babel/core | 0.76 | 0.741 | 0.481 | 0.581 | 0.575 |
| npm | request | 0.29 | 0.648 | 0.620 | 0.546 | 0.529 |
| npm | lodash | 0.24 | 0.693 | 0.658 | 0.384 | 0.362 |
| PyPI | numpy | 2.65 | 2.388 | 2.065 | 0.885 | 0.596 |
| PyPI | boto3 | 2.10 | 3.112 | 2.855 | 0.998 | 0.621 |
| PyPI | requests | 0.12 | 3.166 | 3.094 | 0.669 | 0.657 |

The full-read charges use listings with at least one step of sources, merges included.
npm's realistic maximum is 0.837052 (react, single document) and its all-shape maximum is 0.958492
(express, oldest heavy base). The margin figure rounds 1.25 times the first to 1.1, while the
all-shape figure rounds the second to 1.0, so npm's charge falls from 1.2 to 1.1.
PyPI's maximum for both sets is 3.112123 (boto3, single document). Its margin figure is 3.9 and
its all-shape figure is 3.2, so its charge falls from 4.0 to 3.9.
The rules use measured byte counts before rounding, rather than the rounded table cells.

The same run gives these read peaks per source byte. An asterisk marks a listing below one step,
which the per-byte calibration excludes. The paid-step checks still cover it.

| Ecosystem | Package | Single | Identical | Overlapping | Disjoint | Publish order | Heavy base | Oldest heavy base |
|---|---|--:|--:|--:|--:|--:|--:|--:|
| npm | typescript | 0.556 | 0.512 | 0.512 | 0.517 | 0.545 | 0.690 | 0.707 |
| npm | @types/node | 0.375 | 0.340 | 0.341 | 0.345 | 0.365 | 0.566 | 0.590 |
| npm | react | 0.837 | 0.767 | 0.764 | 0.765 | 0.831 | 0.877 | 0.877 |
| npm | webpack | 0.464 | 0.395 | 0.418 | 0.433 | 0.386 | 0.622 | 0.642 |
| npm | @aws-sdk/client-s3 | 0.487 | 0.420 | 0.443 | 0.465 | 0.466 | 0.637 | 0.634 |
| npm | express | 0.975* | 0.772 | 0.795 | 0.786* | 0.824 | 0.934 | 0.958 |
| npm | @babel/core | 0.741* | 0.627 | 0.687 | 0.717* | 0.683* | 0.828 | 0.839 |
| npm | request | 0.648* | 0.673* | 0.757* | 0.564* | 0.838* | 0.735* | 0.876* |
| npm | lodash | 0.693* | 0.706* | 0.682* | 0.667* | 0.666* | 1.000* | 1.050* |
| PyPI | numpy | 2.388 | 2.222 | 2.213 | 2.224 | 2.271 | 1.927 | 1.930 |
| PyPI | boto3 | 3.112 | 2.983 | 2.988 | 2.991 | 3.062 | 2.436 | 2.438 |
| PyPI | requests | 3.166* | 3.364* | 3.239* | 3.263* | 3.546* | 2.685* | 2.674* |

The same run gives each listing's output working set per byte of its basis below.
The realistic maxima from one step of basis up remain 1.523900 for npm (@aws-sdk/client-s3) and
1.242301 for PyPI (boto3). The charge rule therefore leaves the output factors at 2.0 and 1.6.

| Ecosystem | Package | Single | Identical | Overlapping | Disjoint | Publish order | Heavy base | Oldest heavy base |
|---|---|--:|--:|--:|--:|--:|--:|--:|
| npm | typescript | 1.319 | 1.319 | 1.318 | 1.318 | 1.319 | 1.546 | 1.598 |
| npm | @types/node | 0.350 | 0.350 | 0.350 | 0.350 | 0.350 | 0.900 | 0.942 |
| npm | react | 0.990 | 0.990 | 0.987 | 0.988 | 0.990 | 1.325 | 1.329 |
| npm | webpack | 1.423 | 1.423 | 1.411 | 1.402 | 1.178 | 1.602 | 1.679 |
| npm | @aws-sdk/client-s3 | 1.524 | 1.524 | 1.513 | 1.502 | 1.524 | 1.667 | 1.665 |
| npm | express | 1.151 | 1.151 | 1.122 | 1.091 | 1.017 | 1.388 | 1.427 |
| npm | @babel/core | 1.149 | 1.149 | 1.148 | 1.148 | 1.149 | 1.431 | 1.461 |
| npm | request | 1.058 | 1.058 | 0.943 | 0.854 | 0.821 | 1.201 | 1.223 |
| npm | lodash | 0.725 | 0.725 | 0.687 | 0.650 | 0.650 | 1.077 | 1.092 |
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

The same run's retained Shared entries peak at 0.696389 for npm (react) and 3.043151 for PyPI
(requests). They fit both the independent retained gates of 1.0 and 3.5 and the full-read charges
of 1.1 and 3.9. Those retained gates do not scale with an admission charge and keep their existing
calibration. The same 12 Shared measurements also keep twice the encoded size within the unchanged
output factors of 2.0 and 1.6.


### Read evaluation

`MetadataResidencySpec` checks that a full read hands back a fully evaluated result. For each
capture, a fresh child process reads the held bytes through the production read driver, as a fetch
from the capture's registry runs it. It then takes two samples of the same rooted cache entry:

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
`ECOSYSTEM` is `npm` or `pypi`.
PyPI pins use the same PEP 440 canonicalisation as production artifact routes.
`LIMIT` is the decompressed body ceiling in bytes. `PATH` remains the complete source capture.
Modes are `BufferedLegacy`, `BufferedCompact`, `StreamedFull`, `StreamedSelected` and
`StreamedVersions`. The first uses the prior complete Aeson representation. The second buffers the
whole file, then runs the production read driver's full read over the held bytes, so it differs
from `StreamedFull` by its buffering alone. The streamed modes read the file in 32 KiB chunks.
`StreamedFull` and `StreamedSelected` run the driver's full and selected reads, and
`StreamedVersions`, which is npm-only, runs the version-list reader. The two full reads hash the
source inside the measured read, as the driver does, and `BufferedLegacy` hashes its buffered body
there too. `StreamedSelected` and `StreamedVersions` take no digest, so the probe hashes the file
after its last sample and reports that.

Each invocation makes one read with no warm-up. Accounting walks force the retained result without
`Show` or output encoding. `read_project_ns` covers that read, projection and forcing.
The existing `Measurement` record supplies allocation and major-GC live samples.
`compactBytes` and `cacheWeight` are zero because this mode neither encodes nor admits a cache entry.
`compact_byte_estimate` reports the representation estimate separately.

Capture the child process's peak RSS externally, for example with GNU `time`.
That peak includes the runtime, input, output, and foreign library allocations.
RTS allocation and live bytes do not include every foreign allocation. Report both scopes.
The Haskell lexer scans immutable chunks and can construct scalar number tokens while skipping
unknown fields. It never retains a complete unknown value tree.

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

CI runs smoke tests on main pushes, the nightly schedule and manual dispatches.
A pull request skips them. Dispatch `ci.yml` on the PR branch when its change needs a live registry check.

Make live calls to the public npm, PyPI, RubyGems and Cargo registries to check decoding and protocol handling.
The PyPI case reads a project index and one bounded wheel through the shipped adapter, then checks
the bytes against the advertised integrity. Only transport unavailability makes that case pending.
The cases depend on uncontrolled external services, so an occasional failure is normal
and never blocks a merge. The CI `gate` does not depend on them. Treat a failure as a
prompt to investigate (protocol drift or flakiness?), not a blocker. Run: `task test-smoke`.

The hermetic span and metric assertions run in `ecluse-integration`. A request drives an in-process
Écluse, and a real Collector container asserts the spans and metrics arrived. The unit tier covers
config parsing, the denial span-attribute mapping, the JSONL scribe, and the metric-label guard.

## End-to-end conformance: fixed subjects (gating)

The only tier that assembles the whole system through the real composition root and drives it with
the real `npm` and `pip` clients. It runs the default OCI image built by `nix build .#dockerImage`.
Each profile adds the required nginx upstreams, Verdaccio store, queue, and advisory services on a Docker network.
PyPI read profiles need no package-write store. Both full ecosystem profiles require their native write stores, queues and concrete providers.
PyPI write profiles also require durable mirror state and declared companions.
The cases assert client- and mirror-observable outcomes:

- an allow-listed package installs,
- the shipped npm install-script deny blocks a package and never mirrors it,
- the shipped npm external-dependency deny blocks a package whose dependency names a git source
  and never mirrors it,
- the worker mirrors an installed package, and a new project then installs it through a second
  proxy while both public upstreams are down,
- a dependency graph installed from the mirror alone leaves the tree that the same install left
  when public served it,
- mirroring an older version after a newer one leaves the mirror's `dist-tags.latest` alone,
- a tampered artifact fails the integrity gate and never publishes,
- `npm` prints a deprecated version's notice when it installs that version, before and after the
  worker mirrors it,
- `pip` reports a yanked release and the reason for the yank when an exact pin installs it,
- `pip` installs a wheel from a `pypi` mount in hash-checking mode, pinned to the sha256 the
  served Simple index advertised, so the installed bytes are the advertised ones,
- `pip` installs and imports a release through its metadata companion, and refuses a companion
  whose bytes miss the advertised digest,
- each ecosystem fetches an artifact from the gzipping public upstream in identity coding, and a
  client that accepts gzip receives the advertised bytes with no content coding,
- the same admitted PyPI release serves its source distribution with the advertised sha256.

The spec modules sit under `test/e2e/Ecluse/E2E/`, with one directory per ecosystem, so a module's
name states the ecosystem of its cases. The suite's log heads each section with that name, less the
`Spec` suffix. A module in an ecosystem directory holds that ecosystem's cases alone. A module whose
cases belong to no single ecosystem sits beside the directories, and there are two:

- `Ecluse.E2E.MixedEcosystemE2ESpec` holds the one case in which a single proxy serves three
  clients in turn: an `npm` install from the public leg, a `pip` install, a Cargo build, then an
  `npm` install once the mirror holds the version. This case does not assert which leg serves that
  install.
- `Ecluse.E2E.TelemetryE2ESpec` holds the telemetry cases. The telemetry configuration is
  independent of the mount, and the traffic is npm's. Two of its cases need a mount with a mirror
  target: the mirror-span case, and the case that serves a mirrored artifact from the private leg
  under a healthy collector.

| Module | Cases |
|---|---|
| `Ecluse.E2E.Npm.InstallE2ESpec` | The npm mount on the base topology: policy, artifact protocol responses, supported mirror metadata, upstream-latest preservation, and publish refusal with no publication target. One proxy serves every case. |
| `Ecluse.E2E.Npm.MirrorInstallE2ESpec` | The install the mirror alone serves, compared with the same install served from public. Described below. |
| `Ecluse.E2E.Npm.PublishE2ESpec` | First-party publication with a publication target configured. |
| `Ecluse.E2E.Npm.DredgerE2ESpec` | The Dredger groups described below. |
| `Ecluse.E2E.PyPI.InstallE2ESpec` | The `pip` install from the `pypi` mount, and its failure while both public upstreams are down. |
| `Ecluse.E2E.PyPI.MirrorInstallE2ESpec` | Complete-version mirroring, durable recovery, companions and cross-target file selection. |
| `Ecluse.E2E.PyPI.PublishE2ESpec` | Native multipart first-party publication and refusal controls. |
| `Ecluse.E2E.PyPI.DredgerE2ESpec` | CodeArtifact inventory, consent, scored and MAL advisory revocation, and recovery with deterministic control-plane responses. |
| `Ecluse.E2E.AdvisoriesE2ESpec` | Pilot publishes independent scored, MAL, and partial-fix sources for npm and PyPI. The shared laws check exact listings, native client refusal, direct 403, structured advisory audit evidence, and the exact-fix and partial-fix remediation controls. The npm MAL session also observes absent mirror publication. |
| `Ecluse.E2E.PyPI.AdvisoriesE2ESpec` | The native canonical-fix extension checks equivalent version spellings and retains its remediation-disabled and scored-denial controls. |
| `Ecluse.E2E.TelemetryE2ESpec` | Telemetry export under each configuration. An `npm` client supplies the traffic, and the mirror cases require a mirror target. |
| `Ecluse.E2E.MixedEcosystemE2ESpec` | The npm, pip, and Cargo clients use one proxy in turn. |

The cases of one telemetry group share a collector, and each install emits the same span names. A
span assertion therefore reads whole spans through `exportedSpans` and keys on an attribute of its
own case, such as the coordinate of the package that the case installs. The private-leg case owns
a separate coordinate, its public install, the mirror wait, and its second fresh install.

Each declared group owns a complete data-plane lifecycle. Its cases cannot read another group's mutable store state.
Profiles declare npm, PyPI, or mixed mounts and acquire the clients and services their cases need.
The harness and fixtures stay in `Ecluse.E2E.Harness.*` and `Ecluse.E2E.Fixtures.*`.
The runner checks actual declarations against independent obligations before acquiring Docker resources, images, clients, or credentials.
A required run fails when a prerequisite is absent. Pending examples never satisfy its coverage audit.

One nginx container, the stub, answers for every registry name the product dials: both public
upstreams, the mirror's front, a private upstream that holds nothing, and the private cache of the
Dredger groups. A case that needs one of them to fail holds a fault on that route alone:

| Control | Fault |
|---|---|
| `withPublicUpstreamsDown` | The stub closes every connection to the npm and PyPI public upstreams with no answer. The other routes keep answering, so only a private store can supply a client. |
| `withPrivateCacheDeletesRefused` | The private cache refuses every write method and still answers reads. |

Each control rewrites the stub's configuration, reloads nginx, and returns once the stub's log shows
that every worker of the old configuration has stopped accepting connections. It restores the
routes the same way when its action ends. A case confirms a public outage through its own proxy
before it relies on one: `npmPublicReachable` asks that proxy for a package that only the public
upstream holds. The stub's access log records the route, method, status, path, response coding,
and requested codings of every request it finishes. `stubReadsNow` reads it back, so a case can
name the store that answered a client and the coding the proxy asked for.

Both public routes gzip any response whose request permits gzip. A public registry's CDN gzips
JSON and Core Metadata companions, and the stub gzips archives too. A proxy that relays those bytes
to a client breaks the advertised digest, so every installation through a public route runs under
that condition.

`Ecluse.E2E.Npm.MirrorInstallE2ESpec` holds the install the mirror alone serves against a control.
Its fixture graph is a root with a dependency that has a dependency of its own, a peer dependency,
and an executable, and each package's module exports a known value. A new project installs the root
by exact version through the public gate, and the worker mirrors every version. A second proxy and
a second new project then install the same root while both public upstreams are down. Each project
starts with an empty npm cache, no lockfile, and no `node_modules`, and the first proxy stops before
the outage begins, so nothing a client or a proxy kept can supply the second install. The cases
compare what a client can see of the two installs:

- `npm ls` reports the same dependency, transitive, and peer edges,
- the root's module loads every package's export,
- npm links the root's executable, and the executable prints the root's export,
- the stub's log shows that the mirror answered every packument and artifact read and that the
  public routes answered nothing, and the lockfile names the proxy as the source of every package,
- `node_modules`, the lockfile, and the manifest are identical after normalising only
  `packages.*.resolved` source URLs in `package-lock.json` and `node_modules/.package-lock.json`.
  A failure prints each differing path, entry kind, executable permission, and exact file bytes.

A last case boots a proxy whose private upstream holds nothing, and the same install fails under
the same outage. The tree comparison detects differences in installed paths, file bytes,
executable permissions, link targets, and every byte outside those lockfile source URL values.
It detects metadata loss only when that loss changes this fixture's installed tree or lockfile.
It does not prove that every metadata field survives, even when the fixture declares that field.
Every ecosystem with a mirror write owes this scenario. PyPI compares exact installed package and
new script bytes and executable permissions across public and hot-mirror installs. Its full contract
also covers a second target architecture, complete wheel and sdist inventory, interrupted copying,
restart, and public outage. A mount without private storage retains its failing outage control.

The Dredger cases seed Verdaccio through the proxy and mirror worker, then run the same
image with an identity deny and no advisory database. They cover `--once`, `--dry-run`,
consent refusal, first-party protection, and listing preservation after the final version
is deleted. `Ecluse.E2E.Npm.DredgerE2ESpec` also drives `listPackagesIn` against the store and
compares complete package-version snapshots with the audit records and cycle counts. Its
first-party fixture holds two versions. These cases do not verify behaviour against a real
CodeArtifact repository.

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

The [fixed E2E entrypoints](test-contracts.md#fixed-e2e-entrypoints) list each contract and its named concrete fixture.
npm, PyPI, Cargo, and RubyGems run the full contract with concrete serving, mirroring, publication, Dredger and telemetry providers.
Native cases retain protocol-specific observations beside the shared laws.
Their fixture and provider modules share the `ecluse-e2e-test-support` library.
Metadata checks declarations and reports. It does not choose test combinations at runtime.

CI prepares the real image once. The npm, PyPI, Cargo, RubyGems, and shared-system subjects each run on a separate arm64 runner.
The npm subject composes the former serving and write arrangements under one ecosystem entrypoint.
The shared-system subject owns cross-ecosystem and core capability cases that belong to no single ecosystem.
The subject matrix disables fail-fast, so one failure does not stop the other subjects.
Preparation, subject runs, and the execution audit all gate.
The separate `e2e` job runs the execution and cross-tier completion audits after the subject
and coverage jobs. It compares reports with the core-unit, runtime-unit, and integration
plans and reports from the same workflow attempt. The audit checks compiled plans, required laws,
execution status, source, architecture, run identity, and the E2E image identity where it applies.
A partial retry can retain a previous attempt's E2E plan, which artifact consumers reject with `run identity mismatch`.
Rerun the complete workflow so artifact plans, reports and consumers share the new attempt identity.
The existing change classifier alone permits a documentation-only omission.
The audit runs after failed subjects and rejects missing artifacts or execution results.
Cancellation stops an obsolete workflow's pending subject and completeness audit jobs.
The terminal gate still checks every required job result.
It compares the source revision, architecture, image identity, plan digest, groups, and case IDs.
Changing a subject's direct declaration list does not change the full contract's required families.
No timing improvement is claimed until comparable runs measure it.

The tier stays outside local `task gate` and `task check`. Run `task test-e2e` inside the Nix flake.
It prepares and loads the image in a fresh `scratchpad/e2e/run.*` directory, then runs each subject and the full audit.
An ordinary case failure with a valid current report does not stop the later subjects or audit.
The final command fails after it collects their evidence.
A prerequisite, cleanup, cancellation, timeout, or unexpected native exit stops execution.
It prints the artifact directory. Set `ECLTEST_ARTIFACT_DIR` to that path for later runs or auditing.
Staged commands require an explicit directory:

```bash
mkdir -p scratchpad/e2e
export ECLTEST_ARTIFACT_DIR="$(mktemp -d scratchpad/e2e/run.XXXXXXXXXX)"
task test-e2e-prepare
task test-e2e-load
task test-e2e-npm
task test-e2e-pypi
task test-e2e-cargo
task test-e2e-rubygems
task test-e2e-shared-system
task test-e2e-audit
```

Preparation writes `docker-image.tar`, `plan.json`, and `identity.json` once.
Another preparation cannot overwrite that directory, including after a failed preparation.
An artifact lock prevents overlapping preparation, loading, execution, or auditing in the same directory.
Task keeps that lock for each complete operation, including the full local run.
`test-e2e-load` validates the prepared identity before loading the archive.
`test-e2e-audit` checks current reports against the complete plan and identity.
Rerun a subject with its fixed task, such as `task test-e2e-pypi`.
Each attempt invalidates its previous published report before checking prerequisites.
Each nonempty current report publishes to `reports/<subject>.json`, including after a failed runner or cleanup.
The command preserves its native failure status, including exit 7, and the audit rejects failed results.
Attempt reports, native status, cleanup status, and logs remain under `reports/attempts/`.
Test entrypoints accept no custom selection arguments.
`ECLTEST_SEED` defaults to 1. `ECLTEST_TIMEOUT` accepts positive seconds.
The separate manifest and audit commands appear in the [coverage and reproduction map](test-contracts.md#reproduction).

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

## Prepared PyPI assembly rows

`All.ecluse-core (work-per-request).ecosystem: pypi.serve (filter + merge-assemble).prepared PyPI assembly` adds six native rows:

| Capture label | Survivor rows |
|---|---|
| `requests (medium)` | `all`, `one` |
| `numpy (large)` | `all`, `one` |
| `boto3 (heavy)` | `all`, `one` |

`all` keeps every projected version. `one` keeps the first version in map key order.
Each row assembles the authenticated packed capture through the native metadata adapter.
The measured operation forces the resulting render plan's envelope, prefix, table references, and every piece.
It excludes rendering, encoding, capture reads, authentication, decoding, and preparation of the survivor and merge plans.
Before timing, an independent tree projection selects admitted source coordinates and rewrites each file URL from its filename.
Setup compares the complete expected JSON with the native assembled output and requires a native render plan.
The expected output does not call the production assembly function.

Existing npm assembly, PyPI encoding, serving rows, conditions, budgets, cadence, and gates remain unchanged.
Use `task performance:benchmark-measure` and `task performance:benchmark-receipt` for native allocation and timing evidence.
Keep the source commit, capture pins and hashes, compiler identity, build options, and exact RTS options with those results.
The added rows establish baselines. They do not show a production improvement.

## Allocation budgets: `perf-allocation` (gating)

The `allocation` job in `ci.yml` holds the bytes each measured leg allocates to reviewed budgets,
and the `CI gate` requires it. It runs `task perf-allocation`, the `captures` mode of the
performance-acceptance harness in `acceptance/`, over the committed captures in `bench/corpus/`:
nine npm packuments, three PyPI Simple JSON documents, three Cargo sparse index files, and three
RubyGems `/info` records. The input never changes between runs, so
a changed figure comes from a changed build. The budgets cover allocation only. CPU work that
allocates nothing leaves the figure unchanged, and shows only in the reports' time columns, which
carry no budget.

Every leg reads its capture through the production read driver, `Ecluse.Core.Registry.Metadata.Fetch`,
as a fetch from the capture's registry runs it. The harness holds the capture's bytes and feeds the
driver 32 KiB chunks of them in place of a socket, cut before the measured pass.

- The full legs run the driver's full read (`readManifest`): the charge and the source digest for
  each chunk, the walk over a table keyed afresh for the read, the finish, and the artifact location
  check. They then run the rules, the advisory reads, the merge, assembly, and serialisation. The
  charge has no payer, as on a read outside the memory gate.
- The single-version leg runs the driver's selected read (`readVersion`), which takes no digest and
  pays no charge, and checks the selected version's artifact locations.

Each package has four legs:

| Leg | Work measured |
|---|---|
| `full` | Decode, projection, rules, assembly, and serialisation of the whole document, under a one-day minimum age and no advisory database |
| `singleVersion` | Selective projection of one version, forcing its artifact digests |
| `fullShippedAdvisories` | `full` under the source-configured shipped policy (`DenyIfKnownMalicious`, `AllowIfOlderThan` 7 days, and the differential `AllowIfRemediatesCve`), with the corpus advisories served |
| `fullAllAdvisoryRules` | `full` under the source-configured shipped policy plus fail-closed `DenyIfCve` (CVSS 8) and `DenyIfEpss` (EPSS 0.5), with the corpus advisories served |

The advisory legs use the captured corpus advisories in `bench/corpus/advisories/`, which the
[advisory rule rows](#advisory-rule-rows) also read. Setup compiles them through
`Ecluse.Core.Osv.Compile`, and a slot serves the artifact, as a synced mount reads it. A rule that
finds no row abstains, which would pass for a speed-up. So setup fails when the artifact holds no
range, when it names none of the captures, and when a capture it names yields no row through the
rules' own read, under the key the rules look it up by.

The harness runs each leg five times, each pass on its own copy of the capture, and reports the
median, with the smallest and largest pass beside it. It counts the bytes each pass allocates with
GHC's per-thread allocation counter. For one build over one input, a leg's figure moves by a few
hundredths of a percent between passes and runs, because each read draws a fresh table key. That
stays far inside the margin, and the median absorbs the spread within a run. Wall-clock time appears
in the report for information only.
The age rules evaluate at each capture's `capturedAt` time in `bench/corpus/pins.json`, so they
admit the same versions on every run. The harness links the shipped server's RTS options (the
`shipped-rts` stanza in `ecluse.cabal`), and the report prints the capabilities and allocation area
it read from the running RTS.

[acceptance/criteria.json](../acceptance/criteria.json) holds each leg's calibrated figure in
bytes. A leg's budget is that figure plus the file's `marginPercent`, rounded up to a whole byte.
The margin is 1%. Over six main runs a leg's median moved by at most 0.051%, so 1% is about twenty
times the noise and fails a regression of 1% on any leg.
The calibration record names the default architecture, runner, commit, and run URLs.
Optional `calibration.sourcesByEcosystem` overrides those source fields for a named ecosystem.
Each override contains only `arch`, `runner`, `commit`, and non-empty `runs`.
All ecosystems use the sole global `marginPercent`. Unchanged ecosystems keep their recorded
figures and source records. Reports and architecture warnings select each ecosystem's source.
The job fails closed. It fails when:

- a leg allocates more than its budget,
- a measured leg has no calibrated figure,
- a capture is missing or does not project, or a leg does not complete,
- an advisory fixture file differs from its pin in `bench/corpus/pins.json`, or the advisory setup
  above fails,
- the criteria calibrate a package that the run did not measure.

A red result needs a human decision: fix the regression, or recalibrate. A leg that allocates more
than the margin below its figure passes, and the job raises a warning on it that asks for a
recalibration, because the budget no longer holds the gain.
To recalibrate an ecosystem, take all its figures from one arm64 CI report.
Record the runner, URL, and executed commit in its source override.
Keep other ecosystems' values and source records unchanged. A full recalibration can instead
replace every figure and the default source together, removing obsolete overrides.
A pull request run checks out the merge of its head into the base.
The checkout log names that commit. The job writes its report to the run summary and uploads it as
`perf-allocation-report.md`.

Arm64 CI is the calibration source, and you can also run `task perf-allocation` on x86_64. On an
architecture other than the calibration's, the run still holds every leg to its budget and names
both architectures in the report and in a warning. Allocation can differ between architectures, so a
leg close to its budget can read differently there. On one x86_64 host every leg allocated between
1.16% less and 0.01% less than its arm64 figure. Every leg passed, and the PyPI requests
`singleVersion` leg raised the below-margin warning. The job is not part of `task check` or
`task gate`.
The CI job writes native row outcomes after measurement and audits one receipt per ecosystem.
The receipt binds the reviewed four-leg requirements, report and corpus bytes, process result,
run attempt, and recorded build conditions. A missing or failed row keeps the allocation job red.

## Benchmarks (non-gating)

Use the benchmark tier to assess cost alongside the Cabal test suites. None of its
three workflows gates a merge or belongs in branch protection as a required check.
The workflow YAML owns schedules and run options.

Full work-per-request measurements and live performance acceptance do not run automatically on PRs.
The CI build still compiles the benchmark, including its correctness and complexity assertions.
Main supplies performance baselines, and the nightly live run checks registry drift.
Dispatch on the reviewed PR branch when its change needs performance evidence:

```bash
gh workflow run bench.yml --ref <branch>
gh workflow run perf-acceptance.yml --ref <branch>
```

The work-per-request run retains every generator, advisory, scaling and complexity assertion.
The [handoff requirements](../.agents/orchestration-strategy.md#definition-of-done) still require
work-per-request and default load evidence for changes to npm serving or fetching.
A later push requires evidence from the new head.

Each workflow puts its report in the GitHub run summary and uploads the listed files on that run's page.
A run off `main` also sets its figures beside the latest run on `main`, in a [comparison against main](#comparison-against-main).
The load test alone stores reviewed minimums, its [success floors](#success-floors), and its scheduled run fails below them.

| Workflow | Measurement | Downloadable report files |
|---|---|---|
| [Work per request](../.github/workflows/bench.yml) | Time and allocations for the benchmark groups over committed and synthetic corpora | `bench-results.csv`, `bench-output.txt`, per-ecosystem receipts, and `against-main.md` off `main` |
| [Performance acceptance](../.github/workflows/perf-acceptance.yml) | Allocation and time for each [allocation leg](#allocation-budgets-perf-allocation-gating) over live registry documents, with no budget | `perf-acceptance-report.md`, native observations, per-ecosystem receipts or an unavailable diagnostic, and `against-main.md` off `main` |
| [Load](../.github/workflows/bench-load.yml) | npm, PyPI, Cargo, and RubyGems successes, latency, memory, and collector cost through a proxy process under each pod shape, with separate ecosystem sections and baseline sources | `bench-load-results.md`, native observations and per-ecosystem receipts for complete runs, a diagnostic record for selected or GC-thrash runs, and `against-main.md` off `main`. A [sweep](#capability-sweeps) job uploads a directory per arm, `comparison.md`, and `comparison.json` |

Each workflow calls a fixed Task measurement target, then a receipt target after the native process exits.
Task records the options it passes before measurement. The native producer records its own measurement conditions.
The typed receipt code compares these facts with independent required conditions.
The receipt audits actual row outcomes against independent obligations and hashes its source files.
It records the process result, run attempt, compiler, optimisation, profiling and relevant run settings.
This postprocess step does not change the measured workload, sampling or verdict.
A selected load dispatch and the GC-thrash probe write diagnostic-only records.
They keep their native verdicts but make no full read-load completion claim.
Live upstream unavailability also writes a diagnostic-only record and keeps the live workflow's warning verdict.
A complete load receipt requires the native run to hold its calibrated success floors.
The native record names every reason when the floors are not held.
An unheld or missing enforcement status makes diagnostic-only evidence while preserving the harness verdict.
Benchmark and load workflows stay outside the main CI graph and the functional subject jobs.

Read a red result according to its measurement:

- Work-per-request benchmarks fail on build errors, harness crashes, failed complexity assertions, or an advisory row that leaves a version undecidable. They do not compare performance against regression thresholds.
- Performance acceptance runs `task perf-acceptance`, the harness's `live` mode. It fails when the proxy's own code or limits
  refuse a document, for example a body over the metadata size limit or a request it cannot form, and when a document does
  not decode. It also fails on a status that says the proxy asked wrongly, such as a 404 for a pinned package. The
  harness's HTTP manager is not the proxy's egress, so a TLS or protocol fault says nothing about the proxy. A transport
  fault, a 408, a 429, a 5xx, a 401, or a 403 makes the package unavailable instead. The report then marks the run
  incomplete, and the job raises a warning. Live documents grow as packages publish, so no budget applies to them. Its
  report separates upstream time from the legs.
- Load benchmarks use `oha` against a proxy process. A run fails when a required load or ramp step gets no successful response,
  when a count of successes is below its [success floor](#success-floors), when a scheduled run's configuration keeps it
  off the floors, when the kernel OOM-kills a proxy, when a proxy exits on heap overflow, and when a proxy ends any other
  way than the clean shutdown the harness asks for, early exits included. It also fails when the harness or a proxy cannot
  boot, when `oha` cannot run, and when a fixture preflight sees an unexpected status, index shape, or wheel body. Latency
  and memory have no regression threshold. Shared-runner noise and the load run's cost make it unsuitable as a per-PR
  signal, so it never runs on a pull request and never gates a merge. Its scheduled run goes red on these checks.

Corpus pins and capture policy belong in [bench/corpus/pins.json](../bench/corpus/pins.json).

### CPU quotas, processors, and GHC capabilities

Read the CPU-time quota separately from GHC parallelism. A GHC capability runs one Haskell
thread at a time. Its count does not reserve a processor or set CPU affinity.

| Setting or report label | Meaning |
|---|---|
| `2cpu` or `4cpu` in a load pod shape, and `BENCH_LOAD_THRASH_CPUS` | CPU-time quota in CPU equivalents. `2cpu` writes `200000 100000` to `cpu.max`, allowing 200 ms of CPU time per 100 ms period. It assigns no specific CPUs. |
| Runner logical processors | The runner's online logical CPUs, from `/sys/devices/system/cpu/online`, separate from the capability count and from any placement |
| Load harness `-N3` | Three GHC capabilities for the harness and each scenario child. The measured proxy resolves its own count at boot. |
| Unlimited load shape `runtime.cores` | Sets the proxy's GHC capability count to the harness's count, unless `BENCH_LOAD_PROXY_CORES` sets it. The harness imposes no CPU-time quota. |
| `BENCH_LOAD_PROXY_CPUS` | The CPUs the proxy's cgroup may run on (`cpuset.cpus`), such as `2-3`. The harness cgroup gets the other CPUs until the run ends, and then an empty list, so it uses its parent's CPUs again. The proxy's RTS counts its processors from this affinity, so a derived capability count clamps to it. With `BENCH_LOAD_ISOLATE_OHA=1`, the harness refuses a list that holds CPU 0. |
| `BENCH_LOAD_PROXY_CORES` | The proxy's `runtime.cores`, under any shape. An explicit count is never clamped, so a placed proxy can emulate a node with more CPUs. |
| Performance acceptance `-N` | The shipped RTS default selects capabilities from visible processors. The report reads the running count. The harness does not boot a proxy or apply its cgroup-derived runtime plan. |
| Work-per-request benchmarks | One GHC capability by default. Their Cabal stanza enables `-T`, with no `-N` override in the CI invocation. |
| Serve admission (requests) | Concurrent-request limit, explicit or derived from the proxy's resolved GHC capabilities. It is neither a CPU quota nor a capability count. |
| GC share of CPU, and CPU throttled during the window | Time measurements from RTS counters and `cpu.stat`, respectively |

`BENCH_LOAD_ISOLATE_OHA=1` pins only `oha` to logical CPU 0 with `taskset`. Without
`BENCH_LOAD_PROXY_CPUS`, the harness and proxy keep their inherited CPU affinity, so they can also
run on CPU 0.
The [GHC RTS guide](https://downloads.haskell.org/ghc/9.10.3/docs/users_guide/using-concurrent.html#rts-options-for-smp-parallelism)
defines capabilities and affinity. The [cgroup v2 reference](https://docs.kernel.org/admin-guide/cgroup-v2.html#cpu-interface-files)
defines CPU bandwidth quotas.

### Comparison against main

Use this section of a run summary to see what a branch changed without a download and a hand
diff. A run of any of the three workflows on a ref other than `main` sets its figures beside the
most recent successful run of the same workflow on `main`. Dispatch on the PR branch to obtain
that comparison. The section has a part for each ecosystem, and the run's artifact holds it as
`against-main.md`.

| Workflow | Figure compared | Rows |
|---|---|---|
| Work per request | Mean time | Each bench, with a summary for each top group, and the precision tasty-bench reached for the bench in each run (`2*stdev` as a share of the mean) |
| Performance acceptance | Time | Each leg of each package, with both version counts when the live document grew between the runs |
| Load | Successes, refusals, success p50 and p99, and the proxy's ending | Each scenario of the pod shape, from the "At a glance" table, with the operating point rows that differ between the runs |

The section compares no allocation. The [allocation budgets](#allocation-budgets-perf-allocation-gating)
hold it.

`scripts/perf-baseline.sh` fetches the baseline with the job's read-only token. It takes the newest
successful run on `main` that still holds the artifact, and for the load test the newest one that
measured the same pod shape. A run counts only when this repository started it on `main` by a
push, a schedule, or a dispatch. A load run succeeds or fails as a whole, so one failed pod shape
drops that run as a baseline for all four. The section names the baseline run and its commit, which
can be older than the commit a pull request merges into. A change merged to `main` after the
baseline run then shows up as the pull request's own. `bench-report against-main`
(`task against-main`) renders the section from the two reports.

The section carries no verdict. It marks no row, applies no threshold, and fails no job: both
steps continue on error and stop after five minutes. When no run on `main` holds the artifact, or
the token cannot read it, the section says "no baseline" with the reason. None of the three
workflows joins branch protection.

One run stands on each side, so the section does not separate a change from the difference
between two runners:

- One runner measures every row of a job: every bench of a run, every leg of a run, every scenario
  of a pod shape. The run on `main` had another runner, so a whole job can move together. Each
  part therefore leads with how many rows rose, fell, and held, the median change, and the range,
  and says so when no row moved against the others.
- A row means something only beside the rows that the change does not touch. The rows of one
  ecosystem share a table, and the other ecosystems' sections hold the rest.
- Every run off `main` shares one baseline run until a newer run on `main` succeeds. A row that
  the baseline run drew slow or fast therefore shows the same difference in every pull request and
  in every repeat. A second baseline needs a new run on `main`, and all three workflows accept a
  dispatch there.
- A bench's `2*stdev` is what one run resolves for it. One run on each side does not resolve a
  smaller change, and a larger one can still be the runner.
- Each ecosystem injects its own measured public RTT or configured fallback.
  Its latency-bound scenarios move with that value. The operating point rows show both values when they differ.
- A live acceptance document grows as its package publishes, so its legs then read more versions.
- A finite replay sends the same requests every run, so its successes hold and only its latency
  can show a change. It sends few requests, and its percentiles differ widely between two runs of
  the same code.

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

The proxy dials the private and mirror target stubs at `127.0.0.1` and the public stub at
`localhost`. Boot refuses a mirror target on the public upstream's host, so the two names differ.
The public leg keeps a name because the internal-range block refuses a public artifact URL whose
host is a literal loopback address. Over plain HTTP, `http-client` starts each further address of a
name 250 ms after the one before, even when that one refused, and `localhost` resolves to `::1`
first. So each stub listens on one port at both `127.0.0.1` and `::1`, or at `127.0.0.1` alone on a
host without an IPv6 loopback. After a miss the proxy closes that connection, so a row whose stub
misses connects to it again on each request. While the stubs listened at `127.0.0.1` alone, each of
those requests on a leg dialled by name waited 250 ms. An HTTPS deployment connects through
`crypton-connection`, which tries the next address as soon as one refuses.

Every load proxy runs with native metadata gzip off. The shared environment builder sets
`ECLUSE_SERVER__METADATA_GZIP=false` for every scenario, ecosystem and pod shape, and drops an
ambient `ECLUSE_*` value. The harness reads the resolved `server.metadataGzip` line back from the
boot log, refuses a proxy that states another posture or none, and names the posture in the
report's operating point. The floors therefore measure identity listings. A load run says nothing
of gzip-enabled throughput: the gzip rows under [Metadata gzip rows](#metadata-gzip-rows) measure
the enabled path's cost.

`BENCH_LOAD_POD` names the pod shape: `unlimited`, a CPU-time quota such as `2cpu`, a memory limit
such as `1gib`, or both, such as `2cpu-1gib` or `4cpu-2gib`. Under any shape but `unlimited` the
proxy starts inside its own cgroup, a child of the directory `BENCH_LOAD_CGROUP` names. That cgroup
gets `cpu.max` at the quota, and `memory.max` at the limit with `memory.swap.max` at zero. The load
generator and the stubs stay outside that cgroup. The
proxy links the shipped RTS options and gets only `-T`, for its statistics, through `GHCRTS` at
launch. Its boot reads the cgroup and derives its capabilities, its heap ceiling, and its memory
plan as it would in a pod. The unlimited shape sets `runtime.cores` to the harness's capability
count instead. The workflow delegates the cgroup subtree with `sudo` before the run, enables the
cpuset, cpu, memory, and pids controllers for it without a task limit, and turns swap off. The cgroup
outlives the proxy, so an OOM kill stays countable after the process is gone, and the harness
retires any proxy cgroup a killed run left. A scheduled run measures `unlimited`, `2cpu-1gib`,
`4cpu-1gib`, and `4cpu-2gib` in a matrix. A manual dispatch defaults to the `read-paths` profile
under `2cpu-1gib`. It can select another profile, one shape, `all`, or `thrash` for the GC-thrash probe.
To measure a branch, dispatch the workflow on that branch after merging this harness into it.
Hosted runners have four logical processors. Without a placement, a four-CPU quota still shares
those processors with `oha` and the stubs.

Manual profiles select complete native scenarios, including their usual loaded and concurrency-one passes.
They keep the duration, traffic, limits and assertions unchanged.

| Profile | Selected measurements |
| --- | --- |
| `read-paths` | Cold metadata, shipped advisories, full private copies, assembled hits, 304 revalidation, cache-fit and eviction controls, and every managed PyPI read |
| `memory-admission` | Full private copies, slow readers, idle bursts, bursts under private load, warm reads under cold load, and admission ramps |
| `artifacts` | Private artifact relay, public onboarding, and relay ceilings for npm, PyPI, Cargo, and RubyGems |
| `full` | Every routine scenario, including worker and replay measurements |

Explicit `scenarios` override the profile and appear as `custom` in the native report.
Named partial profiles remain diagnostic. They do not enforce calibrated floors or produce full completion receipts.

Full nightly runs retain all four pod shapes. Remaining floor values and eligibility checks stay unchanged.
The full profile now omits the 5% private-copy cases and default restart replays.
They remain available through explicit scenario selection. The npm and PyPI grid contains 332 pass observations, down from 356.
Cargo adds 154 and RubyGems 162 pass observations, giving 648 across the four ecosystems.

The serving handoff requires an explicit full run:

```bash
gh workflow run bench-load.yml --ref <branch> -f profile=full -f pod-shape=all
```

Local callers keep the full selection unless they set `BENCH_LOAD_PROFILE`.
Capability sweeps retain their explicit scenario selections.

A run can also pin settings on every measured proxy. A scheduled run sets none of them, and a run
that sets any of them is not held to the [success floors](#success-floors):

| Variable | Effect |
|---|---|
| `BENCH_LOAD_PROXY_CPUS` | Places the proxy on these CPUs and the harness cgroup on the rest, as the [CPU table](#cpu-quotas-processors-and-ghc-capabilities) describes |
| `BENCH_LOAD_PROXY_CORES` | Sets `runtime.cores` |
| `BENCH_LOAD_PROXY_MAX_HEAP_BYTES` | Sets `runtime.maxHeapBytes` |
| `BENCH_LOAD_PROXY_ALLOC_AREA_MIB` | Adds `-A<n>m` to the proxy's `GHCRTS`. The boot keeps an area that is neither the shipped one nor the one it derives. |
| `BENCH_LOAD_PASSES=loaded` | Runs the loaded pass alone, which roughly halves the run |

Every proxy without a memory limit gets `memory.high` at 4 GiB, which protects the runner. The
untuned `unlimited` control is the exception. The proxy's runtime sizing reads `memory.max` alone,
so the backstop does not change its posture.

Each scenario reports:

- its name and traffic description in the summary table and before its detailed metrics
- successes in the window (the primary figure) beside the [success floor](#success-floors) they
  are held to, refusals (`429` and `503`), other statuses, transport failures, and the p50 and p99
  of successful responses only
- the proxy's allocation per successful request beside the attempt count, its GC share of CPU,
  its major collections and the mean live data they left, its RTS `max_live_bytes` and
  `max_mem_in_use_bytes`, whether its small-object live data crossed the compaction threshold, and
  the processors its RTS sees. A paired scenario or a ramp divides by every success in the window
- the allocation per refusal: the same window's allocation divided by its `429` and `503`
  responses. It and the allocation per success each charge the whole window to one kind of
  response, so each is an upper bound
- the idle floor after boot, before any load: live data after a major collection and the cgroup's
  `memory.current`
- cgroup `memory.peak` against `memory.max` and against the RTS's own peak, `memory.stat` (`anon`,
  `file`, `kernel`, `sock`) as the window closes, the `memory.events` counters, and the CPU time
  the quota withheld during the window. A scenario that sends a burst also shows the cgroup's
  memory through the burst, as [The herd under heavy private load](#the-herd-under-heavy-private-load)
  describes
- how the proxy ended: a clean shutdown, a heap overflow (from its own report or the RTS exit
  status), a kernel OOM kill, or another exit
- the serve-admission limit in requests and the memory admission budget at boot, read from the
  proxy's boot log, with the runtime lines quoted
- `ecluse.serve.admission.in_flight` and the proxy's thread count (`pids.current`) sampled each
  second, and every admission series at the end of the window
- the serve slot held per success: the in-flight gauge's mean times the window, divided by the
  window's successes. It estimates how long one success kept its CPU slot
- the metadata cache's hit, miss, and collapsed request counts in the window for the full,
  version, and assembled stores, from scrapes of the proxy's Prometheus exposition at each end of
  the window. A collapsed request waited for another request's fetch instead of making its own
- the CPU time of the window. The proxy's CPU-seconds per second come from its cgroup's `cpu.stat`
  and from the RTS, with the cgroup's share of the CPUs it may run on. The harness cgroup's figures
  cover the load generator, the stubs, and the harness. `/proc/stat` gives CPU 0's busy share. For
  a proxy placed on CPUs of its own, it also gives the busy time on them that the proxy did not
  spend. The load generator keeps its headroom while the harness cgroup and CPU 0 each stay below
  85% busy

Each ecosystem section also shows:

- the rule policy the first scenario's proxy logged at boot: each rule its configuration names,
  with its type, its other keys, and the layers they came from, and the order each mount
  evaluates the rules in. A proxy of either pass that logged a different policy is named
- a cost table that sets each scenario's allocation per success and success p50 beside the same
  figures from the concurrency-one pass, which runs the scenario again on a fresh proxy with the
  actual concurrency set to one, regardless of its loaded-pass connection scale. The
  table adds the concurrency-one pass's success floor, the allocation per refusal, each
  allocation with the count it divides by, and the missed and collapsed cache lookups summed over
  the stores. One request can look up more than one store, so these count lookups, not requests.
  A loaded figure far above its concurrency-one figure points at contention, or at work the
  concurrent requests did not share. A scenario outside the concurrency-one pass shows `n/a` for
  those figures

`npm/tarball-ceiling` relays a private-hit artifact with a fixed 2 ms injected delay. Its loaded
pass uses four times the base connections, and its service pass uses one. The attribution table
keeps its raw p50 and p99 totals but marks the public-leg split unavailable, with the reason that
the relay has no public leg. Ordinary scenarios still subtract only the public baseline, with
private work in Écluse overhead. p50 remains primary, and p99 remains the labelled tail.
`pypi/wheel-ceiling`, `cargo/crate-ceiling`, and `rubygems/gem-ceiling` use the same settings and attribution.

A boot that fails because the runtime could not start an OS thread is booted once more, two
seconds later, into a fresh cgroup, so every reading comes from the boot that succeeded. That
failure is a task limit reached outside the harness, which sets none. The harness prints the
failure with the process limits, the task counts, and the failed attempt's cgroup as it read them,
and the report counts the boot attempts. Any other boot failure fails the scenario.

Four scenarios per ecosystem stress admission under memory pressure.
`npm/herd` sends 100 simultaneous cold `typescript` listings to an idle proxy.
`pypi/herd` sends the same burst of cold `boto3` Simple-index requests.
`cargo/herd` sends it for the `aws-sdk-ec2` sparse index.
`rubygems/herd` sends it for the `rails` per-gem `/info` record.
Each ecosystem's `herd-under-heavy-private` sends that burst during a heavy private load.
See [The herd under heavy private load](#the-herd-under-heavy-private-load).
The npm, PyPI and Cargo `warm-under-cold` scenarios measure assembled hits and retained selected reads.
`rubygems/warm-under-cold` measures `/info` replies rebuilt per request and retained selected reads.
A second generator drives heavy-tier listings that never reuse an assembled response.
Each ecosystem's `ramp` steps from 10 to 400 connections, with one configured duration per step, and reports each step.
None of these sixteen scenarios joins the concurrency-one pass.

`npm/slow-readers`, `pypi/slow-readers`, `cargo/slow-readers`, and `rubygems/slow-readers` measure what slow clients cost the requests beside them.
Each repeats and reports its cold listing load: `npm/merge-cold`, `pypi/index-cold`, `cargo/index-cold`, or `rubygems/info-cold`.
Beside it, the same number of further clients request the same listings and read each response at 1 MiB/s. These slow readers run in the harness process:

- Each advertises a 1448-byte segment size, as an Ethernet path does. The proxy's socket send
  buffer then stays near a few hundred KiB, where loopback lets it grow to 4 MiB, so a listing
  larger than that buffer keeps the proxy writing while the client reads.
- After a `429` or `503`, or a failed request, a slow reader waits one second, the proxy's
  admission wait, before it asks again. It does not read `Retry-After`.
- The scenario's section shows the slow readers' successes, refusals, and time to the last byte
  beside the measured load. The report lists the scenario after the private-copy rows.

No slow-reader scenario joins the concurrency-one pass.

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
| `npm/heavy-private-5pct`, `pypi/heavy-private-5pct`, `cargo/heavy-private-5pct`, `rubygems/heavy-private-5pct` | The newest 5% of the versions |
| `npm/heavy-private-25pct`, `pypi/heavy-private-25pct`, `cargo/heavy-private-25pct`, `rubygems/heavy-private-25pct` | The newest 25% of the versions |
| `npm/heavy-private`, `pypi/heavy-private`, `cargo/heavy-private`, `rubygems/heavy-private` | The complete capture, as a private registry that proxies the public one returns |

Only Cargo's private-copy rows, `cargo/herd-under-heavy-private` included, boot with durable mirror state and a mirror target.
Before boot, their state fixture keeps each private row that matches its public source row and verifies the matching target archive, so these rows also measure the managed read.
The npm, PyPI and RubyGems private-copy rows read no durable state.
Every other Cargo row boots without durable state or a mirror target, as its counterparts do, so `cargo/crate-onboarding` enqueues no mirror job.

The full profile runs the 25% and complete-copy cases after the cold listing and its advisory variants.
The 25% case keeps an intermediate comparison, and the complete copy keeps the stress case.
The 5% case requires explicit scenario selection. All four rows join the concurrency-one pass when selected.

Three scenarios per ecosystem load an advisory database, so the per-version advisory cost
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
| `cargo/index-cold-advisories` | `cargo/index-cold` | The shipped policy | The captured RustSec advisory for `tokio`, and none for `windows` or `aws-sdk-ec2` |
| `cargo/index-cold-all-advisory-rules` | `cargo/index-cold` | The shipped policy with `DenyIfCve` at CVSS 8 and `DenyIfEpss` at 0.5, both failing closed | As `cargo/index-cold-advisories` |
| `cargo/revalidate-not-modified-advisories` | `cargo/revalidate-not-modified` | The shipped policy | Nothing: `aws-sdk-ec2` has no advisory, so this measures a present database and an absent package |
| `rubygems/info-cold-advisories` | `rubygems/info-cold` | The shipped policy | The captured advisories for `rails`, and none for `nokogiri` or `aws-sdk-ec2` |
| `rubygems/info-cold-all-advisory-rules` | `rubygems/info-cold` | The shipped policy with `DenyIfCve` at CVSS 8 and `DenyIfEpss` at 0.5, both failing closed | As `rubygems/info-cold-advisories` |
| `rubygems/revalidate-not-modified-advisories` | `rubygems/revalidate-not-modified` | The shipped policy | The captured advisories for `rails`, the first gem in the working set |

The load harness boots the shipped mount policy. npm therefore also carries the admission-only
`deny-scripts` and `deny-external-dependencies` rules, Cargo carries the admission-only
`deny-external-dependencies`, and PyPI and RubyGems carry only the shared defaults. No captured
crates.io row names another registry. Two captured npm versions declare
`postinstall` and the default denies them. No captured npm version has a git or URL dependency. Check a scheduled-equivalent npm run against the existing
success floors when changing this default, keeping every failure visible.

A fail-closed advisory rule answers a version it cannot decide with 503, and the report counts
that 503 with the admission refusals. When the proxy records such a failure in
`ecluse.rule.effectful.failures` during the window, the scenario's section flags the count.

`BENCH_LOAD_SCENARIOS` runs a comma-separated subset, such as `npm/merge-cold,npm/herd`.
`BENCH_LOAD_THRASH_LIMITS_MIB` runs the GC-thrash probe instead of the passes: one scenario
(`BENCH_LOAD_THRASH_SCENARIO`, `npm/heavy-private` unless set) at a CPU-time quota of
`BENCH_LOAD_THRASH_CPUS` CPU equivalents (two unless set) under each listed memory limit, highest
first. The probe records OOM kills and
heap overflows as its reading, so they do not fail it. It fails only when no limit produced a report.

#### The herd under heavy private load

The heap ceiling leaves room in the memory limit for memory outside the heap, such as kernel socket
buffers ([runtime-sizing arithmetic](../web/content/docs/operations.md#appendix-runtime-sizing-arithmetic)).
Each ecosystem's `herd-under-heavy-private` measures that memory while the
proxy sends a burst of large responses and private reads hold its heap high. A change that lets the
proxy hold more live data shrinks the margin, so run these scenarios under every pod shape for such
a change. Like every scenario, each fails on an OOM kill or a heap overflow.

Each scenario runs its ecosystem's `heavy-private` load at the base concurrency for the window.
Halfway through, 100 more connections each send one request for the herd's listing at the same
moment: `typescript` for npm, `boto3` for PyPI, `aws-sdk-ec2` for Cargo, and `rails` for RubyGems.
The burst passes through the same admission as the load. From that moment until the last burst response ends, the harness reads the proxy cgroup's
files every 10 ms. A herd on an idle proxy completes in about a second, so the one-second gauges
would miss its peak. The scenario's own section adds three rows to the usual figures:

| Row | What it shows |
|---|---|
| Cgroup readings through the burst | How many readings the harness took |
| Highest `memory.current` in the burst | The most memory charged to the cgroup, with `anon`, `file`, `kernel` and `sock` from `memory.stat` at the same reading |
| Highest `memory.stat` `sock` in the burst | The most memory in kernel socket buffers, with `memory.current` at the same reading |

Each ecosystem's `herd` shows the same rows for its burst on an idle proxy. A run without
`BENCH_LOAD_CGROUP` starts its proxy outside any cgroup of its own, so these rows read `no cgroup`
there.

`sock` counts response bytes the kernel holds until the client reads them, and `anon` holds the heap
with the rest of the process's own memory. Compare the `sock` peak with `memory.peak` less the RTS
maximum memory in use. A `sock` peak near that gap shows that socket buffers take the room outside
the heap. The at-a-glance row shows the burst's successes, and the concurrent load row shows the
`heavy-private` load's. Only the background `heavy-private` load requires successes and meets a floor.
The burst's successes and refusals remain diagnostic, including zero successes. This exception applies
only to these combined scenarios. All process and memory-failure checks still apply.

The npm and PyPI combined scenarios stay in scheduled coverage.
Their eight background floors use 12 qualifying source runs, with three observations per scheduled pod shape.
The [calibration record](../bench/load/floors.json) keeps each measured commit, the earlier source groups, and the native latency provenance.

#### Capability sweeps

Use a sweep to compare proxy configurations on one runner, where separate dispatches would land on
different runners. A file under [bench/load/sweeps/](../bench/load/sweeps/) defines arms and jobs.
Each arm is a pod shape with the proxy settings it pins. Each job runs the anchor arm, its test
arms, and the anchor again, so the runner's speed moves every arm of a job alike.

| Field | Meaning |
|---|---|
| `upstreamLatencyMs`, `passes`, `scenarios` | What every arm shares. The runner turns the round-trip probe off and injects the fixed latency for every selected ecosystem. |
| `anchor`, `scored` | The arm every job opens and closes with, and the scenarios whose anchor ratios make an arm's score |
| `arms.<name>`: `shape`, `cpus`, `cores`, `maxHeapBytes`, `allocAreaMib`, `serveMaxInFlight` | The arm's pod shape, its proxy settings, and an explicit admission. The proxy's boot resolves any it leaves out. |
| `arms.<name>.expect` | The capabilities, allocation area, heap ceiling, and admission every proxy of the arm must report |
| `arms.<name>`: `podMemoryMib`, `referenceShape` | The memory the arm's peak is judged against, and the scheduled shape whose loaded floors its counts sit beside |
| `phases` | Groups of jobs. A phase marked `onRequest` runs only when a dispatch names it. |

Dispatch the load workflow with `sweep` set to the file's name and `sweep-select` set to phases or
jobs, such as `A` or `B1,B2`. A blank selection runs every phase not marked `onRequest`. A plan job
lists the jobs with `scripts/load-sweep.sh plan`, and each job runs on a runner of its own. One
dispatch runs six jobs at most at once, so two dispatches can run twelve. `scripts/load-sweep.sh
run` measures the job's arms in order through `task performance:load-measure`. Each arm's report,
native observations, and diagnostic-only receipt go to `sweep-results/<job>/<slot>-<arm>/`. The
receipt's `conditions.json` records the arm's pinned settings, passes, and latency. A `scenarios`
input narrows every arm, and `duration-seconds` shortens every window, for a short check of the
sweep path.

`bench-report sweep` then sets each arm beside the mean of the job's anchors, scenario by scenario.
It writes `comparison.md` for the run summary, and `comparison.json` with the same facts for
analysis across jobs. Both name the commit and the window's connections and length, so an analysis
never mixes a short check or a second commit into one noise band. For each arm the comparison
shows:

- each scenario's successes, the anchors' mean, the ratio between them, and the reference shape's
  loaded floor. The floor applies only to a window of the connections and length the floors were
  calibrated at, so a short check marks every floor not applicable
- the score: the geometric mean of the ratios over the scored scenarios the job ran. A scored
  scenario without an anchor ratio leaves the arm unscored, and the cell names it
- whether every proxy reported the expected boot values
- each safety failure:
  - an ending other than a clean shutdown, such as an OOM kill
  - a `memory.high` event
  - a peak above the arm's pod memory
  - a count below its reference floor
- whether the load generator kept its headroom in every scenario

A boot value or a safety failure voids that repeat of the arm. The comparison also shows how far
the closing anchor drifted from the opening one. Sweep runs are never held to the floors.

`capability-1634` compares capability counts, admission, and memory with no CPU-time quota. Its
arms place the proxy on CPUs 2-3 and the harness cgroup on 0-1. Phase `F` places the proxy on CPU 3
alone, and phase `A` also measures the `2cpu-1gib` shape with no placement.

### Success floors

Each ecosystem requires reviewed minimum successes in [bench/load/floors.json](../bench/load/floors.json).
A held run requires a floor for every checked scenario, pass, and pod shape.
A run held to the floors fails when a count is below its floor. A floor catches what the zero-success check cannot: a scenario at a tenth of its normal successes is not at zero. The floors
are loose. Each is half the lowest count of its reviewed calibration runs, so it leaves room for
variation between runs and for a change that moves throughput by a few percent. The load test never gates a
merge: a breach turns only its own scheduled or dispatched run red.

| Count | What its floor holds |
|---|---|
| Loaded pass | The successes in the scenario's window |
| Concurrency-one pass | The same count, for each scenario that joins that pass |
| `npm/ramp`, `pypi/ramp`, `cargo/ramp`, `rubygems/ramp` | Every step. The one floor comes from the lowest step of the calibration runs, and each step must reach it |
| Each ecosystem's `warm-under-cold` and `slow-readers` | Both loads, the measured one and the one beside it |
| Each ecosystem's `herd-under-heavy-private` | Only the concurrent `heavy-private` background load. Burst successes and refusals stay diagnostic |
| `npm/worker-mirroring`, `pypi/worker-mirroring`, `cargo/worker-mirroring`, `rubygems/worker-mirroring` | Complete jobs under `unlimited`, the only shape that runs the in-process worker |
| The request-pattern replays | A finite trace whose count is the same every night, so the floor is half of it |

The PyPI worker row runs in process under `unlimited`. Its admission capacity is `maxBound(Int) / 2`,
not a constrained worker pod. The three managed-read HTTP rows retain constrained-pod coverage.
The worker payload setting controls uncompressed bytes per archive file. Its timer excludes fixture
reset, queue setup and assertions, while the scenario duration includes that work.
The Cargo worker uses the same in-process scope and nonbinding admission capacity.
It copies one crate archive, with native discovery, metadata, archive inspection, upload, conditional state writes, and completion proof.

The floor checks fail closed over the routine full profile:

- A run held to the floors fails on a scenario that has no floor for its pass.
- It fails on a floor for a count that no scenario reports.
- `Ecluse.BenchLoad.ScenariosSpec`, in the gating unit suite, compares the file with the harness's
  scenarios and the four scheduled pod shapes. A scenario added without floors therefore fails its
  pull request instead of the next scheduled run.
- The same spec compares the four shapes with the workflow's scheduled matrix, which names the
  baseline artifacts. A shape added to or dropped from the matrix alone fails its pull request.

A run is held to the floors only when it matches what they were calibrated at. The file's
`calibration` record holds each part, and the harness reads the run's side from its environment:

| Part | A held run | The harness reads |
|---|---|---|
| Runner | Runs on GitHub Actions under `runnerOs` and `runnerArch` (Linux, ARM64) | `GITHUB_ACTIONS`, `RUNNER_OS`, `RUNNER_ARCH` |
| Settings | Uses the `full` profile at `operatingPoint`: 30 seconds, 100 connections, default payload, computed admission and pools. Both passes cover every scenario. The run pins no proxy setting or `BENCH_PATTERN_*` variable. | The `BENCH_LOAD_*` knobs, `BENCH_LOAD_PROFILE`, `BENCH_LOAD_SCENARIOS`, `BENCH_LOAD_PASSES`, the `BENCH_LOAD_PROXY_*` settings, and the names of the `BENCH_PATTERN_*` variables that are set |
| Pod shape | Runs under a shape that `floors` holds | `BENCH_LOAD_POD` |
| Measurement conditions | Matches `measurementConditions` | The harness reports `unified-v2` |
| Native injected latency | Each ecosystem stays below its own ceiling | Each native public RTT, or the declared configured fallback |

The harness skips floors when a calibration condition does not match.
Report cells read `not held`, and the "Success floors" section names every reason.
Every other check still runs.
The harness applies floors to any full-profile dispatch under the four pod shapes when all calibration conditions match.
Runs on other machines and GC-thrash probes remain unheld.

The harness requires a scheduled run to enforce floors.
It fails the run if the settings, runner, shape, measurement conditions or GC-thrash probe keep the floors off.
The violation names the reason.
An uncalibrated change to the scheduled settings therefore fails the next scheduled run.
Latency above a ceiling remains the network exception.
That run stays green and does not enforce floors.

After the calibration conditions match, the harness compares nothing else:

- `BENCH_LOAD_PROBE_RTT` only swaps the probed latency for the configured 5 ms, which is under the
  ceiling. A run with the probe off, or with a probe that failed, is therefore held, and at 5 ms its
  latency-bound counts sit so far above their floors (more than thirty times in the concurrency-one
  pass) that those floors detect nothing on it. The `upstream-latency-ms` dispatch input turns the
  probe off, and any value but 5 keeps the run off the floors.
- The workflow fixes `BENCH_LOAD_ISOLATE_OHA` and the harness's `-N`, and no dispatch input changes
  them.
- The harness reads the runner's operating system and architecture, not its label, so any Linux
  ARM64 runner on GitHub Actions counts as the calibrated `ubuntu-26.04-arm`.

All four ecosystems use measurement conditions `unified-v2`:

- Each eviction bound is `min(max(1, configured entries), actual selected project count - 1)`.
  A selected working set with fewer than two projects fails before capture reads or proxy boot.
- Each listing pass preflights every distinct selected URL before its measurement callback.
  npm checks JSON, package identity, native metadata, rewritten artifact URLs, and the private
  overlay for merged listings. PyPI retains its media, project, file, artifact URL, and overlay checks.
  Cargo checks nonempty native records, checksum fields, and local archive discovery.
  RubyGems parses each `/info` reply with an independent reader, runs the Ruby reference decoder,
  and requires nonempty rows, nonempty versions, and a checksum field on every row.
  Private-copy listings retain their native checks without requiring a synthetic overlay.
  Cold admission bursts and ramps remain unprimed.
- All shared upstream stubs skip the delay at zero or negative latency. Positive delays stay unchanged.
- Each ecosystem warms a native public listing connection before probing its own RTT.
  npm fetches packuments. PyPI requests PEP 691 JSON Simple indexes, not the project JSON API.
  Cargo requests native sparse indexes from `index.crates.io`.
  RubyGems requests per-gem `/info/` records from `rubygems.org`.
  Disabled probes, failed warm-ups, and probes with no successful samples report an explicit configured fallback.
  Each report and observation artifact retains the baseline identity and sample count.

Service attribution still subtracts one public baseline. Extra upstream hops remain in the reported
proxy overhead. Scenario names, traffic mixes, timed workloads, and workflow cadence stay unchanged.
The full timed grid remains outside ordinary pull-request CI.

The `calibration.measurementConditions` field identifies the conditions behind the floors.
Older files without this field mean `legacy-v1`: raw npm eviction bounds, PyPI-only listing preflight,
an unconditional PyPI delay, and configured PyPI latency. The committed counts retain that identity
until qualified recalibration replaces the affected rows. A `unified-v2` run against legacy floors
is not held, even when a dispatched workflow exits green. It is not a qualifying sample by that fact.
`retainedFloorConditions` maps unaffected scenario names to their earlier condition identity after
partial recalibration. It records retained row provenance without changing those counts.

Latency-bound counts fall as injected latency rises. Derive each native ceiling separately:
1.5 times the lowest of its pod shapes' highest qualifying latencies, rounded up to 10 ms.
`npmInjectedLatency.highestMs`, `pypiInjectedLatency.highestMs`, `cargoInjectedLatency.highestMs`, and `rubygemsInjectedLatency.highestMs` record the per-shape observations.
The reader checks the formula and pod identities for each ceiling.
`unified-v2` calibration requires npm and PyPI ceilings. Cargo and RubyGems floors also require their own native ceilings.
A run above any native ceiling is not held as a whole, including the other ecosystems.
The report names the ecosystem, its injected latency, and its own ceiling.
The network exception applies to each ceiling breach.
A missing calibration or condition mismatch is a configuration fault.
Missing Cargo or RubyGems calibration or a missing native latency observation fails scheduled and dispatched runs.

To recalibrate, take the last ten scheduled runs of the Load test workflow on `main`, and take the
next older run in place of each one you leave out:

| Leave out a run | Why |
|---|---|
| That was red, or that the committed floors would fail | A collapse must never lower a floor |
| With a job above the latency ceiling, which is green and not held | It would lift the recorded latency, and the ceiling with it |

Read each pod shape's `bench-load-results.md`:

| Figure | Where the report shows it |
|---|---|
| Loaded pass | The `successes` column of "At a glance" |
| Each ramp step | The step table in the scenario's own section |
| The second load of a paired scenario | The "concurrent load: successes" or "slow readers: successes" row in the scenario's own section |
| Concurrency-one pass | The count of successes in the first "at base concurrency one" column of "Loaded cost against concurrency one" |
| Native injected latencies | The "injected upstream latency" row in each ecosystem section's operating point |

Set each floor to half the lowest count over the ten runs, rounded down, and at least 1. A scenario
with several required loads takes the lowest count of those loads. The combined probes use only
their background counts. Then write the `calibration` record:

- `runs`: each run's URL with the commit it measured. Ten runs can span commits, so every run
  carries its own. Keep the exact measured commit when a later patch changes only calibration data or report wording.
- `rule`, `runner`, `runnerOs`, `runnerArch`, and `operatingPoint`. The rule identifies the run groups
  that supply retained floors, recalibrated floors, and each native latency record. Historical runs
  in `runs` do not count as observations under the new conditions.
- `measurementConditions`: the conditions measured by the new calibration.
- `retainedFloorConditions`: unaffected rows that retain an earlier calibration identity.
- `npmInjectedLatency`, `pypiInjectedLatency`, `cargoInjectedLatency`, and `rubygemsInjectedLatency`: each native ecosystem's highest qualifying injected
  latency by pod shape and its derived ceiling. Preserve the measured baseline and fallback identities
  when deciding whether an observation qualifies.

Two cases have no ten scheduled runs to take.
Request `profile=full` and `pod-shape=all` for either case:

| Case | Why | What to do |
|---|---|---|
| A new scenario | No night has run it | Dispatch the workflow at least three times on the branch that adds it. Those runs go red on the missing floors, and their reports carry the counts all the same. Set the scenario's floors to half its lowest count |
| A deliberate change of measurement conditions, a knob default, the runner, or the pod shapes | A scheduled run at the changed setting is red by rule until the file matches it | Make the change and the recalibration in one pull request. Dispatch the workflow at least three times on that branch. Dispatched runs are on demand, so the rule does not fail them, and their reports carry every count. Replace only affected floors from those runs. Retain unaffected counts and their provenance |

Compare old and new conditions serially on each supported resource profile before replacing affected floors.
Retain failed, adverse, and disqualified observations with their exclusion reasons. Do not turn green
unheld workflow results into qualified measurements or invent calibration values.

In both cases, collect at least three qualifying observations per applicable pod shape. Add their
run URLs and exact measured commits to `runs`. Preserve earlier source records and their assigned
floor groups when those floors stay unchanged. A whole run above any applicable native ceiling
contributes no floor counts or latency maxima, including its otherwise eligible profiles.

For a new native latency policy, select bootstrap samples under the existing applicable constraints.
A ceiling that does not yet exist cannot exclude those inputs. Derive the new ceiling after selection
and apply it to future observations. Recorded per-shape maxima can exceed the derived ceiling because
the formula uses the lowest shape maximum. Do not retrospectively exclude the samples using their
new ceiling. Apply the same calculation independently to each native ecosystem because its floors
depend on its own injected RTT.

Each native latency record uses only its assigned qualifying source group. In a partial recalibration,
a retained latency record keeps its historical source group. New observations qualify against its
existing ceiling. They do not revise retained per-shape maxima merely because their
RTT exceeds one historical maximum while remaining below the ceiling. A changed native policy
receives a separate record derived from its qualifying observations.

For mixed calibrations, `rule` identifies which historical runs supply the retained counts and
latency record, and which new runs supply the affected counts and changed native latency record.
The report's provenance count includes both groups. It is not the number of new qualifying samples.
The next routine recalibration uses ten qualifying scheduled runs. Remove historical source records
only after no retained floor or latency record refers to them.

After a calibration-only patch, the committed-floor unit checks parse the actual file and verify
default-condition enforcement under each scheduled shape at every native ceiling. Review and CI
check the final commit. Keep the measured harness code unchanged and record any later report-only
change. These checks do not require another timed grid.

A report prints each floor beside the count it holds, so you can read a run against the floors
without the file.

### Benchmark captures

The catalogue records complete npm packuments, PyPI PEP 691 Simple JSON snapshots, Cargo sparse indexes, and RubyGems per-gem `/info/` records.
Each capture keeps the upstream response body unchanged, including prereleases, operational fields,
and PyPI serial metadata. The catalogue records its source, capture time, actual media type, byte
size, and SHA-256 digest. Requests use identity content encoding, so these bytes match decompressed
metadata input rather than compressed transport traffic.

Run `task gen-bench-corpus` only for a deliberate recapture. Version pins identify workloads and do
not limit the captured releases. Run `BENCH_CORPUS_VERIFY=1 task gen-bench-corpus` to check committed
sizes, hashes, provenance fields, and basic document shape without network access.
The shared `Ecluse.Test.Corpus` reader authenticates captures against `bench/corpus/pins.json`.
It refuses a missing pin, a catalogue path mismatch, a wrong byte count, or a wrong SHA-256 digest.
Load fixtures authenticate before proxy startup. Residency parents authenticate before probe setup,
including preparation of merge inputs. Pin hashing stays outside measured work and child probes.
The harness separately validates each capture through the production read driver before measurement.

| Group | Capture use |
|---|---|
| `wire+project (per package)` | Complete bodies feed decoding and production full-document projection on every iteration. |
| `single-version metadata (per package)` | Complete bodies feed production full-document and selective projections. |
| `cold production reads (per package)` | Unchanged complete bodies pass through the production full-document and selected-version HTTP readers for npm, PyPI, Cargo, and RubyGems on every iteration. |
| `source digest (32 KiB chunks)` | Unchanged complete bodies pass through the production digesting source in 32 KiB pieces, and nothing reads them further. |
| Realistic serve, merge, rules, and version groups | Inputs derive from complete captures, with preparation outside the measured operation. |
| Load metadata and cache scenarios | Fixture upstreams serve captured metadata with native local artifact routing. Cargo substitutes checksums for owned relay bytes. npm and PyPI rebase artifact URLs. The private upstream of the 5% and 25% private-copy points serves cut captures, and that of the 100% points serves all captured versions. These are derived bodies, not byte-identity measurements. |
| Scaled groups | Synthetic bodies measure growth separately and do not establish wire-to-resident ratios. They pass through the projection under a fixed table key, with no location check. |

The projection groups read held bytes, including the structural guards. Their rows that read a
whole capture or select one version run the production read driver over the capture's 32 KiB chunks,
so they include the source digest of a full read and the artifact location check. No projection row
measures the HTTP exchange.

The cold-read group calls `fetchFullManifest` and `fetchVersionMetadata` through uncached production
clients. Each iteration includes request formation, loopback HTTP, bounded response consumption,
source hashing on full reads, extraction, projection, and artifact-location checks. The manager
redirects every request to the local replay server and disables proxies. Like every production
manager, it applies `typedBodyReads` to each body read. No timed request reaches a
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

The [comparison against main](#comparison-against-main) pairs these rows by name and has no fixed
control group. It cannot tell that a row's work changed, so match successful work, capture hashes,
limits, and forcing when you read a row against another revision.
Replacing trimmed captures breaks historical comparability, so comparisons must use the same capture hashes.

The wire-to-resident factor still requires measurements of raw and typed retention on these bodies
under [#1421](https://github.com/AlexaDeWit/Ecluse/issues/1421).
Capture byte sizes alone do not establish an expansion ratio. Replacing a capture changes its
[allocation figures](#allocation-budgets-perf-allocation-gating), so a recapture also needs a recalibration.

### Advisory rule rows

These in-process rows use the shared shipped policy and omit the npm mount deny. Their figures
remain comparable across ecosystems.

The `rules with an advisory database (per package)` group shows what an advisory database adds to
one request's rule phase. It measures the typescript, react, @types/node, and numpy captures, and
reports time and RTS allocation per request like the other rows. Each iteration prepares the policy,
decides every release, and builds the filter plan. A row with the fail-closed deny rules fails when
any version ends undecidable, because an unanswered read would measure an outage instead of a
decision. Under the source-configured shipped policy, an artifact that serves nothing makes the
advisory rules abstain, which would pass for a speed-up. So setup also checks that each artifact serves ranges:
the captured one for react and numpy, and the generated one for its targets and every filler
package.

| Row set | Advisory database | Policy |
|---|---|---|
| `shipped policy without a database` | None | `AllowIfOlderThan` (7 days), the source-free shipped policy |
| `shipped policy over corpus advisories` | Captured advisories | Source-configured defaults: `DenyIfKnownMalicious`, the quarantine, and the differential `AllowIfRemediatesCve` |
| `all advisory rules over corpus advisories` | Captured advisories | The source-configured shipped policy plus fail-closed `DenyIfCve` (CVSS 8) and `DenyIfEpss` (EPSS 0.5) |
| `all advisory rules over synthetic advisories` | Generated worst case | The five source-configured rules, over typescript and numpy only |
| `configured shipped policy over synthetic advisories` | Generated worst case | The source-configured defaults alone, over typescript and numpy only. No scored deny decides first, so every generated partial fix reaches the remediation differential |

The source-free row runs the source-free shared policy. Source-configured rows run the automatic
MAL default. The captured scored corpus has no primary MAL records, so those rows measure abstention.
Separate native MAL-hit and mixed-heavy rows use attributed, labelled variants under
test/fixtures/osv/malicious/. Setup requires a named MAL denial. The scored synthetic reference
holds 200 target advisories and 20,000 filler names. The mixed row adds MAL points to that reference.

End-to-end MAL qualifications require explicit BENCH_LOAD_SCENARIOS selection:
npm/merge-cold-mal-hit, npm/merge-cold-mal-mixed-heavy,
pypi/index-cold-mal-hit, or pypi/index-cold-mal-mixed-heavy.
They use the selected workload's operating settings, normal compilation and sync.
They are separate from the default held rows and have no calibrated floors. Held execution
therefore refuses their missing floors. Observe qualifications separately without altering floors.
Qualification stderr records source and artifact hashes, the serving ETag, admitted versions, and the response-body hash.
Refusal throughput is not comparable successful throughput. Keep complete reports, source and
artifact hashes, serving generation, capture hashes, boot policy, admitted versions, and body hashes.
Report p99, refusals, other failures, allocation, and whole-process memory, including adverse rows.

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

### Streaming pump rows

The `stream (pump into Warp sink)` group shows the CPU cost of a flush policy on the artifact relay.
Each row pumps a held body into a model of Warp's stream response. The load test prices the socket
sends that the model leaves out.

| Condition | Value |
|---|---|
| Body | 8 MiB, with a byte pattern that repeats every 2053 bytes, so no two 4096-byte-aligned blocks are equal |
| Upstream chunk sizes | 4096, 16384, and 65536 bytes, cut before timing |
| `opening-flush (shipped)` | The production `pumpBody`, which flushes once after the first chunk |
| `flush-threshold-64k` | A flush once 64 KiB has arrived since the last flush, and one for any remainder at the end |
| `flush-end-only` | One flush at the end |
| Sink | streaming-commons' builder receiver, as Warp's `sendRsp` drives it for `RspStream`. Each row allocates its 16 KiB buffer once before timing and reuses it on every run, as Warp reuses a connection's write buffer |
| Timed work | Every write, a send of each full or flushed buffer, and a send of the buffered tail at the end of the stream |
| Excluded | The socket, the response header, chunked framing, and the upstream read |

A send only counts the bytes it receives. The pump writes each chunk with `byteString`, which
copies a chunk of at most 8160 bytes into the buffer. It hands a larger chunk to the sink whole,
and Warp sends such a chunk without a copy on the serve path too. The 16384-byte and 65536-byte
rows therefore copy no body bytes.

`Ecluse.Core.StreamBench.SinkSpec` in `ecluse-core-unit` checks that every pump sends the exact body,
in order, at every chunk size. It compares the sent bytes with the body itself, never with another
pump's output. It also checks that every chunk size cuts the body into equal, distinct chunks.

The [comparison against main](#comparison-against-main) pairs rows by name, so it lists a row with
no namesake under "Only in this run" or "Only on `main`" and compares no figure for it. The first
run on `main` that holds these rows is their baseline. No allocation budget covers them.

### Metadata gzip rows

The `metadata response (gzip enabled, level 1)` group measures native metadata gzip, which the
shipped defaults turn on. It sits under each ecosystem beside the identity rows, which it leaves
unchanged. Each capture's production served body is gzipped through the production deflater.

| Row | Timed work |
|---|---|
| `held body, gzip` | Compressing a held served body: what an assembled hit adds |
| `render, then gzip` | A miss's render of the served body, then its compression |
| `sizes` | A test row whose output states the identity and gzip lengths, their ratio, and the memory the response is charged |

Setup inflates every output and refuses a capture whose bytes do not come back. The native
workspace is measured by the residency gate, under
[Residency gate](#residency-gate-ecluse-residency-gating), not by these rows.

### Dredger scale rows

The `dredger scale` group times complete Dredger cycles as a CodeArtifact store grows. Each row
drives the production CodeArtifact maintenance handles over the fixed-page fixture in
`Ecluse.Runtime.Test.PackageStore.FixedPages`, which counts every native call by kind. The fixture
pages names and versions at the request's `maxResults`, or 100 at a time when the request sets
none, with one empty page for each empty bucket. Every cycle runs the shipped CodeArtifact quota
model at its computed one-quarter share, with a `chunkSize` of 50 and a `chunkPause` of 2 seconds.
In a paced row the fixture waits 20 ms before each answer, and every wait is real. An unpaced row
asks for the same waits but serves none, and its fixture answers at once, so it counts the native
calls of a full walk in seconds.

| Row | Store | Shape | Advisory catalogue | Cycles |
|---|---|---|---|---|
| Small default control | 3 names with 2 versions each, in one bucket | Default cycle | 100 names | 2 |
| 1,000 and 10,000 names | Ten populated buckets of the ecosystem's alphabet | Default cycle | 100 names | 2 |
| 1,000 and 10,000 names, unpaced | As above | Full walk | 20,000 names | 1 |
| 1,000 names | As above | Full walk | 20,000 names | 1 |
| 10,000 names, npm only | As above | Full walk | 20,000 names | 1 |

In the larger stores, 90% of names hold 10 versions, 9% hold 100, and 1% hold 1,000. The mirror
holds every name and the cache a fifth of each stratum. npm walks 68 buckets and PyPI 36. Every
advisory range ends before the stored versions, so the rules read and keep every version. Each
cycle runs as a deleting run with consent, so a full walk writes the bucket cursor after every
bucket. The fixture refuses every delete.

Each cycle is checked against counts derived from the inventory alone: the tally, the native calls
per store and kind, the metered requests, the quota and chunk waits asked for, the cursor writes,
and a checksum of every identity the rules decided. Every advisory read must borrow the served
generation. The Dredger asks for pages of 1,000, the most CodeArtifact allows, and the counts follow
that size. Derived at the design's 100 per page instead, the npm counts must reach the native totals
the scale design calculated: 2,790 and 25,462 calls for the full walks, and 391 and 491 for the
default cycles. A row with two cycles requires the same decisions from both.

Each cycle writes one JSON line to `dredger-scale.jsonl` in `$PERF_ARTIFACT_DIR`, or in
`scratchpad/performance/` when that is unset. The benchmark workflow uploads it with the run's
other files. The rows are tests, so `bench-results.csv` and the comparison against main leave them
out.

| Field | Meaning |
|---|---|
| `paced` | Whether the row served its waits and the fixture delay |
| `wall_seconds`, `cpu_seconds` | The whole cycle, with every wait a paced row serves. An unpaced row serves none |
| `active_seconds` | Wall time less the quota waits, chunk pauses, and fixture delay |
| `allocated_bytes` | Bytes allocated during the cycle, the fixture's documents and the counters included |
| `live_bytes_before`, `live_bytes_after` | The live heap after a major GC on each side of the cycle |
| `major_gc_live_bytes_mean` | The mean live heap over the cycle's own major GCs, from the RTS's totals. Null when none ran |
| `major_gc_live_bytes_peak_sampled` | The largest live heap of those major GCs that a 10 ms sampler saw. It can miss the true peak |
| `process_max_live_bytes` | The process peak, which includes every earlier benchmark |
| `native_requests_per_version` | Native calls of each kind per decided version |
| `versions_per_hour` | Decided versions per hour of wall time. A paced row runs at the ceiling pace. An unpaced row serves no wait, so its figure is not a rate the quotas allow |
| `decisions`, `decision_digest` | The tally, request counts, waits asked for, cursor writes, and identity checksum, then their SHA-256. Times and the covered-name query count stay out, so a change that only saves work keeps the digest |

The group runs default cycles and unpaced full walks for inventories of 1,000 and 10,000 names in each ecosystem.
It also runs each ecosystem's small control and paced full walk of 1,000 names.
Only npm runs the paced full walk of 10,000 names.
To skip the group in a local run, pass
`-p '!/dredger scale/'` in the benchmark options. The fixture shares the process, so the allocation
and live heap figures include its documents. The RTS measures the live heap only at a major GC, and
a short cycle may run none, so the rows force none to keep the timings clean. The rows cover
CodeArtifact alone and no restart.

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
The full profile omits Restart because its default trace duplicates CI fleet at 100 ms client spacing.
Select `npm/pattern-restart` or `pypi/pattern-restart` explicitly to vary its arrival interval.
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
uses matching accounted bytes for the full store, computed through the production full read and
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

## The maintained JSON lexer

The npm and PyPI metadata reads walk the tokens of the lexer in `json-stream/`. The Haskell lexer preserves
upstream json-stream's token behaviour. These checks cover its public interface and the registry walk.

- **Recorded C traces** in `Data.JsonStream.LexerSpec` compare 2,914 frozen cases through the public
  lexer interface. They include chunk waits, payloads, number results, failures, and leftover input.
  The recorded reference does not call the replacement lexer. Public parser tests also preserve
  large input pieces and chunk-sensitive lenient numbers. Lexer and residency tests check plain
  string validation, immutable snapshots, offset slices, and backing-allocation lifetime.
- **The inherited decoder oracle** in `Data.JsonStream.UnescapeSpec` checks UTF-8, escapes,
  surrogate pairs, truncation and offset slices against a frozen digest from the old decoder.
  Its 68,577 deterministic cases use the public `decodeString` interface. Typed object tests cover
  mixed field types, repeated keys, result order, lazy values and malformed selected or skipped fields.
- **Differential properties** in `ecluse-core-unit` compare the walk with json-stream's own parser
  combinators on generated bodies: every emitted field, the byte count, the refusal, and whether a
  failure is the nesting limit. The bodies carry escapes, lone surrogates, invalid and overlong
  UTF-8, raw control bytes, deep nesting, long numbers and wide exponents, duplicate keys and
  truncation, split at random. A selected PyPI read skips the rest of a file its name rejects,
  which its reference decodes. So a second property compares that read with the reference's read
  of the same index once the undecodable members in those places are repaired. They run with the
  suite, or alone with
  `cabal test ecluse-core-unit --test-options='--match Json --match Reader'`. Both readers use the
  Haskell lexer, so these properties check the walk rather than independently checking tokenisation.

## One pattern for every ecosystem

Every ecosystem follows the testing pattern that npm and PyPI follow.
[Adding an ecosystem](adding-an-ecosystem.md) gives the lifecycle sequence, and
[Metadata readers and performance](ecosystem-metadata.md) gives the reader techniques. Two
obligations come with the testing pattern, beside the checklist in
[Onboarding an ecosystem](#onboarding-an-ecosystem):

- **Hold each metadata walk to a reference reader.** A differential property in `ecluse-core-unit`
  compares the walk with an independent reader of the same fields. For a new ecosystem, that reader
  is a test oracle written for the walk, outside the production parser. npm writes its oracle with
  json-stream's parser combinators: the independent `npmFields` oracle lives in
  `Ecluse.Test.Registry.Npm.Streaming`.
  `referenceVersionListParser` in `Ecluse.Test.Registry.Npm.Project` reads with that oracle.

  The production `npmVersionListWalk` lives in `Ecluse.Core.Registry.Npm.VersionListReader`.
  Its 2,000-case property compares emitted events, final inventories, byte counts, limit refusals,
  and nesting-failure classification. PyPI's reference, `pypiFields` in
  `Ecluse.Test.Registry.PyPI.Streaming`, is the reader PyPI ran before its walk.
  Where a walk skips more than its reference, as PyPI's selected read does, a property states the difference.
  Cargo's `referenceIndex` in `Ecluse.Test.Registry.Cargo.Reference` decodes each sparse index line
  with Aeson, and `Cargo.ReaderSpec` holds the native reader to its physical rows.
  RubyGems' `referenceInfo` in `Ecluse.Test.Registry.RubyGems.Reference` reads Compact Index rows,
  and `RubyGems.ReaderSpec` compares generated native facts with it in full and selected reads.
  The 2,000-case property for `packageListingWalk` compares package names,
  byte counts and failures with `referencePackageListingParser` in `Ecluse.Test.Registry.Npm.Maintenance`.
  The production walk lives in `Ecluse.Core.Registry.Npm.Maintenance.Listing`.
  The public fixture helper calls that walk.
  PyPI supplies no version-list-only reader. Mirror state still uses production combinator readers.
- **Give every test a counterpart in every ecosystem.** When a change adds a test, a benchmark row
  or a load scenario for one ecosystem, it adds a counterpart for each other ecosystem. This applies
  wherever the other ecosystem has the same path, even while its support is incomplete, because its
  existing paths still need measuring. A cost that shows in one ecosystem often has a twin in
  another, and a missing counterpart hides it. Where no counterpart can exist yet, the pull request
  names the gap.

## Shared behaviour contracts

`ecluse-test-contracts` depends on core and existing test support. It imports no application, runtime, Docker, or AWS implementation.
Contracts own scenario order and expected observations. Drivers return facts through small operation records.
Native syntax, backend faults, raw-wire oracles, and independent expected output fixtures stay with their providers.
A contract never selects a backend by name or accepts a callback that conceals its complete verdict.

The pure catalogue records capability metadata, obligations, lifecycle groups, and case IDs for explicit source declarations.
Reviewed obligations remain independent of concrete registrations and production capability discovery.
An omitted supported provider or required case fails validation before fixture acquisition.
Unsupported combinations carry reasons. Planned operations carry their delivery references and create no passing test result.
See the [coverage and reproduction map](test-contracts.md) for the migration and retained native tests.

The full ecosystem composition requires proxying, installation, policy, advisories, mirroring,
publication, Dredger, and telemetry for every ecosystem. The full package-store composition
requires reads, writes, inventory, deletion, integrity, and recovery for every backend subject.
An incomplete driver cannot reduce either required list. Discovery is pure, and missing families
produce named failures without stopping unrelated contracts. A passing full composition defines
completion only when its lower-tier obligations and execution reports also pass.
npm, PyPI, Cargo, and RubyGems register the full ecosystem composition.
CI uploads actual plan and report pairs from the instrumented core unit, runtime unit and
integration legs. The `e2e` job downloads all three pairs and audits each ecosystem against its subject's E2E report.
All four tiers share the workflow run and attempt identity and use `arm64` as the architecture name.
The lower tiers have no product image, while E2E binds the prepared image digest.

Measurement runners keep their existing process and sampling boundaries.
`checkBenchmarkCoverage` compares reviewed native obligations, concrete providers, and production adapter registration before loading the corpus.
Both the benchmark and allocation harness call that loader.
The load harness composes 27 shared read and stress laws with `readLoadContract` for each ecosystem.
`requiredLoadLaws` stays independent of each fixture's actual scenario names.
`loadCoverageGaps` names a missing or duplicate law before measured work begins.
`FullLoad` also requires worker mirroring from each native fixture.
Worker mirroring applies only to unlimited runs.
Existing residency registration checks still require captures, positive limits, and every calibrated shape.
The shared retention contract changes neither 2,000/16,000 repetition counts nor the strict live-byte allowance.

### Cargo native provider

Cargo uses the same `fullEcosystemContract` families and independent lower-tier obligations as npm and PyPI.
Its fixed subject is `ecluse-e2e-cargo`, declared by `Ecluse.E2E.Suites.Cargo`.
Run `task test-e2e-cargo` against the prepared product image and immutable execution plan.
The `task test-e2e-cargo-completion` audit requires actual core-unit, runtime-unit, integration, and Cargo E2E reports.
Registration, capability flags, or metadata-only success cannot replace executed laws.
Independent obligations, catalogues, stable case IDs, lifecycle groups, and report identities must agree.
Missing providers, tools, cases, reports, or required native observations fail explicitly.

Real current and floor Cargo clients resolve, build, and install owned fixtures using sparse HTTPS.
The supported floor is 1.74. A current client alone cannot establish floor compatibility.
Fixtures cover dependency graphs, renamed dependencies, features, targets, Rust-version constraints, and lockfiles.
Observe archive requests through Écluse and upstream controls that expose direct bypass.
Checking only discovery or index bytes does not prove asset routing.
The merged `/cargo/` registry runs the shared combined serving contract, as npm, PyPI and RubyGems do.

Native reader tests cover published spelling, lookup paths, ownership collisions, raw versions, physical record keys, and independent wire oracles.
They preserve record order, schema and additive fields, dependency source references, and omission versus null.
Malformed and duplicate rows require quarantine evidence on full and selected reads.
Whole-read transport and resource faults remain failures.
The shared projection contract supports `RecordStream` quarantine while npm and PyPI retain exact `SingleDocument` error assertions.

Authentication cases require native `401` retry at the edge, local discovery that requires a token whenever a private upstream exists, placeholder tokens for anonymous private registries, real `403` refusal, and edge-authentication precedence.
Upstream discovery runs after caller or service credentials exist, never at boot, and private discovery remains uncached across callers.
Authority negatives cover public, neighbouring, read, upload, and artifact coordinates without sending credentials to refused destinations.
Publication cases bind native metadata, body lengths, manifest, archive root, digest, and first-party authority before writes.
They preserve distinct read and upload roots and bound charged archive/TOML work.
Policy cases prove that boot and `check-config` refuse active `DenyInstallTimeExecution`, `DenyIfDeprecated`, and `AllowScope` rules with the adapter's reasons.
They also prove that the shipped `deny-external-dependencies` entry, or one from the shared rules, boots and denies a public row whose normal or build dependency names another registry.

Mirror cases require exact source rows, SHA-256 archive proof, retry, lost replies, restart, and public outage.
Yank and timestamp controls change target metadata and verify retained original-source facts with fresh client state.
Missing, corrupt, and unrecorded state must not become completed managed reads during outage.
Unrecorded target rows reached through the private upstream serve as trusted private content.
CodeArtifact maintenance fixtures prove physical deletion through deterministic control-plane boundaries, without claiming live AWS fidelity.

Cargo corpus and OSV fixtures retain capture identity, hashes, acquisition records, and attribution beside their native data.
Independent benchmark references can adapt NDJSON to arrays internally. Native served output remains line-framed.
Benchmark-only `MetadataRendering` separates pure captured assembly and serialisation from runtime admission factors.
Ordinary arm64 CI emits native resource observations before production factors can be calibrated.
Missing native factors remain failures. Existing npm and PyPI factors and budgets do not supply Cargo values.

Cargo floors and latency policy derive from the standard load workflows with the full profile and every applicable pod shape.
Each shape needs at least three qualifying observations, and the committed floors carry their source and run provenance.
Keep adverse and disqualified observations. A provider registration or unmeasured value cannot satisfy calibration.

## RubyGems native coverage and evidence limits

RubyGems uses the shared contracts with native independent oracles and providers.
The baseline clients are Bundler 2.6.9 and 4.0.23 with compatible Ruby runtimes recorded in each execution receipt.
Required client operations include lock, install, update, frozen lockfiles, checksums, prereleases, dependency constraints, Ruby requirements, and platform selection.
Require Compact Index `/versions` and on-demand `/info` requests, with local archive paths and no Dependencies API fallback for ordinary supported installs.
Each real client must install from fresh state without an existing lockfile or preceding lock command.
An incorrect registry archive checksum must fail with `Bundler::ChecksumMismatchError`.
Historical full-index clients, `--full-index`, Git dependencies, and local-path dependencies remain outside registry conformance claims.

Native coverage must exercise these boundaries:

- Basic caller reads, raw publication keys, optional OTP, and declared CodeArtifact service credential presentation.
- Admission, advisory decisions, direct denied downloads, and boot rejection of unsupported rules.
- Exact raw archive publication, identity mismatches, bounded Marshal and archive decoding, and independent reference fixtures.
- Complete platform mirroring, retained original source facts, restart, public outage, and incomplete or corrupt inventory refusal.
- Physical maintenance deletion, first-party protection, role authority, consent, caps, and native telemetry.
- Per-gem reads, selected compact specifications, long histories, platform multiplicity, and whole-version worker costs.

Source registration alone supplies no runtime evidence. Backend fixtures supply no live AWS or IAM evidence.
A completion claim requires the actual native providers, independent obligations, CI receipts for the claimed commit, and measured resource factors.
Missing providers, observations, factors, or calibration remain failures.
RubyGems floors come from best-effort real observations.
`bench/load/floors.json` and `bench/load/rubygems-calibration.json` record their source groups and run provenance.
Keep original eligibility verdicts and every adverse or excluded observation. Do not relabel failed runs as held receipts.
Existing ecosystems' factors, budgets, and floors do not supply RubyGems values.

### Native RubyGems info load

RubyGems per-gem load rows rebuild each admitted Compact Index reply.
`info-repeated-corpus` reads the weighted three captured histories.
`info-working-set-one` selects one history.
`info-working-set-many` reads the configured corpus working set uniformly and requires at least two histories.
The report states the selected cardinalities, and each row preflights every distinct info request through an independent native reader.
The local backend retains no full package document, and these replies do not use assembled-response retention.
Concurrent public reads can share an active read. A configured external provider can retain full source documents.
The full-store miss/collapse observations describe public fetches and waiting consumers, not private reads or exact per-name request counts.
Pattern rows measure per-gem metadata and artifacts. They exclude `/versions` and do not represent a complete Bundler installation.
A separate bounded directory fixture measures discovery with an immutable externally supplied capture.
The `versions-directory` row reads that capture from the file `BENCH_RUBYGEMS_DIRECTORY_BODY` names.
It refuses a file whose byte count or SHA-256 differs from the pins in `Ecluse.BenchLoad.RubyGems.Directory`,
whose scenario description records the capture's source URL and date.

### RubyGems Compact Index and streaming evidence

Require fresh root and transitive discovery, native archive SHA-256 values, and client refusal of changed archive bytes.
Force directory and later per-gem failures while valid archives remain available.
Both clients must fail rather than install through the checksumless Dependencies API fallback.
Test public and caller-authorised private sources with both supported Bundler versions.
Keep the Compact Index client cache while changing policy, private visibility or checksums, and require fresh `/info` revalidation.
Test source collisions, platform selection, mirror restart/outage, and complete `200` replacements after ranged requests.
Keep independent reference readers separate from production parsers.

The directory parser must reconcile repeated update rows and removals under byte, row and name bounds.
Measure directory acquisition, packed retention, private merge, response assembly and full-body transfer separately from per-gem policy work.
Record captured input sizes, name counts, source digests, actual response bytes, settings, completions, failures and measurement limits.
A directory measurement that bypasses production authorisation or admission does not prove production throughput or a pod memory bound.

Archive serving uses the shared streaming relay. Preserve its authorisation, framing, conditional, HEAD, cancellation, queue and residency controls.
Mirroring verifies copied archive bytes. Publication and mirroring retain their bounded native archive validation.
Neither streaming nor metadata checks prove that an archive is harmless.

## Onboarding an ecosystem

An ecosystem counts as onboarded when it supplies each item below for its supported operations.
Register new modules in [ecluse.cabal](../ecluse.cabal) and the applicable harness entry point.
`<Ecosystem>` denotes the module component, such as `Npm` or `PyPI`, and `<ecosystem>` denotes the corpus directory name.
The pending links identify work needed to bring existing ecosystems up to this bar.
Supply drivers for applicable canonical contracts before adding native examples.
Write direct contract calls with named concrete fixtures in the applicable suite entrypoint.
Register stable case IDs, capability metadata, and independently reviewed obligations.
Demonstrate that omitted registrations and deliberately wrong observations fail.
Keep live unavailability distinct from refused product behaviour and planned coverage.

| Obligation | Expected file or module pattern | Worked examples and current gaps |
|---|---|---|
| Unit contracts, gating in `ecluse-core-unit` | Mirror `Ecluse.Core.Registry.<Ecosystem>.*` with `core/test/unit/Ecluse/Core/Registry/<Ecosystem>/*Spec.hs`, including the adapter contracts | [npm](../core/test/unit/Ecluse/Core/Registry/Npm/) and [PyPI](../core/test/unit/Ecluse/Core/Registry/PyPI/). The shared [adapter spec](../core/test/unit/Ecluse/Core/Registry/AdapterSpec.hs) pins ecosystem dispatch and typed rule blocks with their reasons. [Boot validation](../test/unit/Ecluse/Composition/ValidateSpec.hs) checks every blocked entry in active resolved policies and accumulation beside independent refusals. The shared [read driver spec](../core/test/unit/Ecluse/Core/Registry/Metadata/FetchSpec.hs) runs each adapter's `metadataRead` from a response and from held chunks, and fails for an adapter it holds no documents for. |
| Recorded corpus outputs, gating in `ecluse-core-unit` | List the captures in [Ecluse.Test.Corpus](../test/support/Ecluse/Test/Corpus.hs) with a `CaptureUpstream`, give `core/test/unit/Ecluse/Core/Registry/<Ecosystem>/StreamingSpec.hs` a `CorpusRead`, and record each capture's lines in [corpus-outputs.tsv](../core/test/unit/fixtures/corpus-outputs.tsv) | The [npm](../core/test/unit/Ecluse/Core/Registry/Npm/StreamingSpec.hs) and [PyPI](../core/test/unit/Ecluse/Core/Registry/PyPI/StreamingSpec.hs) specs compare each capture's outputs, computed through `Ecluse.Test.Corpus.Outputs`, with its recorded lines. No tool writes the file. A new capture's spec fails, and its failure output lists the computed lines as Haskell string literals, with each tab shown as `\t`, to review and record. |
| Walk parity, gating in `ecluse-core-unit` | `core/test/unit/Ecluse/Core/Registry/<Ecosystem>/ReaderSpec.hs`, a differential property over bodies from `Ecluse.Test.Registry.JsonBytes` | [npm](../core/test/unit/Ecluse/Core/Registry/Npm/ReaderSpec.hs) and [PyPI](../core/test/unit/Ecluse/Core/Registry/PyPI/ReaderSpec.hs), against the references that [One pattern for every ecosystem](#one-pattern-for-every-ecosystem) names. |
| Adapter integration, gating in `ecluse-integration` | `test/integration/Ecluse/Core/Registry/<Ecosystem>/AdapterIntegrationSpec.hs` for metadata and artifact routes against local upstreams | [PyPI adapter](../test/integration/Ecluse/Core/Registry/PyPI/AdapterIntegrationSpec.hs). Existing npm coverage lives in the [pipeline specs](../test/integration/Ecluse/Core/Server/Pipeline/), without a separate adapter module. |
| At least one real-client install, gating in a fixed E2E subject | `test/e2e/Ecluse/E2E/<Ecosystem>/InstallE2ESpec.hs`, with fixtures under `test/e2e/Ecluse/E2E/Fixtures/<Ecosystem>.hs` | [npm](../test/e2e/Ecluse/E2E/Npm/InstallE2ESpec.hs) and [PyPI](../test/e2e/Ecluse/E2E/PyPI/InstallE2ESpec.hs), using the [npm](../test/e2e/Ecluse/E2E/Fixtures/Npm.hs) and [PyPI](../test/e2e/Ecluse/E2E/Fixtures/PyPI.hs) fixtures. An ecosystem's other end-to-end modules sit in the same directory, as [Npm/](../test/e2e/Ecluse/E2E/Npm/) shows. |
| For an ecosystem with a mirror write, a mirror-only install compared with its control, gating in a fixed E2E subject | `test/e2e/Ecluse/E2E/<Ecosystem>/MirrorInstallE2ESpec.hs`, with its fixture graph in `test/e2e/Ecluse/E2E/Fixtures/<Ecosystem>.hs` | [npm](../test/e2e/Ecluse/E2E/Npm/MirrorInstallE2ESpec.hs), as [End-to-end conformance](#end-to-end-conformance-fixed-subjects-gating) describes. [PyPI](../test/e2e/Ecluse/E2E/PyPI/MirrorInstallE2ESpec.hs) checks the complete version, public outage, restart, and file selection for a second target architecture. These target checks prove installed contents, not foreign-code execution. |
| Walk residency, gating in `ecluse-residency` | `test/residency/Ecluse/Core/Registry/<Ecosystem>/ReaderResidencySpec.hs`, registered in `test/residency/Main.hs` | [npm](../test/residency/Ecluse/Core/Registry/Npm/ReaderResidencySpec.hs) and [PyPI](../test/residency/Ecluse/Core/Registry/PyPI/ReaderResidencySpec.hs) check that eight times more dropped input leaves the bytes a walk holds level, sampled through `Ecluse.Core.Registry.Json.WalkProbe`. |
| Metadata residency captures and limits, gating in `ecluse-residency` | Add a `Residency` record through `residencyFor` in [Probe.hs](../test/residency/Ecluse/Core/Server/MemoryModel/Probe.hs). Supply authenticated captures, an exact `Calibration` for the charges, measured `Figures` for each capture, the entry bound and probe hooks. Add the document cut and heavy-base text in `Ecluse.Test.Corpus.Subset` and `Ecluse.Test.Corpus.Merge` | The residency coverage check fails by ecosystem name when a registered adapter lacks its provider. It checks non-empty captures, and one `Figures` record for each capture and no other. Ecosystems without adapters remain excluded. The complete corpus needs a capture above 3,687,514 bytes. [Benchmark captures](#benchmark-captures) defines authentication. [Listing peaks](#listing-peaks) defines calibration and its checks. |
| Read evaluation, gating in `ecluse-residency` | The same captures, and an arm for the ecosystem's served-document form in `documentKeys` in [MetadataResidencySpec.hs](../test/residency/Ecluse/Core/Registry/MetadataResidencySpec.hs) | Without that arm, the weak-pointer check of [Read evaluation](#read-evaluation) finds only the document itself and fails. |
| Work-per-request instance and corpus | Register an `EcosystemBench` in [Ecluse.Test.EcosystemBench](../test/support/Ecluse/Test/EcosystemBench.hs), with frozen bytes under `bench/corpus/<ecosystem>/`, pins in `bench/corpus/pins.json`, and a synthetic byte generator | npm and PyPI run every metadata group through the shared record. [PyPI captures](../bench/corpus/pypi/) use the shipped PEP 691 Simple JSON format. Generator checks cover decoding, projection, selective reads, and artifact URL rewriting. New instances require no changes to the benchmark groups or report renderer. |
| Allocation budgets | Per-package, per-leg figures in the ecosystem's section of `acceptance/criteria.json`, and osv.dev records for at least one capture under `bench/corpus/advisories/<ecosystem>/`, pinned at `advisories.records.<ecosystem>` in `bench/corpus/pins.json` | The [harness](../acceptance/app/Main.hs) measures every entry of the registered `EcosystemBench` corpus: the committed captures in the gating `captures` mode, and live npm packuments and PyPI PEP 691 Simple JSON documents in the `live` mode. Each ecosystem has its own report section. A new corpus entry needs calibrated figures before the gate passes, and the covered captures join the expected list in `Ecluse.Test.Corpus.AdvisoriesSpec`. |
| Load fixture | `bench/load/Ecluse/BenchLoad/<Ecosystem>.hs` exporting an `UpstreamFixture`, registered in `bench/load/Ecluse/BenchLoad/Scenarios.hs`, with a [success floor](#success-floors) for each scenario in `bench/load/floors.json` | [npm](../bench/load/Ecluse/BenchLoad/Npm.hs) and [PyPI](../bench/load/Ecluse/BenchLoad/PyPI.hs) run metadata, artifact, and cache scenarios through shared proxy wiring. Each fixture gives `Ecluse.BenchLoad.PrivateCopy` its corpus, stub, listing URL and `Ecluse.Test.Corpus.Subset` cut for the private-copy scenarios. PyPI checks PEP 691 indices and wheel bodies before load, and uses a labelled configured baseline. Its eviction cache stays below the actual corpus working set. PyPI worker mirroring measures four native archive files per job through verified extraction, upload, durable state and final completion proof. |

The shared residency gate remains in
[`test/residency/Ecluse/Core/Server/Pipeline/TarballResidencySpec.hs`](../test/residency/Ecluse/Core/Server/Pipeline/TarballResidencySpec.hs).
Extend its cases if an ecosystem adds a distinct artifact relay path.
Live protocol checks use `test/smoke/Ecluse/Core/Registry/<Ecosystem>SmokeSpec.hs`, following
the [npm](../test/smoke/Ecluse/Core/Registry/NpmSmokeSpec.hs) and
[PyPI](../test/smoke/Ecluse/Core/Registry/PyPISmokeSpec.hs) examples. PyPI reads the live Simple index
and one wheel, then verifies its advertised integrity. Bounds, identity and protocol failures fail
the case. For shared metadata reads, only timeouts and unreachable peers make the case pending.
Smoke coverage never replaces a gating case.

## OSV advisory fixtures

Advisory-shaped test data comes from committed OSV JSON, apart from the benchmarks' generated
worst case. The suites read `test/fixtures/osv/`
(`v1/`, plus the `v2/` delta). The benchmarks' [advisory rule rows](#advisory-rule-rows), the
[allocation gate](#allocation-budgets-perf-allocation-gating) and the live performance-acceptance
run, and the load harness's advisory scenarios read `bench/corpus/advisories/`. A suite derives
everything it consumes from those files at test time.
No `osv.db` is ever committed as a binary, so a fixture cannot drift from the artifact contract
(`Ecluse.Core.Osv.Schema`). Helpers in `ecluse-test-support` assemble the osv.dev-shaped zip, plus
*hostile* artifacts for rejection tests. They compile the corpus through the real OSV pipeline
(`Ecluse.Core.Osv.Compile`, in `ecluse-core`, so `ecluse-core-unit` can link it). The corpus carries
versions, so shadow-swap tests observe an ETag change and a rule-outcome flip.

## Tests and Docker

The integration and end-to-end tiers start Docker containers through the Task wrappers.
Integration uses `testcontainers`. E2E uses the raw Docker harness and the real product image.
Containers carry tier, worktree, run, and shard ownership labels under `com.ecluse.test`.
The wrappers supply `ECLTEST_SCOPE` and invocation ownership before setup.
A daemon lock serialises local container planes while they share `203.0.113.0/24`.
Run `task test-integration`, `task test-e2e`, or the coverage task instead of invoking those suites directly.
A direct invocation without required ownership fails before setup.

Every harness and CI variable uses the `ECLTEST_` prefix, never `ECLUSE_`. The config loader claims
the whole `ECLUSE_` prefix and aborts the boot on any variable under it that is not a config key, so
a harness variable on that prefix would stop the very proxy the tests are booting.

Both harnesses tear their own containers down on a normal exit, and the `docker run`s carry `--rm`.
The gap is a **hard kill** (SIGKILL, OOM, a timed-out command), which runs no cleanup and leaves the
topology behind. Two reaping commands close it, both driven by `scripts/test-containers.sh`:

- **`task test-clean`** waits for active planes and reaps stale resources belonging to this worktree.
- **`task test-clean-all`** waits for active planes and reaps stale Écluse resources across the daemon.

Automatic teardown and reaping target their own invocation. They preserve sibling runs.

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

Workflows use `$/` for actions and reusable workflows from this repository.
GitHub resolves these references at the running commit, independent of the checkout.
The flake carries a commit- and hash-pinned patch from
[actionlint PR #732](https://github.com/rhysd/actionlint/pull/732) until a release supports this syntax.
`task lint-workflows` checks inputs with the patched actionlint and keeps zizmor's `self-repository` audit enabled.
The runner policy checks both `./` and `$/` calls against the workflow files it reads.

`task sast` replaces the registry's mutable-action-tag rule with the fixed rule from
[Semgrep rules PR #4042](https://github.com/semgrep/semgrep-rules/pull/4042).
Nix fetches the rule and its tests at a fixed commit and verifies both content hashes.
`task test-sast` runs those tests before the scan.
The registry rule incorrectly flags `$/` references, so the scan excludes only that original rule and loads the fixed copy.
Remove the replacement and its exclusion when the registry publishes the fix.

Two things are easy to get backwards:

- **The integration tier is not "the tier for thorough tests."** A test goes to integration because
  its collaborator can only be a *real* (emulated) service, not because the test is broad. A
  cross-component test that needs no live external service is a **unit** test, even when it wires the
  whole pipeline. The proxy request-lifecycle runs against an in-process WAI stub in `ecluse-unit`.
  Put a test wherever its subject runs *deterministically*.
- **The smoke tier is a drift *detector*, never a correctness *guarantee*.** It depends on
  uncontrolled external services, so it cannot gate, and nothing we rely on for correctness may live
  *only* there. Every required behaviour needs deterministic, gating coverage in the unit or
  integration tier. A smoke test only confirms the model still matches the live world. Version
  ordering is the template. The gate checks it offline against a committed fixture, and the smoke
  suite also regenerates that fixture from the live oracles as a differential check.

Cargo version tests also compare grammar and precedence with the pinned Rust `semver` oracle in
`test/oracles/cargo-semver/`. The dev and CI shells build its locked dependencies from registry checksums.
`Cargo.lock` pins `semver` 1.0.28 with its crates.io checksum.
The wrapper uses [`Version::cmp_precedence`](https://docs.rs/semver/1.0.28/semver/struct.Version.html#method.cmp_precedence), which ignores build metadata.
Stable sorting preserves equal-precedence input order.
Cargo unit checks fail if the tool is missing or fails. Live registry failures remain optional smoke observations.

Beyond the test tiers, two static-analysis tools gate. **`weeder`** reports library code not reachable
from the entry point (`Ecluse.run`). **`stan`** runs HIE-based partial-function and bug analysis at
the floor in `.stan.toml`. The `analysis` job builds their `.hie` input once and runs each tool in its
own step, and stan also runs after a weeder failure. Any weeder finding, or a stan finding above
its floor, blocks the merge. The non-gating smoke job runs outside ordinary PR validation.

The Haskell jobs run in parallel where their prerequisites permit it. `build` compiles every
target and then runs the residency suite, the doctests, and `cabal check`. `coverage` is a matrix
of three legs. `ecluse-runtime-unit` and `ecluse-unit` share one leg, because `ecluse-unit` reuses
the instrumented libraries that the runtime suite builds. `allocation`, `docs`, `analysis`, and
`static-checks` each hold their own runner. E2E preparation feeds the ecosystem and
shared-system subject jobs, which start without waiting for coverage. One `backend-conformance`
job runs the Verdaccio package-store subject, then the CodeArtifact one. A job that runs two checks
runs the second after a failed first, so one failure cannot hide the other. `codecov-notify`
follows the coverage legs and releases the Codecov statuses.
The core unit, runtime unit and integration suites upload their plan and execution reports.
The `e2e` job waits for the subjects and the coverage legs. It audits every subject report
against the plan, then runs the npm, PyPI, Cargo, and RubyGems completion audits over all four tiers' reports.

CI's primary architecture is arm64: every job that builds or tests the code runs on the
`ubuntu-26.04-arm` runner. amd64 is also supported: the release dry-run builds and starts the amd64
image on the `ubuntu-26.04` runner, and no test tier runs on amd64. Every job names its Ubuntu
release, never `ubuntu-latest`, so a move to a new Ubuntu release is a reviewed change.
`scripts/ci-runner-policy.sh` (in `task lint-workflows`) fails a job on any other runner unless its
allow-list names the job with a reason.

The release dry-run also gates. It runs `release-build.yml`, the reusable workflow that
`release.yml` builds its images with, so both architectures build natively and without a cache, as
in a release. Each image build runs `scripts/image-archive.sh executables`, which fails when the
image holds a second Écluse program or a second store path with an Écluse build. It does not read
the programs that library packages carry in their own store paths. The build then runs
`scripts/image-archive.sh trust-store`, which reads every regular file directly in an
`etc/ssl/certs` directory of the image, and none in its subdirectories, as a CA bundle. It fails when
such a file holds no certificate, a block with OpenSSL trust rules, or one of the distrusted roots
the script lists, and when `SSL_CERT_FILE` names no bundle in the image.
`release-dry-run-assemble` assembles the multi-arch index without pushing it, and
`release-dry-run-boot` starts each image with `--version` on its own architecture. No dry-run job
logs in to a registry, signs, attests, or pushes. The nightly run and a manual dispatch also scan
both images' SBOMs with grype. That scan is report-only and never gates.

`release-rebuild` runs the same workflow a second time for the same commit, on separate runners
that share no cache and no Nix store with the first build, and it uploads no SBOM. It runs on a
push to main, the nightly run, and a manual dispatch, and never on a pull request.
`release-compare` then compares the two archives of each architecture by SHA-256
(`scripts/image-archive.sh compare`), and on a difference it prints the archive members and the
files that differ. `release-compare` gates both architectures. A difference fails `CI gate`, as
does a skipped comparison after a failed or cancelled build. The gate accepts a comparison skip
only when the change classifier sets `release-rebuild=false`, which it does on every pull request.

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
that never builds into `dist-newstyle` skips its restore. The jobs that build into a directory of
their own restore these:

| Job | Build directories it restores |
| --- | --- |
| `coverage`, each unit leg | `dist-coverage`, where it builds its suites instrumented |
| `coverage`, the integration leg | `dist-coverage` for its instrumented suite, and `dist-newstyle` for its manifest and audit tools |
| `analysis` | none: it builds `dist-analysis` with `-fwrite-ide-info` and never caches it |

Each `coverage` leg writes its own `dist-coverage` key, so no two legs race and each leg starts warm
for the suites it owns.

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

The same script skips the release dry-run for a PR whose paths cannot change the release image. Such
paths are documentation, Haskell source, runbooks, and analysis-tool configuration. Scripts that the
release workflows, the dry-run, and the Taskfiles' top-level variables never run also qualify.
`e2e-prepare` builds the arm64 image from that source, but such a PR compiles nothing on amd64.
[`scripts/ci-cabal-diff.sh`](../scripts/ci-cabal-diff.sh) reads an `ecluse.cabal` change line by
line. The change skips the dry-run when each changed line is a comment, a blank line, or a
module-list entry. A test-suite or benchmark field also qualifies, unless it names dependencies,
imports a common stanza, sets `buildable`, or is a conditional. Evaluating the image resolves the
dependency names of every buildable stanza. Any other changed line runs the dry-run, as does a diff
that the script cannot parse. The flake, `cabal.project`, the freeze, the Taskfiles (an included
file's top-level variables reach every task), the workflows, the CI actions, the other scripts,
`test/oracles/` (it feeds the `.#ci` shell), and any unlisted path run it. A push to main, the
nightly run, and a manual dispatch always run it. The `gate` job accepts a skipped job from the
classifier's outputs and from nothing else, so a job that silently never ran still fails the gate.

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

## Coverage: Codecov

CI measures coverage per gating suite and reports it to [Codecov](https://about.codecov.io/).
The conformance jobs add completion evidence. They do not change the four coverage uploads or their merged denominator.
Generation is local and tool-agnostic. A suite is built instrumented: HPC, in an isolated
`dist-coverage/` that leaves the normal build cache alone. Then `hpc-codecov` converts the
`.tix`/`.mix` output to Codecov's native JSON.
Task orders the four suites and the tick unions in [`taskfiles/coverage.yml`](../taskfiles/coverage.yml).
One converter, `scripts/coverage.sh`, receives explicit tick and output paths for both partial and combined reports.

**Codecov is the merged authority, and `task coverage` reproduces it.** Codecov merges the per-flag
uploads into one project total. A single tier's number therefore *under-counts* the modules the
others exercise: only integration covers the SQS `MirrorQueue` and the worker's fetch/publish path.
`task coverage` runs the three instrumented unit suites plus `ecluse-integration` and
`hpc combine --union`s them into `coverage/combined.json`, so it agrees with the dashboard. It runs
the integration tier, so it needs a Docker daemon. Without one it fails and points at the fast path.
For a quick, Docker-free loop, `task coverage-unit` (default `SUITE=ecluse-unit`, or another suite)
measures one tier and prints loudly that it is a partial view.

Integration coverage rejects selection arguments, report overrides, and extra suite targets in `COVERAGE_FLAGS`.
Accepted options are `-fdev-http-egress` (or `--flags=dev-http-egress`), `--enable-coverage`, `--builddir=dist-coverage`, and `--test-show-details=direct`.
Separate manifest and audit executables check the report against every compiled integration declaration.
The test binary retains its own `.tix` file, preserving the suite's coverage ticks.

**What CI uploads.** The `coverage` job is a three-leg matrix. Each leg calls the fixed Task
coverage target once per suite, and each suite uploads its own JSON: `ecluse-core-unit`,
`ecluse-runtime-unit`, and `ecluse-unit` (all under the Codecov flag `unit`), and
`ecluse-integration` (flag `integration`). Only the integration leg needs a Docker daemon.

**When the Codecov statuses post.** `notify.manual_trigger: true` in
[`codecov.yml`](../codecov.yml) holds every Codecov status and comment until the CLI asks for them.
The `codecov-notify` job makes that call, and it runs only once all three legs are green, so
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

Codecov posts two commit statuses, both configured in [`codecov.yml`](../codecov.yml).
`codecov/project` allows no regression versus the PR base, within a 1% threshold, and it is the
one the ruleset requires. `codecov/patch` asks for new and changed lines at ≥ 85%, a floor that
verifies behaviour rather than a number to chase. Uploads use GitHub OIDC (`use_oidc: true`), so
there is no `CODECOV_TOKEN` to leak. Coverage
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
