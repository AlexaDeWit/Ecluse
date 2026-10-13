# Temporary React allocation diagnostic

This controller records diagnostic evidence. It does not replace the complete comparison or its accepted limits.
Only branch `measure/refactor-allocation-diagnostics` can run its manual job in `bench.yml`.
The fixed entrypoint is `task performance:allocation-diagnostics:run`.

## Fixed comparison

- A: `690a4cd522f3146fc04ae42d6abac684a44a422c`.
- B: `ab5f4af6e55b90ae79392f3d10a9505295f1239f`.
- Row: `All.ecluse-core (work-per-request).ecosystem: npm.single-version metadata (per package).react (large).selective decode`.
- Build: optimisation 1, profiling disabled, original benchmark RTS defaults and explicit `+RTS -T -RTS`.
- Native estimator: tasty-bench `0.4.1`, `--stdev 10`, original adaptive stopping.

Task builds the controller and all four source variants before launching any measurements.
The original product worktrees contain no source edits. Instrumented worktrees add only an archived `cabal.project.local` dependency selection.
The local dependency overlay holds the modified tasty-bench source. All original row bodies, fresh table keys and forcing remain intact.

Task lists eight React launches in this order: C-A1, C-B1, C-B2, C-A2, I-A1, I-B1, I-B2, I-A2.
It then lists four empty controls: O-A-native, O-A-record, O-B-native, O-B-record.
Every launch retains the original benchmark startup and HTTP/advisory resource lifetime.
Filtering to one row changes preceding benchmark history. The output therefore remains diagnostic.
A separate ten-minute process guard refuses hung launches. It does not change the native adaptive algorithm.

## Dependency identity and attribution

The source is [tasty-bench 0.4.1](https://hackage-content.haskell.org/package/tasty-bench-0.4.1/tasty-bench-0.4.1.tar.gz), copyright Andrew Lelechenko, under the MIT licence.
`LICENSE.tasty-bench` preserves that licence. `tasty-bench.patch` describes this controller's modifications.
`Diagnostic.hs` supplies the recorder. The archived dependency includes its original licence, original tarball and revised Cabal file.

The original archive SHA-256 is `315c77e1f0f5fddf3b8465060867bb30c6d6562867b8609afdc7f6e14e295777`.
[Revision 1](https://hackage-content.haskell.org/package/tasty-bench-0.4.1/revision/1.cabal) has SHA-256 `c38a77747f14e8d26ca99869b879ee56099b04e5381a74a62255dcbab85aab84`.
Both identities match the qualified native build plan. Preparation refuses different bytes.
It normalises the local Cabal copy from CRLF to LF before applying the patch without fuzzy matching.
The archived revision keeps its original bytes. The zero-context patch relies on the verified upstream hashes.

The patch preserves upstream documentation and style in its context. Reformatting upstream prose would obscure the small diagnostic changes and their attribution.
The edited controller entrypoint needed no structural rewrite. Its long baseline-directory comment was shortened without changing behaviour.

## Recorder lifetime and counter order

The Tasty provider brackets one recorder around the selected row. It captures and fully evaluates the display form of `ThreadId` before measurement.
It allocates two foreign arrays for at most 256 records. Unsigned fields use `Word64`. Thread allocation endpoints use `Int64`.
The bracket flushes partial records after failures and frees both arrays, including when flushing fails.
Overflow raises a typed exception. It never overwrites earlier records.

The recorded batch executes this exact order:

1. Perform the original major GC.
2. Read the original starting time.
3. Read the original starting global allocation/copied/maximum counters.
4. Read the calling thread's allocation counter.
5. Execute the original `act n` synchronously.
6. Read the calling thread's allocation counter again.
7. Read the original ending time.
8. Perform the original minor GC.
9. Read the original ending global allocation/copied/maximum counters.
10. Check the current `ThreadId`, then store the record.

The same provider thread performs steps 4, 5 and 6. Neither the recorder nor the patch forks the action.
The post-measurement check rejects a different thread. Capability migration does not change a Haskell thread's identity.
No labels, string formatting, file writes or buffer writes occur between the measured endpoints.
The two extra thread-counter calls contribute diagnostic overhead. Native time and global allocation include those calls.
The thread-counter difference brackets the action more narrowly and does not include the final GC or global counter query.

Records contain the sequence index, n, both native time endpoints, both native global-allocation endpoints, both signed thread endpoints and the captured thread display.
Writing occurs after the final native counter query. Flushing occurs after `measureUntil` returns or fails.
Storage work can affect later runtime state or scheduling, even though it falls outside the current batch's accounting.
The uninstrumented controls and empty controls expose that limit.

The native `measure` stays unchanged. `measureUntilWith` receives either that function or the recorded function.
The patch preserves prediction, integer division, progress reporting and stopping conditions.
The final two records permit exact reconstruction of native allocation:

```
((a1 quot 5) + 2 * (a2 quot 5)) quot n
```

Here a1 and a2 are global deltas from the final n and 2*n batches. Every intermediate batch remains in `batches.csv`.

## Empty controls

Only modes `empty-native` and `empty-record` substitute the dependency's existing `whnfIO (pure ())` action at its provider.
The original React row definition remains unchanged. The mode in `conditions.json` prevents an empty result from representing a React measurement.
Both controls run the same original startup, adaptive estimator and selected label.
`empty-native` uses the unmodified measurement function and allocates no recorder.
`empty-record` adds the same counter reads, buffer and flush used for React diagnostics.

`empty-overhead.tsv` records their observed allocation and timing differences for each source.
These are separate overhead observations, not exact corrections for a real action. Batch sizes and scheduling can differ.
The controller never subtracts overhead from React data or accepted measurements.

## Evidence and audit

The artifact includes original source archives, inputs, dependency sources, overlay files, actual executables, build plans, options and commands.
Build and pass bindings hash archived files. Before/after snapshots check live binaries, source identities, inputs, lock files, plans and dependency files.
The controller validates the native dependency's source and the overlay's local build selection.
Other external plan entries must match after removing directory metadata. The local Ecluse component graph can differ between A and B.

The audit requires exactly twelve named passes in the prescribed serial order.
Each pass needs exactly the selected CSV row, zero native and outer statuses, matched identities and exact commands.
Recorded passes need one measurement thread, bounded signed/unsigned fields, a complete doubling sequence and exact estimator reconstruction.
Missing, duplicated, failed or timed-out evidence fails the audit. Native allocation estimates and raw batch records remain untouched.
Remote negative controls exercise absent, duplicated, reordered and oversized batch records, invalid counters and mismatched estimates.

`observations.tsv` retains every native estimate. `empty-overhead.tsv` keeps the four empty controls separate.
No full-suite acceptance receipt is generated or reused.
The native CSV and raw batch records remain the source for later analysis.

## Validation boundary

`task performance:allocation-diagnostics:static` checks Haskell formatting, HLint, actionlint, Zizmor, the runner policy and Task parsing.
Local Haskell builds, Haskell tests and performance measurements are prohibited on the shared host.
The remote controller build and dependency overlay build therefore remain required before any measurement can qualify.
