# Temporary full-history React diagnostic

Run `task performance:full-history-diagnostic:run` through the guarded manual workflow on
`measure/refactor-full-history`. This controller never enters the product branch.

The fixed ARM64 schedule runs original A/B, marker-only B/A, then recorded A/B/B/A.
All four executables build and qualify before measurement. Every process runs the full suite,
with 343 native CSV rows and all 361 test outcomes. The command has no pattern selection or
native timeout override. A 30-minute external guard retains failed and partial evidence.
The build/run workflow step ends after 210 minutes within the 240-minute job cap.

The pinned tasty-bench mapper marks only React selective decode through a private option.
Both marked modes share the tree traversal, inventory check and provider lookup.
Only the marked provider reads the mode. Other rows and complexity assertions use native
measurement. Benchmark actions, corpus preparation, scheduler and resource lifetime stay intact.
CSV rows describe presentation order, not execution order.

The overlay keeps the qualified recorder, counter types and order, 256-record foreign buffers,
cleanup and native estimator. Added tree traversal, option lookup, output setup and changed
compiled dependency context remain diagnostic overhead. Marker-only controls assess their
combined effects. One original or marker observation per source gives no variability bound.

The reporter authenticates source, dependency, configuration and corpus bytes from a fixed
catalogue. It requires every artifact, actual executable, plan edge, mode, command, status,
serial event, native row, assertion outcome and marked row binding. Semantic controls refresh
outer digests and must fail at their intended boundary before the restored fixture passes.
The full-plan guard permits only the native library, or the marked library and bench-fibo.
It resolves the five library edges and the two bench-fibo edges within each plan.
The guard compares the library edges across all builds before launch and during final audit.
The original focused recipe and its controls remain available.

Earlier focused empty batches allocated 12,424-12,912 global bytes despite displayed zero
per-operation estimates. These different workloads do not prove zero overhead or supply a
correction. This campaign adds no empty replacements and subtracts no overhead.
All signed comparisons remain supplementary evidence. No diagnostic outcome issues an
acceptance receipt, changes a budget or resolves the original native full-suite increase.
