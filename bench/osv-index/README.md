# Temporary range-index experiment

This branch measures issue #1331. Do not merge this harness into the product.
Task owns acquisition, two isolated builds, sample ordering and the final audit.
The A compiler retains all production indexes. The generated B source removes only
`uq_ranges_segment`. Both retain `INSERT OR IGNORE` and the lookup indexes.

Run the existing `bench.yml` workflow by this branch only after independent review.
Its extra job runs only on manual dispatch. Existing benchmark jobs remain unchanged.
The four inputs are npm corpus, PyPI corpus, npm full export and PyPI full export.
Each receives an A/B warm-up, then six measured pairs in alternating AB/BA order.
Downloads and builds occur before measurement. Every compilation uses a fresh directory.

The acquisition helper refuses a generation, version, ETag or size mismatch.
If an explicit pin disappears, stop and select a new complete snapshot before any rerun.
Never mix snapshots across variants. The manifest records the complete input inventory,
source manifests, source delta, binaries, toolchain and hosted execution identity.
The audit rehashes these files and checks A against the signed checkout. It requires
exactly the approved one-line compiler difference between A and B.

Each query batch measures package enumeration, five range lookups and five current
in-memory fixed-version lookups. Selection roles cover absent, corpus, median,
95th-percentile and maximum fan-out names. Roles retain unique operation IDs even
when they name the same package. Each operation repeats for at least 20 ms.
Thirty alternating pairs run in each mode. Canonically sorted rows determine the
fixed-version probe cycle, so an SQL row-order change cannot change the workload.

Reused mode opens one accepted connection outside its operation timers. Fresh mode
opens, accepts, uses and closes a reader inside every operation timer. Fresh fixed-version
work also loads the package and builds its in-memory index. Both modes use a fresh
process for each batch. Neither mode flushes the OS page cache or measures cold disks.

Each native process hashes its actual executable and inputs before its timing window.
It records its command arguments, hosted execution identity, total CPU time and start/end
events. Sequence numbers count actual starts in an append-only journal. The audit compares
the journal with an independent serial plan and the outer wrapper's timing envelope.
Native output hashes bind each snapshot or query report to the process that produced it.

Python uses the existing Nix dependency only to capture one child process's wall time,
user/system CPU and peak RSS. A fresh wrapper prevents accumulated RSS from earlier children.
These are process-envelope measurements, including identity verification and fixture memory.
Native wall and total CPU measurements exclude identity hashing. Haskell owns the semantic
checks, hashes, manifests, qualification and summaries.

Typed sorted rows retain NULLs, scores and duplicate copies. Every sample compares with
its case's A warm-up. Metadata comparisons exclude only the `built_at` value and validated ephemeral ports.
The two authority fields must match their paired full URLs before normalisation.
All keys, hosts, schemes, paths and other values remain significant. Full original values
remain in the snapshots. The controls include an unchanged snapshot from the rejected run.
Schema comparisons allow only the candidate index. The audit recomputes both selections,
fan-out counts and exact production SQL plans from the retained databases.

Native reader reports contain covered names, package digests, complete typed verdict
categories and terminal counts. The audit rebuilds expected results from the retained
database and captured releases. It requires exact unique package and rule-probe coverage.
It also checks query result counts against independent cycles from the retained databases.
Rule comparisons include selected bounds, captured releases and injected fixed, inclusive,
unbounded, invalid and duplicate rows. They normalise advisory ID order only and use a
stable comparison ETag. Raw row-sequence changes remain visible.

The remote build target runs adversarial audit controls and a tiny connection-lifetime
fixture before the full experiment. These controls need no full feed and do not replace
normal repository gates. Missing, malformed, duplicate, reordered or mismatched evidence
produces a stored qualification fault before the audit returns failure.

Failed and partial sample files remain visible. Successful repeated databases are removed
only after equality checks. Warm-up pairs remain available for review. Compilation summaries
state their successful count and preserve every adverse result. No automatic removal
threshold is defined. Review variance before recommending retention or removal.
A compatible reduction keeps the schema epoch and follows issue #1234's migration override.
