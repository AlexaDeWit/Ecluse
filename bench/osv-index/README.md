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
If an explicit pin disappears, stop. Select a new complete snapshot before any rerun.
Never mix snapshots across variants. The final manifest hashes inputs and both binaries.
Source manifests hash every tracked file separately. Their hashes identify the generated trees.
The source diff records the single compiler change.

Each query batch measures package enumeration, selected range lookups and the current
in-memory fixed-version lookup. The selected names include absent, corpus, median,
95th-percentile and maximum fan-out names. Each operation repeats for at least 20 ms.
Thirty alternating pairs run in each timing mode. Internal operation times describe
reused connections. External process times include startup and reader acceptance.
The OS page cache is uncontrolled. This experiment does not measure cold disks.

Python uses the existing Nix dependency only to capture one child process's wall time,
CPU time and peak RSS. A fresh wrapper prevents accumulated child RSS from earlier samples.
RSS includes the local HTTP stubs' retained fixture bytes. Haskell owns all semantic
checks, hashes, manifests, qualification and summaries.

Typed sorted rows retain NULLs, scores and duplicate copies. Every sample compares with
its case's A warm-up. Metadata comparisons exclude `built_at` and normalise only the port
in the four loopback source URL fields. Full original values remain in the snapshots.
Schema comparisons allow only the candidate index. Native readers compare every package's
complete row multiset and record raw sequence changes. Rule comparisons use captured
releases, selected bounds and injected fixed, inclusive, unbounded, invalid and duplicate rows.
They normalise advisory ID order only and use a stable comparison ETag.

Failed and partial samples remain visible. Successful repeated databases are removed only
after their equality check. Warm-up pairs remain available for review. A missing observation,
nonzero status or semantic mismatch makes the run incomplete. No automatic removal threshold
is defined. Review variance and adverse observations before recommending retention or removal.
A compatible storage reduction keeps the schema epoch and follows issue #1234's explicit
migration override policy.
