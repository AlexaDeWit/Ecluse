# Temporary issue 1565 comparison

The manifest preserves all 343 parsed benchmark names in their historical order.
Source: `690a4cd522f3146fc04ae42d6abac684a44a422c`, [run 38335071591](https://github.com/AlexaDeWit/Ecluse/actions/runs/38335071591).
Artifact: `bench-results-ARM64-690a4cd522f3146fc04ae42d6abac684a44a422c`.
The manual workflow requires `refactor_comparison=true` and an exact 40-character `refactor_source` commit.
The baseline stays fixed. No candidate commit is the default.
The controller and its controls run outside both measured source trees. This branch must not merge into product development.

The benchmark order is A1, A2, B1, A3, B2, B3. Allocation and both maintenance probes use A1, B1, A2, B2.
Residency uses A1, B1. All measurements run serially on one ARM64 worker.
Every repetition remains in the artifacts. Reports compare every pair, with A/A and B/B variability separate from A/B changes.
No pair selection, averaging, or statistical exception changes a tolerance.

The controller requires all 343 ordered benchmark names, 20 native receipts, 48 allocation rows, and 144 metadata residency rows per pass.
Fixed and prepared benchmark allocations allow 1 KiB in either direction. Production reads allow 0.1% in either direction.
Current single-version benchmark rows use the production reader. The allocation gate keeps its distinct single-version allowance of 1 KiB.
Full allocation medians allow 0.1%. ARM64 live-byte figures allow 1 KiB. Residency preparation allocation can change.
Mutable cache rows remain unclassified diagnostics. Each row requires review regardless of its delta.
The report records that scope limit and makes no universal parity claim.
Timing and process peak RSS remain informational. Every signed delta stays visible, including reductions outside a tolerance.
Separate lower and higher allocation tables retain every pair. `audit-faults.txt` records invalid evidence. `tolerance-breaches.tsv` records measured changes.

R9a calls the unchanged `StreamedVersions` command on all nine authenticated npm captures, including TypeScript.
R9b reads fixed listings with 20 and 20,000 valid names through the native bounded reader.
The listings include duplicate and invalid names and skipped metadata. A separate expected-name file supplies the output oracle.
Input loading, chunk preparation and oracle preparation occur before the measurement bracket.
Both maintenance probes record allocation, live bytes, peak live bytes, process peak RSS and elapsed time with `+RTS -N1 -A4m -T`.
Their allocation must not rise. Source, architecture, run, attempt, RTS options and input digests must match their declarations.

R9b uses separate source checkouts and separate build directories. Its overlay replaces only the residency entrypoint and adds one listing adapter.
The adapter selects the old combinator or the new walk. It changes no production source or production benchmark option.
The artifacts record each overlay patch, adapter hash, source tree, build plan and executable hash.
The custom probe executable is an overlaid build. The native benchmark and existing residency executables remain unmodified builds.
Failed reads, builds and audits retain their statuses and available output. A completed report alone does not establish performance parity.
