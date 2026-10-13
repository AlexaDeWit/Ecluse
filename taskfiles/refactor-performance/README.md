# Temporary issue 1565 comparison

The manifest preserves all 343 parsed benchmark names in their historical order.
Source: `690a4cd522f3146fc04ae42d6abac684a44a422c`, [run 38335071591](https://github.com/AlexaDeWit/Ecluse/actions/runs/38335071591).
Artifact: `bench-results-ARM64-690a4cd522f3146fc04ae42d6abac684a44a422c`.
The manual workflow requires `refactor_comparison=true` and an exact 40-character `refactor_source` commit.
The baseline stays fixed. No candidate commit is the default.
The controller and its controls run outside both measured source trees. This branch must not merge into product development.

The benchmark order is A1, A2, B1, A3, B2, B3. Allocation and both maintenance probes use A1, B1, A2, B2.
Residency also uses A1, B1, A2, B2. Its report retains one A/A pair, one B/B pair and all four A/B pairs.
All measurements run serially on one ARM64 worker.
Every repetition remains in the artifacts. Reports compare every pair, with A/A and B/B variability separate from A/B changes.
No pair selection, averaging, or statistical exception changes a tolerance.
The [earlier comparison](https://github.com/AlexaDeWit/Ecluse/actions/runs/38386330799) took about 133 minutes, with each residency pass taking 14 minutes 45 seconds.
Two more residency passes add about 29 minutes 30 seconds at that rate. The workflow cap remains 240 minutes.

The controller requires all 343 ordered benchmark names, 20 native receipts, 48 allocation rows, and 144 metadata residency rows per pass.
Fixed and prepared benchmark allocations allow 1 KiB in either direction. Production reads allow 0.1% in either direction.
Current single-version benchmark rows use the production reader. The allocation gate keeps its distinct single-version allowance of 1 KiB.
Full allocation medians allow 0.1%. ARM64 live-byte figures allow 1 KiB. Residency preparation allocation can change.
The nine pure cache key-construction rows use the fixed-subject 1 KiB allowance.
Residency `preparationMaxLive` also uses the 1 KiB allowance. R9 source-probe peaks remain informational.
Mutable cache rows remain unclassified diagnostics. Each row requires review regardless of its delta.
The report records that scope limit and makes no universal parity claim.
Timing and process peak RSS remain informational. Every signed delta stays visible, including reductions outside a tolerance.
Separate lower and higher allocation tables retain every pair. `audit-faults.txt` records invalid evidence. `tolerance-breaches.tsv` records measured changes.
Each pass writes `binding.json` after its final status. The binding names the source tree, run, attempt, worker, RTS recipe, executable digest and build-plan digest.
It hashes the native outputs, input files, receipts, source snapshots, build records, statuses and per-pass event records.
The aggregate audit rehashes every required file and checks the recorded serial order before it compares numbers.
Native receipt digests independently cover the copied CSV, Markdown, observation JSON and archived corpus inputs.
Probe obligations come from the compiled catalogue and authenticated capture pins.
The controller reads `source-inputs-A/bench/corpus/pins.json` under the supplied evidence root, including after an artifact download.
It checks SHA-256 `5f7a6509afb84a8269cf9e382ffa60b5255628dac9d33dece010248c480db14a` before parsing the pins.
That digest identifies the pins at baseline `690a4cd522f3146fc04ae42d6abac684a44a422c`. Missing or different bytes produce an audit fault.
The audit requires exactly nine npm inputs, including TypeScript, and both fixed listing inputs.
It checks every input field, the full command list, both archived corpora and pins, listing bodies and oracles, and all twelve capture sizes.
Listing manifest paths are relative to the evidence root. Native probe arguments and input bytes stay unchanged.
Missing or changed evidence blocks numeric qualification. The archive retains the available raw output.
Artifact paths in bindings are relative to the archive root. The archive records binary digests, rather than distributing the executables.
Those digests come from the recorder's live hashes before and after each pass. They do not provide an independent build attestation.

The listing residency comparison includes the retained document as well as the peak fields.
It multiplies `document_per_source_byte` by the authenticated capture size and rounds to the nearest integer byte, with ties to even.
The recovered figure uses the same 1 KiB allowance. A control changes only this figure by 1,025 bytes.

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
Remote controls remove or alter each recorded artifact class and replace a complete 343-row CSV with stale values.
The native receipt check also rejects that stale CSV after the control regenerates its outer binding.
Workload controls regenerate matching outer bindings before rejecting empty, omitted, duplicate or altered inputs and capture sizes.

## Supplementary three-probe residency diagnostic

The temporary `performance:residency-diagnostics:run` task measures aws-sdk-client-s3
HeavyBase, webpack Shared and lodash PublishOrder. It uses native sources
`690a4cd522f3146fc04ae42d6abac684a44a422c` and
`ab5f4af6e55b90ae79392f3d10a9505295f1239f`. A1, B1, A2 and B2 each run the
three native children in that order on one ARM64 worker.

Two private helper checkouts change only `test/residency/Main.hs`. They call
`writeMergeDocuments` from each exact source. The eight calls archive both
returned byte sequences, paths, byte counts and SHA-256 digests. The controller
requires identical bytes within and between sources before measurement.
Webpack uses its authenticated committed capture. No input is re-encoded.

The native checkouts stay clean. All builds and input generation finish before
the first child. Each child keeps its original argv and RTS options. The audit
checks mandatory artifacts, exact helper overlays, source and tree identities,
build plans, executable bytes, toolchains, corpus pins, worker identity,
externally supplied CI run and attempt, commands, statuses and timestamps.
Snapshots and live input hashes surround each child. Native stdout and stderr
remain separate, including successful children that print diagnostics.

Source archives use `git -c tar.umask=0000 archive --format=tar SOURCE`.
The controller authenticates fixed uncompressed archive hashes and all four
configuration files against each exact source. Both plans require the native
residency component, its matching checkout, GHC 9.10.3, Cabal 3.16.1.0,
Linux/aarch64 and the original flags. External dependency comparisons retain
identities, hashes and flags while removing only source-specific paths.

Build qualification finishes before input generation. Full preparation
qualification finishes before the first child. Live helper hashes must match
the archived build before and after every call. Corpus and lock inventories
must qualify before the audit reads their recorded paths.

The report retains all six signed pairs per probe. Source, basis and served
byte fields stay exact. The original live-field rule allows 1024 bytes.
Preparation allocation remains informational. Adverse rows stay visible.
This supplementary campaign does not establish whole-suite parity, replace
acceptance or waive a native breach. R9b listing recovery is separately qualified.
Archived evidence does not attest the worker or prove the absence of other host
processes. The isolated serial recipe records the controller's scheduling.

Setup has four builds. Allow about 25-35 minutes with similar caches, then
10 minutes for helpers and children initially. Each child has a 10-minute limit.
The workflow retains its 240-minute hard cap and uploads partial evidence.
Per-child costs remain unverified until CI runs this campaign.

Use the mutually exclusive `residency_diagnostic` dispatch input on
`measure/refactor-residency-diagnostic`. Shared hosts permit static checks only.
The static controls inspect the real Task include tree, poison sibling variables,
check exact nested snapshots and confine previews to a scratch fixture.
They also execute the measure target against a Bash recorder. This checks
returned paths and original arguments without running a native sampler.
The runtime controls exercise the complete audit, refresh outer digests for
semantic mutations and require a restored positive fixture.
