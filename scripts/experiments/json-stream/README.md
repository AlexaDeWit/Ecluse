# Maintained parser comparison

This measurement branch compares three fixed sources on each CI runner.
It retains the existing upstream programs, 14 fixtures, token fold, and timing method.

| Artifact variant | Source | Commit |
| --- | --- | --- |
| original | Latest C main | `7f214208ea93a54584f0687bef6870d68f7ac8a8` |
| baseline | Previously published native parser | `53dd7943129211f882f98a89bcb828b1cbba45e4` |
| candidate | Maintained parser with ST decoding and typed Object accumulators | `689b7a19f4c3c1840a115cc185582075293c050e` |

The historical `original` artifact label means C main here, not original upstream C.
Upstream commit `537a43a775e64f50dc63c373193323de98619799` supplies the benchmark programs and fixtures.

## Preparation

The source loader accepts `vendor/json-stream` and `json-stream/src`.
It fails if a revision contains both layouts or neither layout.
Each extracted library keeps its own licence.
The source manifest records existing C files when present, without requiring a C directory in the maintained layout.
`variants.json` records the original source path, Git tree identity, and licence path.

Both native folds use the public Lexer facade and the same record, digest, and forcing functions.
The C fold keeps its tokenParser adapter.
The differing-API control still requires identical Haskell source trees, independent of their repository layout.

Dependency and Nix input comparisons remain mandatory.
CI retains generated package definitions, source hashes, dependency plans, binary hashes, symbol lists, verification output, and every raw sample.
Native executables must lack `lex_json`, and the C reference must contain it.

## Measurement method

The matrix retains default caller flags and `-fno-full-laziness` on TokenFold only.
Library and upstream Parser compilation remain at `-O2` with their existing source pragmas.
All variants build before verification and timing.
Verification compares token payload digests at 1, 1024, and 32768-byte chunks.
Measurement uses the existing five rotated repetitions, 0.3-second calibration target, and two fresh input copies per fold pass.

Report both candidate versus prior native and candidate versus latest C main.
Keep all 14 fixtures and both profiles, including regressions and overlapping sample ranges.
Selected-parser measurements remain secondary to full Parser and full token traversal.

## Interpretation limits

The upstream full Parser benchmark includes file seeks and reads and tests successful decoding.
It does not explicitly deep-force the resulting Aeson Value.
Its result does not establish Ecluse full typed projection or cold HTTP read performance.
The typed Object change is also exercised by the existing selected-object benchmark.

The token fold forces numeric payloads and string lengths during measurement.
Verification hashes complete string payloads.
The fold does not call string decoding or build typed Object accumulators, so unchanged fold results would not validate those changes.

The earlier facade comparison recorded a roughly 12.8% sigma slowdown at 32768-byte fold chunks.
That time regression persisted when residency and GC counts matched and remains unexplained.
The reported maximum-live increase occurred in three of ten candidate samples, each with an extra major collection.
The harness keeps the whole input and makes complete copies, so sparse GC samples can observe overlapping buffers.
Untimed cursor finaliser checks found no retention regression in that earlier comparison.
Keep the current sigma time and residency rows, without treating allocation or process maximum-live data as per-request retained memory.

## Dispatch state

The owner authorised this pinned comparison after hosted compilation and formatting evidence.
This comparison builds all three sources and verifies the workloads in CI before timing.
Publishing a branch matching `measure/json-upstream-*` triggers the workflow automatically.
No local formatting, Nix, compilation, tests, HLS requests, or probes were run while preparing this update.
No upstream issue, comment, or pull request is part of this experiment.
