# Large full-read validation

This measurement branch compares main `dc1bfb35`, native plus prepared keys `3b074ce6`,
and C plus prepared keys `a45f883a`. It changes benchmark and test-support code only.
The branch-scoped `json-shape-validation.yml` workflow builds all three before running any timers.

The fixed inputs are the three PyPI captures, two generated indexes with 8,192 and 16,384 files,
four transformations each of NumPy and boto3, and three large npm captures. The transformations
add retained text across input chunks, encode equivalent escaped JSON, add one ignored string,
or distribute ignored objects across files. The ignored cases add the same encoded byte count.
The original capture files and pins remain unchanged.

Generation verifies the original pins and records each derived body's size, digest, file count,
version count, maximum string length, and backslash count. Every read enforces the existing body,
artifact, version, and nesting limits before entering the production reader.

Semantic preflight checks full typed facts, the serialised document, consumed bytes, and source
digest. It compares all implementations on identical wire bytes. Escaped and ignored cases also
keep the original projection. Long retained text and escapes receive four chunk-boundary checks.
Raw digests can differ between equivalent encodings, so those comparisons use projections only.

Each measured subprocess contains one case. This prevents replay-map collisions between different
bodies for the same package. The runner reuses the existing full metadata projection and loopback
cold-read benchmarks. Input generation, slicing, and semantic preflight remain outside timers.

The fixed orders are main/native/C, native/C/main, and C/main/native on one runner. These are a
bounded measurement design, not an acceptance rule. The runner retains every CSV and log, including
partial results. It sets one capability and keeps the default allocation-area setting. No numeric
performance threshold passes or fails the run.

The workflow runs on pushes to `measure/json-shape-validation`. All three source revisions must
be available on the remote before that push. CI calls `task shape-validation` inside the pinned
Nix shell. The script refuses to run timing measurements outside GitHub Actions.

Local compilation is `nix develop --command task shape-validation-build`. Untimed modes use the built
binary with `ECLUSE_SHAPE_MODE=generate` or `preflight`, `ECLUSE_SHAPE_DIR` for inputs, and
`ECLUSE_SHAPE_OUTPUT` for semantic output. `ECLUSE_SHAPE_BASELINE` names main's preflight directory
when checking another implementation. `ECLUSE_SHAPE_MODE=bench` is reserved for the CI script.

The separate load harness supplies cgroup peak and GC evidence. These read benchmarks report
allocation and time, which do not establish per-request live memory or admitted parallelism.
