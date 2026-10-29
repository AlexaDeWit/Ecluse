# Cargo sparse captures

These files contain complete original sparse index responses from crates.io.
`../pins.json` records each source, capture time, byte size and SHA-256.
Workload pins select releases without cutting the captured release lists.
No crate archive is executed or included in this corpus.
`BENCH_CORPUS_ECOSYSTEM=cargo task gen-bench-corpus` replaces these snapshots.
`BENCH_CORPUS_VERIFY=1` authenticates the committed bytes without network access.

The crates.io team maintains the index: https://github.com/rust-lang/crates.io-index.
The source index repository declares no separate licence for these response bodies.
These factual registry observations do not inherit the proxy's MIT licence.
The package source licences remain separate from the metadata capture.
Tokio publishes under MIT, Windows under MIT OR Apache-2.0, and AWS SDK EC2 under Apache-2.0.
Their source repositories are https://github.com/tokio-rs/tokio,
https://github.com/microsoft/windows-rs and https://github.com/awslabs/aws-sdk-rust.

The native files retain every release row, dependency, registry reference, feature,
yank state, publication time, checksum, schema and MSRV field in the response.
Windows and AWS SDK EC2 each exceed one MiB and qualify for native per-byte charge measurements.
The corpus-wide 3,687,514-byte threshold also includes the existing npm and PyPI captures.

Private-copy cuts use each row's captured publication time.
Heavy private variants add a long valid feature key to retained records.
This synthetic stress models unusually large feature declarations and is never labelled a public capture.
The array representation used by held-tree comparisons exists only inside the harness.
Every emitted Cargo document remains LF-delimited native records.

HTTP load fixtures derive relay responses from these captures.
They replace only each checksum with the hash of owned synthetic relay bytes.
Each replacement preserves the source row's byte width and leaves every other source field unchanged.
Private-copy setup reads both actual indices and checks each target archive's bytes before recording its source inventory.
These relay fixtures do not prove native client installation.
The separate worker fixture uses a valid owned crate archive and native upload framing.
The warm-under-cold row changes a public additive field because unmanaged private fields do not control Cargo's public view.
