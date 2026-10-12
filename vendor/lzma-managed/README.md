# Managed lzma decoder

This local package retains the lzma 0.0.1.1 implementation and its BSD-3-Clause licence.
The package name is `ecluse-lzma-managed`. Its modules retain their upstream names.

- Upstream archive: <https://hackage.haskell.org/package/lzma-0.0.1.1/lzma-0.0.1.1.tar.gz>
- Archive SHA-256: `80a5278291a2ce114f0395733ee3a0cfbe106aa748e3e4b19b07982392eadcf1`
- Upstream repository: <https://github.com/hvr/lzma>
- Copyright: Herbert Valerio Riedel, 2015.
- Advisory identity: [upstream.freeze](upstream.freeze) retains `lzma ==0.0.1.1` for scanning.

`withDecompressIO` closes decoder state when its callback ends, including exceptions and cancellation.
The callback must not retain the decoder stream. Decoder initialisation closes failed allocations.

This private fork retains the upstream IO decode loop, decoder parameters, buffer size and native ABI.
It removes compression, lazy/ST convenience APIs and the unscoped decoder entry point.
Decoder stream fields are strict. Forcing a continuation does not execute its IO action.
The existing return-code parser remains total through its unknown-code case.
The removed compression-level Enum was the only partial enum conversion.
Headers and obsolete API comments are shortened to match the remaining decoder surface.
A redundant bang on the IO finalisation result is removed without changing when finalisation runs.

The native C wrapper and licence are byte-identical to the pinned upstream archive.
First-party archive wrappers own byte bounds, accounting, metadata selection and refusal behaviour.
Unit and residency fixtures compress with the separate upstream `lzma ==0.0.1.1` package and its defaults.
Package-qualified imports distinguish fixture compression from this managed decoder.
Both packages contain the same native wrapper symbols, so CI must link the unit and residency executables.

The native residency negative control opens eight nested managed scopes on one thread.
Each scope reaches nonempty decoder output before the next opens.
The baseline precedes native allocation, and the final sample runs with all eight scopes still open, before cleanup.
The interval requires zero GC and must exceed the same independent 1 MiB VM gate as the positive probes.
