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
The existing decoder loop, compression API, native wrapper and licence remain upstream-owned.
One inherited comment uses the repository's current terminology.

The native C wrapper and licence are unchanged. First-party archive wrappers and their recurring tests
own byte bounds, accounting, metadata selection and refusal behaviour.
